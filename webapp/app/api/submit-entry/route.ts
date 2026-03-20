/**
 * POST /api/submit-entry
 *
 * Accepts an entry submission, validates it, and sends a confirmation email.
 * NO DATABASE WRITES – entries are submitted via email only.
 */

import { NextResponse } from "next/server";
import { v4 as uuidv4 } from "uuid";
import { query } from "@/lib/db";
import { sendEntryConfirmationEmail } from "@/lib/email";
import type { EntrySubmission, SubmissionResult, Team } from "@/lib/types";
import { BUDGET_CAP, SEED_COSTS } from "@/lib/types";

// Basic email regex
const EMAIL_RE = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;

export async function POST(request: Request): Promise<NextResponse<SubmissionResult>> {
  let body: Partial<EntrySubmission>;

  try {
    body = await request.json();
  } catch {
    return NextResponse.json(
      { success: false, error: "Invalid request body" },
      { status: 400 }
    );
  }

  const { entryName, email, selectedTeams, tiebreakerPoints } = body;

  // ─── Validation ─────────────────────────────────────────────────────────────
  if (!entryName || typeof entryName !== "string" || entryName.trim().length === 0) {
    return NextResponse.json({ success: false, error: "Entry name is required" }, { status: 400 });
  }

  if (!email || !EMAIL_RE.test(email)) {
    return NextResponse.json({ success: false, error: "Valid email address is required" }, { status: 400 });
  }

  if (!Array.isArray(selectedTeams) || selectedTeams.length === 0) {
    return NextResponse.json({ success: false, error: "At least one team must be selected" }, { status: 400 });
  }

  if (
    tiebreakerPoints == null ||
    typeof tiebreakerPoints !== "number" ||
    tiebreakerPoints <= 0 ||
    !Number.isFinite(tiebreakerPoints)
  ) {
    return NextResponse.json(
      { success: false, error: "Tiebreaker must be a positive number" },
      { status: 400 }
    );
  }

  // ─── Load team data from DB (READ-ONLY) ────────────────────────────────────
  let allTeams: Team[];
  try {
    allTeams = await query<Team>(
      "SELECT team_name, seed, cost FROM march_madness_teams"
    );
  } catch (err) {
    console.error("DB read error in submit-entry:", err);
    return NextResponse.json(
      { success: false, error: "Could not validate teams – try again" },
      { status: 500 }
    );
  }

  const teamMap = new Map<string, Team>(allTeams.map((t) => [t.team_name, t]));

  // Validate selected teams exist and compute total cost
  const resolvedTeams: { teamName: string; seed: number; cost: number }[] = [];
  for (const name of selectedTeams) {
    const team = teamMap.get(name);
    if (!team) {
      return NextResponse.json(
        { success: false, error: `Unknown team: ${name}` },
        { status: 400 }
      );
    }
    resolvedTeams.push({ teamName: team.team_name, seed: team.seed, cost: team.cost });
  }

  const totalCost = resolvedTeams.reduce((sum, t) => sum + t.cost, 0);
  if (totalCost > BUDGET_CAP) {
    return NextResponse.json(
      { success: false, error: `Total cost $${totalCost} exceeds the $${BUDGET_CAP} salary cap` },
      { status: 400 }
    );
  }

  // ─── Send email (NO DB WRITE) ───────────────────────────────────────────────
  const entryId = uuidv4();
  const submission: EntrySubmission = {
    entryName: entryName.trim(),
    email: email.trim().toLowerCase(),
    selectedTeams,
    tiebreakerPoints,
  };

  try {
    await sendEntryConfirmationEmail(submission, resolvedTeams, totalCost, entryId);
  } catch (err) {
    console.error("Email send error:", err);
    return NextResponse.json(
      { success: false, error: "Failed to send confirmation email – please try again" },
      { status: 500 }
    );
  }

  return NextResponse.json({ success: true, entryId });
}
