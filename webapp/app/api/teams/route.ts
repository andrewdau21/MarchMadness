import { NextResponse } from "next/server";
import { query } from "@/lib/db";
import type { Team, TeamsApiResponse } from "@/lib/types";

export const dynamic = 'force-dynamic';

export async function GET(): Promise<NextResponse<TeamsApiResponse>> {
  try {
    const teams = await query<Team>(
      "SELECT team_name, seed, cost FROM march_madness_teams ORDER BY seed ASC, team_name ASC"
    );
    return NextResponse.json({ teams });
  } catch (err) {
    console.error("GET /api/teams error:", err);
    return NextResponse.json(
      { teams: [], error: "Failed to load teams" } as TeamsApiResponse & { error: string },
      { status: 500 }
    );
  }
}
