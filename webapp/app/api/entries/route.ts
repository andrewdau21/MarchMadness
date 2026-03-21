import { NextResponse } from "next/server";
import { query } from "@/lib/db";

export const dynamic = 'force-dynamic';

export async function GET(request: Request): Promise<NextResponse> {
  const apiKey = request.headers.get("x-api-key");
  if (!apiKey || apiKey !== process.env.ENTRIES_API_KEY) {
    return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
  }

  try {
    const rows = await query(`
      SELECT
        b."Entry"      AS entry_name,
        s.tiebreaker_points,
        s.email,
        b."Team1",  b."Team2",  b."Team3",  b."Team4",
        b."Team5",  b."Team6",  b."Team7",  b."Team8",
        b."Team9",  b."Team10", b."Team11", b."Team12",
        b."Team13", b."Team14", b."Team15", b."Team16",
        b."Team17", b."Team18", b."Team19", b."Team20",
        b."Team21", b."Team22", b."Team23", b."Team24"
      FROM bracket_entries5 b
      LEFT JOIN submission_totals s
        ON b."Entry" = s.entry_name
      ORDER BY b."Entry" ASC
    `);

    return NextResponse.json({ entries: rows, count: rows.length });
  } catch (err) {
    console.error("GET /api/entries error:", err);
    return NextResponse.json(
      { entries: [], count: 0, error: "Failed to load entries" },
      { status: 500 }
    );
  }
}
