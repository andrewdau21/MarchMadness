import { NextResponse } from "next/server";
import { fetchTodayScoreboard, fetchWinProbability, toLiveScoreGames } from "@/lib/espn";
import { query } from "@/lib/db";
import type { LiveScoresApiResponse } from "@/lib/types";

export const dynamic = 'force-dynamic';

interface DbEntry {
  Entry: string;
  [key: string]: string;
}

export async function GET(): Promise<NextResponse<LiveScoresApiResponse>> {
  try {
    // Fetch scoreboard
    const games = await fetchTodayScoreboard();

    // Fetch win probabilities for in-progress games
    const inProgressGames = games.filter(
      (g) => !g.isFirstFour && (g.status === "in-progress" || g.status === "halftime")
    );

    const probEntries = await Promise.all(
      inProgressGames.map(async (g) => {
        const prob = await fetchWinProbability(g.id);
        return [g.id, prob] as [string, number | null];
      })
    );
    const probMap = new Map<string, number | null>(probEntries);

    const liveGames = toLiveScoreGames(games, probMap);

    // Sort: in-progress first, then scheduled, then final
    const statusOrder: Record<string, number> = {
      "in-progress": 0,
      halftime: 1,
      scheduled: 2,
      uncontested: 3,
      final: 4,
      unknown: 5,
    };
    liveGames.sort(
      (a, b) => (statusOrder[a.status] ?? 5) - (statusOrder[b.status] ?? 5)
    );

    return NextResponse.json({
      games: liveGames,
      lastUpdated: new Date().toISOString(),
    });
  } catch (err) {
    console.error("GET /api/live-scores error:", err);
    return NextResponse.json(
      {
        games: [],
        lastUpdated: new Date().toISOString(),
        error: "Live scores temporarily unavailable",
      },
      { status: 500 }
    );
  }
}

/**
 * GET /api/live-scores/entries?team=TeamName
 * Returns count + list of entries that selected a given team.
 * Used by the live scores modal.
 */
export async function POST(request: Request): Promise<NextResponse> {
  try {
    const body = await request.json();
    const teamName: string = body?.teamName ?? "";

    if (!teamName) {
      return NextResponse.json({ count: 0, entries: [] });
    }

    // Query bracket_entries5 for all entries that contain this team in any slot
    const conditions = Array.from({ length: 24 }, (_, i) => `Team${i + 1} = $1`).join(
      " OR "
    );
    const rows = await query<DbEntry>(
      `SELECT DISTINCT "Entry" FROM bracket_entries5 WHERE ${conditions}`,
      [teamName]
    );

    const entries = rows.map((r) => r.Entry);
    return NextResponse.json({ count: entries.length, entries });
  } catch (err) {
    console.error("POST /api/live-scores error:", err);
    return NextResponse.json({ count: 0, entries: [], error: "Failed to fetch entries" }, { status: 500 });
  }
}
