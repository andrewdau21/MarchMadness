import { NextResponse } from "next/server";
import { computeStandings } from "@/lib/standings";
import type { StandingsApiResponse } from "@/lib/types";

export const dynamic = 'force-dynamic';

// Server-side cache — reuse the last result for 60 seconds so row clicks
// and rapid navigation don't hammer ESPN + the DB on every request.
let cache: { data: StandingsApiResponse; expiresAt: number } | null = null;

export async function GET(): Promise<NextResponse<StandingsApiResponse>> {
  const now = Date.now();
  if (cache && now < cache.expiresAt) {
    return NextResponse.json(cache.data);
  }

  try {
    const standings = await computeStandings();
    const payload: StandingsApiResponse = {
      standings,
      lastUpdated: new Date().toISOString(),
    };
    cache = { data: payload, expiresAt: now + 60_000 };
    return NextResponse.json(payload);
  } catch (err) {
    console.error("GET /api/standings error:", err);
    // Return stale cache if available rather than an empty error
    if (cache) return NextResponse.json(cache.data);
    return NextResponse.json(
      { standings: [], lastUpdated: new Date().toISOString(), error: "Failed to compute standings" },
      { status: 500 }
    );
  }
}
