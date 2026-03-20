import { NextResponse } from "next/server";
import { computeStandings } from "@/lib/standings";
import type { StandingsApiResponse } from "@/lib/types";

export const dynamic = 'force-dynamic';

export async function GET(): Promise<NextResponse<StandingsApiResponse>> {
  try {
    const standings = await computeStandings();
    return NextResponse.json({
      standings,
      lastUpdated: new Date().toISOString(),
    });
  } catch (err) {
    console.error("GET /api/standings error:", err);
    return NextResponse.json(
      {
        standings: [],
        lastUpdated: new Date().toISOString(),
        error: "Failed to compute standings",
      },
      { status: 500 }
    );
  }
}
