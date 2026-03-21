/**
 * ESPN API integration layer.
 *
 * Fetches scoreboard and game summary data from the public ESPN API and
 * normalises it into the app's internal types.
 */

import type { EspnGame, GameStatus, LiveScoreGame } from "./types";

// ─── Constants ────────────────────────────────────────────────────────────────

const ESPN_BASE = "http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball";

/**
 * Build the scoreboard URL for the current tournament year.
 * Tournament runs from roughly March 19 – April 14 each year.
 */
function getScoreboardUrl(date?: string): string {
  const year = new Date().getFullYear();
  const dateParam = date ?? `${year}0319-${year}0414`;
  return `${ESPN_BASE}/scoreboard?lang=en&region=us&limit=50&dates=${dateParam}&groups=100`;
}

function todayESPN(): string {
  // Use Pacific time — games don't roll over to the next slate until noon PT
  const parts = new Intl.DateTimeFormat("en-US", {
    timeZone: "America/Los_Angeles",
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    hour12: false,
  }).formatToParts(new Date());

  const get = (type: string) => parts.find(p => p.type === type)?.value ?? "0";
  let year = parseInt(get("year"));
  let month = parseInt(get("month"));
  let day = parseInt(get("day"));
  const hour = parseInt(get("hour"));

  // Before noon PT, still show the previous day's games
  if (hour < 12) {
    const prev = new Date(year, month - 1, day - 1);
    year = prev.getFullYear();
    month = prev.getMonth() + 1;
    day = prev.getDate();
  }

  return `${year}${String(month).padStart(2, "0")}${String(day).padStart(2, "0")}`;
}

function getSummaryUrl(gameId: string): string {
  return `${ESPN_BASE}/summary?event=${gameId}`;
}

// ─── Status normalisation ─────────────────────────────────────────────────────

function normaliseStatus(description: string): GameStatus {
  const d = description.toLowerCase();
  if (d === "final") return "final";
  if (d === "halftime") return "halftime";
  if (d.includes("progress") || d.includes("live")) return "in-progress";
  if (d === "scheduled") return "scheduled";
  if (d === "uncontested") return "uncontested";
  return "unknown";
}

/**
 * Build a human-readable period string matching the R app logic.
 */
function buildPeriodDisplay(
  status: GameStatus,
  period: number,
  clock: string,
  scheduledDetail: string
): string {
  if (status === "final") return "Final";
  if (status === "halftime") return "Halftime";
  if (status === "scheduled") return scheduledDetail || "Scheduled";
  if (period === 1) return `1st ${clock}`;
  if (period === 2) return `2nd ${clock}`;
  if (period === 3) return "OT-1";
  if (period >= 4) return `OT-${period - 2}`;
  return clock || "—";
}

// ─── ESPN Scoreboard ──────────────────────────────────────────────────────────

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function extractTeamInfo(competitor: any, index: number) {
  const team = competitor?.team ?? {};
  return {
    id: competitor?.id ?? "",
    name: team?.displayName ?? team?.shortDisplayName ?? "",
    shortDisplayName: team?.shortDisplayName ?? team?.displayName ?? "",
    abbreviation: team?.abbreviation ?? "",
    logo: team?.logo ?? "",
    seed: competitor?.curatedRank?.current ?? index + 1,
  };
}

/**
 * Fetch and parse the ESPN tournament scoreboard.
 * Filters out First Four play-in games (notes containing "First Four").
 */
export async function fetchTodayScoreboard(): Promise<EspnGame[]> {
  return fetchScoreboard(todayESPN());
}

export async function fetchScoreboard(date?: string): Promise<EspnGame[]> {
  const url = getScoreboardUrl(date);
  const res = await fetch(url, { next: { revalidate: 60 } });
  if (!res.ok) {
    throw new Error(`ESPN scoreboard fetch failed: ${res.status}`);
  }

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const data: any = await res.json();
  const events: unknown[] = data?.events ?? [];
  const games: EspnGame[] = [];

  for (const event of events) {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const e = event as any;
    const competition = e?.competitions?.[0];
    if (!competition) continue;

    // Filter out First Four play-in games
    const notes: string = (competition?.notes ?? [])
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      .map((n: any) => n?.text ?? "")
      .join(" ")
      .toLowerCase();
    const isFirstFour = notes.includes("first four");

    const competitors: unknown[] = competition?.competitors ?? [];
    // ESPN puts home team first in competitors array
    const homeComp = competitors[0];
    const awayComp = competitors[1];

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const homeTeam = extractTeamInfo(homeComp as any, 0);
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const awayTeam = extractTeamInfo(awayComp as any, 1);

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const statusDesc: string = (e?.status as any)?.type?.description ?? "Scheduled";
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const statusDetail: string = (e?.status as any)?.type?.detail ?? "";
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const clock: string = (e?.status as any)?.displayClock ?? "0:00";
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const period: number = (e?.status as any)?.period ?? 0;

    const status = normaliseStatus(statusDesc);
    const periodDisplay = buildPeriodDisplay(status, period, clock, statusDetail);

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const homeScore = (homeComp as any)?.score != null ? parseFloat((homeComp as any).score) : null;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const awayScore = (awayComp as any)?.score != null ? parseFloat((awayComp as any).score) : null;

    games.push({
      id: e?.id ?? "",
      name: e?.name ?? "",
      shortName: e?.shortName ?? "",
      homeTeam,
      awayTeam,
      homeScore,
      awayScore,
      status,
      clock,
      period,
      statusDetail,
      venue: competition?.venue?.fullName ?? "",
      isFirstFour,
    });
  }

  return games;
}

// ─── ESPN Game Summary (win probability) ─────────────────────────────────────

/**
 * Fetch win probability for a single in-progress game.
 * Returns the home win percentage (0–1) or null if unavailable.
 */
export async function fetchWinProbability(gameId: string): Promise<number | null> {
  try {
    const url = getSummaryUrl(gameId);
    const res = await fetch(url, { cache: "no-store" });
    if (!res.ok) return null;

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const data: any = await res.json();

    // Primary path: competitions[0].situation.lastPlay.probability.homeWinPercentage
    const prob =
      data?.competitions?.[0]?.situation?.lastPlay?.probability?.homeWinPercentage ??
      // Fallback: winprobability array (last element)
      (Array.isArray(data?.winprobability) && data.winprobability.length > 0
        ? data.winprobability[data.winprobability.length - 1]?.homeWinPercentage
        : null);

    if (prob == null) return null;
    return typeof prob === "number" ? prob : parseFloat(prob);
  } catch {
    return null;
  }
}

// ─── Live Score Shape ─────────────────────────────────────────────────────────

/**
 * Convert EspnGame list (with optional win prob already attached) into the
 * LiveScoreGame shape consumed by the UI.
 */
export function toLiveScoreGames(
  games: EspnGame[],
  probMap: Map<string, number | null>
): LiveScoreGame[] {
  return games
    .filter((g) => !g.isFirstFour)
    .map((g) => {
      const homeWinProbability = probMap.get(g.id) ?? null;
      const periodDisplay = buildPeriodDisplay(g.status, g.period, g.clock, g.statusDetail);

      return {
        id: g.id,
        name: g.name,
        shortName: g.shortName,
        homeTeamName: g.homeTeam.shortDisplayName,
        homeTeamAbbr: g.homeTeam.abbreviation,
        homeTeamLogo: g.homeTeam.logo,
        homeTeamSeed: g.homeTeam.seed,
        homeScore: g.homeScore,
        awayTeamName: g.awayTeam.shortDisplayName,
        awayTeamAbbr: g.awayTeam.abbreviation,
        awayTeamLogo: g.awayTeam.logo,
        awayTeamSeed: g.awayTeam.seed,
        awayScore: g.awayScore,
        status: g.status,
        clock: g.clock,
        period: g.period,
        periodDisplay,
        homeWinProbability,
      };
    });
}
