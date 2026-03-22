/**
 * Standings calculation logic – TypeScript port of standings_function.R
 *
 * Algorithm:
 * 1. Fetch ESPN scoreboard for the full tournament date range.
 * 2. For FINAL games  → count 1 win per winning team.
 * 3. For UNCONTESTED  → count 0.5 wins (halved, as in the R code).
 * 4. For IN-PROGRESS / HALFTIME → fetch win probability for each game from the
 *    summary endpoint; use homeWinProbability as fractional home win and
 *    (1 - homeWinProbability) as fractional away win.
 * 5. Join results with bracket_entries5 DB table.
 * 6. Sum wins per entry.
 * 7. Join tiebreaker from submission_totals.
 * 8. Sort: total wins DESC, tiebreaker DESC.
 * 9. For each team slot in each entry, compute opacity:
 *      0.1  if the team has lost at least once AND is not currently playing
 *      1.0  otherwise
 */

import { query } from "./db";
import { fetchScoreboard, fetchWinProbability } from "./espn";
import type { StandingsRow, StandingsTeamSlot } from "./types";

// ─── DB Queries ───────────────────────────────────────────────────────────────

interface DbBracketEntry {
  Entry: string;
  Team1: string; Team2: string; Team3: string; Team4: string;
  Team5: string; Team6: string; Team7: string; Team8: string;
  Team9: string; Team10: string; Team11: string; Team12: string;
  Team13: string; Team14: string; Team15: string; Team16: string;
  Team17: string; Team18: string; Team19: string; Team20: string;
  Team21: string; Team22: string; Team23: string; Team24: string;
  [key: string]: string;
}

interface DbTiebreaker {
  entry_name: string;
  tiebreaker_points: number;
}

interface DbMasterTeam {
  name: string;
  // ESPN team ID used to build logo URL
  espn_id?: string | null;
  logo?: string | null;
  seed?: number | null;
}

async function fetchBracketEntries(): Promise<DbBracketEntry[]> {
  return query<DbBracketEntry>('SELECT * FROM bracket_entries5');
}

async function fetchTiebreakers(): Promise<DbTiebreaker[]> {
  return query<DbTiebreaker>(
    'SELECT entry_name, tiebreaker_points FROM submission_totals'
  );
}

async function fetchMasterTeams(): Promise<DbMasterTeam[]> {
  return query<DbMasterTeam>('SELECT DISTINCT * FROM all_teams');
}

interface DbTeamCost {
  team_name: string;
  cost: number;
}

async function fetchTeamCosts(): Promise<Map<string, number>> {
  const rows = await query<DbTeamCost>('SELECT team_name, cost FROM march_madness_teams');
  return new Map(rows.map(r => [r.team_name, r.cost]));
}

// ─── Win / Loss Tracking ──────────────────────────────────────────────────────

interface TeamRecord {
  wins: number;        // completed wins
  liveWins: number;    // fractional wins from in-progress games
  losses: number;      // completed losses
  isPlaying: boolean;  // currently in an in-progress/halftime game
}

// ─── Main Standings Function ──────────────────────────────────────────────────

export async function computeStandings(): Promise<StandingsRow[]> {
  // Fetch everything in parallel where possible
  const [bracketEntries, tiebreakers, masterTeams, games, teamCostMap] = await Promise.all([
    fetchBracketEntries(),
    fetchTiebreakers(),
    fetchMasterTeams(),
    fetchScoreboard(),
    fetchTeamCosts(),
  ]);

  // Build a map: team shortDisplayName → master record (for logos / seeds)
  const masterByName = new Map<string, DbMasterTeam>();
  for (const t of masterTeams) {
    if (t.name) masterByName.set(t.name, t);
  }

  // Build ESPN team ID → canonical DB name mapping (resolves name mismatches)
  const espnIdToDbName = new Map<string, string>();
  for (const t of masterTeams) {
    if (t.espn_id && t.name) espnIdToDbName.set(t.espn_id, t.name);
  }

  // Helper: resolve ESPN team info to the canonical DB team name
  function resolveEspnName(team: { id: string; shortDisplayName: string }): string {
    return espnIdToDbName.get(team.id) ?? team.shortDisplayName;
  }

  // Track which games are in-progress so we can fetch win probabilities
  const inProgressGameIds: string[] = games
    .filter((g) => !g.isFirstFour && (g.status === "in-progress" || g.status === "halftime"))
    .map((g) => g.id);

  // Fetch win probabilities for all in-progress games concurrently
  const probEntries = await Promise.all(
    inProgressGameIds.map(async (id) => {
      const prob = await fetchWinProbability(id);
      return [id, prob] as [string, number | null];
    })
  );
  const probMap = new Map<string, number | null>(probEntries);

  // Build team record map
  const teamRecords = new Map<string, TeamRecord>();

  function getRecord(teamName: string): TeamRecord {
    if (!teamRecords.has(teamName)) {
      teamRecords.set(teamName, { wins: 0, liveWins: 0, losses: 0, isPlaying: false });
    }
    return teamRecords.get(teamName)!;
  }

  for (const game of games) {
    if (game.isFirstFour) continue;

    const homeTeam = resolveEspnName(game.homeTeam);
    const awayTeam = resolveEspnName(game.awayTeam);

    if (game.status === "final") {
      const homeScore = game.homeScore ?? 0;
      const awayScore = game.awayScore ?? 0;
      if (homeScore >= awayScore) {
        getRecord(homeTeam).wins += 1;
        getRecord(awayTeam).losses += 1;
      } else {
        getRecord(awayTeam).wins += 1;
        getRecord(homeTeam).losses += 1;
      }
    } else if (game.status === "uncontested") {
      // R code uses wins/2 for uncontested
      const homeScore = game.homeScore ?? 0;
      const awayScore = game.awayScore ?? 0;
      if (homeScore >= awayScore) {
        getRecord(homeTeam).wins += 0.5;
        getRecord(awayTeam).losses += 0.5;
      } else {
        getRecord(awayTeam).wins += 0.5;
        getRecord(homeTeam).losses += 0.5;
      }
    } else if (game.status === "in-progress" || game.status === "halftime") {
      const homeProb = probMap.get(game.id) ?? 0.5;
      getRecord(homeTeam).liveWins += homeProb;
      getRecord(awayTeam).liveWins += 1 - homeProb;
      getRecord(homeTeam).isPlaying = true;
      getRecord(awayTeam).isPlaying = true;
    }
  }

  // Build a lookup: team name → logo URL + seed
  // Prefer master DB data; fall back to ESPN scoreboard data
  const teamLogoMap = new Map<string, string>();
  const teamSeedMap = new Map<string, number>();

  for (const t of masterTeams) {
    if (t.name) {
      if (t.logo) teamLogoMap.set(t.name, t.logo);
      if (t.seed != null) teamSeedMap.set(t.name, t.seed);
    }
  }

  // Also populate from ESPN scoreboard (for teams not in master)
  for (const game of games) {
    if (game.isFirstFour) continue;
    const home = game.homeTeam;
    const away = game.awayTeam;
    const homeName = resolveEspnName(home);
    const awayName = resolveEspnName(away);
    if (!teamLogoMap.has(homeName) && home.logo) teamLogoMap.set(homeName, home.logo);
    if (!teamSeedMap.has(homeName)) teamSeedMap.set(homeName, home.seed);
    if (!teamLogoMap.has(awayName) && away.logo) teamLogoMap.set(awayName, away.logo);
    if (!teamSeedMap.has(awayName)) teamSeedMap.set(awayName, away.seed);
  }

  // Build tiebreaker map
  const tiebreakerMap = new Map<string, number>();
  for (const tb of tiebreakers) {
    tiebreakerMap.set(tb.entry_name, tb.tiebreaker_points);
  }

  // Build standings rows
  const TEAM_SLOTS = 24;
  const rows: StandingsRow[] = bracketEntries.map((entry) => {
    let wins = 0;
    let liveWins = 0;

    const teamSlots: StandingsTeamSlot[] = [];

    for (let i = 1; i <= TEAM_SLOTS; i++) {
      const teamName: string = (entry[`Team${i}`] as string) ?? "";

      const record = teamRecords.get(teamName);
      const teamWins = record?.wins ?? 0;
      const teamLiveWins = record?.liveWins ?? 0;
      const teamLosses = record?.losses ?? 0;
      const isPlaying = record?.isPlaying ?? false;

      wins += teamWins;
      // In the R code: live_wins = live_wins_sum + wins (cumulative), but we
      // keep them separate for display and add them in the UI.
      liveWins += teamLiveWins;

      // Opacity: faded if team has lost at least once AND is not currently playing
      const isEliminated = teamLosses > 0 && !isPlaying;
      const opacity = isEliminated ? 0.1 : 1.0;

      const logoUrl = teamLogoMap.get(teamName) ?? "";
      const seed = teamSeedMap.get(teamName) ?? 0;

      const cost = teamCostMap.get(teamName) ?? 0;
      teamSlots.push({ teamName, opacity, logoUrl, seed, isPlaying, cost });
    }

    const tiebreaker_points = tiebreakerMap.get(entry.Entry) ?? 0;

    return {
      entry_name: entry.Entry,
      wins,
      live_wins: wins + liveWins, // match R: live_wins = cumulative (wins + fractional)
      tiebreaker_points,
      teams: teamSlots,
    };
  });

  // Sort: total wins DESC, tiebreaker DESC
  rows.sort((a, b) => {
    const aTotal = a.live_wins;
    const bTotal = b.live_wins;
    if (bTotal !== aTotal) return bTotal - aTotal;
    return b.tiebreaker_points - a.tiebreaker_points;
  });

  return rows;
}
