// ─── Team / Entry Types ───────────────────────────────────────────────────────

export interface Team {
  team_name: string;
  seed: number;
  cost: number;
}

export interface BracketEntry {
  Entry: string;
  Team1: string;
  Team2: string;
  Team3: string;
  Team4: string;
  Team5: string;
  Team6: string;
  Team7: string;
  Team8: string;
  Team9: string;
  Team10: string;
  Team11: string;
  Team12: string;
  Team13: string;
  Team14: string;
  Team15: string;
  Team16: string;
  Team17: string;
  Team18: string;
  Team19: string;
  Team20: string;
  Team21: string;
  Team22: string;
  Team23: string;
  Team24: string;
  [key: string]: string;
}

export interface SubmissionTotals {
  entry_name: string;
  tiebreaker_points: number;
}

// ─── ESPN API Types ───────────────────────────────────────────────────────────

export type GameStatus = "scheduled" | "in-progress" | "halftime" | "final" | "uncontested" | "unknown";

export interface EspnTeamInfo {
  id: string;
  name: string;
  shortDisplayName: string;
  abbreviation: string;
  logo: string;
  seed: number;
}

export interface EspnGame {
  id: string;
  name: string;
  shortName: string;
  homeTeam: EspnTeamInfo;
  awayTeam: EspnTeamInfo;
  homeScore: number | null;
  awayScore: number | null;
  status: GameStatus;
  clock: string;
  period: number;
  venue: string;
  isFirstFour: boolean;
}

// ─── Standings Types ──────────────────────────────────────────────────────────

export interface StandingsRow {
  entry_name: string;
  wins: number;
  live_wins: number;
  tiebreaker_points: number;
  teams: StandingsTeamSlot[];
}

export interface StandingsTeamSlot {
  teamName: string;
  opacity: number;
  logoUrl: string;
  seed: number;
  isPlaying: boolean;
}

// ─── Live Scores Types ────────────────────────────────────────────────────────

export interface LiveScoreGame {
  id: string;
  name: string;
  shortName: string;
  homeTeamName: string;
  homeTeamAbbr: string;
  homeTeamLogo: string;
  homeTeamSeed: number;
  homeScore: number | null;
  awayTeamName: string;
  awayTeamAbbr: string;
  awayTeamLogo: string;
  awayTeamSeed: number;
  awayScore: number | null;
  status: GameStatus;
  clock: string;
  period: number;
  periodDisplay: string;
  homeWinProbability: number | null;
}

// ─── Entry Submission Types ───────────────────────────────────────────────────

export interface EntrySubmission {
  entryName: string;
  email: string;
  selectedTeams: string[];
  tiebreakerPoints: number;
}

export interface SubmissionResult {
  success: boolean;
  entryId?: string;
  message?: string;
  error?: string;
}

// ─── API Response Types ───────────────────────────────────────────────────────

export interface TeamsApiResponse {
  teams: Team[];
}

export interface StandingsApiResponse {
  standings: StandingsRow[];
  lastUpdated: string;
  error?: string;
}

export interface LiveScoresApiResponse {
  games: LiveScoreGame[];
  lastUpdated: string;
  error?: string;
}

// ─── Seed Cost Map ────────────────────────────────────────────────────────────

export const SEED_COSTS: Record<number, number> = {
  1: 25,
  2: 19,
  3: 13,
  4: 12,
  5: 11,
  6: 10,
  7: 8,
  8: 5,
  9: 5,
  10: 4,
  11: 4,
  12: 3,
  13: 2,
  14: 2,
  15: 1,
  16: 1,
};

export const BUDGET_CAP = 100;
