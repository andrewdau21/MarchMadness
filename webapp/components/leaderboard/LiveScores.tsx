"use client";

import { useState } from "react";
import { useQuery } from "@tanstack/react-query";
import type { LiveScoreGame, LiveScoresApiResponse } from "@/lib/types";

// ─── Team Entry Modal ─────────────────────────────────────────────────────────

function TeamEntryModal({
  game,
  onClose,
}: {
  game: LiveScoreGame;
  onClose: () => void;
}) {
  const [homeData, setHomeData] = useState<{ count: number; entries: string[] } | null>(null);
  const [awayData, setAwayData] = useState<{ count: number; entries: string[] } | null>(null);
  const [loading, setLoading] = useState(false);

  async function loadEntries() {
    setLoading(true);
    try {
      const [homeRes, awayRes] = await Promise.all([
        fetch("/api/live-scores", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ teamName: game.homeTeamName }),
        }),
        fetch("/api/live-scores", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ teamName: game.awayTeamName }),
        }),
      ]);
      const [hd, ad] = await Promise.all([homeRes.json(), awayRes.json()]);
      setHomeData(hd);
      setAwayData(ad);
    } finally {
      setLoading(false);
    }
  }

  // Load on first render
  useState(() => {
    loadEntries();
  });

  return (
    <div
      className="fixed inset-0 z-50 flex items-center justify-center p-4"
      style={{ background: "rgba(0,0,0,0.75)" }}
      onClick={onClose}
    >
      <div
        className="mc-card w-full max-w-md rounded-2xl overflow-hidden"
        onClick={(e) => e.stopPropagation()}
      >
        {/* Header */}
        <div
          className="px-5 py-4 border-b flex items-center justify-between"
          style={{ borderColor: "var(--border)" }}
        >
          <h3 className="font-semibold text-sm" style={{ color: "var(--text)" }}>
            {game.shortName}
          </h3>
          <button
            onClick={onClose}
            className="text-lg leading-none"
            style={{ color: "var(--text-muted)" }}
            aria-label="Close"
          >
            ×
          </button>
        </div>

        {/* Body */}
        <div className="p-5 space-y-5">
          {loading ? (
            <div className="flex justify-center py-4">
              <div
                className="w-6 h-6 rounded-full border-2 border-t-transparent animate-spin"
                style={{ borderColor: "var(--accent)", borderTopColor: "transparent" }}
              />
            </div>
          ) : (
            <>
              {[
                { team: game.homeTeamName, data: homeData },
                { team: game.awayTeamName, data: awayData },
              ].map(({ team, data }) => (
                <div key={team}>
                  <p className="text-sm font-semibold mb-1" style={{ color: "var(--text)" }}>
                    <span style={{ color: "var(--accent)" }}>{data?.count ?? 0}</span> entr
                    {data?.count === 1 ? "y" : "ies"} selected{" "}
                    <span className="font-normal" style={{ color: "var(--text-muted)" }}>
                      {team}
                    </span>
                  </p>
                  {data?.entries && data.entries.length > 0 && (
                    <p className="text-xs" style={{ color: "var(--text-muted)" }}>
                      {data.entries.join(", ")}
                    </p>
                  )}
                </div>
              ))}
            </>
          )}
        </div>
      </div>
    </div>
  );
}

// ─── Status Badge ─────────────────────────────────────────────────────────────

function StatusBadge({ status, periodDisplay }: { status: string; periodDisplay: string }) {
  if (status === "in-progress" || status === "halftime") {
    return (
      <span className="flex items-center gap-1.5 font-semibold text-xs" style={{ color: "var(--accent)" }}>
        <span
          className="inline-block w-2 h-2 rounded-full live-dot"
          style={{ background: "var(--accent)" }}
        />
        {periodDisplay}
      </span>
    );
  }
  if (status === "final") {
    return <span className="text-xs font-medium" style={{ color: "var(--text-muted)" }}>Final</span>;
  }
  return <span className="text-xs" style={{ color: "var(--text-muted)" }}>{periodDisplay}</span>;
}

// ─── Score Row ────────────────────────────────────────────────────────────────

function ScoreRow({
  game,
  onClick,
}: {
  game: LiveScoreGame;
  onClick: () => void;
}) {
  const homePct = game.homeWinProbability != null
    ? Math.round(game.homeWinProbability * 100)
    : null;
  const awayPct = homePct != null ? 100 - homePct : null;

  return (
    <tr
      className="cursor-pointer hover:bg-white/5 transition-colors"
      onClick={onClick}
      style={{ borderBottom: "1px solid var(--border)" }}
    >
      {/* Home team */}
      <td className="px-2 sm:px-4 py-2.5">
        <div className="flex items-center gap-1.5">
          <div className="relative w-8 h-8 shrink-0">
            {game.homeTeamLogo ? (
              // eslint-disable-next-line @next/next/no-img-element
              <img src={game.homeTeamLogo} alt={game.homeTeamName} width={32} height={32}
                className="object-contain w-8 h-8" loading="lazy" />
            ) : (
              <div className="w-8 h-8 rounded-full flex items-center justify-center text-xs font-bold"
                style={{ background: "var(--muted)", color: "var(--text-muted)" }}>
                {game.homeTeamAbbr?.slice(0, 3)}
              </div>
            )}
            <span className="absolute -top-1 -left-1 font-bold rounded px-0.5"
              style={{ background: "var(--muted)", color: "var(--text-muted)", fontSize: "8px", lineHeight: "13px" }}>
              {game.homeTeamSeed}
            </span>
          </div>
          <div className="min-w-0">
            <p className="font-medium truncate hidden sm:block text-sm" style={{ color: "var(--text)" }}>
              {game.homeTeamName}
            </p>
            <p className="font-medium text-xs sm:hidden" style={{ color: "var(--text)" }}>
              {game.homeTeamAbbr}
            </p>
            {homePct != null && (game.status === "in-progress" || game.status === "halftime") && (
              <p className="text-xs hidden sm:block" style={{ color: "var(--accent)" }}>{homePct}%</p>
            )}
          </div>
        </div>
      </td>

      {/* Score / status */}
      <td className="px-1 py-2.5 text-center whitespace-nowrap">
        <div className="flex flex-col items-center gap-0.5">
          {game.status !== "scheduled" ? (
            <span className="font-bold tabular-nums text-sm" style={{ color: "var(--text)" }}>
              {game.homeScore ?? "–"}–{game.awayScore ?? "–"}
            </span>
          ) : (
            <span className="font-medium text-xs" style={{ color: "var(--text-muted)" }}>vs</span>
          )}
          <StatusBadge status={game.status} periodDisplay={game.periodDisplay} />
        </div>
      </td>

      {/* Away team */}
      <td className="px-2 sm:px-4 py-2.5">
        <div className="flex items-center gap-1.5 flex-row-reverse">
          <div className="relative w-8 h-8 shrink-0">
            {game.awayTeamLogo ? (
              // eslint-disable-next-line @next/next/no-img-element
              <img src={game.awayTeamLogo} alt={game.awayTeamName} width={32} height={32}
                className="object-contain w-8 h-8" loading="lazy" />
            ) : (
              <div className="w-8 h-8 rounded-full flex items-center justify-center text-xs font-bold"
                style={{ background: "var(--muted)", color: "var(--text-muted)" }}>
                {game.awayTeamAbbr?.slice(0, 3)}
              </div>
            )}
            <span className="absolute -top-1 -right-1 font-bold rounded px-0.5"
              style={{ background: "var(--muted)", color: "var(--text-muted)", fontSize: "8px", lineHeight: "13px" }}>
              {game.awayTeamSeed}
            </span>
          </div>
          <div className="min-w-0 text-right">
            <p className="font-medium truncate hidden sm:block text-sm" style={{ color: "var(--text)" }}>
              {game.awayTeamName}
            </p>
            <p className="font-medium text-xs sm:hidden" style={{ color: "var(--text)" }}>
              {game.awayTeamAbbr}
            </p>
            {awayPct != null && (game.status === "in-progress" || game.status === "halftime") && (
              <p className="text-xs hidden sm:block" style={{ color: "var(--accent)" }}>{awayPct}%</p>
            )}
          </div>
        </div>
      </td>
    </tr>
  );
}

// ─── Live Scores Table ────────────────────────────────────────────────────────

export function LiveScores() {
  const [selectedGame, setSelectedGame] = useState<LiveScoreGame | null>(null);

  const { data, isLoading, isError, dataUpdatedAt } = useQuery<LiveScoresApiResponse>({
    queryKey: ["live-scores"],
    queryFn: async () => {
      const res = await fetch("/api/live-scores");
      if (!res.ok) throw new Error("Failed to fetch scores");
      return res.json();
    },
  });

  const games = data?.games ?? [];
  const lastUpdated = dataUpdatedAt
    ? new Date(dataUpdatedAt).toLocaleTimeString()
    : null;

  if (isLoading) {
    return (
      <div className="mc-card p-6 flex items-center justify-center gap-3 text-sm" style={{ color: "var(--text-muted)" }}>
        <div
          className="w-5 h-5 rounded-full border-2 border-t-transparent animate-spin"
          style={{ borderColor: "var(--accent)", borderTopColor: "transparent" }}
        />
        Loading scores...
      </div>
    );
  }

  if (isError || data?.error) {
    return (
      <div className="mc-card p-6 text-center text-sm" style={{ color: "var(--text-muted)" }}>
        Live scores temporarily unavailable. Will retry automatically.
      </div>
    );
  }

  if (games.length === 0) {
    return (
      <div className="mc-card p-6 text-center text-sm" style={{ color: "var(--text-muted)" }}>
        No games found for today. Scores will appear here during the tournament.
      </div>
    );
  }

  return (
    <>
      <div className="mc-card overflow-hidden">
        <div
          className="px-5 py-3 border-b flex items-center justify-between"
          style={{ borderColor: "var(--border)" }}
        >
          <h2 className="font-semibold text-sm" style={{ color: "var(--text)" }}>
            Tournament Games
          </h2>
          <div className="flex items-center gap-3 text-xs" style={{ color: "var(--text-muted)" }}>
            {games.some((g) => g.status === "in-progress" || g.status === "halftime") && (
              <span className="flex items-center gap-1" style={{ color: "var(--accent)" }}>
                <span className="w-2 h-2 rounded-full live-dot" style={{ background: "var(--accent)", display: "inline-block" }} />
                Live
              </span>
            )}
            {lastUpdated && <span>Updated {lastUpdated}</span>}
            <span className="hidden sm:block text-xs" style={{ color: "var(--text-muted)" }}>
              Click a game to see who picked each team
            </span>
          </div>
        </div>

        <div className="overflow-x-auto">
          <table className="w-full">
            <tbody>
              {games.map((game) => (
                <ScoreRow
                  key={game.id}
                  game={game}
                  onClick={() => setSelectedGame(game)}
                />
              ))}
            </tbody>
          </table>
        </div>
      </div>

      {selectedGame && (
        <TeamEntryModal
          game={selectedGame}
          onClose={() => setSelectedGame(null)}
        />
      )}
    </>
  );
}
