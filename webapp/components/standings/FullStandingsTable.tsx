"use client";

import { useState, useMemo, Fragment } from "react";
import { useQuery } from "@tanstack/react-query";
import {
  useReactTable,
  getCoreRowModel,
  getSortedRowModel,
  getExpandedRowModel,
  getFilteredRowModel,
  flexRender,
  createColumnHelper,
  type SortingState,
  type ExpandedState,
} from "@tanstack/react-table";
import type { StandingsRow, StandingsApiResponse, StandingsTeamSlot } from "@/lib/types";
import { BUDGET_CAP } from "@/lib/types";

// ─── Expanded team grid ───────────────────────────────────────────────────────

function ExpandedTeamGrid({ teams }: { teams: StandingsTeamSlot[] }) {
  const validTeams = teams.filter((t) => t.teamName);
  const alive = validTeams.filter((t) => t.opacity >= 0.5).length;

  return (
    <div
      className="px-5 py-4 border-t"
      style={{ background: "rgba(0,163,108,0.03)", borderColor: "var(--border)" }}
    >
      <div className="flex items-center justify-between mb-2">
        <span className="text-xs font-medium" style={{ color: "var(--text-muted)" }}>
          Teams alive:{" "}
          <span style={{ color: alive > 0 ? "var(--accent)" : "var(--text-muted)", fontWeight: 700 }}>
            {alive}
          </span>
          <span>/{validTeams.length}</span>
        </span>
      </div>
      <div className="flex flex-wrap gap-3">
        {validTeams.map((slot, idx) => {
          const isEliminated = slot.opacity < 0.5;
          return (
            <div
              key={idx}
              className="flex flex-col items-center gap-1 relative"
              title={`${slot.teamName} — Seed ${slot.seed}${isEliminated ? " — Eliminated" : ""}`}
            >
              <div
                className="relative w-14 h-14"
                style={{ opacity: isEliminated ? 0.28 : 1, transition: "opacity 0.2s" }}
              >
                {slot.logoUrl ? (
                  // eslint-disable-next-line @next/next/no-img-element
                  <img
                    src={slot.logoUrl}
                    alt={slot.teamName}
                    width={56}
                    height={56}
                    className="object-contain w-14 h-14"
                    loading="lazy"
                  />
                ) : (
                  <div
                    className="w-14 h-14 rounded-full flex items-center justify-center text-xs font-bold"
                    style={{ background: "var(--muted)", color: "var(--text-muted)" }}
                  >
                    {slot.teamName.slice(0, 3).toUpperCase()}
                  </div>
                )}
                <span className="seed-badge">{slot.seed}</span>
              </div>
              {isEliminated && (
                <span
                  className="absolute pointer-events-none flex items-center justify-center"
                  style={{
                    top: 0, left: "50%", transform: "translateX(-50%)",
                    width: 56, height: 56,
                    color: "#ef4444", fontSize: "34px", fontWeight: 900, lineHeight: 1,
                  }}
                >
                  ✕
                </span>
              )}
              <span
                style={{
                  fontSize: "9px",
                  color: "var(--text-muted)",
                  maxWidth: "56px",
                  textAlign: "center",
                  overflow: "hidden",
                  textOverflow: "ellipsis",
                  whiteSpace: "nowrap",
                  display: "block",
                  opacity: isEliminated ? 0.28 : 1,
                }}
              >
                {slot.teamName}
              </span>
            </div>
          );
        })}
      </div>
    </div>
  );
}

// ─── Team Filter Panel ────────────────────────────────────────────────────────

interface UniqueTeam {
  teamName: string;
  logoUrl: string;
  seed: number;
  isEliminated: boolean;
}

function TeamChip({
  team,
  selected,
  onToggle,
}: {
  team: UniqueTeam;
  selected: boolean;
  onToggle: () => void;
}) {
  return (
    <button
      onClick={onToggle}
      title={`${team.teamName}${team.isEliminated ? " — Eliminated" : ""}`}
      style={{
        position: "relative",
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
        gap: "4px",
        padding: "6px",
        borderRadius: "10px",
        border: selected ? "2px solid var(--accent)" : "2px solid transparent",
        background: selected ? "rgba(0,163,108,0.12)" : "var(--muted)",
        cursor: "pointer",
        transition: "all 0.15s ease",
        boxShadow: selected ? "0 0 0 1px var(--accent)" : "none",
        minWidth: "52px",
      }}
    >
      <div style={{ position: "relative", width: 36, height: 36 }}>
        <div style={{ opacity: team.isEliminated ? 0.28 : 1, width: 36, height: 36 }}>
          {team.logoUrl ? (
            // eslint-disable-next-line @next/next/no-img-element
            <img
              src={team.logoUrl}
              alt={team.teamName}
              width={36}
              height={36}
              style={{ objectFit: "contain", width: 36, height: 36 }}
              loading="lazy"
            />
          ) : (
            <div
              style={{
                width: 36, height: 36, borderRadius: "50%",
                background: "var(--border)",
                display: "flex", alignItems: "center", justifyContent: "center",
                fontSize: "9px", fontWeight: 700, color: "var(--text-muted)",
              }}
            >
              {team.teamName.slice(0, 3).toUpperCase()}
            </div>
          )}
        </div>
        <span
          style={{
            position: "absolute", bottom: -2, right: -4,
            background: "var(--accent)", color: "#fff",
            fontSize: "8px", fontWeight: 700,
            borderRadius: "4px", padding: "0 3px", lineHeight: "13px",
          }}
        >
          {team.seed}
        </span>
        {team.isEliminated && (
          <span
            style={{
              position: "absolute", top: "50%", left: "50%",
              transform: "translate(-50%, -50%)",
              color: "#ef4444", fontSize: "22px", fontWeight: 900,
              lineHeight: 1, pointerEvents: "none",
            }}
          >
            ✕
          </span>
        )}
      </div>
      <span
        style={{
          fontSize: "8px",
          color: selected ? "var(--accent)" : "var(--text-muted)",
          fontWeight: selected ? 700 : 400,
          maxWidth: "48px",
          overflow: "hidden",
          textOverflow: "ellipsis",
          whiteSpace: "nowrap",
          textAlign: "center",
        }}
      >
        {team.teamName}
      </span>
    </button>
  );
}

function TeamFilterPanel({
  teams,
  selected,
  onToggle,
  onClear,
}: {
  teams: UniqueTeam[];
  selected: Set<string>;
  onToggle: (name: string) => void;
  onClear: () => void;
}) {
  const [open, setOpen] = useState(false);
  const alive = teams.filter((t) => !t.isEliminated);
  const eliminated = teams.filter((t) => t.isEliminated);

  return (
    <div style={{ borderBottom: "1px solid var(--border)" }}>
      {/* Header toggle */}
      <button
        onClick={() => setOpen((o) => !o)}
        style={{
          width: "100%", display: "flex", alignItems: "center",
          justifyContent: "space-between", padding: "10px 20px",
          background: "none", border: "none", cursor: "pointer", color: "var(--text)",
        }}
      >
        <div style={{ display: "flex", alignItems: "center", gap: "10px" }}>
          <span style={{ fontSize: "12px", fontWeight: 600 }}>Filter by Team</span>
          {selected.size > 0 && (
            <span
              style={{
                background: "var(--accent)", color: "#fff",
                fontSize: "10px", fontWeight: 700,
                borderRadius: "999px", padding: "1px 7px",
              }}
            >
              {selected.size} selected
            </span>
          )}
          {!open && selected.size > 0 && (
            <div style={{ display: "flex", gap: "2px", alignItems: "center" }}>
              {teams.filter((t) => selected.has(t.teamName)).slice(0, 5).map((t) => (
                // eslint-disable-next-line @next/next/no-img-element
                <img
                  key={t.teamName}
                  src={t.logoUrl}
                  alt={t.teamName}
                  width={20}
                  height={20}
                  style={{ objectFit: "contain", opacity: t.isEliminated ? 0.4 : 1 }}
                  loading="lazy"
                />
              ))}
              {selected.size > 5 && (
                <span style={{ fontSize: "10px", color: "var(--text-muted)", marginLeft: 2 }}>
                  +{selected.size - 5}
                </span>
              )}
            </div>
          )}
        </div>
        <div style={{ display: "flex", alignItems: "center", gap: "8px" }}>
          {selected.size > 0 && (
            <span
              role="button"
              onClick={(e) => { e.stopPropagation(); onClear(); }}
              style={{
                fontSize: "10px", color: "var(--accent)", fontWeight: 600,
                padding: "2px 8px", borderRadius: "6px",
                background: "rgba(0,163,108,0.1)", cursor: "pointer",
              }}
            >
              Clear
            </span>
          )}
          <span
            style={{
              fontSize: "16px", color: "var(--text-muted)",
              transition: "transform 0.2s",
              transform: open ? "rotate(90deg)" : "rotate(0deg)",
              display: "block", lineHeight: 1,
            }}
          >
            ›
          </span>
        </div>
      </button>

      {/* Logo grid */}
      {open && (
        <div style={{ padding: "0 20px 16px" }}>
          <div style={{ marginBottom: "12px" }}>
            <div
              style={{
                fontSize: "10px", fontWeight: 600, textTransform: "uppercase",
                letterSpacing: "0.06em", color: "var(--accent)", marginBottom: "8px",
              }}
            >
              Still Alive — {alive.length} teams
            </div>
            <div style={{ display: "flex", flexWrap: "wrap", gap: "6px" }}>
              {alive.map((t) => (
                <TeamChip key={t.teamName} team={t} selected={selected.has(t.teamName)} onToggle={() => onToggle(t.teamName)} />
              ))}
            </div>
          </div>
          {eliminated.length > 0 && (
            <div>
              <div
                style={{
                  fontSize: "10px", fontWeight: 600, textTransform: "uppercase",
                  letterSpacing: "0.06em", color: "var(--text-muted)", marginBottom: "8px",
                }}
              >
                Eliminated — {eliminated.length} teams
              </div>
              <div style={{ display: "flex", flexWrap: "wrap", gap: "6px" }}>
                {eliminated.map((t) => (
                  <TeamChip key={t.teamName} team={t} selected={selected.has(t.teamName)} onToggle={() => onToggle(t.teamName)} />
                ))}
              </div>
            </div>
          )}
        </div>
      )}
    </div>
  );
}

// ─── Column helper ────────────────────────────────────────────────────────────

const col = createColumnHelper<StandingsRow>();

function makeColumns(rankMap: Map<string, string>, scoreMap?: Map<string, number>) {
  return [
  col.display({
    id: "expander",
    header: () => null,
    cell: ({ row }) => (
      <button
        onClick={row.getToggleExpandedHandler()}
        className="px-2 py-0.5 text-sm leading-none transition-transform"
        style={{
          color: "var(--text-muted)",
          transform: row.getIsExpanded() ? "rotate(90deg)" : "rotate(0deg)",
          display: "block",
        }}
        aria-label={row.getIsExpanded() ? "Collapse" : "Expand"}
      >
        ›
      </button>
    ),
    size: 32,
  }),

  col.display({
    id: "rank",
    header: "#",
    enableSorting: true,
    cell: ({ row }) => {
      const rank = rankMap.get(row.original.entry_name) ?? "";
      const isTie = rank.startsWith("T");
      return (
        <span className="font-bold tabular-nums text-sm" style={{ color: isTie ? "var(--accent)" : "var(--text-muted)" }}>
          {rank}
        </span>
      );
    },
    sortingFn: (a, b) => {
      const ra = parseInt((rankMap.get(a.original.entry_name) ?? "").replace("T", ""), 10);
      const rb = parseInt((rankMap.get(b.original.entry_name) ?? "").replace("T", ""), 10);
      return ra - rb;
    },
    size: 40,
  }),

  col.accessor("entry_name", {
    id: "entry_name",
    header: "Entry",
    cell: (info) => (
      <span className="font-semibold text-sm" style={{ color: "var(--text)" }}>
        {info.getValue()}
      </span>
    ),
  }),

  col.accessor("wins", {
    id: "wins",
    header: "Wins",
    cell: (info) => (
      <span className="font-bold tabular-nums text-sm" style={{ color: "var(--text)" }}>
        {info.getValue().toFixed(1)}
      </span>
    ),
    size: 80,
  }),

  col.accessor("live_wins", {
    id: "live_wins",
    header: "Total",
    cell: (info) => {
      const lw = info.getValue();
      const w = info.row.original.wins;
      const hasLive = lw !== w;
      return (
        <span
          className="font-bold tabular-nums text-sm"
          style={{ color: hasLive ? "var(--accent)" : "var(--text)" }}
        >
          {lw.toFixed(2)}
        </span>
      );
    },
    size: 80,
  }),

  col.accessor("tiebreaker_points", {
    id: "tiebreaker",
    header: "Tiebreaker",
    cell: (info) => (
      <span className="tabular-nums text-xs" style={{ color: "var(--text-muted)" }}>
        {info.getValue()}
      </span>
    ),
    size: 90,
  }),

  // Live $
  col.display({
    id: "live_money",
    header: "Live $",
    enableSorting: true,
    cell: ({ row }) => {
      const deadMoney = row.original.teams.reduce(
        (sum, s) => sum + (s.teamName && s.opacity < 0.5 ? s.cost : 0), 0
      );
      const remaining = BUDGET_CAP - deadMoney;
      return (
        <span className="tabular-nums text-xs font-medium" style={{ color: remaining > 0 ? "var(--accent)" : "var(--text-muted)" }}>
          ${remaining}
        </span>
      );
    },
    sortingFn: (a, b) => {
      const liveMoney = (r: StandingsRow) => BUDGET_CAP - r.teams.reduce((sum, s) => sum + (s.teamName && s.opacity < 0.5 ? s.cost : 0), 0);
      return liveMoney(a.original) - liveMoney(b.original);
    },
    size: 65,
  }),

  // Team count
  col.display({
    id: "teams_count",
    header: "Alive",
    enableSorting: true,
    cell: ({ row }) => {
      const active = row.original.teams.filter((t) => t.teamName && t.opacity >= 1).length;
      const total = row.original.teams.filter((t) => t.teamName).length;
      return (
        <span className="text-xs tabular-nums" style={{ color: "var(--text-muted)" }}>
          <span style={{ color: "var(--accent)" }}>{active}</span>/{total}
        </span>
      );
    },
    sortingFn: (a, b) => {
      const alive = (r: StandingsRow) => r.teams.filter((t) => t.teamName && t.opacity >= 1).length;
      return alive(a.original) - alive(b.original);
    },
    size: 70,
  }),
  // Enhanced score column — only shown in enhanced mode
  ...(scoreMap ? [col.display({
    id: "enhanced_score",
    header: "Score ✦",
    enableSorting: true,
    cell: ({ row }) => {
      const score = scoreMap.get(row.original.entry_name) ?? 0;
      return (
        <span className="tabular-nums text-xs font-bold" style={{ color: "var(--accent)" }}>
          {score.toFixed(1)}
        </span>
      );
    },
    sortingFn: (a, b) => (scoreMap.get(a.original.entry_name) ?? 0) - (scoreMap.get(b.original.entry_name) ?? 0),
    size: 75,
  })] : []),
  ]; // end makeColumns
}

// ─── Full Standings Table ─────────────────────────────────────────────────────

export function FullStandingsTable() {
  const [sorting, setSorting] = useState<SortingState>([]);
  const [expanded, setExpanded] = useState<ExpandedState>({});
  const [globalFilter, setGlobalFilter] = useState("");
  const [selectedTeams, setSelectedTeams] = useState<Set<string>>(new Set());
  const [rankMode, setRankMode] = useState<"standard" | "money" | "enhanced">("standard");

  const { data, isLoading, isError, dataUpdatedAt } = useQuery<StandingsApiResponse>({
    queryKey: ["standings"],
    queryFn: async () => {
      const res = await fetch("/api/standings");
      if (!res.ok) throw new Error("Failed to fetch standings");
      return res.json();
    },
    staleTime: 60_000,
    refetchInterval: 60_000,
  });

  const rows = data?.standings ?? [];
  const lastUpdated = dataUpdatedAt ? new Date(dataUpdatedAt).toLocaleTimeString() : null;

  // Compute global tie-aware ranks from unfiltered sorted rows
  const rankMap = useMemo(() => {
    const map = new Map<string, string>();
    let i = 0;
    while (i < rows.length) {
      const lw = rows[i].live_wins;
      let j = i;
      while (j < rows.length && rows[j].live_wins === lw) j++;
      const label = j - i > 1 ? `T${i + 1}` : `${i + 1}`;
      for (let k = i; k < j; k++) map.set(rows[k].entry_name, label);
      i = j;
    }
    return map;
  }, [rows]);

  // Enhanced score: 50% live wins + 30% live $ + 20% teams alive (all normalized)
  const { enhancedScoreMap, enhancedRankMap } = useMemo(() => {
    const liveMoney = (r: StandingsRow) =>
      BUDGET_CAP - r.teams.reduce((sum, s) => sum + (s.teamName && s.opacity < 0.5 ? s.cost : 0), 0);
    const aliveCount = (r: StandingsRow) =>
      r.teams.filter((t) => t.teamName && t.opacity >= 0.5).length;

    const maxWins = Math.max(...rows.map((r) => r.live_wins), 0.001);
    const maxMoney = Math.max(...rows.map(liveMoney), 0.001);
    const maxAlive = Math.max(...rows.map(aliveCount), 0.001);

    const scoreMap = new Map<string, number>();
    for (const r of rows) {
      const score =
        (r.live_wins / maxWins) * 50 +
        (liveMoney(r) / maxMoney) * 30 +
        (aliveCount(r) / maxAlive) * 20;
      scoreMap.set(r.entry_name, Math.round(score * 10) / 10);
    }

    // Sort rows by score desc, compute tie ranks
    const sorted = [...rows].sort((a, b) => (scoreMap.get(b.entry_name) ?? 0) - (scoreMap.get(a.entry_name) ?? 0));
    const rMap = new Map<string, string>();
    let i = 0;
    while (i < sorted.length) {
      const score = scoreMap.get(sorted[i].entry_name) ?? 0;
      let j = i;
      while (j < sorted.length && (scoreMap.get(sorted[j].entry_name) ?? 0) === score) j++;
      const label = j - i > 1 ? `T${i + 1}` : `${i + 1}`;
      for (let k = i; k < j; k++) rMap.set(sorted[k].entry_name, label);
      i = j;
    }

    return { enhancedScoreMap: scoreMap, enhancedRankMap: rMap };
  }, [rows]);

  // Live $ rank: sort by live_wins DESC → live $ DESC, ties only when BOTH match
  const moneyRankMap = useMemo(() => {
    const liveMoney = (r: StandingsRow) =>
      BUDGET_CAP - r.teams.reduce((sum, s) => sum + (s.teamName && s.opacity < 0.5 ? s.cost : 0), 0);
    const sorted = [...rows].sort((a, b) => {
      if (b.live_wins !== a.live_wins) return b.live_wins - a.live_wins;
      return liveMoney(b) - liveMoney(a);
    });
    const map = new Map<string, string>();
    let i = 0;
    while (i < sorted.length) {
      const lw = sorted[i].live_wins;
      const lm = liveMoney(sorted[i]);
      let j = i;
      while (j < sorted.length && sorted[j].live_wins === lw && liveMoney(sorted[j]) === lm) j++;
      const label = j - i > 1 ? `T${i + 1}` : `${i + 1}`;
      for (let k = i; k < j; k++) map.set(sorted[k].entry_name, label);
      i = j;
    }
    return map;
  }, [rows]);

  const activeRankMap = rankMode === "enhanced" ? enhancedRankMap : rankMode === "money" ? moneyRankMap : rankMap;
  const columns = useMemo(
    () => makeColumns(activeRankMap, rankMode === "enhanced" ? enhancedScoreMap : undefined),
    [activeRankMap, rankMode, enhancedScoreMap]
  );

  // Derive unique teams: alive first sorted by seed, then eliminated
  const uniqueTeams = useMemo<UniqueTeam[]>(() => {
    const map = new Map<string, UniqueTeam>();
    for (const row of rows) {
      for (const slot of row.teams) {
        if (!slot.teamName || map.has(slot.teamName)) continue;
        map.set(slot.teamName, {
          teamName: slot.teamName,
          logoUrl: slot.logoUrl,
          seed: slot.seed,
          isEliminated: slot.opacity < 0.5,
        });
      }
    }
    return Array.from(map.values()).sort((a, b) => {
      if (a.isEliminated !== b.isEliminated) return a.isEliminated ? 1 : -1;
      return a.seed - b.seed;
    });
  }, [rows]);

  // Filter + pre-sort by active rank mode (TanStack sort overrides when user clicks a header)
  const filteredRows = useMemo(() => {
    let result = rows;
    if (globalFilter) {
      result = result.filter((r) =>
        r.entry_name.toLowerCase().includes(globalFilter.toLowerCase())
      );
    }
    if (selectedTeams.size > 0) {
      result = result.filter((r) =>
        Array.from(selectedTeams).every((name) =>
          r.teams.some((t) => t.teamName === name)
        )
      );
    }
    const liveMoney = (r: StandingsRow) =>
      BUDGET_CAP - r.teams.reduce((sum, s) => sum + (s.teamName && s.opacity < 0.5 ? s.cost : 0), 0);
    if (rankMode === "enhanced") {
      result = [...result].sort(
        (a, b) => (enhancedScoreMap.get(b.entry_name) ?? 0) - (enhancedScoreMap.get(a.entry_name) ?? 0)
      );
    } else if (rankMode === "money") {
      result = [...result].sort((a, b) => {
        if (b.live_wins !== a.live_wins) return b.live_wins - a.live_wins;
        return liveMoney(b) - liveMoney(a);
      });
    } else {
      result = [...result].sort((a, b) => {
        if (b.live_wins !== a.live_wins) return b.live_wins - a.live_wins;
        return liveMoney(b) - liveMoney(a);
      });
    }
    return result;
  }, [rows, globalFilter, selectedTeams, rankMode, enhancedScoreMap]);

  const toggleTeam = (name: string) =>
    setSelectedTeams((prev) => {
      const next = new Set(prev);
      next.has(name) ? next.delete(name) : next.add(name);
      return next;
    });

  const table = useReactTable({
    data: filteredRows,
    columns,
    state: { sorting, expanded },
    onSortingChange: setSorting,
    onExpandedChange: setExpanded,
    getCoreRowModel: getCoreRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getExpandedRowModel: getExpandedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    getRowCanExpand: () => true,
  });

  if (isLoading) {
    return (
      <div className="mc-card p-8 flex items-center justify-center gap-3" style={{ color: "var(--text-muted)" }}>
        <div
          className="w-6 h-6 rounded-full border-2 border-t-transparent animate-spin"
          style={{ borderColor: "var(--accent)", borderTopColor: "transparent" }}
        />
        Computing standings...
      </div>
    );
  }

  if (isError || data?.error) {
    return (
      <div className="mc-card p-6 text-center" style={{ color: "var(--text-muted)" }}>
        Could not load standings. Will retry automatically.
      </div>
    );
  }

  return (
    <div className="mc-card overflow-hidden">
      {/* Toolbar */}
      <div
        className="px-5 py-3 border-b flex flex-col sm:flex-row sm:items-center justify-between gap-3"
        style={{ borderColor: "var(--border)" }}
      >
        <div className="flex items-center gap-3 flex-wrap">
          <h2 className="font-semibold text-sm" style={{ color: "var(--text)" }}>
            Full Standings
          </h2>
          <span className="text-xs" style={{ color: "var(--text-muted)" }}>
            {table.getRowModel().rows.length}
            {selectedTeams.size > 0 || globalFilter ? ` of ${rows.length}` : ""} entries
          </span>
          {/* Rank mode toggle */}
          <div
            style={{
              display: "flex", borderRadius: "8px", overflow: "hidden",
              border: "1px solid var(--border)", fontSize: "11px", fontWeight: 600,
            }}
          >
            {(["standard", "money", "enhanced"] as const).map((mode) => (
              <button
                key={mode}
                onClick={() => { setRankMode(mode); setSorting([]); }}
                title={
                  mode === "money" ? "Rank by Live Wins then Live $, ties only when both match" :
                  mode === "enhanced" ? "Score = 50% Live Wins + 30% Live $ + 20% Teams Alive" :
                  "Rank by Live Wins (ties on live wins)"
                }
                style={{
                  padding: "3px 10px", border: "none", cursor: "pointer",
                  background: rankMode === mode ? "var(--accent)" : "var(--muted)",
                  color: rankMode === mode ? "#fff" : "var(--text-muted)",
                  transition: "all 0.15s",
                }}
              >
                {mode === "standard" ? "Standard" : mode === "money" ? "Live $" : "Enhanced ✦"}
              </button>
            ))}
          </div>
        </div>
        <div className="flex items-center gap-3">
          {lastUpdated && (
            <span className="text-xs hidden sm:block" style={{ color: "var(--text-muted)" }}>
              Updated {lastUpdated}
            </span>
          )}
          <div className="relative">
            <input
              value={globalFilter}
              onChange={(e) => setGlobalFilter(e.target.value)}
              placeholder="Search entry..."
              className="px-3 py-1.5 rounded-lg text-xs border"
              style={{
                background: "var(--bg)",
                borderColor: "var(--border)",
                color: "var(--text)",
                width: "180px",
                paddingRight: globalFilter ? "24px" : undefined,
              }}
            />
            {globalFilter && (
              <button
                onClick={() => setGlobalFilter("")}
                className="absolute right-2 top-1/2 -translate-y-1/2 text-xs leading-none"
                style={{ color: "var(--text-muted)", background: "none", border: "none", cursor: "pointer" }}
                aria-label="Clear search"
              >
                ✕
              </button>
            )}
          </div>
        </div>
      </div>

      {/* Team filter panel */}
      {uniqueTeams.length > 0 && (
        <TeamFilterPanel
          teams={uniqueTeams}
          selected={selectedTeams}
          onToggle={toggleTeam}
          onClear={() => setSelectedTeams(new Set())}
        />
      )}

      {/* Legend */}
      <div
        className="px-5 py-2 border-b text-xs flex flex-wrap gap-4"
        style={{ borderColor: "var(--border)", color: "var(--text-muted)" }}
      >
        {rankMode === "enhanced" ? (
          <span><span style={{ color: "var(--accent)" }}>Enhanced ✦</span> = 50% Live Wins + 30% Live $ + 20% Teams Alive (normalized 0–100)</span>
        ) : rankMode === "money" ? (
          <span><span style={{ color: "var(--accent)" }}>Live $</span> rank = Live Wins → Live $ · ties only when both match exactly</span>
        ) : (
          <span><span style={{ color: "var(--accent)" }}>Total</span> = wins + live fractional wins · ties on Live Wins</span>
        )}
        <span>Teams: <span style={{ color: "var(--accent)" }}>active</span>/total (faded = eliminated)</span>
      </div>

      <div className="overflow-x-auto">
        <table className="w-full text-sm">
          <thead>
            {table.getHeaderGroups().map((hg) => (
              <tr key={hg.id}>
                {hg.headers.map((header) => (
                  <th
                    key={header.id}
                    style={{
                      width: header.getSize(),
                      cursor: header.column.getCanSort() ? "pointer" : "default",
                      padding: "8px 12px",
                      textAlign: "left",
                      fontSize: "11px",
                      fontWeight: 600,
                      textTransform: "uppercase",
                      letterSpacing: "0.05em",
                      color: "var(--text-muted)",
                      borderBottom: "1px solid var(--border)",
                      background: "var(--bg-card)",
                      userSelect: "none",
                    }}
                    onClick={header.column.getToggleSortingHandler()}
                  >
                    <span className="flex items-center gap-1">
                      {flexRender(header.column.columnDef.header, header.getContext())}
                      {header.column.getIsSorted() === "asc" && " ↑"}
                      {header.column.getIsSorted() === "desc" && " ↓"}
                    </span>
                  </th>
                ))}
              </tr>
            ))}
          </thead>
          <tbody>
            {table.getRowModel().rows.map((row) => (
              <Fragment key={row.id}>
                <tr
                  style={{ borderBottom: row.getIsExpanded() ? "none" : "1px solid var(--border)" }}
                  className="hover:bg-white/[0.03] transition-colors"
                >
                  {row.getVisibleCells().map((cell) => (
                    <td key={cell.id} style={{ padding: "10px 12px" }}>
                      {flexRender(cell.column.columnDef.cell, cell.getContext())}
                    </td>
                  ))}
                </tr>
                {row.getIsExpanded() && (
                  <tr key={`${row.id}-exp`}>
                    <td colSpan={columns.length} style={{ padding: 0 }}>
                      <ExpandedTeamGrid teams={row.original.teams} />
                    </td>
                  </tr>
                )}
              </Fragment>
            ))}
          </tbody>
        </table>

        {table.getRowModel().rows.length === 0 && (
          <div className="py-12 text-center text-sm" style={{ color: "var(--text-muted)" }}>
            No entries match your filters.{" "}
            <button
              onClick={() => { setGlobalFilter(""); setSelectedTeams(new Set()); }}
              style={{ color: "var(--accent)", fontWeight: 600, background: "none", border: "none", cursor: "pointer" }}
            >
              Clear all filters
            </button>
          </div>
        )}
      </div>
    </div>
  );
}
