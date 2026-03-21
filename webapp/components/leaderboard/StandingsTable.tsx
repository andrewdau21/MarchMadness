"use client";

import { useState, Fragment, memo, useEffect, useRef } from "react";
import { useQuery } from "@tanstack/react-query";
import type { StandingsRow, StandingsApiResponse, StandingsTeamSlot } from "@/lib/types";
import { BUDGET_CAP } from "@/lib/types";

const MY_ENTRY_KEY = "marchcapness_my_entry";

// ─── Team Logo Grid ───────────────────────────────────────────────────────────

const TeamLogoGrid = memo(function TeamLogoGrid({ teams }: { teams: StandingsTeamSlot[] }) {
  return (
    <div
      className="px-4 py-4 border-t"
      style={{ background: "rgba(0,163,108,0.04)", borderColor: "var(--border)" }}
    >
      <div className="flex flex-wrap gap-3">
        {teams.map((slot, idx) => {
          if (!slot.teamName) return null;
          const isEliminated = slot.opacity < 0.5;
          return (
            <div
              key={idx}
              className="flex flex-col items-center gap-1"
              style={{ opacity: isEliminated ? 0.35 : 1, transition: "opacity 0.2s" }}
              title={`${slot.teamName} (Seed ${slot.seed})${isEliminated ? " — Eliminated" : ""}`}
            >
              <div className="relative w-12 h-12">
                {slot.logoUrl ? (
                  // eslint-disable-next-line @next/next/no-img-element
                  <img
                    src={slot.logoUrl}
                    alt={slot.teamName}
                    width={48}
                    height={48}
                    className="object-contain w-12 h-12"
                    loading="lazy"
                  />
                ) : (
                  <div
                    className="w-12 h-12 rounded-full flex items-center justify-center text-xs font-bold"
                    style={{ background: "var(--muted)", color: "var(--text-muted)" }}
                  >
                    {slot.teamName.slice(0, 3).toUpperCase()}
                  </div>
                )}
                <span className="seed-badge">{slot.seed}</span>
                {isEliminated && (
                  <span
                    className="absolute inset-0 flex items-center justify-center font-black pointer-events-none"
                    style={{ color: "#ef4444", fontSize: "28px", lineHeight: 1, opacity: 0.85 }}
                  >
                    ✕
                  </span>
                )}
                {slot.isPlaying && (
                  <span
                    className="absolute -bottom-1 -right-1 text-white text-center font-bold live-dot"
                    style={{
                      fontSize: "8px",
                      background: "var(--accent)",
                      borderRadius: "3px",
                      padding: "1px 3px",
                      lineHeight: "1.4",
                    }}
                  >
                    LIVE
                  </span>
                )}
              </div>
              <span
                className="text-center leading-tight"
                style={{
                  fontSize: "9px",
                  color: "var(--text-muted)",
                  maxWidth: "48px",
                  overflow: "hidden",
                  textOverflow: "ellipsis",
                  whiteSpace: "nowrap",
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
});

// ─── Single row ───────────────────────────────────────────────────────────────

const StandingsRowItem = memo(function StandingsRowItem({
  row,
  rank,
  isExpanded,
  isMyEntry,
  onToggle,
  onPin,
}: {
  row: StandingsRow;
  rank: number;
  isExpanded: boolean;
  isMyEntry: boolean;
  onToggle: () => void;
  onPin: () => void;
}) {
  const hasFractional = row.live_wins !== row.wins;
  // dead money = cost of eliminated teams only (opacity 0.1 = has a loss)
  const deadMoney = row.teams.reduce((sum, s) => sum + (s.teamName && s.opacity < 0.5 ? s.cost : 0), 0);
  const remaining = BUDGET_CAP - deadMoney;
  return (
    <Fragment>
      <tr
        style={{
          borderBottom: isExpanded ? "none" : "1px solid var(--border)",
          cursor: "pointer",
          background: isMyEntry ? "rgba(0,163,108,0.08)" : "transparent",
        }}
        className="hover:bg-white/[0.03] transition-colors"
        onClick={onToggle}
      >
        <td style={{ padding: "10px 12px", width: 32 }}>
          <span
            className="text-base leading-none transition-transform inline-block"
            style={{
              color: "var(--text-muted)",
              transform: isExpanded ? "rotate(90deg)" : "rotate(0deg)",
            }}
          >
            ›
          </span>
        </td>
        <td style={{ padding: "10px 12px", width: 40 }}>
          <span className="font-bold tabular-nums" style={{ color: "var(--text-muted)", fontSize: "13px" }}>
            {rank}
          </span>
        </td>
        <td style={{ padding: "10px 12px" }}>
          <div className="flex items-center gap-2">
            <span className="font-semibold text-sm" style={{ color: isMyEntry ? "var(--accent)" : "var(--text)" }}>
              {row.entry_name}
            </span>
            {isMyEntry && (
              <span
                className="text-xs px-1.5 py-0.5 rounded font-medium"
                style={{ background: "rgba(0,163,108,0.2)", color: "var(--accent)", fontSize: "10px" }}
              >
                You
              </span>
            )}
          </div>
        </td>
        <td style={{ padding: "10px 12px", width: 70 }}>
          <span className="font-bold tabular-nums text-sm" style={{ color: "var(--text)" }}>
            {row.wins.toFixed(1)}
          </span>
        </td>
        <td style={{ padding: "10px 12px", width: 110 }}>
          <span
            className="font-bold tabular-nums text-sm"
            style={{ color: hasFractional ? "var(--accent)" : "var(--text)" }}
          >
            {row.live_wins.toFixed(2)}
          </span>
        </td>
        <td style={{ padding: "10px 6px", width: 36 }}>
          <span className="tabular-nums text-xs" style={{ color: remaining > 0 ? "var(--accent)" : "var(--text-muted)" }}>
            ${remaining}
          </span>
        </td>
        <td style={{ padding: "10px 6px", width: 28 }}>
          <button
            onClick={(e) => { e.stopPropagation(); onPin(); }}
            title={isMyEntry ? "Unpin entry" : "Track this entry"}
            style={{ color: isMyEntry ? "var(--accent)" : "var(--text-muted)", fontSize: "15px", lineHeight: 1 }}
            className="hover:opacity-80 transition-opacity"
          >
            {isMyEntry ? "★" : "☆"}
          </button>
        </td>
      </tr>
      {isExpanded && (
        <tr style={{ background: isMyEntry ? "rgba(0,163,108,0.04)" : "transparent" }}>
          <td colSpan={7} style={{ padding: 0 }}>
            <TeamLogoGrid teams={row.teams} />
          </td>
        </tr>
      )}
    </Fragment>
  );
});

// ─── Standings Table ──────────────────────────────────────────────────────────

export function StandingsTable({ limit }: { limit?: number }) {
  const [expandedRows, setExpandedRows] = useState<Set<string>>(new Set());
  const [search, setSearch] = useState("");
  const [myEntry, setMyEntry] = useState<string | null>(null);
  const myEntryRowRef = useRef<HTMLTableRowElement | null>(null);

  // Load saved entry from localStorage on mount
  useEffect(() => {
    const saved = localStorage.getItem(MY_ENTRY_KEY);
    if (saved) setMyEntry(saved);
  }, []);

  const { data, isLoading, isError, dataUpdatedAt } = useQuery<StandingsApiResponse>({
    queryKey: ["standings"],
    queryFn: async () => {
      const res = await fetch("/api/standings");
      if (!res.ok) throw new Error("Failed to fetch standings");
      return res.json();
    },
    staleTime: 60_000,
  });

  const allRows = data?.standings ?? [];

  function toggleRow(entryName: string) {
    setExpandedRows(prev => {
      const next = new Set(prev);
      if (next.has(entryName)) next.delete(entryName);
      else next.add(entryName);
      return next;
    });
  }

  function pinEntry(entryName: string) {
    if (myEntry === entryName) {
      setMyEntry(null);
      localStorage.removeItem(MY_ENTRY_KEY);
    } else {
      setMyEntry(entryName);
      localStorage.setItem(MY_ENTRY_KEY, entryName);
    }
  }

  // Rows to display: top N, plus pinned entry if outside top N, plus search results
  const trimmed = search.trim().toLowerCase();
  let visibleRows: { row: StandingsRow; rank: number }[];

  if (trimmed) {
    // Search mode: show all matches across full standings
    visibleRows = allRows
      .map((row, i) => ({ row, rank: i + 1 }))
      .filter(({ row }) => row.entry_name.toLowerCase().includes(trimmed));
  } else {
    const topN = limit != null ? allRows.slice(0, limit) : allRows;
    visibleRows = topN.map((row, i) => ({ row, rank: i + 1 }));

    // If pinned entry is outside the top N, append it
    if (myEntry && limit != null) {
      const myIdx = allRows.findIndex(r => r.entry_name === myEntry);
      if (myIdx >= limit) {
        visibleRows = [...visibleRows, { row: allRows[myIdx], rank: myIdx + 1 }];
      }
    }
  }

  const lastUpdated = dataUpdatedAt ? new Date(dataUpdatedAt).toLocaleTimeString() : null;

  if (isLoading) {
    return (
      <div className="mc-card p-6 flex items-center justify-center gap-3 text-sm" style={{ color: "var(--text-muted)" }}>
        <div className="w-5 h-5 rounded-full border-2 border-t-transparent animate-spin"
          style={{ borderColor: "var(--accent)", borderTopColor: "transparent" }} />
        Loading standings...
      </div>
    );
  }

  if (isError || data?.error) {
    return (
      <div className="mc-card p-6 text-center text-sm" style={{ color: "var(--text-muted)" }}>
        Could not load standings. Will retry automatically.
      </div>
    );
  }

  if (allRows.length === 0) {
    return (
      <div className="mc-card p-6 text-center text-sm" style={{ color: "var(--text-muted)" }}>
        No entries found yet.
      </div>
    );
  }

  return (
    <div className="mc-card overflow-hidden">
      {/* Header */}
      <div className="px-4 py-3 border-b flex flex-col sm:flex-row sm:items-center gap-2"
        style={{ borderColor: "var(--border)" }}>
        <div className="flex items-center justify-between gap-2">
          <h2 className="font-semibold text-sm shrink-0" style={{ color: "var(--text)" }}>
            Standings
            {!trimmed && limit != null && allRows.length > limit && (
              <span className="ml-1.5 text-xs font-normal" style={{ color: "var(--text-muted)" }}>
                (Top {limit} of {allRows.length})
              </span>
            )}
          </h2>
          {lastUpdated && (
            <span className="text-xs sm:hidden shrink-0" style={{ color: "var(--text-muted)" }}>
              {lastUpdated}
            </span>
          )}
        </div>
        <div className="flex items-center gap-2 sm:flex-1 sm:justify-end">
          <div className="relative flex-1 sm:flex-none">
            <span className="absolute left-2 top-1/2 -translate-y-1/2 text-xs" style={{ color: "var(--text-muted)" }}>
              🔍
            </span>
            <input
              type="text"
              placeholder="Find entry…"
              value={search}
              onChange={e => setSearch(e.target.value)}
              className="pl-7 pr-3 py-1.5 rounded-md text-xs w-full sm:w-36 focus:outline-none"
              style={{
                background: "var(--muted)",
                color: "var(--text)",
                border: "1px solid var(--border)",
              }}
            />
          </div>
          {lastUpdated && (
            <span className="text-xs shrink-0 hidden sm:block" style={{ color: "var(--text-muted)" }}>
              {lastUpdated}
            </span>
          )}
        </div>
      </div>

      <div className="overflow-x-auto">
        <table className="w-full text-sm">
          <thead>
            <tr>
              {["", "#", "Entry", "Wins", "Total (w/Live)", "Live $", ""].map((h, i) => (
                <th key={i} style={{
                  padding: "8px 6px",
                  textAlign: "left",
                  fontSize: "11px",
                  fontWeight: 600,
                  textTransform: "uppercase",
                  letterSpacing: "0.05em",
                  color: "var(--text-muted)",
                  borderBottom: "1px solid var(--border)",
                  background: "var(--bg-card)",
                }}>
                  {h}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {visibleRows.map(({ row, rank }, idx) => {
              const isPinned = myEntry === row.entry_name;
              // Separator before pinned entry if it's outside top N
              const showSeparator = !trimmed && limit != null && rank > limit && idx > 0;
              return (
                <Fragment key={row.entry_name}>
                  {showSeparator && (
                    <tr>
                      <td colSpan={7} style={{ padding: "4px 12px", borderBottom: "1px solid var(--border)" }}>
                        <span className="text-xs" style={{ color: "var(--text-muted)" }}>· · · Your entry · · ·</span>
                      </td>
                    </tr>
                  )}
                  <StandingsRowItem
                    row={row}
                    rank={rank}
                    isExpanded={expandedRows.has(row.entry_name)}
                    isMyEntry={isPinned}
                    onToggle={() => toggleRow(row.entry_name)}
                    onPin={() => pinEntry(row.entry_name)}
                  />
                </Fragment>
              );
            })}
            {trimmed && visibleRows.length === 0 && (
              <tr>
                <td colSpan={7} style={{ padding: "20px", textAlign: "center", color: "var(--text-muted)", fontSize: "13px" }}>
                  No entries matching &ldquo;{search}&rdquo;
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
    </div>
  );
}
