"use client";

import { useState, Fragment, memo } from "react";
import { useQuery } from "@tanstack/react-query";
import type { StandingsRow, StandingsApiResponse, StandingsTeamSlot } from "@/lib/types";

// ─── Team Logo Grid (expanded row) ───────────────────────────────────────────

const TeamLogoGrid = memo(function TeamLogoGrid({ teams }: { teams: StandingsTeamSlot[] }) {
  return (
    <div
      className="px-4 py-4 border-t"
      style={{ background: "rgba(0,163,108,0.04)", borderColor: "var(--border)" }}
    >
      <div className="flex flex-wrap gap-3">
        {teams.map((slot, idx) => {
          if (!slot.teamName) return null;
          return (
            <div
              key={idx}
              className="flex flex-col items-center gap-1"
              style={{ opacity: slot.opacity, transition: "opacity 0.2s" }}
              title={`${slot.teamName} (Seed ${slot.seed})`}
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

const StandingsRow = memo(function StandingsRow({
  row,
  rank,
  isExpanded,
  onToggle,
}: {
  row: StandingsRow;
  rank: number;
  isExpanded: boolean;
  onToggle: () => void;
}) {
  const hasFractional = row.live_wins !== row.wins;
  return (
    <Fragment>
      <tr
        style={{ borderBottom: isExpanded ? "none" : "1px solid var(--border)", cursor: "pointer" }}
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
          <span className="font-semibold text-sm" style={{ color: "var(--text)" }}>
            {row.entry_name}
          </span>
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
        <td style={{ padding: "10px 12px", width: 60 }}>
          <span className="tabular-nums text-xs" style={{ color: "var(--text-muted)" }}>
            {row.tiebreaker_points}
          </span>
        </td>
      </tr>
      {isExpanded && (
        <tr>
          <td colSpan={6} style={{ padding: 0 }}>
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
  const rows = limit != null ? allRows.slice(0, limit) : allRows;

  function toggleRow(entryName: string) {
    setExpandedRows(prev => {
      const next = new Set(prev);
      if (next.has(entryName)) next.delete(entryName);
      else next.add(entryName);
      return next;
    });
  }

  const lastUpdated = dataUpdatedAt ? new Date(dataUpdatedAt).toLocaleTimeString() : null;

  if (isLoading) {
    return (
      <div className="mc-card p-6 flex items-center justify-center gap-3 text-sm" style={{ color: "var(--text-muted)" }}>
        <div
          className="w-5 h-5 rounded-full border-2 border-t-transparent animate-spin"
          style={{ borderColor: "var(--accent)", borderTopColor: "transparent" }}
        />
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

  if (rows.length === 0) {
    return (
      <div className="mc-card p-6 text-center text-sm" style={{ color: "var(--text-muted)" }}>
        No entries found yet.
      </div>
    );
  }

  return (
    <div className="mc-card overflow-hidden">
      <div
        className="px-5 py-3 border-b flex items-center justify-between"
        style={{ borderColor: "var(--border)" }}
      >
        <h2 className="font-semibold text-sm" style={{ color: "var(--text)" }}>
          Standings
          {limit != null && allRows.length > limit && (
            <span className="ml-2 text-xs font-normal" style={{ color: "var(--text-muted)" }}>
              (Top {limit} of {allRows.length})
            </span>
          )}
        </h2>
        {lastUpdated && (
          <span className="text-xs" style={{ color: "var(--text-muted)" }}>
            Updated {lastUpdated}
          </span>
        )}
      </div>

      <div className="overflow-x-auto">
        <table className="w-full text-sm">
          <thead>
            <tr>
              {["", "#", "Entry", "Wins", "Total (w/Live)", "TB"].map((h) => (
                <th
                  key={h}
                  style={{
                    padding: "8px 12px",
                    textAlign: "left",
                    fontSize: "11px",
                    fontWeight: 600,
                    textTransform: "uppercase",
                    letterSpacing: "0.05em",
                    color: "var(--text-muted)",
                    borderBottom: "1px solid var(--border)",
                    background: "var(--bg-card)",
                  }}
                >
                  {h}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {rows.map((row, i) => (
              <StandingsRow
                key={row.entry_name}
                row={row}
                rank={i + 1}
                isExpanded={expandedRows.has(row.entry_name)}
                onToggle={() => toggleRow(row.entry_name)}
              />
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}
