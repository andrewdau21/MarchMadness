"use client";

import { useState, Fragment } from "react";
import { useQuery } from "@tanstack/react-query";
import {
  useReactTable,
  getCoreRowModel,
  getSortedRowModel,
  getExpandedRowModel,
  flexRender,
  createColumnHelper,
  type SortingState,
  type ExpandedState,
} from "@tanstack/react-table";
import type { StandingsRow, StandingsApiResponse, StandingsTeamSlot } from "@/lib/types";

// ─── Team Logo Grid (expanded row) ───────────────────────────────────────────

function TeamLogoGrid({ teams }: { teams: StandingsTeamSlot[] }) {
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
}

// ─── Column helper ────────────────────────────────────────────────────────────

const col = createColumnHelper<StandingsRow>();

const columns = [
  // Expand toggle
  col.display({
    id: "expander",
    header: () => null,
    cell: ({ row }) => (
      <button
        onClick={row.getToggleExpandedHandler()}
        className="px-2 text-base leading-none transition-transform"
        style={{
          color: "var(--text-muted)",
          transform: row.getIsExpanded() ? "rotate(90deg)" : "rotate(0deg)",
        }}
        aria-label={row.getIsExpanded() ? "Collapse" : "Expand"}
      >
        ›
      </button>
    ),
    size: 32,
  }),

  // Rank
  col.display({
    id: "rank",
    header: "#",
    cell: ({ row }) => (
      <span className="font-bold tabular-nums" style={{ color: "var(--text-muted)", fontSize: "13px" }}>
        {row.index + 1}
      </span>
    ),
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
    size: 70,
  }),

  col.accessor("live_wins", {
    id: "live_wins",
    header: "Total (w/Live)",
    cell: (info) => {
      const liveWins = info.getValue();
      const wins = info.row.original.wins;
      const hasFractional = liveWins !== wins;
      return (
        <span
          className="font-bold tabular-nums text-sm"
          style={{ color: hasFractional ? "var(--accent)" : "var(--text)" }}
        >
          {liveWins.toFixed(2)}
        </span>
      );
    },
    size: 110,
  }),

  col.accessor("tiebreaker_points", {
    id: "tiebreaker",
    header: "TB",
    cell: (info) => (
      <span className="tabular-nums text-xs" style={{ color: "var(--text-muted)" }}>
        {info.getValue()}
      </span>
    ),
    size: 60,
  }),
];

// ─── Standings Table ──────────────────────────────────────────────────────────

export function StandingsTable({ limit }: { limit?: number }) {
  const [sorting, setSorting] = useState<SortingState>([]);
  const [expanded, setExpanded] = useState<ExpandedState>({});

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

  const allRows = data?.standings ?? [];
  const rows = limit != null ? allRows.slice(0, limit) : allRows;

  const table = useReactTable({
    data: rows,
    columns,
    state: { sorting, expanded },
    onSortingChange: setSorting,
    onExpandedChange: setExpanded,
    getCoreRowModel: getCoreRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getExpandedRowModel: getExpandedRowModel(),
    getRowCanExpand: () => true,
  });

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
      {/* Table header row */}
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
                  <tr key={`${row.id}-expanded`}>
                    <td colSpan={columns.length} style={{ padding: 0 }}>
                      <TeamLogoGrid teams={row.original.teams} />
                    </td>
                  </tr>
                )}
              </Fragment>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}
