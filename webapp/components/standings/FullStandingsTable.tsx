"use client";

import { useState, Fragment } from "react";
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

// ─── Column helper ────────────────────────────────────────────────────────────

const col = createColumnHelper<StandingsRow>();

const COLUMNS = [
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
    cell: ({ row }) => (
      <span className="font-bold tabular-nums text-sm" style={{ color: "var(--text-muted)" }}>
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

  // Team count
  col.display({
    id: "teams_count",
    header: "Teams",
    cell: ({ row }) => {
      const active = row.original.teams.filter((t) => t.teamName && t.opacity >= 1).length;
      const total = row.original.teams.filter((t) => t.teamName).length;
      return (
        <span className="text-xs tabular-nums" style={{ color: "var(--text-muted)" }}>
          <span style={{ color: "var(--accent)" }}>{active}</span>/{total}
        </span>
      );
    },
    size: 70,
  }),
];

// ─── Full Standings Table ─────────────────────────────────────────────────────

export function FullStandingsTable() {
  const [sorting, setSorting] = useState<SortingState>([{ id: "live_wins", desc: true }]);
  const [expanded, setExpanded] = useState<ExpandedState>({});
  const [globalFilter, setGlobalFilter] = useState("");

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

  const table = useReactTable({
    data: rows,
    columns: COLUMNS,
    state: { sorting, expanded, globalFilter },
    onSortingChange: setSorting,
    onExpandedChange: setExpanded,
    onGlobalFilterChange: setGlobalFilter,
    globalFilterFn: (row, _columnId, value: string) =>
      row.original.entry_name.toLowerCase().includes(value.toLowerCase()),
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
        <div className="flex items-center gap-4">
          <h2 className="font-semibold text-sm" style={{ color: "var(--text)" }}>
            Full Standings
          </h2>
          <span className="text-xs" style={{ color: "var(--text-muted)" }}>
            {table.getFilteredRowModel().rows.length} entries
          </span>
        </div>
        <div className="flex items-center gap-3">
          {lastUpdated && (
            <span className="text-xs hidden sm:block" style={{ color: "var(--text-muted)" }}>
              Updated {lastUpdated}
            </span>
          )}
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
            }}
          />
        </div>
      </div>

      {/* Legend */}
      <div
        className="px-5 py-2 border-b text-xs flex gap-4"
        style={{ borderColor: "var(--border)", color: "var(--text-muted)" }}
      >
        <span>
          <span style={{ color: "var(--accent)" }}>Total</span> = wins + live fractional wins
        </span>
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
                    <td colSpan={COLUMNS.length} style={{ padding: 0 }}>
                      <ExpandedTeamGrid teams={row.original.teams} />
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
