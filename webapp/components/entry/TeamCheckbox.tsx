"use client";

import type { Team } from "@/lib/types";

interface TeamCheckboxProps {
  team: Team;
  selected: boolean;
  onToggle: (teamName: string) => void;
  disabled?: boolean;
}

export function TeamCheckbox({ team, selected, onToggle, disabled }: TeamCheckboxProps) {
  const handleClick = () => {
    if (!disabled || selected) {
      onToggle(team.team_name);
    }
  };

  return (
    <button
      type="button"
      onClick={handleClick}
      disabled={disabled && !selected}
      className="w-full text-left px-3 py-2 rounded-lg border transition-all duration-150 flex items-center justify-between gap-2"
      style={{
        background: selected ? "rgba(0,163,108,0.12)" : "var(--bg)",
        borderColor: selected ? "var(--accent)" : "var(--border)",
        cursor: disabled && !selected ? "not-allowed" : "pointer",
        opacity: disabled && !selected ? 0.5 : 1,
      }}
      aria-pressed={selected}
    >
      <div className="flex items-center gap-2 min-w-0">
        {/* Seed badge */}
        <span
          className="shrink-0 text-xs font-bold rounded px-1.5 py-0.5"
          style={{
            background: "var(--muted)",
            color: selected ? "var(--accent)" : "var(--text-muted)",
            minWidth: "28px",
            textAlign: "center",
          }}
        >
          {team.seed}
        </span>
        <span
          className="text-sm font-medium truncate"
          style={{ color: selected ? "var(--text)" : "var(--text-muted)" }}
        >
          {team.team_name}
        </span>
      </div>
      <span
        className="shrink-0 text-sm font-bold"
        style={{ color: selected ? "var(--accent)" : "var(--text-muted)" }}
      >
        ${team.cost}
      </span>
    </button>
  );
}
