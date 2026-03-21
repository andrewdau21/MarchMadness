"use client";

import { useState, useEffect } from "react";
import { StandingsTable } from "./StandingsTable";
import { LiveScores } from "./LiveScores";

const AUTO_REFRESH_KEY = "marchcapness_auto_refresh";

export function LeaderboardClient({ standingsLimit }: { standingsLimit?: number }) {
  const [autoRefresh, setAutoRefresh] = useState(true);

  useEffect(() => {
    const saved = localStorage.getItem(AUTO_REFRESH_KEY);
    if (saved !== null) setAutoRefresh(saved === "true");
  }, []);

  function toggleAutoRefresh() {
    setAutoRefresh(prev => {
      const next = !prev;
      localStorage.setItem(AUTO_REFRESH_KEY, String(next));
      return next;
    });
  }

  return (
    <>
      {/* Auto-refresh toggle */}
      <div className="flex justify-end mb-3">
        <button
          onClick={toggleAutoRefresh}
          className="flex items-center gap-1.5 text-xs px-2.5 py-1 rounded-md border transition-colors"
          style={{
            borderColor: autoRefresh ? "var(--accent)" : "var(--border)",
            color: autoRefresh ? "var(--accent)" : "var(--text-muted)",
            background: autoRefresh ? "rgba(0,163,108,0.08)" : "transparent",
          }}
          title={autoRefresh ? "Auto-refresh on — click to pause" : "Auto-refresh off — click to enable"}
        >
          <span style={{ fontSize: "11px" }}>⟳</span>
          {autoRefresh ? "Live (60s)" : "Paused"}
        </button>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <div>
          <StandingsTable limit={standingsLimit} autoRefresh={autoRefresh} />
        </div>
        <div>
          <LiveScores autoRefresh={autoRefresh} />
        </div>
      </div>
    </>
  );
}
