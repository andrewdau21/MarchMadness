"use client";

import { useState } from "react";
import { useQuery } from "@tanstack/react-query";
import type { Team, TeamsApiResponse } from "@/lib/types";
import { BUDGET_CAP, SEED_COSTS } from "@/lib/types";

export function EntryForm() {
  const [entryName, setEntryName] = useState("");
  const [email, setEmail] = useState("");
  const [tiebreakerPoints, setTiebreakerPoints] = useState("");
  const [selectedTeams, setSelectedTeams] = useState<string[]>([]);
  const [submitting, setSubmitting] = useState(false);
  const [result, setResult] = useState<{ success: boolean; message: string } | null>(null);

  const { data, isLoading } = useQuery<TeamsApiResponse>({
    queryKey: ["teams"],
    queryFn: async () => {
      const res = await fetch("/api/teams");
      if (!res.ok) throw new Error("Failed to load teams");
      return res.json();
    },
  });

  const teams = data?.teams ?? [];
  const totalCost = selectedTeams.reduce((sum, name) => {
    const team = teams.find((t) => t.team_name === name);
    return sum + (team?.cost ?? 0);
  }, 0);
  const remaining = BUDGET_CAP - totalCost;

  function toggleTeam(teamName: string) {
    setSelectedTeams((prev) =>
      prev.includes(teamName) ? prev.filter((t) => t !== teamName) : [...prev, teamName]
    );
  }

  async function handleSubmit(e: React.FormEvent) {
    e.preventDefault();
    setSubmitting(true);
    setResult(null);
    try {
      const res = await fetch("/api/submit-entry", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          entryName,
          email,
          selectedTeams,
          tiebreakerPoints: Number(tiebreakerPoints),
        }),
      });
      const json = await res.json();
      setResult({
        success: json.success,
        message: json.success
          ? "Entry submitted! Check your email for confirmation."
          : json.error ?? "Something went wrong.",
      });
      if (json.success) {
        setSelectedTeams([]);
        setEntryName("");
        setEmail("");
        setTiebreakerPoints("");
      }
    } catch {
      setResult({ success: false, message: "Network error — please try again." });
    } finally {
      setSubmitting(false);
    }
  }

  // Group teams by seed
  const bySeed = teams.reduce<Record<number, Team[]>>((acc, t) => {
    (acc[t.seed] ??= []).push(t);
    return acc;
  }, {});

  return (
    <form onSubmit={handleSubmit} className="space-y-6">
      {/* Entry info */}
      <div className="mc-card p-4 space-y-4">
        <h2 className="font-semibold text-sm" style={{ color: "var(--text)" }}>Your Info</h2>
        <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
          <div>
            <label className="block text-xs mb-1" style={{ color: "var(--text-muted)" }}>Entry Name</label>
            <input
              required
              value={entryName}
              onChange={(e) => setEntryName(e.target.value)}
              placeholder="e.g. Andrew's Picks"
              className="w-full px-3 py-2 rounded-lg text-sm border focus:outline-none"
              style={{ background: "var(--muted)", borderColor: "var(--border)", color: "var(--text)" }}
            />
          </div>
          <div>
            <label className="block text-xs mb-1" style={{ color: "var(--text-muted)" }}>Email</label>
            <input
              required
              type="email"
              value={email}
              onChange={(e) => setEmail(e.target.value)}
              placeholder="you@example.com"
              className="w-full px-3 py-2 rounded-lg text-sm border focus:outline-none"
              style={{ background: "var(--muted)", borderColor: "var(--border)", color: "var(--text)" }}
            />
          </div>
        </div>
        <div className="max-w-xs">
          <label className="block text-xs mb-1" style={{ color: "var(--text-muted)" }}>
            Tiebreaker — Total points in the National Championship game
          </label>
          <input
            required
            type="number"
            min={1}
            value={tiebreakerPoints}
            onChange={(e) => setTiebreakerPoints(e.target.value)}
            placeholder="e.g. 142"
            className="w-full px-3 py-2 rounded-lg text-sm border focus:outline-none"
            style={{ background: "var(--muted)", borderColor: "var(--border)", color: "var(--text)" }}
          />
        </div>
      </div>

      {/* Budget tracker */}
      <div className="mc-card p-4 flex items-center justify-between">
        <span className="text-sm" style={{ color: "var(--text-muted)" }}>
          {selectedTeams.length} team{selectedTeams.length !== 1 ? "s" : ""} selected · ${totalCost} spent
        </span>
        <span
          className="font-bold text-lg"
          style={{ color: remaining >= 0 ? "var(--accent)" : "#ef4444" }}
        >
          ${remaining} remaining
        </span>
      </div>

      {/* Team selection */}
      <div className="mc-card overflow-hidden">
        <div className="px-4 py-3 border-b" style={{ borderColor: "var(--border)" }}>
          <h2 className="font-semibold text-sm" style={{ color: "var(--text)" }}>Pick Your Teams</h2>
        </div>
        {isLoading ? (
          <div className="p-8 text-center text-sm" style={{ color: "var(--text-muted)" }}>Loading teams...</div>
        ) : (
          <div className="p-4 space-y-4">
            {Object.entries(bySeed).map(([seed, seedTeams]) => (
              <div key={seed}>
                <div className="flex items-center gap-2 mb-2">
                  <span className="text-xs font-semibold" style={{ color: "var(--text-muted)" }}>
                    #{seed} Seed
                  </span>
                  <span className="text-xs" style={{ color: "var(--accent)" }}>
                    ${SEED_COSTS[Number(seed)]}
                  </span>
                </div>
                <div className="flex flex-wrap gap-2">
                  {seedTeams.map((team) => {
                    const selected = selectedTeams.includes(team.team_name);
                    const wouldExceed = !selected && totalCost + team.cost > BUDGET_CAP;
                    return (
                      <button
                        key={team.team_name}
                        type="button"
                        disabled={wouldExceed}
                        onClick={() => toggleTeam(team.team_name)}
                        className="px-3 py-1.5 rounded-lg text-xs font-medium transition-colors border"
                        style={{
                          background: selected ? "rgba(0,163,108,0.15)" : "transparent",
                          borderColor: selected ? "var(--accent)" : "var(--border)",
                          color: selected ? "var(--accent)" : wouldExceed ? "var(--text-muted)" : "var(--text)",
                          opacity: wouldExceed ? 0.4 : 1,
                          cursor: wouldExceed ? "not-allowed" : "pointer",
                        }}
                      >
                        {team.team_name}
                      </button>
                    );
                  })}
                </div>
              </div>
            ))}
          </div>
        )}
      </div>

      {/* Result */}
      {result && (
        <div
          className="mc-card p-4 text-sm text-center font-medium"
          style={{ color: result.success ? "var(--accent)" : "#ef4444" }}
        >
          {result.message}
        </div>
      )}

      {/* Submit */}
      <button
        type="submit"
        disabled={submitting || selectedTeams.length === 0 || remaining < 0}
        className="w-full py-3 rounded-xl font-semibold text-sm transition-colors"
        style={{
          background: "var(--accent)",
          color: "#fff",
          opacity: submitting || selectedTeams.length === 0 || remaining < 0 ? 0.5 : 1,
          cursor: submitting || selectedTeams.length === 0 || remaining < 0 ? "not-allowed" : "pointer",
        }}
      >
        {submitting ? "Submitting…" : "Submit Entry"}
      </button>
    </form>
  );
}
