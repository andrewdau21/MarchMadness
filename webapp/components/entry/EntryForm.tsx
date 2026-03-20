"use client";

import { useState, useMemo } from "react";
import { useQuery } from "@tanstack/react-query";
import { TeamCheckbox } from "./TeamCheckbox";
import type { Team, TeamsApiResponse } from "@/lib/types";
import { BUDGET_CAP } from "@/lib/types";

interface FormState {
  entryName: string;
  email: string;
  tiebreakerPoints: string;
  selectedTeams: Set<string>;
}

interface SubmitState {
  status: "idle" | "submitting" | "success" | "error";
  message: string;
  entryId?: string;
}

async function fetchTeams(): Promise<TeamsApiResponse> {
  const res = await fetch("/api/teams");
  if (!res.ok) throw new Error("Failed to load teams");
  return res.json();
}

async function submitEntry(payload: {
  entryName: string;
  email: string;
  selectedTeams: string[];
  tiebreakerPoints: number;
}) {
  const res = await fetch("/api/submit-entry", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(payload),
  });
  return res.json();
}

export function EntryForm() {
  const { data, isLoading, isError } = useQuery({
    queryKey: ["teams"],
    queryFn: fetchTeams,
    staleTime: Infinity,
    refetchInterval: false,
  });

  const [form, setForm] = useState<FormState>({
    entryName: "",
    email: "",
    tiebreakerPoints: "",
    selectedTeams: new Set(),
  });

  const [submitState, setSubmitState] = useState<SubmitState>({
    status: "idle",
    message: "",
  });

  const teams: Team[] = data?.teams ?? [];

  // Group teams by seed
  const teamsBySeed = useMemo(() => {
    const map = new Map<number, Team[]>();
    for (const team of teams) {
      if (!map.has(team.seed)) map.set(team.seed, []);
      map.get(team.seed)!.push(team);
    }
    return map;
  }, [teams]);

  const seedOrder = useMemo(
    () => Array.from(teamsBySeed.keys()).sort((a, b) => a - b),
    [teamsBySeed]
  );

  // Compute total cost
  const totalCost = useMemo(() => {
    return teams
      .filter((t) => form.selectedTeams.has(t.team_name))
      .reduce((sum, t) => sum + t.cost, 0);
  }, [teams, form.selectedTeams]);

  const remaining = BUDGET_CAP - totalCost;
  const overBudget = totalCost > BUDGET_CAP;

  function toggleTeam(teamName: string) {
    setForm((prev) => {
      const next = new Set(prev.selectedTeams);
      if (next.has(teamName)) {
        next.delete(teamName);
      } else {
        next.add(teamName);
      }
      return { ...prev, selectedTeams: next };
    });
  }

  function isTeamDisabled(team: Team): boolean {
    if (form.selectedTeams.has(team.team_name)) return false;
    return remaining < team.cost;
  }

  const emailValid = /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(form.email);
  const tiebreakerNum = parseFloat(form.tiebreakerPoints);
  const tiebreakerValid = !isNaN(tiebreakerNum) && tiebreakerNum > 0;
  const canSubmit =
    form.entryName.trim().length > 0 &&
    emailValid &&
    form.selectedTeams.size > 0 &&
    tiebreakerValid &&
    !overBudget &&
    submitState.status !== "submitting";

  async function handleSubmit(e: React.FormEvent) {
    e.preventDefault();
    if (!canSubmit) return;

    setSubmitState({ status: "submitting", message: "Submitting your entry..." });

    try {
      const result = await submitEntry({
        entryName: form.entryName.trim(),
        email: form.email.trim(),
        selectedTeams: Array.from(form.selectedTeams),
        tiebreakerPoints: tiebreakerNum,
      });

      if (result.success) {
        setSubmitState({
          status: "success",
          message: `Entry submitted! Entry ID: ${result.entryId}. A confirmation email has been sent.`,
          entryId: result.entryId,
        });
        // Reset form
        setForm({
          entryName: "",
          email: "",
          tiebreakerPoints: "",
          selectedTeams: new Set(),
        });
      } else {
        setSubmitState({ status: "error", message: result.error ?? "Submission failed" });
      }
    } catch {
      setSubmitState({ status: "error", message: "Network error – please try again." });
    }
  }

  if (isLoading) {
    return (
      <div className="flex items-center justify-center py-24">
        <div className="text-center">
          <div
            className="inline-block w-8 h-8 rounded-full border-2 border-t-transparent animate-spin mb-3"
            style={{ borderColor: "var(--accent)", borderTopColor: "transparent" }}
          />
          <p style={{ color: "var(--text-muted)" }}>Loading teams...</p>
        </div>
      </div>
    );
  }

  if (isError) {
    return (
      <div
        className="rounded-xl p-6 text-center"
        style={{ background: "rgba(239,68,68,0.1)", border: "1px solid rgba(239,68,68,0.3)" }}
      >
        <p className="text-red-400 font-medium">Failed to load team list. Please refresh the page.</p>
      </div>
    );
  }

  return (
    <form onSubmit={handleSubmit} className="space-y-6">
      {/* Success Banner */}
      {submitState.status === "success" && (
        <div
          className="rounded-xl p-4"
          style={{ background: "rgba(0,163,108,0.12)", border: "1px solid var(--accent)" }}
        >
          <p className="font-semibold" style={{ color: "var(--accent)" }}>
            Entry Submitted Successfully!
          </p>
          <p className="text-sm mt-1" style={{ color: "var(--text-muted)" }}>
            {submitState.message}
          </p>
        </div>
      )}

      {/* Error Banner */}
      {submitState.status === "error" && (
        <div
          className="rounded-xl p-4"
          style={{ background: "rgba(239,68,68,0.1)", border: "1px solid rgba(239,68,68,0.4)" }}
        >
          <p className="font-semibold text-red-400">Submission Error</p>
          <p className="text-sm mt-1" style={{ color: "var(--text-muted)" }}>
            {submitState.message}
          </p>
        </div>
      )}

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Left column: info + budget tracker */}
        <div className="lg:col-span-1 space-y-4">
          {/* Entry details */}
          <div className="mc-card p-5 space-y-4">
            <h2 className="font-semibold text-base" style={{ color: "var(--text)" }}>
              Your Entry
            </h2>

            <div>
              <label className="block text-xs font-medium mb-1" style={{ color: "var(--text-muted)" }}>
                Entry Name *
              </label>
              <input
                type="text"
                value={form.entryName}
                onChange={(e) => setForm((p) => ({ ...p, entryName: e.target.value }))}
                placeholder="e.g. Bracket McBracketface"
                className="w-full px-3 py-2 rounded-lg text-sm border"
                style={{
                  background: "var(--bg)",
                  borderColor: "var(--border)",
                  color: "var(--text)",
                }}
                required
              />
            </div>

            <div>
              <label className="block text-xs font-medium mb-1" style={{ color: "var(--text-muted)" }}>
                Email Address *
              </label>
              <input
                type="email"
                value={form.email}
                onChange={(e) => setForm((p) => ({ ...p, email: e.target.value }))}
                placeholder="you@example.com"
                className="w-full px-3 py-2 rounded-lg text-sm border"
                style={{
                  background: "var(--bg)",
                  borderColor: form.email && !emailValid ? "rgb(239,68,68)" : "var(--border)",
                  color: "var(--text)",
                }}
                required
              />
              {form.email && !emailValid && (
                <p className="text-xs mt-1 text-red-400">Enter a valid email address.</p>
              )}
            </div>

            <div>
              <label className="block text-xs font-medium mb-1" style={{ color: "var(--text-muted)" }}>
                Tiebreaker — Combined Championship Points *
              </label>
              <input
                type="number"
                min="1"
                step="1"
                value={form.tiebreakerPoints}
                onChange={(e) => setForm((p) => ({ ...p, tiebreakerPoints: e.target.value }))}
                placeholder="e.g. 143"
                className="w-full px-3 py-2 rounded-lg text-sm border"
                style={{
                  background: "var(--bg)",
                  borderColor:
                    form.tiebreakerPoints && !tiebreakerValid
                      ? "rgb(239,68,68)"
                      : "var(--border)",
                  color: "var(--text)",
                }}
                required
              />
              <p className="text-xs mt-1" style={{ color: "var(--text-muted)" }}>
                Predict total points scored in the National Championship game.
              </p>
            </div>
          </div>

          {/* Budget tracker */}
          <div className="mc-card p-5">
            <h2 className="font-semibold text-base mb-3" style={{ color: "var(--text)" }}>
              Salary Cap
            </h2>

            {/* Budget bar */}
            <div
              className="w-full rounded-full h-3 mb-2 overflow-hidden"
              style={{ background: "var(--muted)" }}
            >
              <div
                className="h-3 rounded-full transition-all duration-300"
                style={{
                  width: `${Math.min((totalCost / BUDGET_CAP) * 100, 100)}%`,
                  background: overBudget ? "rgb(239,68,68)" : "var(--accent)",
                }}
              />
            </div>

            <div className="flex justify-between text-sm">
              <span style={{ color: "var(--text-muted)" }}>
                Spent: <span className="font-bold" style={{ color: overBudget ? "rgb(239,68,68)" : "var(--text)" }}>
                  ${totalCost}
                </span>
              </span>
              <span style={{ color: "var(--text-muted)" }}>
                Cap: <span className="font-bold" style={{ color: "var(--text)" }}>${BUDGET_CAP}</span>
              </span>
            </div>

            {overBudget ? (
              <p className="text-xs mt-2 text-red-400 font-medium">
                Over budget by ${totalCost - BUDGET_CAP}! Remove some teams.
              </p>
            ) : (
              <p className="text-xs mt-2" style={{ color: "var(--accent)" }}>
                ${remaining} remaining
              </p>
            )}

            <div className="mt-3 pt-3 border-t" style={{ borderColor: "var(--border)" }}>
              <p className="text-xs" style={{ color: "var(--text-muted)" }}>
                {form.selectedTeams.size} team{form.selectedTeams.size !== 1 ? "s" : ""} selected
              </p>
            </div>
          </div>

          {/* Selected teams summary */}
          {form.selectedTeams.size > 0 && (
            <div className="mc-card p-5">
              <h2 className="font-semibold text-sm mb-3" style={{ color: "var(--text)" }}>
                Selected Teams
              </h2>
              <div className="space-y-1">
                {teams
                  .filter((t) => form.selectedTeams.has(t.team_name))
                  .sort((a, b) => a.seed - b.seed)
                  .map((t) => (
                    <div key={t.team_name} className="flex justify-between text-xs">
                      <span style={{ color: "var(--text-muted)" }}>
                        <span
                          className="font-bold mr-1"
                          style={{ color: "var(--accent)" }}
                        >
                          {t.seed}
                        </span>
                        {t.team_name}
                      </span>
                      <span className="font-medium" style={{ color: "var(--text)" }}>
                        ${t.cost}
                      </span>
                    </div>
                  ))}
              </div>
            </div>
          )}

          {/* Submit button */}
          <button
            type="submit"
            disabled={!canSubmit}
            className="w-full py-3 rounded-xl font-semibold text-sm transition-all duration-150"
            style={{
              background: canSubmit ? "var(--accent)" : "var(--muted)",
              color: canSubmit ? "white" : "var(--text-muted)",
              cursor: canSubmit ? "pointer" : "not-allowed",
            }}
          >
            {submitState.status === "submitting" ? (
              <span className="flex items-center justify-center gap-2">
                <span
                  className="inline-block w-4 h-4 rounded-full border-2 border-t-transparent animate-spin"
                  style={{ borderColor: "white", borderTopColor: "transparent" }}
                />
                Submitting...
              </span>
            ) : (
              "Submit Entry"
            )}
          </button>
          <p className="text-xs text-center" style={{ color: "var(--text-muted)" }}>
            Entry confirmation will be emailed to you.
          </p>
        </div>

        {/* Right columns: team picker by seed */}
        <div className="lg:col-span-2">
          <div className="mc-card p-5">
            <h2 className="font-semibold text-base mb-1" style={{ color: "var(--text)" }}>
              Select Your Teams
            </h2>
            <p className="text-xs mb-4" style={{ color: "var(--text-muted)" }}>
              Pick any combination within the $100 salary cap. Teams are grouped by seed.
            </p>

            <div className="space-y-5">
              {seedOrder.map((seed) => {
                const seedTeams = teamsBySeed.get(seed) ?? [];
                const seedCost = seedTeams[0]?.cost ?? 0;
                return (
                  <div key={seed}>
                    <div className="flex items-center gap-2 mb-2">
                      <span
                        className="text-xs font-bold px-2 py-0.5 rounded"
                        style={{ background: "var(--muted)", color: "var(--accent)" }}
                      >
                        Seed {seed}
                      </span>
                      <span className="text-xs" style={{ color: "var(--text-muted)" }}>
                        — ${seedCost} each
                      </span>
                    </div>
                    <div className="grid grid-cols-1 sm:grid-cols-2 gap-1.5">
                      {seedTeams.map((team) => (
                        <TeamCheckbox
                          key={team.team_name}
                          team={team}
                          selected={form.selectedTeams.has(team.team_name)}
                          onToggle={toggleTeam}
                          disabled={isTeamDisabled(team)}
                        />
                      ))}
                    </div>
                  </div>
                );
              })}
            </div>
          </div>
        </div>
      </div>
    </form>
  );
}
