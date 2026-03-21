import { Header } from "@/components/layout/Header";
import { LeaderboardClient } from "@/components/leaderboard/LeaderboardClient";
import Link from "next/link";

export const metadata = {
  title: "Leaderboard | March Capness 2026",
};

export default function LeaderboardPage() {
  return (
    <div>
      <div className="flex items-start justify-between mb-6">
        <Header
          title="Leaderboard"
          subtitle="Live standings with in-progress win probability. Auto-refreshes every 60 seconds."
        />
        <Link
          href="/standings"
          className="shrink-0 mt-1 text-sm px-3 py-1.5 rounded-lg border transition-colors"
          style={{
            borderColor: "var(--border)",
            color: "var(--text-muted)",
          }}
        >
          Full Standings →
        </Link>
      </div>

      <LeaderboardClient standingsLimit={15} />
    </div>
  );
}
