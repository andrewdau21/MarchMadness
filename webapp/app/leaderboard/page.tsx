import { Header } from "@/components/layout/Header";
import { StandingsTable } from "@/components/leaderboard/StandingsTable";
import { LiveScores } from "@/components/leaderboard/LiveScores";
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

      <div className="grid grid-cols-1 xl:grid-cols-3 gap-6">
        {/* Standings — takes 2/3 width on xl */}
        <div className="xl:col-span-2">
          <StandingsTable limit={15} />
        </div>

        {/* Live scores — takes 1/3 width on xl */}
        <div className="xl:col-span-1">
          <LiveScores />
        </div>
      </div>
    </div>
  );
}
