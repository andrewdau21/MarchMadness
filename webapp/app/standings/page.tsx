import { Header } from "@/components/layout/Header";
import { FullStandingsTable } from "@/components/standings/FullStandingsTable";

export const metadata = {
  title: "Full Standings | March Capness 2026",
};

export default function StandingsPage() {
  return (
    <div>
      <Header
        title="Full Standings"
        subtitle="All entries sorted by wins. Click any row to expand team selections. Faded logos are eliminated teams."
      />
      <FullStandingsTable />
    </div>
  );
}
