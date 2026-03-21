import { redirect } from "next/navigation";
import { Header } from "@/components/layout/Header";
import { EntryForm } from "@/components/entry/EntryForm";

export const metadata = {
  title: "Submit Entry | March Capness 2026",
};

const ENTRY_DEADLINE = new Date("2026-03-18T12:00:00-07:00");

export default function EntryPage() {
  const entriesClosed = new Date() > ENTRY_DEADLINE;

  if (entriesClosed) {
    redirect("/leaderboard");
  }

  return (
    <div>
      <Header
        title="March Capness 2026"
        subtitle="Build your bracket within the $100 salary cap. Higher seeds cost more — pick wisely!"
        badge="Entry Phase"
      />

      <div className="mc-card p-4 mb-6 grid grid-cols-2 sm:grid-cols-4 gap-4 text-sm">
        {[
          { label: "Salary Cap", value: "$100" },
          { label: "Scoring", value: "1 pt / win" },
          { label: "Games", value: "63 total" },
          { label: "Tiebreaker", value: "Champ pts" },
        ].map(({ label, value }) => (
          <div key={label} className="text-center">
            <div className="font-bold text-lg" style={{ color: "var(--accent)" }}>
              {value}
            </div>
            <div className="text-xs" style={{ color: "var(--text-muted)" }}>
              {label}
            </div>
          </div>
        ))}
      </div>

      <EntryForm />
    </div>
  );
}
