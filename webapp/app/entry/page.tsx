import { Header } from "@/components/layout/Header";
import { EntryForm } from "@/components/entry/EntryForm";

export const metadata = {
  title: "Submit Entry | March Capness 2026",
};

export default function EntryPage() {
  return (
    <div>
      <Header
        title="March Capness 2026"
        subtitle="Build your bracket within the $100 salary cap. Higher seeds cost more — pick wisely!"
        badge="Entry Phase"
      />

      {/* Rules summary */}
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
