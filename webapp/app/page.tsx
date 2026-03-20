import { redirect } from "next/navigation";

export default function Home() {
  const phase = process.env.UI_PHASE ?? "entry_phase";

  if (phase === "post_entry") {
    redirect("/leaderboard");
  } else {
    redirect("/entry");
  }
}
