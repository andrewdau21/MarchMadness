import nodemailer from "nodemailer";
import type { EntrySubmission } from "./types";

export async function sendEntryConfirmationEmail(
  submission: EntrySubmission,
  resolvedTeams: { teamName: string; seed: number; cost: number }[],
  totalCost: number,
  entryId: string
) {
  const transporter = nodemailer.createTransport({
    host: process.env.SMTP_HOST,
    port: Number(process.env.SMTP_PORT ?? 587),
    secure: false,
    auth: {
      user: process.env.SMTP_USER,
      pass: process.env.SMTP_PASS,
    },
  });

  const teamRows = resolvedTeams
    .map((t) => `  Seed ${t.seed} | $${t.cost} | ${t.teamName}`)
    .join("\n");

  const body = `
New March Capness Entry
=======================
Entry ID:   ${entryId}
Name:       ${submission.entryName}
Email:      ${submission.email}
Tiebreaker: ${submission.tiebreakerPoints} pts
Total Cost: $${totalCost} / $100

Teams Selected (${resolvedTeams.length}):
${teamRows}
`.trim();

  await transporter.sendMail({
    from: process.env.SMTP_USER,
    to: process.env.ENTRY_TO_EMAIL,
    subject: `[March Capness] New Entry — ${submission.entryName}`,
    text: body,
  });
}
