/**
 * Email sending via nodemailer (Gmail SMTP).
 * Used exclusively for entry confirmation – no DB writes occur here.
 */

import nodemailer from "nodemailer";
import type { EntrySubmission } from "./types";

interface TeamWithCost {
  teamName: string;
  seed: number;
  cost: number;
}

function buildTransport() {
  return nodemailer.createTransport({
    service: "gmail",
    auth: {
      user: process.env.SMTP_USERNAME,
      pass: process.env.SMTP_PASSWORD,
    },
  });
}

function buildEmailHtml(
  submission: EntrySubmission,
  teams: TeamWithCost[],
  totalCost: number,
  entryId: string
): string {
  const tableRows = teams
    .map(
      (t) => `
        <tr>
          <td style="padding:8px;border:1px solid #333;color:#e5e7eb;">${t.teamName}</td>
          <td style="padding:8px;border:1px solid #333;text-align:center;color:#e5e7eb;">${t.seed}</td>
          <td style="padding:8px;border:1px solid #333;text-align:right;color:#00A36C;font-weight:bold;">$${t.cost}</td>
        </tr>`
    )
    .join("\n");

  return `<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <style>
    body { font-family: Arial, sans-serif; background: #0f1117; color: #e5e7eb; margin: 0; padding: 0; }
    .wrapper { max-width: 640px; margin: 0 auto; padding: 32px 20px; }
    h1 { color: #00A36C; margin-bottom: 4px; }
    .subtitle { color: #9ca3af; margin-bottom: 24px; font-size: 14px; }
    .card { background: #1a1d27; border: 1px solid #2a2d3a; border-radius: 8px; padding: 20px; margin-bottom: 20px; }
    table { width: 100%; border-collapse: collapse; }
    th { background: #2a2d3a; padding: 8px; border: 1px solid #333; text-align: left; color: #9ca3af; font-size: 13px; text-transform: uppercase; letter-spacing: 0.05em; }
    .total-row { font-size: 18px; font-weight: bold; margin-top: 12px; color: #e5e7eb; }
    .total-row span { color: #00A36C; }
    .tiebreaker { margin-top: 8px; color: #9ca3af; }
    .entry-id { font-family: monospace; font-size: 12px; color: #6b7280; margin-top: 4px; }
    .footer { font-size: 12px; color: #6b7280; text-align: center; margin-top: 32px; border-top: 1px solid #2a2d3a; padding-top: 16px; }
    .good-luck { color: #00A36C; font-size: 18px; font-weight: bold; margin-top: 16px; }
  </style>
</head>
<body>
  <div class="wrapper">
    <h1>March Capness 2026</h1>
    <p class="subtitle">Your entry is locked and loaded.</p>

    <div class="card">
      <p>Hi <strong>${escapeHtml(submission.entryName)}</strong>,</p>
      <p>Thanks for entering! Here&rsquo;s your bracket summary.</p>
      <p class="entry-id">Entry ID: ${entryId}</p>
    </div>

    <div class="card">
      <h3 style="margin-top:0;color:#e5e7eb;">Selected Teams</h3>
      <table>
        <thead>
          <tr>
            <th>Team</th>
            <th>Seed</th>
            <th>Cost</th>
          </tr>
        </thead>
        <tbody>
          ${tableRows}
        </tbody>
      </table>

      <div class="total-row">Total: <span>$${totalCost}</span> / $100</div>
      <div class="tiebreaker">Tiebreaker (Championship Points): <strong>${submission.tiebreakerPoints}</strong></div>
    </div>

    <p class="good-luck">Good luck! May your picks be sharp. 🏀</p>
    <p style="color:#9ca3af;font-size:14px;">Need to modify your entry? Contact Paul Schulz.</p>

    <div class="footer">
      March Capness &bull; Let the games begin!
    </div>
  </div>
</body>
</html>`;
}

function escapeHtml(str: string): string {
  return str
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

export async function sendEntryConfirmationEmail(
  submission: EntrySubmission,
  teams: TeamWithCost[],
  totalCost: number,
  entryId: string
): Promise<void> {
  const transport = buildTransport();

  const html = buildEmailHtml(submission, teams, totalCost, entryId);

  await transport.sendMail({
    from: `"March Capness" <${process.env.SMTP_USERNAME}>`,
    to: [submission.email, "ncaasalarycap@gmail.com"],
    subject: "2026 March Capness Entry Confirmation",
    html,
  });
}
