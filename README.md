# 🏀 March Capness

> *The annual March Madness salary cap bracket contest — pick your teams, manage your budget, and ride it out.*

Live at **[marchcapness.com](https://marchcapness.com)**

---

## What Is This?

March Capness is a custom-built March Madness contest where you don't pick a bracket — you pick a **roster of teams** under a **$100 salary cap**. Teams are priced by seed. Load up on favorites, or go full chaos goblin with a fistful of double-digit seeds. Your score is the total wins your teams accumulate across the 63-game tournament.

First run: 2019. Now in its sixth year.

---

## How to Play

### The Salary Cap

You have **$100** to spend on any combination of teams from the tournament field.

| Seed | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 |
|------|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| Cost | $25 | $19 | $13 | $12 | $11 | $10 | $8 | $5 | $5 | $4 | $4 | $3 | $2 | $2 | $1 | $1 |

No restrictions on how you allocate — as long as you're at or under $100, anything goes. Load up all four 1-seeds for $100 and pray for chalk. Or stack eight 12-seeds and embrace the madness.

### Scoring

- **1 point** per tournament win (First Four games do not count)
- **63 total games** played across the main bracket
- Tiebreaker: total points scored in the National Championship game
- A winning score is typically around **22 wins** — the all-time record is **24**

### Example Roster

| Team | Seed | Cost |
|------|------|------|
| Duke | #1 | $25 |
| Kansas | #1 | $25 |
| Tennessee | #2 | $19 |
| Marquette | #3 | $13 |
| Gonzaga | #4 | $12 |
| Drake | #5 | $6 (×... wait, $11) |

Mix and match to fill your roster — no limit on number of teams, just the $100 cap.

---

## The App

### Tech Stack

| Layer | Technology |
|-------|-----------|
| Frontend | Next.js 15 (App Router, TypeScript) |
| Styling | Tailwind CSS |
| Data Fetching | TanStack React Query |
| Database | PostgreSQL (DigitalOcean Managed) |
| Live Scores | ESPN Public API |
| Hosting | DigitalOcean App Platform |
| CI/CD | GitHub Actions → DigitalOcean Container Registry |

### Features

- **Live leaderboard** with real-time win probability from ESPN
- **Auto-refresh** every 60 seconds (toggleable)
- **Live scoreboard** with in-game scores, win %, and TV network
- **Eliminated team indicators** — faded logos with a red ✕
- **"Teams alive"** count shown per entry when expanded
- **Live $** — remaining salary cap value based on teams still alive
- **Entry search + pin** — find your entry and track it across sessions
- **Mobile-first** responsive design

---

## Running Locally

```bash
cd webapp
npm install
cp ../.env.example .env.local   # fill in your DB credentials
npm run dev
```

App runs at `http://localhost:3000`.

### Environment Variables

| Variable | Description |
|----------|-------------|
| `host` | PostgreSQL host |
| `port` | PostgreSQL port (default 5432) |
| `dbname` | Database name |
| `user` | Database user |
| `password` | Database password |
| `SMTP_HOST` | Email host (for entry submissions) |
| `SMTP_USER` | Email user |
| `SMTP_PASS` | Email password |
| `ENTRY_TO_EMAIL` | Where entry submissions are sent |

---

## Deployment

Pushes to `main` trigger an automatic build and deploy via GitHub Actions:

1. Docker image built and pushed to DigitalOcean Container Registry
2. DigitalOcean App Platform pulls and redeploys

---

## Annual Setup Checklist

Each year before the tournament:

- [ ] Update tournament date range in `webapp/lib/espn.ts` (`getScoreboardUrl`)
- [ ] Update tournament date range in `webapp/lib/standings.ts` (`fetchScoreboard`)
- [ ] Update entry deadline in `webapp/app/entry/page.tsx` (`ENTRY_DEADLINE`)
- [ ] Rebuild `march_madness_teams` DB table with current year's ESPN data
- [ ] Rebuild `all_teams` DB table with current year's team/seed/logo data
- [ ] Truncate `bracket_entries5` and `submission_totals` tables for new entries
- [ ] Confirm `bracket_entries5` schema matches `Team1`–`Team24` column format

---

## Database Tables

| Table | Description |
|-------|-------------|
| `bracket_entries5` | Wide-format entries (Entry, Team1–Team24) |
| `submission_totals` | Tiebreaker points per entry |
| `all_teams` | Master team list with ESPN IDs, seeds, logos |
| `march_madness_teams` | Seed-to-cost mapping (rebuilt annually) |

---

*Built with ❤️ and a $100 budget.*
