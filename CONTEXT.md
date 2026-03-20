# MarchCapness — Project Context

Load this file at the start of a new conversation to get Claude up to speed.

---

## What this is

A salary cap March Madness bracket game at **marchcapness.com**.
Users pick NCAA tournament teams within a $100 budget. Score = total wins by selected teams.
Tiebreaker = combined points in the National Championship game.

**Seed costs:** 1=$25, 2=$19, 3=$13, 4=$12, 5=$11, 6=$10, 7=$8, 8-9=$5, 10-11=$4, 12=$3, 13-14=$2, 15-16=$1

---

## Repo

**GitHub:** https://github.com/andrewdau21/MarchMadness
**Active branch:** `andrew-react-conversion` — all new work goes here, NEVER commit to `main`
**Production site (R Shiny):** running from `main` branch, DO NOT touch

---

## Tech stack (new React app)

| Layer | Tech |
|---|---|
| Framework | Next.js 16, App Router, TypeScript |
| Styling | Tailwind CSS, CSS variables (dark theme) |
| Data fetching | TanStack React Query (`staleTime: 60s`, no auto-poll, no refetchOnWindowFocus) |
| Tables | Plain React + `memo()` (TanStack Table removed from standings — caused freeze) |
| DB | node-postgres (`pg`), **READ-ONLY**, lazy pool init (no connection at import time) |
| Email | nodemailer (Gmail SMTP) |
| Deployment | Docker → DigitalOcean Container Registry (`daucontainerregistry/andrew:test`) |
| CI/CD | `.github/workflows/deploy.yml` — triggers on push to `andrew-react-conversion` |

---

## App structure

```
webapp/
├── app/
│   ├── page.tsx              # Redirects: entry_phase→/entry, post_entry→/leaderboard
│   ├── entry/page.tsx        # Entry form
│   ├── leaderboard/page.tsx  # Top 15 standings + live scoreboard
│   ├── standings/page.tsx    # Full standings table
│   └── api/
│       ├── standings/        # GET — ESPN + DB, 60s server-side memory cache
│       ├── live-scores/      # GET scores, POST {teamName} → entry count
│       ├── teams/            # GET team list from DB
│       └── submit-entry/     # POST — validates, emails entry. NO DB WRITES.
├── components/
│   ├── leaderboard/StandingsTable.tsx   # Plain React table, memo'd rows, Set for expansion
│   ├── leaderboard/LiveScores.tsx
│   ├── standings/FullStandingsTable.tsx
│   └── entry/EntryForm.tsx
├── lib/
│   ├── db.ts        # Lazy pg pool, READ-ONLY query() helper, SSL always on (DO requires it)
│   ├── espn.ts      # fetchScoreboard(), fetchWinProbability()
│   ├── standings.ts # computeStandings() — full port of standings_function.R
│   ├── email.ts     # sendEntryConfirmationEmail()
│   └── types.ts     # All TypeScript types + SEED_COSTS + BUDGET_CAP
```

---

## Database (READ-ONLY — active production DB)

**Host:** DigitalOcean managed PostgreSQL — always requires SSL (`rejectUnauthorized: false`)
**Env vars:** `host`, `port`, `dbname`, `user`, `password` (same names as original R app)

| Table | Key columns |
|---|---|
| `march_madness_teams` | team_name, seed, cost |
| `bracket_entries5` | Entry, Team1–Team24 |
| `submission_totals` | entry_name, tiebreaker_points |
| `submissions` | entry_name, team_name |
| `all_teams` | name, cost, logo(?), seed(?) |

---

## Key decisions & gotchas

- **NO DB WRITES** anywhere — entries go via email only (ncaasalarycap@gmail.com)
- **SSL always on** in `db.ts` — DigitalOcean requires it, don't make it conditional
- **Lazy DB pool** — pool is created on first `query()` call, not at module import (Next.js build was failing otherwise)
- **`force-dynamic`** on all API routes — prevents Next.js from trying to statically render them at build time
- **`ignoreBuildErrors: true`** in `next.config.ts` — unblocked Docker build; fix TS errors separately
- **No TanStack Table in StandingsTable** — caused "page not responding" freeze on row expand; replaced with plain React + `memo()`
- **`refetchOnWindowFocus: false`** globally — clicking rows was triggering expensive ESPN API refetches
- **Server-side cache** in `/api/standings/route.ts` — 60s in-memory cache prevents repeated ESPN hammering
- **`isPlaying`** field on `StandingsTeamSlot` — shows LIVE badge on actively playing teams in expanded rows
- **UI_PHASE** env var controls routing: `entry_phase` → /entry, `post_entry` → /leaderboard

---

## Running locally

```bash
cd webapp
# ensure webapp/.env.local is filled in (see .env.local.example)
npm install
npm run dev   # starts on port 3000 (or next available)
```

`.env.local` needs: `host`, `port=5432`, `dbname`, `user`, `password`, `DB_SSL=false` (local) or `true` (DO), `SMTP_USERNAME`, `SMTP_PASSWORD`, `UI_PHASE`

---

## ESPN API

- Scoreboard: `http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=357&dates={YEAR}0319-{YEAR}0414&groups=100`
- Win probability: `http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event={gameId}`
- First Four games are filtered out (notes field contains "first four")
- Tournament dates are auto-calculated from current year in `espn.ts`
