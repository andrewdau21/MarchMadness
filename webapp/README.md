# March Capness — Next.js Webapp

A Next.js 14 replacement for the R Shiny March Madness salary cap bracket game.

## Running Locally

### Prerequisites

- Node.js 20+
- Access to the PostgreSQL database (read-only)
- Gmail account with an App Password for SMTP

### Setup

```bash
cd webapp
npm install
cp .env.local.example .env.local
# Fill in your actual credentials in .env.local
npm run dev
```

Open [http://localhost:3000](http://localhost:3000).

The root `/` redirects automatically based on `UI_PHASE`:
- `entry_phase` → `/entry` (show the entry form)
- `post_entry`  → `/leaderboard` (show standings + live scores)

### Environment Variables

Copy `.env.local.example` to `.env.local` and populate:

| Variable         | Description                                            |
|------------------|--------------------------------------------------------|
| `host`           | PostgreSQL host                                        |
| `port`           | PostgreSQL port (default 5432)                         |
| `dbname`         | Database name                                          |
| `user`           | Database user (read-only account recommended)          |
| `password`       | Database password                                      |
| `DB_SSL`         | Set `true` if your DB requires SSL                     |
| `SMTP_USERNAME`  | Gmail address used to send confirmation emails         |
| `SMTP_PASSWORD`  | Gmail App Password (not your account password)         |
| `UI_PHASE`       | `entry_phase` or `post_entry`                          |

**Important:** The app is strictly read-only against the database. No writes occur anywhere in the codebase.

### Building for Production

```bash
npm run build
npm start
```

### Docker

```bash
docker build -t marchcapness-next .
docker run -p 8080:8080 --env-file .env.local marchcapness-next
```

## Deployment

Pushing to `main` with changes inside `webapp/` triggers the GitHub Actions workflow (`.github/workflows/webapp.yml`), which builds and pushes the Docker image to:

```
registry.digitalocean.com/daucontainerregistry/marchcapness-next
```

Requires the `DIGITALOCEAN_ACCESS_TOKEN` secret set in the repository.

## Architecture Notes

- **API routes** at `/api/teams`, `/api/standings`, `/api/live-scores`, `/api/submit-entry` are all server-side Next.js Route Handlers.
- **Standings** are computed server-side by fetching the ESPN scoreboard + win probability endpoints and joining with the database bracket entries.
- **Entry submission** sends an HTML email via Gmail SMTP and returns a UUID entry ID — nothing is written to the database.
- All pages are client components that use TanStack Query for data fetching with 60-second auto-refresh.
