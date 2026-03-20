## -----------------------------------------------------------------------------
## Global setup for the Shiny app
## This file is sourced once at app start. Keep it minimal: load libraries,
## set basic configuration, open a DB connection, and load small lookup tables
## that the server and UI will use during the session.
## -----------------------------------------------------------------------------

## Core libraries used by the app (UI, data wrangling, DB, email)
library(shiny)
library(RCurl)
library(rjson)
library(janitor)
library(tibble)
library(tidyr)
library(dplyr)
library(DT)
library(stringr)
library(shinydashboard)
library(shinycssloaders)
library(formattable)
library(plotly)
library(reshape)
library(sqldf)
library(RPostgres)
library(uuid)
library(shinyjs)
library(emayili)

## Ensure consistent timezone for date/time operations used throughout the app
Sys.setenv(TZ = 'US/Pacific')

## Auto-refresh controls how frequently some UI components poll for updates.
## Small integer (seconds) - tuned in the UI where needed.
autorefresh <- 2

## Database connection
## The app expects the following environment variables to be set in the
## environment that launches the app: `host`, `port`, `dbname`, `user`, `password`.
## Do NOT hard-code secrets in the repository; use an external mechanism
## (PowerShell script, .env loader, CI secrets, etc.) when running locally or
## in production.
con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv('host'),
  port = Sys.getenv('port'),
  dbname = Sys.getenv('dbname'),
  user = Sys.getenv('user'),
  password = Sys.getenv('password')
)

## -----------------------------------------------------------------------------
## Load small, frequently used tables from the database
## These tables are used by both the UI and server logic. Keep these queries
## small and cache their results here to avoid repeated round-trips during a
## single user session. If the tables grow large, consider lazy-loading or
## pagination strategies in the server code instead.
## -----------------------------------------------------------------------------


# Basic team lookup (team name, seed, and cost). Sorted by seed for UI.
teams <- dbGetQuery(con, "SELECT team_name, seed, cost FROM march_madness_teams") %>%
  arrange(seed)



# Flattened submissions list useful for joins and quick lookups
raw_selections_long <- dbGetQuery(con, "SELECT entry_name as Entry, team_name as Team FROM submissions") %>%
  # normalize column display names to TitleCase for downstream code
  rename_with(~ paste0(toupper(substr(., 1, 1)), substr(., 2, nchar(.))))



# Full bracket entries table (used to render the main standings view). We
# provide a simple row index column `X` to make joins deterministic.
raw_selections <- dbGetQuery(con, "SELECT * FROM bracket_entries5") %>%
  dplyr::rename(X = entry_id) %>%
  mutate(X = seq_len(nrow(.)))

# Master team table (distinct list of teams). Used for lookups and reporting.
master <- dbGetQuery(con, "SELECT DISTINCT * FROM all_teams")
