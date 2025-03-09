
raw_selections <- read.csv("./Data/raw_selections_real_test.csv")
raw_selections_long <- raw_selections %>% select(Entry, Team1, Team2, Team3, Team4, Team5, Team6, Team7, Team8, Team9, Team10, Team11, Team12, Team13, Team14, Team15, Team16, Team17, Team18, Team19, Team20, Team21, Team22, Team23, Team24) %>%
  pivot_longer(!Entry, names_to = "teamgroup", values_to = "Team") %>% filter(!is.na(Team))




# Database connection
con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv('host'),
  port = Sys.getenv('port'),
  dbname = Sys.getenv('dbname'),
  user = Sys.getenv('user'),
  password = Sys.getenv('password')
)

DBI::dbWriteTable(con, 'raw_selections_real', raw_selections_long)

#i may not need this table in future.  This should hook directly to the actual entries.

