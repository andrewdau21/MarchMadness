



# Database connection
con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv('host'),
  port = Sys.getenv('port'),
  dbname = Sys.getenv('dbname'),
  user = Sys.getenv('user'),
  password = Sys.getenv('password')
)

DBI::dbWriteTable(con, 'all_teams', master)
#i may not need this table in future.  This should hook directly to the actual entries.

