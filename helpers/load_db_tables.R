# Load required libraries
library(DBI)
library(RPostgres)

# Establish connection to PostgreSQL
con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv('host'),         
  port = Sys.getenv('port'),             
  dbname = Sys.getenv('dbname'),
  user = Sys.getenv('user'),
  password = Sys.getenv('password')
)

#write the allteams database table
allteams <- read.csv('./data/allteams2024.csv')
dbWriteTable(con,'allteams',allteams)

df <- data.frame(team_name = c('Arizona','Nebraska','Iowa','Iowa State'),
                 seed = c(1,8,9,16),
                 cost = c(25,5,5,1))

library(dplyr)

# Define seeds and their corresponding costs
seeds <- 1:16
costs <- c(25, 19, 13, 12, 11, 10, 8, 5, 5, 4, 4, 3, 2, 2, 1, 1)

# Creative college team names (4 per seed)
team_names <- c(
  # Seed 1
  "Duke Blue Devils", "Kansas Jayhawks", "Gonzaga Bulldogs", "North Carolina Tar Heels",
  # Seed 2
  "Villanova Wildcats", "Arizona Wildcats", "Kentucky Wildcats", "Baylor Bears",
  # Seed 3
  "Purdue Boilermakers", "Texas Longhorns", "UCLA Bruins", "Michigan Wolverines",
  # Seed 4
  "Illinois Fighting Illini", "Tennessee Volunteers", "Iowa Hawkeyes", "Arkansas Razorbacks",
  # Seed 5
  "Houston Cougars", "LSU Tigers", "Ohio State Buckeyes", "Connecticut Huskies",
  # Seed 6
  "Alabama Crimson Tide", "USC Trojans", "Colorado Buffaloes", "Seton Hall Pirates",
  # Seed 7
  "Michigan State Spartans", "Wisconsin Badgers", "Xavier Musketeers", "Providence Friars",
  # Seed 8
  "San Diego State Aztecs", "Boise State Broncos", "TCU Horned Frogs", "Creighton Bluejays",
  # Seed 9
  "Marquette Golden Eagles", "Memphis Tigers", "Saint Mary's Gaels", "Murray State Racers",
  # Seed 10
  "Miami Hurricanes", "Davidson Wildcats", "Loyola Chicago Ramblers", "San Francisco Dons",
  # Seed 11
  "Virginia Tech Hokies", "Notre Dame Fighting Irish", "Indiana Hoosiers", "Rutgers Scarlet Knights",
  # Seed 12
  "New Mexico State Aggies", "UAB Blazers", "Richmond Spiders", "Chattanooga Mocs",
  # Seed 13
  "Vermont Catamounts", "South Dakota State Jackrabbits", "Akron Zips", "Colgate Raiders",
  # Seed 14
  "Montana State Bobcats", "Longwood Lancers", "Yale Bulldogs", "Georgia State Panthers",
  # Seed 15
  "Delaware Fightin' Blue Hens", "Saint Peter's Peacocks", "Jacksonville State Gamecocks", "Bryant Bulldogs",
  # Seed 16
  "Norfolk State Spartans", "Texas Southern Tigers", "Wright State Raiders", "Long Beach State Beach"
)

# Create the data frame
teams_data <- data.frame(
  team_name = team_names,
  seed = rep(seeds, each = 4),
  cost = rep(costs, each = 4)
) %>%
  arrange(seed)

# Sample output (first 8 rows)
print(head(teams_data, 8))

dbWriteTable(con,'march_madness_teams',teams_data, append=TRUE)


DBI::db

# Test the connection by running a simple query
result <- dbGetQuery(con, "SELECT version();")
print(result)

# Perform a sample query (e.g., selecting data from a table)
data <- dbGetQuery(con, "SELECT * FROM teams LIMIT 5;")
print(data)

# Close the connection when done
dbDisconnect(con)