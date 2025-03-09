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


Sys.setenv(TZ='US/Pacific')

#raw_selections <- read.csv("./Data/raw_selections_real_test.csv")
# raw_selections_long <- raw_selections %>% select(Entry, Team1, Team2, Team3, Team4, Team5, Team6, Team7, Team8, Team9, Team10, Team11, Team12, Team13, Team14, Team15, Team16, Team17, Team18, Team19, Team20, Team21, Team22, Team23, Team24) %>%
#   pivot_longer(!Entry, names_to = "teamgroup", values_to = "Team") %>% filter(!is.na(Team))


#standings <- read.csv("./Data/scores_temp.csv")
#standings <- standings %>% arrange(desc(Score))
#master <- read.csv("./Data/all_teams.csv", stringsAsFactors = FALSE)  
#tiebreaker2 <- read.csv("./Data/tiebreaker_test.csv", stringsAsFactors = FALSE)  
#tiebreaker <- read.csv("./Data/raw_selections.csv") %>% mutate(Entry = Your.Name.) %>% mutate(TieBreaker = as.integer(Total.Points.In.The.National.Title.Game..used.as.tiebreaker..)) %>% select(Entry, TieBreaker)
#tiebreaker <- read.csv("./Data/tiebreaker_master.csv", stringsAsFactors=FALSE)

autorefresh <- 2



# Database connection
con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv('host'),
  port = Sys.getenv('port'),
  dbname = Sys.getenv('dbname'),
  user = Sys.getenv('user'),
  password = Sys.getenv('password')
)



# Get teams data from database
teams <- dbGetQuery(con, "SELECT team_name, seed, cost FROM march_madness_teams") %>% arrange(seed)

raw_selections_long <- dbGetQuery(con, "SELECT entry_name as Entry, team_name as Team  FROM submissions") %>%
  rename_with(~ paste0(toupper(substr(., 1, 1)), substr(., 2, nchar(.))))

 tiebreaker <- dbGetQuery(con, "SELECT entry_name as Entry, tiebreaker_points as Tiebreaker  FROM submission_totals") %>%
   rename_with(~ paste0(toupper(substr(., 1, 1)), substr(., 2, nchar(.)))) %>%
   dplyr::rename(TieBreaker = Tiebreaker) %>%
   mutate (X = row_number()) %>% 
   select(X, Entry, TieBreaker)
#so weird that this was breaking without row index.  Probably need to fix that.

 raw_selections<- dbGetQuery(con, "SELECT * FROM bracket_entries5")  %>%
  # select(-Entry) %>%
   dplyr::rename(X = entry_id) %>%
  # dplyr::rename(Entry = Entry_Name) %>%
   mutate (X = row_number()) 
 
 
master<- dbGetQuery(con, "SELECT * FROM all_teams")  
 