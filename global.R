library(shiny)
library(RCurl)
print("I'm in a new log")
library(rjson)
#library(jsonlite)
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

Sys.setenv(TZ='US/Pacific')

raw_selections <- read.csv("./Data/raw_selections_real.csv")
raw_selections_long <- raw_selections %>% select(Entry, Team1, Team2, Team3, Team4, Team5, Team6, Team7, Team8, Team9, Team10, Team11, Team12, Team13, Team14, Team15, Team16) %>%
  pivot_longer(!Entry, names_to = "teamgroup", values_to = "Team") %>% filter(!is.na(Team))
standings <- read.csv("./Data/scores_temp.csv")
standings <- standings %>% arrange(desc(Score))
master <- read.csv("./Data/all_teams.csv", stringsAsFactors = FALSE)  
tiebreaker <- read.csv("./Data/raw_selections.csv") %>% mutate(Entry = Your.Name.) %>% mutate(TieBreaker = as.integer(Total.Points.In.The.National.Title.Game..used.as.tiebreaker..)) %>% select(Entry, TieBreaker)
#tiebreaker <- read.csv("./Data/tiebreaker_master.csv", stringsAsFactors=FALSE)


get_names_of_entries_by_team <- function(team_name){
  
  sqlf <- paste0('SELECT Entry FROM raw_selections_long WHERE Team = "', team_name, '"')
  return(sqldf(sqlf))
}

get_count_of_entries_by_team <- function(team_name) {
  sqlf <- paste0('SELECT Count(Entry) as Count FROM raw_selections_long WHERE Team = "', team_name, '"')
  return(sqldf(sqlf))
}