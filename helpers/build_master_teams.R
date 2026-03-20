#build logos
#you want the date range to span the entire tournament
#if that full date range causes issues with API (ie too many rows), then shrink it.
#this has to be updated annually
#compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20220315-20220404&groups=100')


library(httr)
library(purrr)
compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/teams?limit=999')

myfile <- GET(compiled_url)
content <- content(myfile)

full_teams <- content[["sports"]][[1]][["leagues"]][[1]][["teams"]]

df <- map_dfr(full_teams, as.data.frame) %>%
  select(team.id, team.shortDisplayName)

write.csv(df, './data/allteams2024.csv', row.names=FALSE)

df <- map_dfr(full_teams, as.data.frame) %>%
  select(team.id, team.shortDisplayName) %>%
  dplyr::rename(team1 = team.id) %>%
  dplyr::rename(name = team.shortDisplayName ) %>%
  dplyr::mutate(team2 = team1
                ,team3 = team1
                ,team4 = team1
                ,team5 = team1
                ,team6 = team1
                ,team7 = team1
                ,team8 = team1
                ,team9 = team1
                ,team10 = team1
                ,team11 = team1
                ,team12 = team1
                ,team13 = team1
                ,team14 = team1
                ,team15 = team1
                ,team16 = team1
                ,team17 = team1
                ,team18 = team1
                ,team19 = team1
                ,team20 = team1
                ,team21 = team1
                ,team22 = team1
                ,team23 = team1
                ,team24 = team1
                                )

#need to add cost...based on seed.

df2 <- df %>% left_join(all_ranks, by = 'team1')


write.csv(df2, './Data/all_teams.csv', row.names=FALSE)



compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20220315-20220404&groups=100')

myfile <- getURL(compiled_url)
#content <- content(myfile)

#work <- fromJSON(content)

#use fromJSON to convert to data frame. 
raw_espn_json <- fromJSON(myfile) 





espn_games_2018 <- raw_espn_json[["events"]] %>% 
  enframe() %>% 
  mutate(row_id = name) %>%
  select(-name) %>%
  unnest_wider(value) %>% 
  mutate(game_id = id) %>%
  select(-id)

espn_season_2018 <- espn_games_2018 %>% 
  unnest_wider(season) %>% 
  unchop(competitions)

ex_id_subset <- espn_season_2018[["competitions"]][[1]][["competitors"]][[1]][["id"]]

ex_id_hoist <- espn_season_2018 %>%
  hoist(competitions, home_team_id = list("competitors", 1, "id"))

all.equal(ex_id_subset, ex_id_hoist[["home_team_id"]][1])

espn_season_2018_final <- espn_season_2018 %>%
  
  #hoist it up
  hoist(
    competitions,
    home_team_id = list("competitors", 1, "id"),
    home_team_abb = list("competitors", 1, "team", "abbreviation"),
    home_team_name = list("competitors", 1, "team", "shortDisplayName"),
    away_team_id = list("competitors", 2, "id"),
    away_team_abb = list("competitors", 2, "team", "abbreviation"),
    away_team_name = list("competitors", 2, "team", "shortDisplayName"),
    home_score = list("competitors", 1, "score"),
    away_score = list("competitors", 2, "score"),
    home_logo = list("competitors",1,"team","logo"),
    away_logo = list("competitors",2,"team","logo"),
    home_prob = list("situation","lastPlay","probability","homeWinPercentage"),
    venue = list("venue","fullName"),
    home_team_seed = list("competitors",1,"curatedRank","current"),
    away_team_seed = list("competitors",2,"curatedRank","current")
    
  ) %>%
  hoist(
    status,
    time = list("displayClock"),
    period = list("period"),
    current_stat = list("type","description")
    
  ) %>%
  select( -where(is.list), -row_id) %>%
  janitor::clean_names() %>%
  #dplyr::rename(season_type = type) %>%
  mutate(season_type = type) %>%
  select(-type) %>%
  mutate(
    season_type = case_when(
      season_type == 1L ~ "Preseason",
      season_type == 2L ~ "Regular Season",
      season_type == 3L ~ "Playoffs",
      TRUE ~ as.character(season_type),
    )
  )

#espn_season_2018_final %>% 
# count(season_type) %>%

live_scores <- espn_season_2018_final %>%
  select(name,short_name,home_prob, away_team_abb, away_score,home_team_abb, home_score,time, period,home_logo, away_logo, current_stat,home_prob, home_team_name, away_team_name) %>%
  mutate(home_logo = paste0('<img src="',home_logo,'" height="52"></img>')) %>%
  mutate(away_logo = paste0('<img src="',away_logo,'" height="52"></img>'))


home_teams <- live_scores %>%
  select(home_team_name, home_logo) %>%
  mutate(team_name = home_team_name) %>%
  mutate(team_logo_id = gsub('<img src="https://a.espncdn.com/i/teamlogos/ncaa/500/', '', home_logo)) %>%
  mutate(team_logo_id = gsub('.png" height="52"></img>', '', team_logo_id)) %>%
  select(-home_logo, -home_team_name) %>% filter(team_name != 'TBD') 

away_teams <- live_scores %>%
  select(away_team_name, away_logo) %>%
  mutate(team_name = away_team_name) %>%
  mutate(team_logo_id = gsub('<img src="https://a.espncdn.com/i/teamlogos/ncaa/500/', '', away_logo)) %>%
  mutate(team_logo_id = gsub('.png" height="52"></img>', '', team_logo_id)) %>%
  select(-away_logo, -away_team_name) %>% filter(team_name != 'TBD')

stacked <- rbind(home_teams, away_teams)

truly_unique <- unique(stacked)

write.csv(truly_unique, 'C:/Users/Andrew/Documents/MarchMadness/mmapp/MarchMadness/Data/teams_2023.csv')

         