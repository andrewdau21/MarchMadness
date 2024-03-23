
standings_function <- function(){
compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20240321-20240414&groups=500')

myfile <- getURL(compiled_url, simplifyVector=FALSE)

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
    #home_prob = list("situation","lastPlay","probability","homeWinPercentage"),
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


#AAAA <<- espn_season_2018_final
current_games <- espn_season_2018_final %>%
  filter(current_stat != "Final") %>% 
  filter(current_stat != 'Scheduled') %>%
  select(game_id)
if(nrow(current_games > 0 ))
{
temp <-  live_wins_function(current_games)


espn_season_2018_final <- espn_season_2018_final %>% left_join(temp, by="game_id")
}
else
{
  espn_season_2018_final$home_prob = NA
}
#espn_season_2018_final %>% 
# count(season_type) %>%

live_scores <- espn_season_2018_final %>%
  select(name,short_name,home_prob, away_team_abb, away_score,home_team_abb, home_score,time, period,home_logo, away_logo, current_stat,home_prob, home_team_name, away_team_name) %>%
  mutate(home_logo = paste0('<img src="',home_logo,'" height="52"></img>')) %>%
  mutate(away_logo = paste0('<img src="',away_logo,'" height="52"></img>')) %>%
  mutate(home_score = as.numeric(home_score)) %>%
  mutate(away_score = as.numeric(away_score))

#live_scores2 <<- live_scores

#print(live_scores)

#calc final wins.

wins <- live_scores %>%
  filter(current_stat == 'Final') %>%
  mutate(winner = ifelse(as.numeric(home_score) > as.numeric(away_score), home_team_name, away_team_name))%>%
  count(winner, name="wins")

#wins2 <<- wins

uncontested_wins <-  live_scores %>%
  filter(current_stat == 'Uncontested') %>%
  mutate(winner = ifelse(home_score > away_score, home_team_name, away_team_name))%>%
  count(winner, name="wins") %>%
  mutate(wins = wins/2)

wins <- rbind(wins, uncontested_wins)

wins$winner <- as.character(wins$winner)

live_wins_home <- live_scores %>%
  filter(current_stat %in% c('In Progress', "Halftime")) %>%
  mutate(winner = home_team_name) %>%
  mutate(live_wins = home_prob) %>%
  dplyr::select(winner, live_wins)

lsdat <- live_scores

live_wins_away <-  live_scores %>%
  filter(current_stat %in% c('In Progress', "Halftime")) %>%
  mutate(winner = away_team_name) %>%
  mutate(live_wins = 1- home_prob) %>%
  dplyr::select(winner, live_wins)



live_wins <- rbind(live_wins_home, live_wins_away)
live_wins$winner <- as.character(live_wins$winner)


total_wins <- wins %>% full_join(live_wins, by = "winner")

#print(total_wins)


##aliveteams
##base on if loss = 0
##dead teams is 
library(reshape)

raw_selections_melted <- melt(raw_selections, id=c("Entry"))
temp_standings <- raw_selections_melted %>% left_join(total_wins, by = c("value"="winner")) 


standings <- temp_standings %>%
  group_by(Entry) %>%
  summarise(wins = sum(wins,na.rm=TRUE), live_wins=sum(live_wins, na.rm=TRUE))

standings <- standings %>%
  mutate(live_wins = live_wins + wins)

standings_live <- standings %>% left_join(raw_selections, by = "Entry")

losers <- live_scores %>%
  filter(current_stat == 'Final') %>%
  mutate(loser = ifelse(home_score < away_score, home_team_name, away_team_name))%>%
  count(loser, name="losses")


uncontested_losers <- live_scores %>%
  filter(current_stat == 'Uncontested') %>%
  mutate(loser = ifelse(home_score < away_score, home_team_name, away_team_name))%>%
  count(loser, name="losses")

losers <- rbind(losers, uncontested_losers)




losers$loser <- as.character(losers$loser)

kickitout <<- losers

temp_money <- raw_selections_melted %>% left_join(losers, by = c("value"="loser")) 

temp_money2 <- temp_money %>% left_join(master, by=c("value"= "name"))
aaa <<- temp_money2

temp_money3 <- temp_money2 %>%
  filter(!is.na(losses)) %>%
  group_by(Entry) %>%
  summarise(dead_money = sum(cost,na.rm=TRUE)) 

#abc <<- temp_money3

standings_live <- standings_live %>% left_join(temp_money3, by = "Entry") %>%
  replace_na(list(dead_money=0)) %>%
  mutate(live_money = 100-dead_money) %>%
  mutate(loser1 = ifelse(Team1 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser2 = ifelse(Team2 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser3 = ifelse(Team3 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser4 = ifelse(Team4 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser5 = ifelse(Team5 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser6 = ifelse(Team6 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser7 = ifelse(Team7 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser8 = ifelse(Team8 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser9 = ifelse(Team9 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser10 = ifelse(Team10 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser11 = ifelse(Team11 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser12 = ifelse(Team12 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser13 = ifelse(Team13 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser14 = ifelse(Team14 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser15 = ifelse(Team15 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser16 = ifelse(Team16 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser17 = ifelse(Team17 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser18 = ifelse(Team18 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser19 = ifelse(Team19 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser20 = ifelse(Team20 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser21 = ifelse(Team21 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser22 = ifelse(Team22 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser23 = ifelse(Team23 %in% as.vector(losers$loser), .1, 1)) %>%
  mutate(loser24 = ifelse(Team24 %in% as.vector(losers$loser), .1, 1)) 


standings_live <-standings_live %>% left_join(tiebreaker, by=c("Entry"))

return(standings_live)

}
