compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20240317-20240323&groups=500')

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
  ) %>% 
  arrange(date)

#trim off first 4 rows, play in games
temp_df <- espn_season_2018_final[1:4,] %>%
  mutate(team_name = paste0(home_team_name, ' or ', away_team_name)) %>%
  mutate(team_id = paste0(home_team_id, ' or ', away_team_id)) %>%
  mutate(seed = home_team_seed) %>%
  select(team_name, team_id, seed)

library(dplyr)
ranks_home <- espn_season_2018_final[5:36,] %>%
  select(home_team_id, home_team_seed, home_team_name) %>%
  dplyr::rename(team_id = home_team_id)%>%
  dplyr::rename(seed = home_team_seed ) %>%
  dplyr::rename(team_name = home_team_name)

ranks_away <- espn_season_2018_final[5:36,] %>%
  select(away_team_id, away_team_seed, away_team_name) %>%
  dplyr::rename(team_id = away_team_id)%>%
  dplyr::rename(seed = away_team_seed ) %>%
  dplyr::rename(team_name = away_team_name)

all_ranks <- dplyr::bind_rows(ranks_home, ranks_away, temp_df) %>% filter(seed != 99) %>%
  mutate(cost = case_when(
    seed == 1  ~ 25,
    seed == 2 ~ 19,
    seed == 3 ~ 13,
    seed == 4 ~ 12,
    seed == 5 ~ 11,
    seed == 6 ~ 10,
    seed == 7 ~ 8,
    seed == 8 ~ 5,
    seed == 9 ~ 5,
    seed == 10 ~ 4,
    seed == 11 ~ 4,
    seed == 12 ~ 3,
    seed == 13 ~ 2,
    seed == 14 ~ 2,
    seed == 15 ~ 1,
    seed == 16 ~ 1
  )) %>%
  distinct()

##put on rank values, merge to all_ranks

# Database connection
con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv('host'),
  port = Sys.getenv('port'),
  dbname = Sys.getenv('dbname'),
  user = Sys.getenv('user'),
  password = Sys.getenv('password')
)

##manipulate the duplicate seeds.
#first define which teams played each other on early days.

DBI::dbWriteTable(con, 'march_madness_teams', all_ranks, overwrite = TRUE)
