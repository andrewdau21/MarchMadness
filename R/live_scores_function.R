live_scores_function <- function(){


i = 0


exitclause = FALSE
while (exitclause == FALSE)
{
  
  temp_date <- str_remove_all(as.character(Sys.Date()+i), "-")
  
  #temp_date <- '20220310'
  
  compiled_url <-  paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=500&dates=', temp_date)
  #compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20210310-20210401&groups=50')
  
  i = i+1
  
  #myfile <- getURL(compiled_url)
  
  myfile <- getURL(compiled_url, simplifyVector=FALSE)
  
  
  raw_espn_json <- fromJSON(myfile)
  #print(length(raw_espn_json[["events"]]))
  if(length(raw_espn_json[["events"]]) > 0)
  {
    exitclause =TRUE
  }
  else if (i == 5)
  {
    exitclause = TRUE
  }
}



if (length(raw_espn_json[["events"]]) > 0 )
{
  
  raw_espn_json %>% str(max.level = 1)
  
  
  
  espn_games_2018 <- raw_espn_json[["events"]] %>% 
    enframe() %>% 
    #dplyr::rename(row_id = name) %>% 
    mutate(row_id = name) %>%
    select(-name) %>%
    unnest_wider(value) %>% 
    mutate(game_id = id) %>%
    select(-id)
  #dplyr::rename(game_id = id)
  
  espn_season_2018 <- espn_games_2018 %>% 
    unnest_wider(season) %>% 
    unchop(competitions)
  
  
  
  #               espn_season_2018 %>% 
  #                select(competitions) %>% 
  #               slice(1) %>% 
  #              str(max.level = 5)
  
  
  ex_id_subset <- espn_season_2018[["competitions"]][[1]][["competitors"]][[1]][["id"]]
  
  ex_id_hoist <- espn_season_2018 %>%
    hoist(competitions, home_team_id = list("competitors", 1, "id"))
  
  all.equal(ex_id_subset, ex_id_hoist[["home_team_id"]][1])
  
  
  
  espn_season_2018_final <- espn_season_2018 %>%
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
      current_stat = list("type","description"),
      current_time = list("type", "detail")
      
    ) %>%
    select(-where(is.list), -row_id) %>%
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
  
  espn_season_2018_final_final <- espn_season_2018_final %>%
    select(short_name,home_prob, away_team_abb, away_score,home_team_abb, home_score,time, period,home_logo, away_logo, current_stat,home_prob, current_time, home_team_seed, away_team_seed) %>%
    #mutate(home_logo = paste0('<img src="',home_logo,'" height="52"></img>')) %>%
    mutate(home_logo = paste0('<div class="container" height = 52 style="width: 60px;"><img src="',home_logo, '" height="52;"  style="width:40;"> <div class="badge">',home_team_seed ,'</div></div>')) %>%
    mutate(away_logo = paste0('<div class="container2" height = 52 style="width: 60px;"><img src="',away_logo, '" height="52;"  style="width:40;"> <div class="badge2">',away_team_seed ,'</div></div>')) %>%
    #mutate(away_logo = paste0('<img src="',away_logo,'" height="52"></img>')) %>%
    mutate(period = ifelse(period == 3, paste0("OT-1"), period)) %>%
    mutate(period = ifelse(period == 4, paste0("OT-2"), period)) %>%
    mutate(period = ifelse(period == 1, paste0(period,"st", " ", time), period)) %>%
    mutate(period = ifelse(period == 2, paste0(period, "nd", " ", time), period)) %>%
    mutate(period = ifelse(current_stat == "Halftime", "Halftime", period)) %>%
    mutate(period = ifelse(current_stat == "Final", "Final", period)) %>%
    mutate(period = ifelse(current_stat == "Scheduled", current_time, period)) %>%
    dplyr::select(home_logo, home_score, period, away_score, away_logo)
  
  #temp_data <- espn_season_2018_final %>%
  # dplyr::select(home_score, home_logo, time, period, away_logo, away_score)
  
  values <- espn_season_2018_final_final %>% mutate(sort_var = ifelse(period == 'Final', 1,0)) %>% arrange(sort_var) %>% select(-sort_var)
  values2 <- espn_season_2018_final
}

else{
  values <- data_frame()
  values2 <- data_frame()
}

return(list(values, values2))
}
       