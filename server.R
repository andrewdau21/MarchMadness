#TODO
# Add configuration file to control tourney start/end date.  
# Make all elements of the appication dynamic...ie number of teams in any entry.  This should clean up the JS callback in DT below.
# Clearly define all necessary inputs.  /Data has some extrenuous data.

vars <- reactiveValues(chat=NULL, users=NULL)
# Restore the chat log from the last session.
if (file.exists("chat.Rds")){
  vars$chat <- readRDS("chat.Rds")
} else {
  vars$chat <- "Welcome to Shiny Chat!"
}

#' Get the prefix for the line to be added to the chat window. Usually a newline
#' character unless it's the first line.
linePrefix <- function(){
  if (is.null(isolate(vars$chat))){
    return("")
  }
  return("<br />")
}


function(input, output, session) {
  
  keep_alive <- shiny::reactiveTimer(intervalMs = 10000, 
                                     session = shiny::getDefaultReactiveDomain())
  
  shiny::observe({keep_alive()})
  
  
  temp_date <- str_remove_all(as.character(Sys.Date()), "-")
 

  #compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20220317-20220404&groups=100')
  compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20230219-20230223&groups=50')
  
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
  
  #print(live_scores)
  
  #calc final wins.
  
  wins <- live_scores %>%
    filter(current_stat == 'Final') %>%
    mutate(winner = ifelse(home_score > away_score, home_team_name, away_team_name))%>%
    count(winner, name="wins")
  
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
  
  lsdat <<- live_scores
  
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
  
  temp_money <- raw_selections_melted %>% left_join(losers, by = c("value"="loser")) 
  
  temp_money2 <- temp_money %>% left_join(master, by=c("value"= "name"))
  aaa <<- temp_money2
  
  temp_money3 <- temp_money2 %>%
    filter(!is.na(losses)) %>%
    group_by(Entry) %>%
    summarise(dead_money = sum(cost,na.rm=TRUE)) 
  
  abc <<- temp_money3
  
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
  
  #print(standings_live)
  
  rv <- reactiveValues(m=standings_live)
  
  #proxy <- DT::dataTableProxy('standings')
  observe({
    # Re-execute this reactive expression after 1000 milliseconds
    invalidateLater(10000, session)
    if (autorefresh==1)
    {
    
    temp_date <- str_remove_all(as.character(Sys.Date()), "-")
    
    #compiled_url <-  paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=500&dates=', temp_date, '&groups=50')
    #compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20210316-20210317')
    #compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20220310-20220406&groups=100')
    #compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20220310-20220406')
    compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20230219-20230223&groups=100')
    
    #compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/teams')
    
    myfile <- getURL(compiled_url, simplifyVector=FALSE)
    
    
    raw_espn_json <- fromJSON(myfile)
    
   # raw_espn_json %>% str(max.level = 1)
    
    
    
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
    
    
    
    #espn_season_2018 %>% 
    #  select(competitions) %>% 
    #  slice(1) %>% 
    #  str(max.level = 5)
    
    
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
    
   # print(live_scores)
    
    #calc final wins.
    
    wins <- live_scores %>%
      filter(current_stat == 'Final') %>%
      mutate(winner = ifelse(home_score > away_score, home_team_name, away_team_name))%>%
      count(winner, name="wins")
    
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
    
    
    live_wins_away <-  live_scores %>%
      filter(current_stat %in% c('In Progress', "Halftime")) %>%
      mutate(winner = away_team_name) %>%
      mutate(live_wins = 1- home_prob) %>%
      dplyr::select(winner, live_wins)
    

    
    live_wins <- rbind(live_wins_home, live_wins_away)
    
    live_wins$winner <- as.character(live_wins$winner)
    
    total_wins <- wins %>% full_join(live_wins, by = "winner")
    
    
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
    #print(standings_live)
    
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
    
    temp_money <- raw_selections_melted %>% left_join(losers, by = c("value"="loser")) 
    
    temp_money2 <- temp_money %>% left_join(master, by=c("value"= "name"))
    
    temp_money3 <- temp_money2 %>%
      filter(!is.na(losses)) %>%
      group_by(Entry) %>%
      summarise(dead_money = sum(cost,na.rm=TRUE)) 
    
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
    
    
    rv$m <- standings_live
    #DT::replaceData(proxy, standings_live)
    }
    
  })
  
  
  observe({
    # Re-execute this reactive expression after 1000 milliseconds
    invalidateLater(60000, session)
    if(autorefresh==1)
    {
      print("I invalidated!")

    # Do something each time this is invalidated.
    # The isolate() makes this observer _not_ get invalidated and re-executed
    # when input$n changes.
    #print("in the invalidator")
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
        if(length(raw_espn_json[["events"]]) > 0)
        {
          exitclause =TRUE
        }
        if (i == 5)
        {
          exitclause = TRUE
        }
      }
    
    
    if(length(raw_espn_json[["events"]]) > 0)
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
      select(short_name,home_prob, away_team_abb, away_score,home_team_abb, home_score,time, period,home_logo, away_logo, current_stat,home_prob,current_time, home_team_seed, away_team_seed) %>%
      #mutate(home_logo = paste0('<img src="',home_logo,'" height="52"></img>')) %>%
      #mutate(away_logo = paste0('<img src="',away_logo,'" height="52"></img>')) %>%
      mutate(home_logo = paste0('<div class="container" height = 52 style="width: 60px;"><img src="',home_logo, '" height="52;"  style="width:40;"> <div class="badge">',home_team_seed ,'</div></div>')) %>%
      mutate(away_logo = paste0('<div class="container2" height = 52 style="width: 60px;"><img src="',away_logo, '" height="52;"  style="width:40;"> <div class="badge2">',away_team_seed ,'</div></div>')) %>%
      mutate(period = ifelse(period == 3, paste0("OT-1"), period)) %>%
      mutate(period = ifelse(period == 4, paste0("OT-2"), period)) %>%
      mutate(period = ifelse(period == 1, paste0(period,"st", " ", time), period)) %>%
      mutate(period = ifelse(period == 2, paste0(period, "nd", " ", time), period)) %>%
      mutate(period = ifelse(current_stat == "Halftime", "Halftime", period)) %>%
      mutate(period = ifelse(current_stat == "Final", "Final", period)) %>%
      mutate(period = ifelse(current_stat == "Scheduled", current_time, period)) %>%
      dplyr::select(home_logo, home_score, period, away_score, away_logo)
    
     # print("proxy1")
      #print(espn_season_2018_final_final)
      #temp_data <-espn_season_2018_final  %>%
       # dplyr::select(home_score, home_logo, time, period, away_logo, away_score)

      #print(temp_data)

      #print(espn_season_2018_final)
      values2$df_data_full <- espn_season_2018_final
      values$df_data <- espn_season_2018_final_final
    }
    
    else{
      values2$df_data_full <- data.frame()
      values$df_data <- data.frame
    }

      #proxy1 <- DT::dataTableProxy('table')
  #    DT::replaceData(proxy1, espn_season_2018_final_final)

      #values <- reactiveValues(df_data =espn_season_2018_final_final)

      #proxy1 <- DT::dataTableProxy('table')
      #print("replace proxy")

      #DT::replaceData(proxy1, temp_data)

   # data(espn_season_2018_final)

}

  })
  
  
                       
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
                         print(length(raw_espn_json[["events"]]))
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
                       
                       values <- reactiveValues(df_data =espn_season_2018_final_final)
                       values2 <- reactiveValues(df_data_full =espn_season_2018_final)
}
                       
                       else{
                         values <- reactiveValues(df_data =data_frame())
                         values2 <- reactiveValues(df_data_full =data_frame())
                       }
                       
output$fullstandings <- DT::renderDataTable(server=FALSE,{
                         
                      temp_table_standings <- rv$m  %>%
                        dplyr::select(Entry, wins, live_wins, live_money, TieBreaker, Team1, Team2, Team3
                                    , Team4, Team5, Team6, Team7, Team8, Team9, Team10,
                                    Team11, Team12, Team13, Team14, Team15, Team16, Team17, Team18, Team19, Team20, Team21, Team22, Team23, Team24 )
                         
                         DT::datatable(temp_table_standings, escape=FALSE, 
                                       rownames=FALSE,
                                       extensions = 'FixedColumns',
                                       
                                       options = list(scrollX = TRUE,
                                                      autoWidth=TRUE,
                                                      pageLength = 100,
                                                      fixedColumns=TRUE
                                                     
                                                      #columnDefs = list(list(targets = 7,visible=TRUE))
                                                      
                                       ))# %>% 
#                           formatStyle('Team1',backgroundColor = styleEqual(c("Gonzaga","Ohio State"), c('gray', 'yellow'))) %>%
 #                          formatStyle('Team2',backgroundColor = styleEqual(c("Gonzaga","Ohio State"), c('gray', 'yellow')))
                  
                         
                       }
                       )
                       
  
 
  output$table <- DT::renderDataTable(server=FALSE,{
    
    #tabledata <- values$df_data  
    #tabledata[1,1]= paste0('<div class="container" height = 52 style="width: 60px;"><img src="https://a.espncdn.com/i/teamlogos/ncaa/500/2294.png" height="52;"  style="width:40;"> <div class="badge">16</div></div>')
    #tabledata[1,5]= paste0('<div class="container2" height = 52 style="width: 60px;"><img src="https://a.espncdn.com/i/teamlogos/ncaa/500/84.png" height="52;"  style="width:40;"> <div class="badge2">1</div></div>')
    
      
      DT::datatable(values$df_data, escape=FALSE, colnames = rep("", ncol(values$df_data)),
                    rownames=FALSE,
                    selection = "single",
                    options = list(scrollX = TRUE,
                                   autoWidth=TRUE,
                                   bsort=FALSE,
                                   dom='t',
                                   ordering=FALSE,
                                   pageLength = 20,
                                   columnDefs = list(list(className = 'dt-center', targets = 1:3),list(className='dt-left',targets=4))
                                  
                                  ))
    
   }
  )
  
  output$standings <- DT::renderDataTable(server=FALSE,{
    
     standings_temper <<- rv$m %>%
       arrange(desc(wins))
    

    #library(DT)
    datatable(
      cbind(' ' = '&oplus;', rv$m ), escape = FALSE,
      options = list(
        selection = 'single',
        dom = 'tl',
        autoWidth = TRUE,
        scrollX=TRUE,
        pageLength = 10,
        colnames = c("Entry", "Wins", "Live Wins", "Money"),
        columns = list(
          NULL,
          NULL,
          list(title = 'Entry'),
          list(title = 'Wins'),
          list(title = 'Live Wins'),
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          
          
          
          list(title = 'Money')
        ),
        order = list(list(4, 'desc'), list(79, 'desc')),
        columnDefs = list(
          
          list(list(width='10px',targets=c(3,4,5))),
          list(visible = FALSE, targets = c(0,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
                                            22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39
                                            ,40, 41,42,43,44,45,46,47,48,49,50,51,52,53,54, 55, 56, 57, 58, 59, 60,61,
                                            62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,80,81,82,83
                                            ,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104)),
          list(orderable = FALSE, className = 'details-control', targets = 1)
        )
      ),
      callback = JS("
  table.column(1).nodes().to$().css({cursor: 'pointer'});
  var format = function(d) {

    return '<div style=\"background-color:#eee; padding: .5em;\">  ' + 
     
    '<img style=\"opacity:' + d[80] + ';\" src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[30] + '.png\" width = \"' +d[54] +  '\" height = \"' + d[54] + '\"</img> ' +
    '<img  style=\"opacity:' + d[81] + ';\" src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[31] + '.png\" width = \"' + d[55] + '\" height = \"' + d[55] + '\" opacity = .2 </img> ' +
    '<img style=\"opacity:' + d[82] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[32] + '.png\" width = \"'+d[56]+'\" height = \"' + d[56] + '\"</img> ' +
    '<img style=\"opacity:' + d[83] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[33] + '.png\" width = \"'+d[57]+ '\" height = \"' + d[57] +'\"</img> '  +
    '<img style=\"opacity:' + d[84] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[34] + '.png\" width = \"'+d[58]+ '\" height = \"' + d[58] +'\"</img> ' +
    '<img style=\"opacity:' + d[85] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[35] + '.png\" width = \"'+d[59]+ '\" height = \"' + d[59] +'\"</img> ' +
    '<img style=\"opacity:' + d[86] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[36] + '.png\" width = \"'+d[60]+ '\" height = \"' + d[60] +'\"</img> ' +
    '<img style=\"opacity:' + d[87] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[37] + '.png\" width = \"'+d[61]+ '\" height = \"' + d[61] +'\"</img> ' +
    '<img style=\"opacity:' + d[88] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[38] + '.png\" width = \"'+d[62]+ '\" height = \"' + d[62] +'\"</img> ' +
    '<img style=\"opacity:' + d[89] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[39] + '.png\" width = \"'+d[63]+ '\" height = \"' + d[63] +'\"</img> ' +
    '<img style=\"opacity:' + d[90] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[40] + '.png\" width = \"'+d[64]+ '\" height = \"' + d[64] +'\"</img> ' +
    '<img style=\"opacity:' + d[91] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[41] + '.png\" width = \"'+d[65]+ '\" height = \"' + d[65] +'\"</img> ' +
    '<img style=\"opacity:' + d[92] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[42] + '.png\" width = \"'+d[66]+ '\" height = \"' + d[66] +'\"</img> ' +
    '<img style=\"opacity:' + d[93] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[43] + '.png\" width = \"'+d[67]+ '\" height = \"' + d[67] +'\"</img> ' +
    '<img style=\"opacity:' + d[94] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[44] + '.png\" width = \"'+d[68]+ '\" height = \"' + d[68] +'\"</img> ' +
    '<img style=\"opacity:' + d[95] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[45] + '.png\" width = \"'+d[69]+ '\" height = \"' + d[69] +'\"</img> ' +
    '<img style=\"opacity:' + d[96] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[46] + '.png\" width = \"'+d[70]+ '\" height = \"' + d[70] +'\"</img> ' +
    '<img style=\"opacity:' + d[97] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[47] + '.png\" width = \"'+d[71]+ '\" height = \"' + d[71] +'\"</img> ' +
    '<img style=\"opacity:' + d[98] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[48] + '.png\" width = \"'+d[72]+ '\" height = \"' + d[72] +'\"</img> ' +
    '<img style=\"opacity:' + d[99] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[49] + '.png\" width = \"'+d[73]+ '\" height = \"' + d[73] +'\"</img> ' +
    '<img style=\"opacity:' + d[100] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[50] + '.png\" width = \"'+d[74]+ '\" height = \"' + d[74] +'\"</img> ' +
    '<img style=\"opacity:' + d[101] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[51] + '.png\" width = \"'+d[75]+ '\" height = \"' + d[75] +'\"</img> ' +
    '<img style=\"opacity:' + d[102] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[52] + '.png\" width = \"'+d[76]+ '\" height = \"' + d[76] +'\"</img> ' +
    '<img style=\"opacity:' + d[103] + ';\"  src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[53] + '.png\" width = \"'+d[77]+ '\" height = \"' + d[77] +'\"</img> '
           '</div>';
  };
  table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&CircleMinus;');
    }
  });"
      ))
  })
  
  observeEvent(input$standings_rows_selected,{
    print(input$standings_rows_selected)
  })
  
  output$standingschart <- renderPlotly({
    
    plot_standings <- rv$m  %>%
      mutate(total_wins = live_wins + wins)
    
    
    fig <- plot_ly(
      y = plot_standings$Entry,
      x = plot_standings$total_wins,
      name = "March Madness Standings",
      type = "bar",
      orientation = 'h'
    )
    
    
    
  })
  
  
  output$picks <- DT::renderDataTable(server=FALSE,{
    
    raw_selections <- raw_selections %>%
      select(-IP_address, -Email)
    DT::datatable(raw_selections,extensions = 'Buttons',
                  options = list(scrollX = TRUE,
                                 pageLength = 100, info = FALSE,
                                 dom = 'frtipB', 
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                 ))
  }
  )
  
  
  
  
  observeEvent(input$picks_rows_selected, {
    showModal(modalDialog(
      title = "Historical Performance",
      "We can put historical performance here.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Create a spot for reactive variables specific to this particular session
  sessionVars <- reactiveValues(username = "")
  
  # Track whether or not this session has been initialized. We'll use this to
  # assign a username to unininitialized sessions.
  init <- FALSE
  
  # When a session is ended, remove the user and note that they left the room. 
  session$onSessionEnded(function() {
    isolate({
      vars$users <- vars$users[vars$users != sessionVars$username]
     # vars$chat <- c(vars$chat, paste0(linePrefix(),
      #                                 tags$span(class="user-exit",
       #                                          sessionVars$username,
        #                                         "left the room.")))
    })
  })
  
  # Observer to handle changes to the username
  observe({
    # We want a reactive dependency on this variable, so we'll just list it here.
    input$user
    
    if (!init){
      # Seed initial username
      sessionVars$username <- paste0("User", round(runif(1, 10000, 99999)))
      isolate({
        #vars$chat <<- c(vars$chat, paste0(linePrefix(),
           #                               tags$span(class="user-enter",
           #                                         sessionVars$username,
            #                                        "entered the room.")))
      })
      init <<- TRUE
    } else{
      # A previous username was already given
      isolate({
        if (input$user == sessionVars$username || input$user == ""){
          # No change. Just return.
          return()
        }
        
        # Updating username      
        # First, remove the old one
        vars$users <- vars$users[vars$users != sessionVars$username]
        
        # Note the change in the chat log
        #vars$chat <<- c(vars$chat, paste0(linePrefix(),
         #                                 tags$span(class="user-change",
          #                                          paste0("\"", sessionVars$username, "\""),
           #                                         " -> ",
            #                                        paste0("\"", input$user, "\""))))
        
        # Now update with the new one
        sessionVars$username <- input$user
      })
    }
    # Add this user to the global list of users
    isolate(vars$users <- c(vars$users, sessionVars$username))
  })
  
  # Keep the username updated with whatever sanitized/assigned username we have
  observe({
    updateTextInput(session, "user", 
                    value=sessionVars$username)    
  })
  
  # Keep the list of connected users updated
  output$userList <- renderUI({
    tagList(tags$ul( lapply(vars$users, function(user){
      return(tags$li(user))
    })))
  })
  
  # Listen for input$send changes (i.e. when the button is clicked)
  observe({
    if(input$send < 1){
      # The code must be initializing, b/c the button hasn't been clicked yet.
      return()
    }
    isolate({
      # Add the current entry to the chat log.
      vars$chat <-  c(vars$chat, 
                      paste0(linePrefix(),
                             tags$span(class="username",
                                       tags$abbr(title=Sys.time(), sessionVars$username)
                             ),
                             ": ",
                             tagList(input$entry)))
    })
    # Clear out the text entry field.
    updateTextInput(session, "entry", value="")
  })
  
  # Dynamically create the UI for the chat window.
  output$chat <- renderUI({
    if (length(vars$chat) > 500){
      # Too long, use only the most recent 500 lines
      vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]
    }
    # Save the chat object so we can restore it later if needed.
    saveRDS(vars$chat, "chat.Rds")
    
    # Pass the chat log through as HTML
    HTML(vars$chat)
  })
  
  selectedRow <- eventReactive(input$table_rows_selected,{
    row.names(values$df_data)[c(input$table_rows_selected)]
  })
  
  output$selected <- renderText({ 
    selectedRow()
  })
  
  
  observeEvent(input$table_rows_selected, {
    row_number <-  row.names(values2$df_data_full)[c(input$table_rows_selected)]
    data <- values2$df_data_full[row_number,]
    home_team_count <- get_count_of_entries_by_team(data$home_team_name)$Count
    home_team_names <- as.vector(get_names_of_entries_by_team(data$home_team_name)$Entry)
    away_team_count <- get_count_of_entries_by_team(data$away_team_name)$Count
    away_team_names <- as.vector(get_names_of_entries_by_team(data$away_team_name)$Entry)
    print(data)
    stufftoprint <- paste0("<b>",home_team_count,"</b>", " entries have selected ", "<b>", data$home_team_name, ":</b><br><br>",
                           toString(home_team_names), "<br><br><b>",away_team_count,"</b>",
                           " entries have selected ","<b>", data$away_team_name, ":</b><br><br>", 
                           toString(away_team_names))
    print(get_names_of_entries_by_team(data$home_team_name))
    
    #stufftoprint <- paste0(data$name)
    showModal(modalDialog(
      title = data$name,
      HTML(stufftoprint),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  


}
