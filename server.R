
#raw_espn_json[["events"]][[3]][["competitions"]][[1]][["situation"]][["lastPlay"]][["probability"]][["homeWinPercentage"]]





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
  
  
  temp_date <- str_remove_all(as.character(Sys.Date()), "-")
  
  #compiled_url <-  paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=500&dates=', temp_date, '&groups=50')
  compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20210316-20210401&groups=50')
  
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
      venue = list("venue","fullName")
      
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
  
  live_scores <<- espn_season_2018_final %>%
    select(name,short_name,home_prob, away_team_abb, away_score,home_team_abb, home_score,time, period,home_logo, away_logo, current_stat,home_prob, home_team_name, away_team_name) %>%
    mutate(home_logo = paste0('<img src="',home_logo,'" height="52"></img>')) %>%
    mutate(away_logo = paste0('<img src="',away_logo,'" height="52"></img>'))
  
  print(live_scores)
  
  #calc final wins.
  
  wins <- live_scores %>%
    filter(current_stat == 'Final') %>%
    mutate(winner = ifelse(home_score > away_score, home_team_name, away_team_name))%>%
    count(winner, name="wins")
  
  live_wins_home <<- live_scores %>%
    filter(current_stat %in% c('In Progress', "Halftime")) %>%
    mutate(winner = home_team_name) %>%
    mutate(live_wins = home_prob) %>%
    dplyr::select(winner, live_wins)
  
  
  live_wins_away <<-  live_scores %>%
    filter(current_stat %in% c('In Progress', "Halftime")) %>%
    mutate(winner = away_team_name) %>%
    mutate(live_wins = 1- home_prob) %>%
    dplyr::select(winner, live_wins)
  
  live_wins <- rbind(live_wins_home, live_wins_away)
  
  
  total_wins <<- wins %>% full_join(live_wins, by = "winner")
  
  print(total_wins)
  
  
  ##aliveteams
  ##base on if loss = 0
  ##dead teams is 
  library(reshape)
  
  raw_selections_melted <- melt(raw_selections, id=c("Entry"))
  temp_standings <- raw_selections_melted %>% left_join(total_wins, by = c("value"="winner")) 
  
  
  standings <- temp_standings %>%
    group_by(Entry) %>%
    summarise(wins = sum(wins,na.rm=TRUE), live_wins=sum(live_wins, na.rm=TRUE))
  
  standings_live <- standings %>% left_join(raw_selections, by = "Entry")
  print(standings_live)
  
  rv <- reactiveValues(m=standings_live)
  
  
  observe({
    # Re-execute this reactive expression after 1000 milliseconds
    invalidateLater(10000, session)
    
    
    temp_date <- str_remove_all(as.character(Sys.Date()), "-")
    
    #compiled_url <-  paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=500&dates=', temp_date, '&groups=50')
    compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20210316-20210401&groups=50')
    
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
        venue = list("venue","fullName")
        
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
    
    print(live_scores)
    
    #calc final wins.
    
    wins <- live_scores %>%
      filter(current_stat == 'Final') %>%
      mutate(winner = ifelse(home_score > away_score, home_team_name, away_team_name))%>%
      count(winner, name="wins")
    
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
    
    standings_live <- standings %>% left_join(raw_selections, by = "Entry")
    print(standings_live)
    
    rv$m <- standings_live
    
  })
  

  observe({
    # Re-execute this reactive expression after 1000 milliseconds
    invalidateLater(30000, session)

    # Do something each time this is invalidated.
    # The isolate() makes this observer _not_ get invalidated and re-executed
    # when input$n changes.
    #print("in the invalidator")



    
    temp_date <- str_remove_all(as.character(Sys.Date()), "-")
    
    compiled_url <-  paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=500&dates=', temp_date, '&groups=50')
    #compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20210310-20210401&groups=50')
    
    
    myfile <- getURL(compiled_url, simplifyVector=FALSE)
    
    
    raw_espn_json <- fromJSON(myfile)
    
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
        away_team_id = list("competitors", 2, "id"),
        away_team_abb = list("competitors", 2, "team", "abbreviation"),
        home_score = list("competitors", 1, "score"),
        away_score = list("competitors", 2, "score"),
        home_logo = list("competitors",1,"team","logo"),
        away_logo = list("competitors",2,"team","logo"),
        home_prob = list("situation","lastPlay","probability","homeWinPercentage"),
        venue = list("venue","fullName")
        
      ) %>%
      hoist(
        status,
        time = list("displayClock"),
        period = list("period"),
        current_stat = list("type","description")
        
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
      select(short_name,home_prob, away_team_abb, away_score,home_team_abb, home_score,time, period,home_logo, away_logo, current_stat,home_prob) %>%
      mutate(home_logo = paste0('<img src="',home_logo,'" height="52"></img>')) %>%
      mutate(away_logo = paste0('<img src="',away_logo,'" height="52"></img>')) %>%
      mutate(period = ifelse(period == 1, paste0(period,"st", " ", time), paste0(period,"nd", " ", time))) %>%
      mutate(period = ifelse(current_stat == "Halftime", "Halftime", period)) %>%
      mutate(period = ifelse(current_stat == "Final", "Final", period)) %>%
      dplyr::select(home_logo, home_score, period, away_score, away_logo)
    
     # print("proxy1")
      #print(espn_season_2018_final_final)
      #temp_data <-espn_season_2018_final  %>%
       # dplyr::select(home_score, home_logo, time, period, away_logo, away_score)

      #print(temp_data)

      #print(espn_season_2018_final)

      values$df_data <- espn_season_2018_final_final

      #proxy1 <- DT::dataTableProxy('table')
      #DT::replaceData(proxy1, espn_season_2018_final_final)

      #values <- reactiveValues(df_data =espn_season_2018_final_final)

      #proxy1 <- DT::dataTableProxy('table')
      #print("replace proxy")

      #DT::replaceData(proxy1, temp_data)

   # data(espn_season_2018_final)



  })
  
  
  
  temp_date <- str_remove_all(as.character(Sys.Date()), "-")
  
 compiled_url <-  paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=500&dates=', temp_date, '&groups=50')
  #compiled_url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?lang=en&region=us&limit=999&dates=20210310-20210401&groups=50')
  
                        
                       myfile <- getURL(compiled_url, simplifyVector=FALSE)
                       
                       
                       raw_espn_json <- fromJSON(myfile)
                       
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
                           away_team_id = list("competitors", 2, "id"),
                           away_team_abb = list("competitors", 2, "team", "abbreviation"),
                           home_score = list("competitors", 1, "score"),
                           away_score = list("competitors", 2, "score"),
                           home_logo = list("competitors",1,"team","logo"),
                           away_logo = list("competitors",2,"team","logo"),
                           home_prob = list("situation","lastPlay","probability","homeWinPercentage"),
                           venue = list("venue","fullName")
                           
                         ) %>%
                         hoist(
                           status,
                           time = list("displayClock"),
                           period = list("period"),
                           current_stat = list("type","description")
                           
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
                         select(short_name,home_prob, away_team_abb, away_score,home_team_abb, home_score,time, period,home_logo, away_logo, current_stat,home_prob) %>%
                         mutate(home_logo = paste0('<img src="',home_logo,'" height="52"></img>')) %>%
                         mutate(away_logo = paste0('<img src="',away_logo,'" height="52"></img>')) %>%
                         mutate(period = ifelse(period == 1, paste0(period,"st", " ", time), paste0(period,"nd", " ", time))) %>%
                         mutate(period = ifelse(current_stat == "Halftime", "Halftime", period)) %>%
                         mutate(period = ifelse(current_stat == "Final", "Final", period)) %>%
                         dplyr::select(home_logo, home_score, period, away_score, away_logo)
                       
                       #temp_data <- espn_season_2018_final %>%
                        # dplyr::select(home_score, home_logo, time, period, away_logo, away_score)
                       
                       values <- reactiveValues(df_data =espn_season_2018_final_final)
                       
output$fullstandings <- DT::renderDataTable({
                         
                         
                         DT::datatable(rv$m, escape=FALSE, 
                                       rownames=FALSE,
                                       
                                       options = list(scrollX = TRUE,
                                                      autoWidth=TRUE
                                                     
                                                      #columnDefs = list(list(targets = 7,visible=TRUE))
                                                      
                                       ))
                         
                       }
                       )
                       
  
 
  output$table <- DT::renderDataTable({
    
      
      DT::datatable(values$df_data, escape=FALSE, colnames = rep("", ncol(values$df_data)),
                    rownames=FALSE,
                    
                    options = list(scrollX = TRUE,
                                   autoWidth=TRUE,
                                   bsort=FALSE,
                                   dom='t',
                                   ordering=FALSE
                                   #columnDefs = list(list(targets = 7,visible=TRUE))
                                  
                                  ))
    
   }
  )
  
  output$standings <- DT::renderDataTable({

    #library(DT)
    datatable(
      cbind(' ' = '&oplus;', rv$m ), escape = FALSE,
      options = list(
        selection = 'single',
        dom = 't',
        autoWidth = TRUE,
        scrollX=TRUE,
        pageLength = 500,
        columnDefs = list(
          
          list(visible = FALSE, targets = c(0,5,6,7,8,9,10,11,12,13,14,15,16)),
          list(orderable = FALSE, className = 'details-control', targets = 1)
        )
      ),
      callback = JS("
  table.column(1).nodes().to$().css({cursor: 'pointer'});
  var format = function(d) {

    return '<div style=\"background-color:#eee; padding: .5em;\"> Teams: ' + 
     
    '<img src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[9] + '.png\" width = \"' +d[13] +  '\" height = \"' + d[13] + '\"</img> ' +
    '<img src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[10] + '.png\" width = \"' + d[14] + '\" height = \"' + d[14] + '\" opacity = .2 </img> ' +
    '<img src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[11] + '.png\" width = \"'+d[15]+'\" height = \"' + d[15] + '\"</img> ' +
  '<img src=\"https://www.whitehotel.com/files/img/bg-lines.png\" width = \"1\" height = \"1\"</img> ' +
    '<img src=\"https://www.whitehotel.com/files/img/bg-lines.png\" width = \"1\" height = \"1\"</img> ' +
    '<img src=\"https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.whitehotel.com%2F&psig=AOvVaw1ou76iULoi_FoJNMPgCArg&ust=1614829349922000&source=images&cd=vfe&ved=0CAIQjRxqFwoTCNip2aSak-8CFQAAAAAdAAAAABAD.png\" width = \"1\" height = \"1\"</img> ' +
    '<img src=\"https://a.espncdn.com/i/teamlogos/ncaa/500/'+d[12] + '.png\" width = \"'+d[16]+ '\" height = \"' + d[16] +'\"</img> ' 


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
  
  
  output$picks <- DT::renderDataTable({
    
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
      vars$chat <- c(vars$chat, paste0(linePrefix(),
                                       tags$span(class="user-exit",
                                                 sessionVars$username,
                                                 "left the room.")))
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
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-enter",
                                                    sessionVars$username,
                                                    "entered the room.")))
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
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-change",
                                                    paste0("\"", sessionVars$username, "\""),
                                                    " -> ",
                                                    paste0("\"", input$user, "\""))))
        
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
      vars$chat <<- c(vars$chat, 
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
  
  
  


}
