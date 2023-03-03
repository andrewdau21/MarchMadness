


live_wins_function <- function(games){
  td <- data.frame()
  GGG <<- games
  for(i in 1:length(games$game_id) )
  {
    #print(i)
  url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event=',games$game_id[i])
  #url <- paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event=401492763')
  myfile <- getURL(url)
  
  raw_espn_json <- fromJSON(myfile)
  if(!is.null(raw_espn_json[['winprobability']]))
  {
  temper <- data.frame(home_prob = raw_espn_json[['winprobability']][length(raw_espn_json[["winprobability"]])][[1]][["homeWinPercentage"]], game_id = games$game_id[i])
  }
  td <- rbind(td, temper)
  }
  
  return(td)
}

