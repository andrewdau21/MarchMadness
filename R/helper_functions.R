
get_names_of_entries_by_team <- function(team_name){
  
  sqlf <- paste0('SELECT Entry FROM raw_selections_long WHERE Team = "', team_name, '"')
  return(sqldf(sqlf))
}

get_count_of_entries_by_team <- function(team_name) {
  sqlf <- paste0('SELECT Count(Entry) as Count FROM raw_selections_long WHERE Team = "', team_name, '"')
  return(sqldf(sqlf))
}