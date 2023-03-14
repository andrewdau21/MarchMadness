
entries <- read.csv('./DoNotCommit/TEST_raw_sections_real.csv', stringsAsFactors = FALSE)

library(tidyr)
library(dplyr)
cleaned <- entries %>% 
  mutate(Entry = Your.Name.) %>%
  mutate(team_list = Choose.your.group.of.teams...100.maximum.budget.) %>%
  dplyr::select(Entry, team_list) %>%
  separate(team_list, c("dummy", "Team1" ,"Team2", "Team3", "Team4", "Team5", "Team6","Team7", "Team8",
                        "Team9", "Team10","Team11", "Team12", "Team13", "Team14", "Team15", "Team16",
                        "Team17", "Team18", "Team19", "Team20", "Team21", "Team22", "Team23", "Team24"), sep = "] ")%>%
  mutate(Team1 = sub(" \\(.*", "", Team1)) %>%
  mutate(Team2 = sub(" \\(.*", "", Team2)) %>%
  mutate(Team3 = sub(" \\(.*", "", Team3)) %>%
  mutate(Team4 = sub(" \\(.*", "", Team4)) %>%
  mutate(Team5 = sub(" \\(.*", "", Team5)) %>%
  mutate(Team6 = sub(" \\(.*", "", Team6)) %>%
  mutate(Team7 = sub(" \\(.*", "", Team7)) %>%
  mutate(Team8 = sub(" \\(.*", "", Team8)) %>%
  mutate(Team9 = sub(" \\(.*", "", Team9)) %>%
  mutate(Team10 = sub(" \\(.*", "", Team10)) %>%
  mutate(Team11 = sub(" \\(.*", "", Team11)) %>%
  mutate(Team12 = sub(" \\(.*", "", Team12)) %>%
  mutate(Team13 = sub(" \\(.*", "", Team13)) %>%
  mutate(Team14 = sub(" \\(.*", "", Team14)) %>%
  mutate(Team15 = sub(" \\(.*", "", Team15)) %>%
  mutate(Team16 = sub(" \\(.*", "", Team16)) %>%
  mutate(Team17 = sub(" \\(.*", "", Team17)) %>%
  mutate(Team18 = sub(" \\(.*", "", Team18)) %>%
  mutate(Team19 = sub(" \\(.*", "", Team19)) %>%
  mutate(Team20 = sub(" \\(.*", "", Team20)) %>%
  mutate(Team21 = sub(" \\(.*", "", Team21)) %>%
  mutate(Team22 = sub(" \\(.*", "", Team22)) %>%
  mutate(Team23 = sub(" \\(.*", "", Team23)) %>%
  mutate(Team24 = sub(" \\(.*", "", Team24)) %>%
  dplyr::select(-dummy)

# #put fixes here for names
# cleaned <- data.frame(lapply(cleaned, function(x) {
#                     gsub("Michigan St.", "Michigan State", x)
#                    
#                 }))
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("Loyola", "Loyola Chicago", x)
#   
# }))
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("Colorado St.", "Colorado State", x)
#   
# }))
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("New Mexico St.", "New Mexico St", x)
#   
# }))
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("S. Dakota St.", "S Dakota St", x)
#   
# }))
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("Iowa St.", "Iowa State", x)
#   
# }))
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   
#   gsub("San Diego St.", "San Diego State", x)
#   
# }))
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("St. Mary's", "Saint Mary's", x)
#   
# }))
# 
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("Boise St.", "Boise State", x)
#   
# }))
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("Ohio St.", "Ohio State", x)
#   
# }))
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("CS Fullerton", "CSU Fullerton", x)
#   
# }))
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("Jacksonville St.", "J'Ville St", x)
#   
# }))
# 
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("Rutgers/Notre Dame", "Notre Dame", x)
#   
# }))
# 
# 
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("Wyoming/Indiana", "Indiana", x)
#   
# }))
# 
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("Wright St./Bryant", "Wright State", x)
#   
# }))
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("Murray St.", "Murray State", x)
#   
# }))
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("St. Peter's", "Saint Peter's", x)
#   
# }))
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("Norfolk St.", "Norfolk State", x)
#   
# }))
# 
# 
# cleaned <- data.frame(lapply(cleaned, function(x) {
#   gsub("Montana St.", "Montana State", x)
#   
# }))
# 













master <- read.csv("./Data/all_teams.csv", stringsAsFactors = FALSE)  

cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team1), by=c("Team1" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team2), by=c("Team2" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team3), by=c("Team3" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team4), by=c("Team4" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team5), by=c("Team5" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team6), by=c("Team6" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team7), by=c("Team7" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team8), by=c("Team8" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team9), by=c("Team9" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team10), by=c("Team10" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team11), by=c("Team11" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team12), by=c("Team12" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team13), by=c("Team13" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team14), by=c("Team14" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team15), by=c("Team15" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team16), by=c("Team16" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team17), by=c("Team17" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team18), by=c("Team18" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team19), by=c("Team19" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team20), by=c("Team20" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team21), by=c("Team21" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team22), by=c("Team22" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team23), by=c("Team23" = "name"))
cleaned <- cleaned %>% left_join(master %>% dplyr::select(name,team24), by=c("Team24" = "name"))

cleaned <- cleaned %>%
  #dplyr::select(-Team6, -Team7, -Team8, -Team9, -Team10, -Team11, -Team12, -Team13, -Team14, -Team15, -Team16) %>%
  mutate(team1p = ifelse(!is.na(team1) , 32, 1)) %>%
  mutate(team2p = ifelse(!is.na(team2) , 32, 1)) %>%
  mutate(team3p = ifelse(!is.na(team3) , 32, 1)) %>%
  mutate(team4p = ifelse(!is.na(team4) , 32, 1)) %>%
  mutate(team5p = ifelse(!is.na(team5) , 32, 1)) %>%
  mutate(team6p = ifelse(!is.na(team6) , 32, 1)) %>%
  mutate(team7p = ifelse(!is.na(team7) , 32, 1)) %>%
  mutate(team8p = ifelse(!is.na(team8) , 32, 1)) %>%
  mutate(team9p = ifelse(!is.na(team9) , 32, 1)) %>%
  mutate(team10p = ifelse(!is.na(team10) , 32, 1)) %>%
  mutate(team11p = ifelse(!is.na(team11) , 32, 1)) %>%
  mutate(team12p = ifelse(!is.na(team12) , 32, 1)) %>%
  mutate(team13p = ifelse(!is.na(team13) , 32, 1)) %>%
  mutate(team14p = ifelse(!is.na(team14) , 32, 1)) %>%
  mutate(team15p = ifelse(!is.na(team15) , 32, 1)) %>%
  mutate(team16p = ifelse(!is.na(team16) , 32, 1)) %>%
  mutate(team17p = ifelse(!is.na(team17) , 32, 1)) %>%
  mutate(team18p = ifelse(!is.na(team18) , 32, 1)) %>%
  mutate(team19p = ifelse(!is.na(team19) , 32, 1)) %>%
  mutate(team20p = ifelse(!is.na(team20) , 32, 1)) %>%
  mutate(team21p = ifelse(!is.na(team21) , 32, 1)) %>%
  mutate(team22p = ifelse(!is.na(team22) , 32, 1)) %>%
  mutate(team23p = ifelse(!is.na(team23) , 32, 1)) %>%
  mutate(team24p = ifelse(!is.na(team24) , 32, 1)) 
  
  
  

#all_teams <-tidyr::gather(cleaned) %>% filter(key != "Entry") %>% filter(!is.na(value))
#all_teams_list <- unique(all_teams$value)
#write.csv(all_teams_list, "C:/Users/Andrew/Documents/MarchMadness/mmapp/MarchMadness/Data/all_teams_raw.csv")


write.csv(cleaned, "./Data/raw_selections_real_test.csv")
 
tiebreaker <- entries %>% mutate(Entry = Your.Name.) %>% mutate(TieBreaker = as.integer(Total.Points.In.The.National.Title.Game..used.as.tiebreaker..)) %>% select(Entry, TieBreaker)

write.csv(tiebreaker, "./Data/tiebreaker_test.csv")
