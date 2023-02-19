#find nonmatches

raw_selections <- read.csv("./Data/raw_selections_real.csv")
raw_selections_long <- raw_selections %>% select(Entry, Team1, Team2, Team3, Team4, Team5, Team6, Team7, Team8, Team9, Team10, Team11, Team12, Team13, Team14, Team15, Team16) %>%
  pivot_longer(!Entry, names_to = "teamgroup", values_to = "Team") %>% filter(!is.na(Team))

temp_long <- raw_selections_long %>% select(Team)
unique_temp_long <- unique(temp_long) %>% mutate(selection = 1)

all_teams_master <- read.csv('C:/Users/Andrew/Documents/MarchMadness/mmapp/MarchMadness/Data/teams.csv') %>% mutate(master=1)

all_teams_final <- all_teams_master %>% full_join(unique_temp_long, by=c("team_name" = "Team"),keep=TRUE)
                                                  