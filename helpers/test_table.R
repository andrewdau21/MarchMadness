#install.packages("reactable")
library(reactable)

td <- espn_season_2018_final %>%
  dplyr::select(home_logo, home_score, time, period, away_score, away_logo) %>%
  mutate(period = ifelse(period == 1, paste0(period,"st"), paste0(period,"nd"))) %>%
  mutate(test_period = "1:45 - 2nd") %>%
  #mutate(test_period = "Final") %>%
  dplyr::select(home_logo, home_score, test_period, away_score, away_logo)
  #dplyr::select(-time, -period)

theme <- reactableTheme(color = "hsl(0, 0%, 87%)", backgroundColor = "hsl(220, 13%, 18%)", 
                        borderColor = "hsl(0, 0%, 22%)", stripedColor = "rgba(255, 255, 255, 0.04)", 
                        highlightColor = "rgba(255, 255, 255, 0.06)", inputStyle = list(backgroundColor = "hsl(0, 0%, 24%)"), 
                        selectStyle = list(backgroundColor = "hsl(0, 0%, 24%)"), 
                        pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 24%)"), 
                        pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 28%)"))

reactable(td,
          columns = list(
            home_logo = colDef(html = TRUE, name = "", minWidth = 20, align = "right"),
            away_logo = colDef(html = TRUE, name = "", minWidth = 20, align = "left"),
            home_score = colDef(minWidth = 20, name="", align = "right"),
            test_period = colDef(minWidth=40, name = "", align = "center"),
            #time = colDef(minWidth = 50, name = ""),
            away_score = colDef(name = "", minWidth = 20, align = "left")
           
          ), theme=theme)


