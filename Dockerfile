FROM rocker/shiny-verse:4.4.3

# You probably don't need to reinstall shiny now
# But keep your other packages if needed
RUN R -e "install.packages( c('RCurl', 'rjson', 'janitor', 'tibble', 'tidyr', 'dplyr', 'DT', 'stringr', 'shinydashboard','shinycssloaders', 'formattable','plotly','reshape', 'RSQLite', 'sqldf','RPostgres','uuid','shinyjs','emayili' ), repos='https://cran.rstudio.com/')"

EXPOSE 8080

COPY . /app/


CMD R -e "options('shiny.port'=8080,shiny.host='0.0.0.0'); shiny::runApp('app')"