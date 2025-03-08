FROM rocker/tidyverse:4.2.1
 
RUN R -e 'install.packages("shiny")'
RUN R -e 'install.packages("RCurl")'
RUN R -e "install.packages( c('rjson', 'janitor', 'tibble', 'tidyr', 'dplyr', 'DT', 'stringr', 'shinydashboard','shinycssloaders', 'formattable','plotly','reshape', 'RSQLite', 'sqldf','RPostgres','uuid','shinyjs','emayili' ), repos='http://cran.rstudio.com/')"


 

 
#CMD R -e 'shiny::runExample("04_mpg", port = 8080, host = "0.0.0.0")'

EXPOSE 8080

COPY . /app/


CMD R -e "options('shiny.port'=8080,shiny.host='0.0.0.0'); shiny::runApp('app')"