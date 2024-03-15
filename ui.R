
header_img <- tags$img(
  src='salbot.jpeg',
  style = 'height: 50px; width: 50px; position: absolute; left: 50%; transform: translateX(-50%);'
)
header <-  htmltools::tagQuery(dashboardHeader(title = "March Capness 2024"))
header <- header$
  addAttrs(style = "position: relative")$ # add some styles to the header 
  find(".navbar.navbar-static-top")$ # find the header right side
  append(header_img)$ # inject our img
  allTags()

ui <- dashboardPage(
 #dashboardHeader(title="March Capness 2024"),
 header,
  #dashboardHeader(title = tags$img(src='salbot.jpeg', height='60', width='50'),
   #                              'March Capness 2024'),
  # dashboardHeader(title = tags$a(href='https://www.google.com',
  #                                tags$img(src='salbot.jpeg', height = '60', width ='60'),
  #                                'March Capness 2024')),
  # 
  # dashboardHeader(title = span(img(src = "salbot.jpeg", height = 35), "March Capness 2024")),
  #dashboardHeader(title=span(tags$img(src='salbot.jpeg'))),
  #dashboardHeader(title = tags$a("March Capness 2024", img(src="salbot.jpeg", height = 50, align = "right"))),


  
  dashboardSidebar(collapsed = TRUE,
    sidebarMenu(
      menuItem("Leaderboard", tabName = "leaderboard", icon = icon("dashboard")),
      menuItem("Full Standings", tabName = "scoreboard", icon = icon("th")),
      menuItem("Chat", tabName="chat", icon =icon("comments") )
      #,radioButtons("autorefresh", label = h3("Auto Refresh"),
      #             choices = list("On - 1 Minute" = 1, "Off" = 2), 
      #             selected = 2)
    )
  ),
  dashboardBody(
    tags$head( 
      tags$style(HTML("a {color: black}")), HTML("<base target='_blank'>") ,
      tags$link(rel="shortcut icon", href="salbot.jpeg"),),
    tabItems(
      # First tab content
      tabItem(tabName = "leaderboard",
              fluidRow(
                box(title = "Leaderboard", 
                    status="primary", 
                    solidHeader = TRUE,
                    width=4,
                    align = "center",
                    collapsible = TRUE,
               
                       
                          DT::dataTableOutput("standings") %>% withSpinner()),
                box(title="Scoreboard",
                    status="primary",
                    solidHeader = TRUE,
                    width=8,
                    align = "center",
                    collapsible=TRUE,
                    DT::dataTableOutput("table")    )),
                #textOutput("selected")
              ),
                # box(title = "Standings Chart", 
                #     status="primary", 
                #     solidHeader = TRUE,
                #   width=9,
                #   
                #     collapsible = TRUE,
                #  plotlyOutput("standingschart", height = "800px")))),
      
    
      
      # Second tab content
      tabItem(tabName = "scoreboard",
  
              DT::dataTableOutput("fullstandings")   
      ),
      tabItem(tabName = "chat",
              includeCSS("./css/shinychat.css"),
              
              # And custom JavaScript -- just to send a message when a user hits "enter"
              # and automatically scroll the chat window for us. Totally optional.
              includeScript("./js/sendOnEnter.js"),
              uiOutput("chat"),
              textInput("entry", ""),
              actionButton("send", "Send"),
              textInput("user", "Your User ID:", value=""),
              tags$hr(),
              h5("Connected Users"),
              # Create a spot for a dynamic UI containing the list of users.
              uiOutput("userList")
              )
    )
  )
)

