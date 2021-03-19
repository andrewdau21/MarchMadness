
ui <- fluidPage(

fluidPage(
  titlePanel("Salary Cap Madness 2021"),
  
  sidebarLayout(
    sidebarPanel("Under Construction"),
    mainPanel(DT::dataTableOutput("standings"))
  )
  
 
  )
)

ui <- dashboardPage(
  dashboardHeader(title="Salary Cap Madness 2021"),
 
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Leaderboard", tabName = "leaderboard", icon = icon("dashboard")),
      menuItem("Full Standings", tabName = "scoreboard", icon = icon("th")),
      menuItem("Chat", tabName="chat", icon =icon("comments") )
    )
  ),
  dashboardBody(
    tags$head( 
      tags$style(HTML("a {color: black}")), HTML("<base target='_blank'>") ,
      tags$link(rel="shortcut icon", href="basketball.jpg"),),
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
                    DT::dataTableOutput("table")    ))),
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
              includeCSS("shinychat.css"),
              
              # And custom JavaScript -- just to send a message when a user hits "enter"
              # and automatically scroll the chat window for us. Totally optional.
              includeScript("sendOnEnter.js"),
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

