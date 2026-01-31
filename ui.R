
header_img <- tags$img(
  src='salbot.jpeg',
  style = 'height: 50px; width: 50px; position: absolute; left: 50%; transform: translateX(-50%);'
)
header <-  htmltools::tagQuery(dashboardHeader(title = "March Capness 2026"))
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
    {
      # UI phase determines which menu items are visible.
      # Two supported phases:
      #  - entry_phase: Entry Form is visible (for initial signups)
      #  - post_entry: Leaderboard and Full Standings are visible
      ui_phase <- Sys.getenv('UI_PHASE', 'entry_phase')
      if (tolower(ui_phase) == 'entry_phase') {
        sidebarMenu(id = "main_tabs", 
          menuItem("Entry Form", tabName = "entry", icon = icon("table"))
        )
      } else {
        sidebarMenu(id = "main_tabs",
          menuItem("Leaderboard", tabName = "leaderboard", icon = icon("dashboard")),
          menuItem("Full Standings", tabName = "scoreboard", icon = icon("th"))
        )
      }
    }
  ),
  dashboardBody(
    useShinyjs(),
    div(id = "loading", style = "display: none;", 
        "Sending email... ", 
        tags$span(class = "spinner")
    ),
    
    # Add some CSS for the spinner
    tags$style(HTML("
  .spinner {
    display: inline-block;
    width: 16px;
    height: 16px;
    border: 2px solid #ccc;
    border-top: 2px solid #333;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }
  @keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }
")),
    tags$head( 
      tags$style(HTML("a {color: black}")), HTML("<base target='_blank'>") ,
      tags$link(rel="shortcut icon", href="salbot.jpeg"),),
     tabItems(
      tabItem(tabName = "entry",
              tags$head(
                tags$style(HTML("
      .team-highlighted {
        background-color: #e6ffe6 !important; /* Light green background */
        font-weight: bold !important;
        padding: 2px 5px !important;
        border-radius: 3px !important;
      }
    "))
              ),

              fluidRow(
                column(6, offset = 3,
                       h4("Select Your Teams (Max $100)"),
                       wellPanel(
                         textInput("entry_name", "Entry Name:", ""),
                         textOutput("name_warning"),
                         br(),
                         textInput("email", "Email Address:", ""),
                         textOutput("email_warning"),
                         br(),
                         div(style = "column-count: 2; -webkit-column-count: 2; -moz-column-count: 2; column-gap: 20px;",
                             checkboxGroupInput("selected_teams",
                                                "Choose teams:",
                                                choices = setNames(teams$team_name,
                                                                   paste(teams$team_name,
                                                                         "(Seed:", teams$seed,
                                                                         "- $", teams$cost, ")")),
                                                selected = NULL,
                                                inline = FALSE)
                         ),
                         hr(),
                         h4("Current Selection:"),
                         textOutput("total_cost"),
                         textOutput("warning"),
                         hr(),
                         numericInput("tiebreaker",
                                      "Tiebreaker: Combined points in National Championship game:",
                                      value = NULL, min = 0, max = 500, step = 1),
                         textOutput("tiebreaker_warning"),
                         hr(),
                         actionButton("submit", "Submit Entry",
                                      style = "background-color: #4CAF50; color: white;",
                                      disabled = TRUE)
                       ),
                       br(),
                       div(
                         style = "text-align: center;",
                         "Scoring: Total wins in main 63-game tournament (First Four excluded).
              Typical winning total: ~22 wins. Tiebreaker: Combined points in championship game."
                       )
                )
              ),

              # Add JavaScript to handle checkbox highlighting
              tags$script(HTML("
    $(document).ready(function() {
      console.log('Document ready - Initializing highlighting');

      function updateHighlighting() {
        console.log('Updating highlighting...');
        $('#selected_teams .checkbox').each(function() {
          var checkbox = $(this).find('input[type=\"checkbox\"]');
          var label = $(this).find('label');
          var checkboxValue = checkbox.attr('value');

          if (checkbox.length > 0 && label.length > 0) {
            if (checkbox.is(':checked')) {
              label.addClass('team-highlighted');
              console.log('Highlighted: ' + label.text() + ' (Value: ' + checkboxValue + ')');
            } else {
              label.removeClass('team-highlighted');
              console.log('Unhighlighted: ' + label.text() + ' (Value: ' + checkboxValue + ')');
            }
          } else {
            console.log('Checkbox or label not found in .checkbox div for value: ' + checkboxValue);
          }
        });
      }

      // Initial call after a delay
      setTimeout(updateHighlighting, 500);

      // Listen for changes
      $('#selected_teams').on('change', 'input[type=\"checkbox\"]', function() {
        console.log('Checkbox changed: ' + $(this).attr('value'));
        updateHighlighting();
      });
    });
  "))
      ),
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

