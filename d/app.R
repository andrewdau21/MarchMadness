library(shiny)
library(dplyr)
library(DT)
library(RPostgres)
library(uuid)
library(shinyjs)
library(emayili)

# Database connection
con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv('host'),
  port = Sys.getenv('port'),
  dbname = Sys.getenv('dbname'),
  user = Sys.getenv('user'),
  password = Sys.getenv('password')
)

# Get teams data from database
teams <- dbGetQuery(con, "SELECT team_name, seed, cost FROM march_madness_teams") %>% arrange(seed)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("March Madness Team Selector"),
  
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
  )
)

server <- function(input, output, session) {
  
  # IP address with X-Forwarded-For support
  ip_address <- reactive({
    forwarded <- session$request$HTTP_X_FORWARDED_FOR
    ip <- if(!is.null(forwarded) && nchar(forwarded) > 0) forwarded else session$request$REMOTE_ADDR
    message("Captured IP: ", ip)
    ip
  })
  
  # Check for existing IP submissions
  ip_exists <- reactive({
    query <- paste0("SELECT COUNT(*) FROM submissions WHERE ip_address = '", ip_address(), "'")
    dbGetQuery(con, query)$count > 0
  })
  
  # IP check on page load
  observe({
    if(ip_exists()) {
      showModal(modalDialog(
        title = "Duplicate Entry Detected",
        "This IP address already has an entry. Creating another will result in a duplicate. 
         Please contact Paul Schulz to delete your original entry if needed.",
        easyClose = TRUE
      ))
    }
  })
  
  # Real-time validation for name, email, and tiebreaker
  output$name_warning <- renderText({
    if(nchar(input$entry_name) == 0) "Please enter an entry name." else ""
  })
  
  output$email_warning <- renderText({
    if(nchar(input$email) == 0) "Please enter an email address." else ""
  })
  
  output$tiebreaker_warning <- renderText({
    if(is.null(input$tiebreaker) || is.na(input$tiebreaker) || input$tiebreaker <= 0) {
      "Please enter a valid tiebreaker score (greater than 0)."
    } else {
      ""
    }
  })
  
  # Enable/disable submit button based on inputs
  observe({
    if(nchar(input$entry_name) > 0 && 
       nchar(input$email) > 0 && 
       !is.null(input$tiebreaker) && 
       !is.na(input$tiebreaker) && 
       input$tiebreaker > 0) {
      shinyjs::enable("submit")
    } else {
      shinyjs::disable("submit")
    }
  })
  
  # Total cost
  total_cost <- reactive({
    selected <- teams %>% 
      filter(team_name %in% input$selected_teams) %>%
      summarise(total = sum(cost)) %>%
      pull(total)
    if(length(selected) == 0) 0 else selected
  })
  
  output$total_cost <- renderText({
    paste("Total Cost: $", total_cost(), " / $100")
  })
  
  output$warning <- renderText({
    if(total_cost() > 100) {
      "WARNING: Selection exceeds $100 cap!"
    } else if(total_cost() > 0) {
      paste("Remaining Budget: $", 100 - total_cost())
    } else {
      ""
    }
  })
  
  # Submission
  observeEvent(input$submit, {
    if(total_cost() > 100) {
      showModal(modalDialog(
        title = "Error",
        "Your selection exceeds the $100 salary cap. Please adjust your teams.",
        easyClose = TRUE
      ))
    } else if(length(input$selected_teams) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Please select at least one team.",
        easyClose = TRUE
      ))
    } else {
      entry_id <- UUIDgenerate()
      
      submission <- teams %>% 
        filter(team_name %in% input$selected_teams) %>%
        select(team_name, seed, cost) %>%
        mutate(
          entry_id = entry_id,
          entry_name = input$entry_name,
          email = input$email,
          ip_address = ip_address(),
          submission_time = Sys.time()
        )
      
      dbWriteTable(con, "submissions", submission, append = TRUE, row.names = FALSE)
      
      totals <- data.frame(
        entry_id = entry_id,
        email = input$email,
        total_dollars = total_cost(),
        tiebreaker_points = input$tiebreaker,  # Add tiebreaker to totals
        submission_time = Sys.time()
      )
      dbWriteTable(con, "submission_totals", totals, append = TRUE, row.names = FALSE)
      
      # Email receipt
      selected_teams_text <- paste(input$selected_teams, collapse = "\n")
      email_body <- paste(
        "Hi ", input$entry_name, ",\n\n",
        "Thank you for submitting your March Madness Team Selector entry! Here’s your receipt:\n\n",
        "Entry ID: ", entry_id, "\n",
        "Selected Teams:\n", selected_teams_text, "\n",
        "Total Cost: $", total_cost(), "\n",
        "Tiebreaker (Combined Championship Points): ", input$tiebreaker, "\n\n",
        "Good luck in the tournament!\n",
        "Contact Paul Schulz if you need to modify your entry.\n\n",
        "Best,\nThe March Madness Team",
        sep = ""
      )
      
      email <- envelope(
        to = input$email,
        from = "Sal Arycap",
        subject = "2025 March Capness Entry",
        text = email_body
      )
      
      smtp <- emayili::server(
        host = "smtp.gmail.com",
        port = 465,
        username = Sys.getenv('SMTP_USERNAME'),
        password = Sys.getenv('SMTP_PASSWORD')
      )
      
      smtp(email, verbose = TRUE)
      
      showModal(modalDialog(
        title = "Success",
        paste("Your entry has been submitted! Entry ID:", entry_id, "A receipt has been emailed to you."),
        easyClose = TRUE
      ))
      
      updateTextInput(session, "entry_name", value = "")
      updateTextInput(session, "email", value = "")
      updateCheckboxGroupInput(session, "selected_teams", selected = character(0))
      updateNumericInput(session, "tiebreaker", value = NULL)
    }
  })
}

onStop(function() {
  dbDisconnect(con)
})

shinyApp(ui = ui, server = server)