library(shiny)
library(shinyjs)
library(jsonlite)
library(googlesheets4)
library(dplyr)
library(bslib)

# Create a custom UMN theme using official colors with Bootstrap version 5
umn_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",        # white background
  fg = "#000000",        # default text color: black
  primary = "#7A0019",   # UMN Maroon
  secondary = "#FFDE7A", # UMN Gold
  base_font = font_google("Open Sans"),
  bootswatch = "materia"
)

# Read JSON credentials from environment variable
service_account_json <- Sys.getenv("GCP_SERVICE_ACCOUNT_JSON")

if (nzchar(service_account_json)) {
  # Parse JSON into a list
  creds <- jsonlite::fromJSON(service_account_json, simplifyVector = FALSE)
  
  # Authenticate directly with the JSON object
  gs4_auth(credentials = creds)
} else {
  stop("No Google service account JSON found in environment variable")
}


# URL or ID of your Google Sheet (faculty responses)
sheet_url <- "https://docs.google.com/spreadsheets/d/1teBeYg2zV3bqq9rYOrQDB7deNZ7XrlVZhUtnT6Ygyhk/edit?usp=sharing"

# Read in the data from the Google Sheet (adjust sheet name if necessary)
raw_data <- read_sheet(sheet_url, sheet = "Form Responses 1")

# Rename columns to shorter names
faculty_data <- raw_data %>%
  rename(
    timestamp             = "Timestamp",
    name                  = "Name",
    email                 = "Email",
    current_rank          = "Current Rank",
    methods               = "Primary methods research areas (select all that apply)",
    collab                = "Primary interdisciplinary/collaborative research areas (select all that apply)",
    link_to_cv            = "Link to CV",
    current_advisees_num  = "I am currently advising X PhD students on their dissertation work, where X =",
    current_advisees_desc = "List the PhD students that you are currently working with and briefly describe what they are working on",
    in_office_most_days   = "[I am in the office at UOP most days]",
    prefer_in_person      = "[I prefer to meet with my advisees in-person]",
    work_prefs            = "What are your preferences and expectations for working with students?",
    open_to_talk          = "Column 12"   # Response indicating whether they're open to talking
  )

# Convert timestamp to POSIXct and eliminate duplicate responses (keep most recent per email)
faculty_data$timestamp <- as.POSIXct(faculty_data$timestamp)
faculty_data <- faculty_data %>% 
  arrange(desc(timestamp)) %>% 
  distinct(email, .keep_all = TRUE) %>%
  mutate(timestamp_date = format(timestamp, "%Y-%m-%d"))

# Prepare lists of possible selections (assuming comma-separated values)
methods_choices <- unique(unlist(strsplit(as.character(faculty_data$methods), ",\\s*")))
methods_choices <- methods_choices[!is.na(methods_choices) & methods_choices != ""]
collab_choices <- unique(unlist(strsplit(as.character(faculty_data$collab), ",\\s*")))
collab_choices <- collab_choices[!is.na(collab_choices) & collab_choices != ""]

# --- Helper Functions ---

create_discrete_progress_bar <- function(response) {
  level_mapping <- c("Strongly Disagree" = 1,
                     "Disagree"          = 2,
                     "Neutral"           = 3,
                     "Agree"             = 4,
                     "Strongly Agree"    = 5)
  level <- level_mapping[trimws(response)]
  if (is.na(level)) level <- 0
  
  color_mapping <- c("Strongly Disagree" = "#cce5ff",
                     "Disagree"          = "#99ccff",
                     "Neutral"           = "#66b2ff",
                     "Agree"             = "#3399ff",
                     "Strongly Agree"    = "#0073e6")
  
  fill_color <- if (trimws(response) %in% names(color_mapping)) {
    color_mapping[trimws(response)]
  } else {
    "#0073e6"
  }
  
  segments <- lapply(1:5, function(i) {
    seg_color <- if (i <= level) fill_color else "#ddd"
    div(
      style = sprintf("flex: 1; height: 20px; background-color: %s; margin-right: %s;", 
                      seg_color, if (i < 5) "2px" else "0px")
    )
  })
  
  div(
    div(style = "display: flex; margin-bottom: 5px;", segments),
    div(style = "text-align: center; font-weight: bold;", trimws(response))
  )
}

create_faculty_profile <- function(row) {
  card(
    card_header(
      # Card header with gold background and black text
      div(class = "d-flex justify-content-between align-items-center",
          # Left side: Faculty info in multiple rows
          div(
            tagList(
              # Row 1: Faculty name (unlinked) with larger [CV] link immediately to its right
              div(
                h6(paste("Last Updated:", row$timestamp_date), style = "color: #777677; font-size: 0.8em;"),
                h3(row$name, class = "card-title", style = "display: inline; margin-right: 10px;"),
                if (!is.na(row$link_to_cv) && row$link_to_cv != "") {
                  a("[CV]", href = row$link_to_cv, target = "_blank", style = "font-size: 1em;")
                }
              ),
              # Row 2: Faculty rank
              p(row$current_rank),
              # Row 3: Full email link
              p(a(row$email, href = paste0("mailto:", row$email))),
              # Row 4: Open-to-talk response (printed directly in italics)
              p(em(row$open_to_talk))
            )
          ),
          # Right side: Discrete progress bars in a fixed container (min-width: 200px)
          div(
            style = "min-width: 200px; text-align: right;",
            div(
              p(strong("In Office Most Days:"), style = "margin-bottom: 2px; font-size: 0.9em;"),
              create_discrete_progress_bar(row$in_office_most_days)
            ),
            div(
              style = "margin-top: 5px;",
              p(strong("Prefer In-Person Meetings:"), style = "margin-bottom: 2px; font-size: 0.9em;"),
              create_discrete_progress_bar(row$prefer_in_person)
            )
          )
      ),
      style = "background-color: #F0EFEE; color: black;"
    ),
    card_body(
      tagList(
        p(strong("Methods: "), row$methods),
        p(strong("Collaborative Areas: "), row$collab),
        p(strong("Currently Advising: "), row$current_advisees_num, " PhD student(s)"),
        if (!is.na(row$current_advisees_desc) && row$current_advisees_desc != "") {
          p(
            strong("Advisees' Topics: "),
            HTML(gsub("\n", "<br/>", row$current_advisees_desc))
          )
        },
        p(strong("Work Preferences & Expectations: "), row$work_prefs)
      )
    )
  )
}

# --- UI Definitions ---
# The login page UI (shown when not authenticated)
loginPage <- fluidPage(
  theme = umn_theme,
  tags$head(
    tags$meta(`http-equiv` = "Content-Security-Policy",
              content = "default-src 'self'; script-src 'self' https://accounts.google.com https://www.gstatic.com 'unsafe-inline' 'unsafe-eval' blob: data:; style-src 'self' 'unsafe-inline' 'unsafe-hashes';"),
    HTML('<script src="https://accounts.google.com/gsi/client" async defer></script>'),
    includeScript("signin.js")
  ),
  titlePanel("Please Log In"),
  # Container for the Google Sign-In button
  div(id = "signin", style = "margin: 50px auto; width: 220px;"),
  # Use HTML() to insert an inline script that triggers initialization
  tags$script(HTML("safeInitGoogleSignIn();"))
)

# The main app page UI (shown when authenticated)
mainPage <- fluidPage(
  theme = umn_theme,
  useShinyjs(),
  tags$head(
    tags$meta(`http-equiv` = "Content-Security-Policy",
              content = "default-src 'self'; script-src 'self' https://accounts.google.com https://www.gstatic.com 'unsafe-inline' 'unsafe-eval' blob: data:; style-src 'self' 'unsafe-inline' 'unsafe-hashes';"),
    HTML('<script src="https://accounts.google.com/gsi/client" async defer></script>'),
    includeScript("signin.js")
  ),
  # absolutePanel(top = 10, right = 10,
  #               actionButton("signout", "Sign Out", onclick = "signOut();", class = "btn-danger")
  # ),
  titlePanel("BHDS Faculty Profiles"),
  sidebarLayout(
    sidebarPanel(
      card(
        card_header("Select Method(s) of Interest", style = "background-color: #FFDE7A; color: black;"),
        card_body(
          checkboxGroupInput(
            inputId  = "methods_selected",
            label    = NULL,
            choices  = sort(methods_choices),
            selected = methods_choices
          ),
          fluidRow(
            column(6, actionButton("select_all_methods", "Select All")),
            column(6, actionButton("select_none_methods", "Select None"))
          )
        )
      ),
      br(),
      card(
        card_header("Select Collaborative/Application Areas", style = "background-color: #FFDE7A; color: black;"),
        card_body(
          checkboxGroupInput(
            inputId  = "collab_selected",
            label    = NULL,
            choices  = sort(collab_choices),
            selected = collab_choices
          ),
          fluidRow(
            column(6, actionButton("select_all_collab", "Select All")),
            column(6, actionButton("select_none_collab", "Select None"))
          )
        )
      ),
      br(),
      checkboxInput(
        inputId = "only_open_to_talk",
        label   = "Show only faculty open to talking about dissertation/Plan B/ILE opportunities",
        value   = FALSE
      )
    ),
    mainPanel(
      uiOutput("faculty_profiles")
    )
  )
)

# The overall UI is generated dynamically based on login status.
ui <- uiOutput("page")

# --- Server ---
server <- function(input, output, session) {
  
  # Render the appropriate page based on login status.
  output$page <- renderUI({
    print(input$g.id)
    print(input$g.email)
    
    if (isTruthy(input$g.id)) {
      if(grepl("umn.edu", input$g.email)) {
        mainPage
      } else {
        loginPage
      }
    } else {
      loginPage
    }
  })
  
  # Also trigger a custom message to initialize Google Sign-In on the client side
  # (in case the inline script didn't fire).
  observe({
    if (!isTruthy(input$g.id)) {
      session$sendCustomMessage("init-signin", "")
    }
  })
  
  # Set up custom handlers for the faculty profile UI.
  observeEvent(input$g.id, {
    if (isTruthy(input$g.id)) {
      # Observers for Methods selection buttons
      observeEvent(input$select_all_methods, {
        updateCheckboxGroupInput(session, "methods_selected",
                                 selected = sort(methods_choices))
      })
      
      observeEvent(input$select_none_methods, {
        updateCheckboxGroupInput(session, "methods_selected",
                                 selected = character(0))
      })
      
      # Observers for Collaborative Areas selection buttons
      observeEvent(input$select_all_collab, {
        updateCheckboxGroupInput(session, "collab_selected",
                                 selected = sort(collab_choices))
      })
      
      observeEvent(input$select_none_collab, {
        updateCheckboxGroupInput(session, "collab_selected",
                                 selected = character(0))
      })
    }
  })
  
  filtered_data <- reactive({
    df <- faculty_data
    
    if (!is.null(input$methods_selected) && length(input$methods_selected) > 0) {
      df <- df[sapply(df$methods, function(x) {
        if (is.na(x) || x == "") return(FALSE)
        row_methods <- trimws(unlist(strsplit(as.character(x), ",\\s*")))
        any(row_methods %in% input$methods_selected)
      }), ]
    }
    
    if (!is.null(input$collab_selected) && length(input$collab_selected) > 0) {
      df <- df[sapply(df$collab, function(x) {
        if (is.na(x) || x == "") return(FALSE)
        row_collab <- trimws(unlist(strsplit(as.character(x), ",\\s*")))
        any(row_collab %in% input$collab_selected)
      }), ]
    }
    
    if (isTruthy(input$only_open_to_talk)) {
      df <- df[grepl("currently open to being contacted", df$open_to_talk, ignore.case = TRUE), ]
    }
    
    df
  })
  
  output$faculty_profiles <- renderUI({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(p("No faculty match your selected criteria."))
    }
    profiles <- lapply(seq_len(nrow(df)), function(i) {
      create_faculty_profile(df[i, ])
    })
    do.call(tagList, profiles)
  })
  
}

# Custom message handler registration so the server can trigger initGoogleSignIn
shinyApp(ui = ui, server = server, options = list(port = 1234))
