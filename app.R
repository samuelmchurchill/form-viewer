library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(stringr)
library(dplyr)
library(shinyjs)
library(knitr)
library(markdown)
library(aws.s3)

# Use AWS credentials if they exist. If they exist, then aws.s3 package uses the
# credentials with the default names below.
# If the credentials file exists in the working directory, 
if(file.exists(".Renviron") 
   &&
   # And all of the AWS credentials are defined
   !("" %in% c(Sys.getenv("AWS_ACCESS_KEY_ID"),
               Sys.getenv("AWS_SECRET_ACCESS_KEY"),
               Sys.getenv("AWS_DEFAULT_REGION"),
               Sys.getenv("AWS_BUCKET_NAME")))
   &&
   # And the credentials work
   Sys.getenv("AWS_BUCKET_NAME") %in% bucketlist()$Bucket) {
  # Then
  use_s3 <- TRUE
} else {
  use_s3 <- FALSE
}

# Pull in custom widgets that are used throughout applications
source(file.path("global-ui.R"), local = TRUE)$value

title<-tags$a(
  # href='https://procogia.com/partner-solutions/rstudio-partner/',
  tags$img(src = "procogia-logo-transparent.png", height='80%'), target="_blank")
# Construct main html document
ui <- tagList(
  # Add custom CSS
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css")
  ),
  # Required in order to activate shinyjs functions
  useShinyjs(),
  
  # Main page creation
  dashboardPage(title="ProCogia",
                # Header UI elements
                dashboardHeader(
                  title = title
                ),
                # Sidebar UI elements
                dashboardSidebar(
                  # hr(),
                  sidebarMenuOutput("file_list")
                  # hr(),
                  # Not for release - used to enter browser() mode for debugging.
                  # actionButton(inputId = 'btn_debug', label = 'Debug')
                ),
                # Body UI elements
                dashboardBody(
                  tags$script(HTML("$('body').addClass('fixed');")),
                  box(width = '100%', htmlOutput("showFile"))
                )
  ),
  
  # Add custom JS to be run at end of document creation
  tags$footer(
    includeScript(path = file.path("www", "script.js"))
  )
)

# Define server logic
server <- function(input, output, session) {
  # Define a temporary directory to read/write from/to for this session
  temp_dir <- tempdir()
  
  # Keep bucket name handy
  bucket_name <- Sys.getenv("AWS_BUCKET_NAME")
  
  # Get a list of all files
  file_df <- get_bucket_df(bucket_name) |> arrange(desc(LastModified))
  file_names <- file_df$Key
  
  output$file_list <- 
    renderMenu(
      sidebarMenu(
        .list = lapply(
          file_names,
          function(nom) {
            sidebarNavPanel(nom)
          }
        )
      )
    )
  
  lapply(
    file_names,
    function(nom) {
      observeEvent(
        input[[nom]],
        {
          # File should be a zip. we'll save it here:
          zip_loc <- file.path(temp_dir, nom)
          unzip_loc <- file.path(temp_dir, str_sub(nom, 1, -5))
          report_loc <- file.path(unzip_loc, "report.html")
          trimmed_report_loc <- file.path(unzip_loc, "trimmed-report.html")

          # If the file doesn't exist yet
          if(!file.exists(trimmed_report_loc)) {          
            # Copy file to temp directory
            save_object(object = nom, 
                        bucket = bucket_name, 
                        file = zip_loc)
            
            # and unzip to folder
            unzip(zipfile = zip_loc, exdir = unzip_loc)
            
            # Trim report down to just the report (not bringing in new css/js)
            full_report <- readLines(report_loc)
            bounds <- which(full_report %in% c('<body>', '</body>'))
            trimmed_report <- full_report[bounds[1]:bounds[2]]
            writeLines(trimmed_report, trimmed_report_loc)
          }
          
          output$showFile <- renderUI(includeHTML(trimmed_report_loc))
          for(fn in file_names) {
            removeClass(id = paste0("li-", fn), class = "active")
          }
          addClass(id = paste0("li-", nom), class = "active")
        }
      )
    }
  )
  
  # * Debug utility ----
  # Not for release - enter debugging mode, button is in the sidebar.
  # observeEvent(input$btn_debug, {browser()})
}


# Run the application 
shinyApp(ui = ui, server = server)
