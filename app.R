
# --------------------------------------------------------------
# Shiny Web Application for Soil Security: Connectivity Evaluation Tool
# --------------------------------------------------------------
#
# Purpose: This Shiny self-evaluation tool uses quantifies the connectivity dimension of the Soil Security Assessment Framework. 

# Author(s): Julio C. Pachón Maldonado (julio.pachon@sydney.edu.au), Emma Leonard, Damien Field, Katie McRobert, Richard Heath, Alex McBratney

# Funding: This research was supported by the Australian Research Council Laureate Fellowship (FL210100054) on Soil Security, titled A Calculable Approach to Securing Australia’s Soils.
# License: Creative Commons Attribution 4.0 International License (CC BY 4.0)
#
# Usage:
# - This application supports an offline mode, using a local SQLite database if no SQL server is available.
# - To toggle between offline and online modes, adjust the `mode` variable in the configuration section below.
# - In offline mode, data is stored locally in `TestDatabase.sqlite`, which is generated in the project directory.
#
# Files Required:
# - 202308_SS_Connectivity_EvalTool_StrataInfo2.csv: CSV file that is used to centre the map on page 2 based on the postal code provided in page 1.
# - www holds the FAQ page and images.
#
# Notes:
# - To ensure reproducibility, dependencies are managed via `renv`. Run `renv::restore()` to replicate the required environment.
# - For Docker usage, build and run the container as per instructions in the README file.
#
# --------------------------------------------------------------



library(shiny)
library(vroom)
library(leaflet)
library(RMariaDB) 
library(DBI)
library(RMySQL)
library(shinyjs)
library(RSQLite) #only needed for creating a local SQL database 



dbtable <-  "Evaltool_data" #name of the table with raw inputs in the SQL database
dbtable2<- "EvalTool_Score" #name of the table with quantified inputs in the SQL database

mode="offline" #comment out or erase this line if SQL database is available
sqlite_path <- "./TestDatabase.sqlite"


# Creates an SQLite database if the mode is offline
if (mode == "offline") {
  conn <- dbConnect(RSQLite::SQLite(), sqlite_path)
  
  # Create the Evaltool_data table with all required columns
  if (!dbExistsTable(conn, dbtable)) {
    dbExecute(conn, "
      CREATE TABLE Evaltool_data (
        session_token TEXT PRIMARY KEY,
        start_time TEXT,
        Role_Q TEXT,
        other_role TEXT,
        Farm_Enterprises TEXT,
        other_Farm_Enterprises TEXT,
        Perceived_Threats TEXT,
        other_Perceived_Threats TEXT,
        Number_soil_types TEXT,
        Main_Soil_Type TEXT,
        postal_code TEXT,
        town_select TEXT,
        lat REAL,
        lon REAL,
        E_K REAL,
        E_Ac REAL,
        E_At REAL,
        Legislated_erosion TEXT,
        A_K REAL,
        A_Ac REAL,
        A_At REAL,
        A_pH REAL,
        SD_K REAL,
        SD_Ac REAL,
        SD_At REAL,
        SD_Val REAL,
        other_SD_Val TEXT,
        S_K REAL,
        S_Ac REAL,
        S_At REAL,
        S_Val REAL,
        HL_K REAL,
        HL_Ac REAL,
        HL_At REAL,
        HL_Val REAL,
        other_HL_Val TEXT,
        HL_Val2 REAL,
        other_HL_Val2 TEXT,
        NM_K REAL,
        NM_Ac REAL,
        NM_At REAL,
        NM_Val REAL,
        NM_Val2 REAL,
        SW_K REAL,
        SW_Ac REAL,
        SW_At REAL,
        SW_Val REAL,
        SW_Val2 REAL,
        DC_K REAL,
        DC_Ac REAL,
        DC_At REAL,
        DC_Val REAL,
        other_DC_Val TEXT,
        DC_Val2 REAL,
        Threat_Val_E REAL,
        Threat_Val_E_comment TEXT,
        Threat_Val_A REAL,
        Threat_Val_A_comment TEXT,
        Threat_Val_SD REAL,
        Threat_Val_SD_comment TEXT,
        Threat_Val_DC REAL,
        Threat_Val_DC_comment TEXT,
        Threat_Val_S REAL,
        Threat_Val_S_comment TEXT,
        Threat_Val_HL REAL,
        Threat_Val_HL_comment TEXT,
        End_time TEXT,
        Submission_time TEXT,
        Age REAL,
        education_level TEXT,
        Land_type TEXT,
        other_land_type TEXT,
        Land_area REAL,
        Word_Familiarity TEXT,
        Val_DC_comment TEXT
      )
    ")
  }
  
  # Create the EvalTool_Score table with all required columns
  if (!dbExistsTable(conn, dbtable2)) {
    dbExecute(conn, "
      CREATE TABLE EvalTool_Score (
        session_token TEXT PRIMARY KEY,
        start_time TEXT,
        E_K_score_num REAL,
        E_Ac_score_num REAL,
        E_At_score_num REAL,
        E_total_score_num REAL,
        A_K_score_num REAL,
        A_Ac_score_num REAL,
        A_At_score_num REAL,
        A_total_score_num REAL,
        SD_K_score_num REAL,
        SD_Ac_score_num REAL,
        SD_At_score_num REAL,
        SD_total_score_num REAL,
        S_K_score_num REAL,
        S_Ac_score_num REAL,
        S_At_score_num REAL,
        S_total_score_num REAL,
        HL_K_score_num REAL,
        HL_Ac_score_num REAL,
        HL_At_score_num REAL,
        HL_total_score_num REAL,
        NM_K_score_num REAL,
        NM_Ac_score_num REAL,
        NM_At_score_num REAL,
        NM_total_score_num REAL,
        SW_K_score_num REAL,
        SW_Ac_score_num REAL,
        SW_At_score_num REAL,
        SW_total_score_num REAL,
        DC_K_score_num REAL,
        DC_Ac_score_num REAL,
        DC_At_score_num REAL,
        DC_total_score_num REAL
      )
    ")
  }
  
  dbDisconnect(conn)
}

# make a progress bar
progressBar <- function(id,
                        value,
                        total = NULL,
                        display_pct = FALSE,
                        size = NULL,
                        status = NULL,
                        striped = FALSE,
                        title = NULL,
                        range_value = NULL,
                        commas = TRUE,
                        unit_mark = "%") {
  if (!is.null(total)) {
    percent <- round(value / total * 100)
  } else {
    value <- round(value)
    if (!is.null(range_value)) {
      percent <- rescale(x = value, from = range_value, to = c(0, 100))
    } else {
      percent <- value
    }
  }
  
  if (!is.null(title) | !is.null(total)) {
    title <- tags$span(
      class = "progress-text",
      id = paste0(id, "-title"),
      title, HTML("&nbsp;")
    )
  }
  
  
  value_for_display <- value
  total_for_display <- total
  
  
  if (!is.null(total)) {
    total <- tags$span(
      class = "progress-number",
      tags$b(value_for_display, id = paste0(id, "-value")),
      "/",
      tags$span(id = paste0(id, "-total"), total_for_display)
    )
  }
  
  tagPB <- tags$div(
    class = "progress-group",
    title, total,
    tags$div(
      class = "progress",
      class = if (!is.null(size)) paste0("progress-", size),
      tags$div(
        id = id,
        style = if (percent > 0) paste0("width:", percent, "%;"),
        style = if (display_pct) "min-width: 2em;",
        class = "progress-bar",
        class = if (!is.null(status)) paste0("progress-bar-", status),
        class = if (!is.null(status)) paste0("bg-", status),
        class = if (striped) "progress-bar-striped",
        role = "progressbar",
        if (display_pct) paste0(percent, unit_mark)
      )
    )
  )
  tagPB <- tagList(
    singleton(
      tags$head(tags$style(".progress-number {position: absolute; right: 40px;}"))
    ), tagPB
  )
}



# Supporting functions to interact with SQL database
saveData0 <- function(data, data2) {
  
  # Establish a database connection based on the selected mode
  if (mode == "offline") {
    db <- dbConnect(RSQLite::SQLite(), sqlite_path)
  } else {

  readRenviron("~/.Renviron")
  db_config <- list(
    "host" = Sys.getenv("MYSQL_HOST"),
    "port" = as.numeric(Sys.getenv("MYSQL_PORT")),
    "user" = Sys.getenv("MYSQL_USER"),
    "password" = Sys.getenv("MYSQL_PASSWORD"),
    "dbname" = Sys.getenv("MYSQL_DBNAME")  
  )
  
  db <- dbConnect(RMySQL::MySQL(), host = db_config$host, port = db_config$port,
                  user = db_config$user, password = db_config$password,
                  dbname = db_config$dbname)
  }
  # Check if session_token with specific data exists
  session_token <- data[1, 1]
  check_query <- sprintf("SELECT COUNT(*) FROM %s WHERE session_token = '%s'", 
                         dbtable, session_token)
  existing_count <- dbGetQuery(db, check_query)$'COUNT(*)'
  
  
  
  
  if (existing_count == 0) {
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      dbtable, 
      paste(names(data), collapse = ", "),
      paste(data, collapse = "', '")
    )
    dbGetQuery(db, query)
    
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      dbtable2, 
      paste(names(data2), collapse = ", "),
      paste(data2, collapse = "', '")
    )
    dbGetQuery(db, query)

    
    
  }
  
  else {
    return()
  }
  
  dbDisconnect(db)
  } 


saveData1 <- function(data = NULL, data2 = NULL) {
  if (mode == "offline") {
    db <- dbConnect(RSQLite::SQLite(), sqlite_path)
  } else {

  readRenviron("~/.Renviron")
  

  db_config <- list(
    "host" = Sys.getenv("MYSQL_HOST"),
    "port" = as.numeric(Sys.getenv("MYSQL_PORT")),
    "user" = Sys.getenv("MYSQL_USER"),
    "password" = Sys.getenv("MYSQL_PASSWORD"),
    "dbname" = Sys.getenv("MYSQL_DBNAME")  
  )
  

  db <- dbConnect(RMySQL::MySQL(), host = db_config$host, port = db_config$port,
                  user = db_config$user, password = db_config$password,
                  dbname = db_config$dbname)
  }
  if (!is.null(data) && nrow(data) > 0 && !is.null(dbtable)) {
    
    
    query <- sprintf(
      "UPDATE %s SET %s WHERE session_token = '%s'",
      dbtable,
      paste(paste0(names(data)[-1], " = '", data[,-1], "'"), collapse = ", "),
      data$session_token
    )

    dbGetQuery(db, query)
  }
  
  if (!is.null(data2) && nrow(data2) > 0 && !is.null(dbtable2)) {
    
    query <- sprintf(
      "UPDATE %s SET %s WHERE session_token = '%s'",
      dbtable2,
      paste(paste0(names(data2)[-1], " = '", data2[,-1], "'"), collapse = ", "),
      data2$session_token
    )

    dbGetQuery(db, query)
  }
  

  dbDisconnect(db)
} 


return_results_data <- function(session_token, table_input) {
 
   if (mode == "offline") {
    db <- dbConnect(RSQLite::SQLite(), sqlite_path)
  } else {

  readRenviron("~/.Renviron")
  

  db_config <- list(
    "host" = Sys.getenv("MYSQL_HOST"),
    "port" = as.numeric(Sys.getenv("MYSQL_PORT")),
    "user" = Sys.getenv("MYSQL_USER"),
    "password" = Sys.getenv("MYSQL_PASSWORD"),
    "dbname" = Sys.getenv("MYSQL_DBNAME")  
  )
  
  db <- dbConnect(RMySQL::MySQL(), host = db_config$host, port = db_config$port,
                  user = db_config$user, password = db_config$password,
                  dbname = db_config$dbname)
  }

  
  query <- sprintf("SELECT * FROM %s WHERE session_token = '%s'", table_input, session_token)
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  
  return(data)
  } 






# textInput2: Custom text input with validation to limit SQL injection risk by restricting input format and length

textInput2 <- function(inputId, label, value = "", width = NULL, 
                       placeholder = NULL, maxlength = 350) {
  
  # Regular expression pattern to allow only letters, hyphens, and spaces up to maxlength
  regex_pattern <- paste0("^[a-zA-Z''\\-\\s]{1,", maxlength, "}$")
  
  tag <- shiny::textInput(
    inputId = inputId,
    label = label,
    value = value,
    width = width,
    placeholder = placeholder
  )
  

  tag <- shiny::div(
    tag,
    `data-regex` = regex_pattern
  )
  

  tag <- htmltools::tagQuery(tag)$
    children("input")$
    addAttrs(pattern = regex_pattern)$
    allTags()
  
  tag
}

digitize <- function(x) {
  suppressWarnings(!is.na(as.numeric(x)))
}
# Sanitize: Cleans user text input by removing problematic characters (like quotes) to prevent SQL injection

Sanitize <- function(text) {
  if (is.null(text) || text == "") {
    return("No answer")
  }
  
  text <- tolower(text)
  text <- gsub("'", '', text)
  text <- gsub("-", ' ', text)
  
  return(text)
}




start_time_reactive <- reactiveVal()
Strata_info <- vroom::vroom("./202308_SS_Connectivity_EvalTool_StrataInfo2.csv",
                            col_types = cols(POA_CODE21 =  col_character()))

start_time <- Sys.time()



#Defining UI Modules####
# page0_content: Intro page
page0_content <- function(id) {
  
  tagList(
    tags$a(name = "top"),
    # JavaScript functions to trigger navigation and FAQ modal actions
    tags$head(
      tags$script("
    function navigateToPage() {
      Shiny.setInputValue('navigate_to_page', true);
    }
    function showFAQModal() {
  // Directly trigger the FAQ modal here
  Shiny.setInputValue('show_faq_modal', true, {priority: 'event'});
}
    ")
    ),
    
    
    h2("How connected are you to your soil?", style = "color: #e04b2f;"),
    
    h4("Please give us 15 minutes of your valuable time – it could change how you think about and manage a very precious asset – your ", 
       HTML("<u>soil.</u>")),
    br(),
    div(
      HTML('<center><img src="Soil connection report graphic.png" width="200"></center>', )
    ),
    br(),
    div(
      p("We are undertaking groundbreaking research that aims to provide a way to measure the human influence on soil security. By completing this evaluation, your insights will help the Australian Farm Institute and University of Sydney researchers uncover the complex relationships between farmers, farming practices, and soil security."),
      p("The evaluation is entirely confidential, and you will only be required to choose whether or not to pin-point your soil location data if you are comfortable"),
      
      p("After completion, you can access ", HTML("<u>your personalised soil connectivity report</u>"),
        " to help you in maintaining and improving your soil quality and its security."),
      p(HTML("We appreciate that soil types can vary even within a paddock, let alone across enterprises, so please:<br><br>
                • Consider your answers <u>in relation to a particular paddock or area in your farming business</u>, and<br>
                • Provide the nearest town and postcode for the location of <u>that</u> area.<br><br>
              We will then apply a weighted scoring to produce results appropriate to the field specified.")),
      p("Our advanced analytics can differentiate between a farmer that would benefit from greater connectivity to their soil, and one who farms in an environment where there are few management options to minimise soil threats."),
      
      p(HTML("By continuing this evaluation, you have voluntarily agreed to share your <i>anonymised data</i> with the research team.")),
      p("Thank you in advance for your participation."),
      
      
      br(),
      p(HTML("<i>Richard Heath - AFI</i>")),
      p(HTML("<i>Dr. Julio Pachon Maldonado - University of Sydney</i>")),
      p(HTML("<i>Dr. Emma Leonard - AgriKnowHow</i>")),
      p(HTML("<i>Professor Alex McBratney - University of Sydney</i>")),
      p(HTML("<i>Professor Damien Field - University of Sydney</i>")),
      br(),
      p("Questions? Check out our ", 
        HTML("<a href='#' onclick='showFAQModal()' style='text-decoration: underline;'>FAQs</a>."))
      
    ),
    br(),
    div(
      class = "d-flex flex-column align-items-center",
      h4("Do you manage soil/land as part of a farming business?"),
      actionButton("next_page0", "Yes"),
      actionButton("Intro_no", "No"),
    ),
    
    
    div(
      class = "footer",
      p("Note: This research is funded (partially or fully) by the Australian Government through the Australian Research Council (ARC). Professor McBratney is the recipient of an ARC Australian Laureate Fellowship (project number FL210100054) funded by the Australian Government.")
    ),
    br(),
    br(),
  )
}



FAQ_content <- function(id){
  tagList(
    tags$script(HTML('
    function toggleDetails(id) {
      var element = document.getElementById(id);
      if (element.style.display === "none") {
        element.style.display = "block";
      } else {
        element.style.display = "none";
      }
    }
  ')),
    h2("Frequently Asked Questions", style = "color: #e04b2f;"),
    h4(actionLink("faq_button", "What is this evaluation for?"), onclick = "toggleDetails('FAQ1');"),
    p("The purpose of this evaluation is to improve the connectivity of people who manage soil with this important resource to ensure its long-term security.  The evaluation also serves to collect information from land managers about their knowledge, attitudes and practices regarding soil security.", id = "FAQ1", style = "display:none; padding: 10px; background-color: #f0f0f0;"),
    
    h4(actionLink("faq_button", "How will this evaluation benefit me?"), onclick = "toggleDetails('FAQ2');"),
    p("If you don’t measure it you cannot manage it, is an old but true adage. This evaluation will provide you with quantified evidence of your connectivity to your soil and how that links to whether you are maintaining or improving the sustainable productivity from your soil. All this information will be provided in a farmer friendly, personalised report. ", id = "FAQ2", style = "display:none; padding: 10px; background-color: #f0f0f0;"),
    
    h4(actionLink("faq_button", "Who will be able to access my data?"), onclick = "toggleDetails('FAQ3');"),
    p("The members of the research team: Richard Heath, Julio Pachon Maldonado, Alex McBratney, Damien Field, Emma Leonard. But dont forget, all data is de-identified and only reported in an aggregated form", id = "FAQ3", style = "display:none; padding: 10px; background-color: #f0f0f0;"),
    
    
    h4(actionLink("faq_button", "How will my data be used?"), onclick = "toggleDetails('FAQ4');"),
    p("Data will be aggregated regionally and analyzed to find patterns in land manager connection to their soil which will be used by researchers to submit a publication and use as a basis for future work. Your participation is anonymous even to the researchers.", id = "FAQ4", style = "display:none; padding: 10px; background-color: #f0f0f0;"),
    
    h4(actionLink("faq_button", "Where will my data be stored?"), onclick = "toggleDetails('FAQ5');"),
    p("No identifying data is recorded and responses are kept in a secure database managed by AFI. ", id = "FAQ5", style = "display:none; padding: 10px; background-color: #f0f0f0;"),
    
    h4(actionLink("faq_button", "Why do you need my postcode?"), onclick = "toggleDetails('FAQ6');"),
    p("Many farms and farming businesses are spread across multiple locations. We have asked you to answer the evaluation questions in relation to the key soil type associated with this postcode so that we can cross reference your answers to national soil maps. We also need to be able to illustrate the locations across Australia from where data has been contributed to ensure the dataset is robust.", 
      id = "FAQ6", style = "display:none; padding: 10px; background-color: #f0f0f0;"),
    
    h4(actionLink("faq_button", "Do you use cookies?"), onclick = "toggleDetails('FAQ7');"),
    p(HTML("We use local storage, which is different from cookies, to enhance your experience with our application. Local storage allows us to save certain data directly in your browser. This feature is particularly useful for ensuring that you can complete the evaluation even if there are issues with internet connectivity.<br><br>
           Local storage is used to temporarily store your progress in the evaluation tool. This means if your connection is interrupted, or if you close and reopen your browser, you can pick up right where you left off. Unlike cookies, local storage does not involve sending data back to our servers, and it is solely managed by your browser. You do need to use the same device and browser in order to resume the evaluation tool. We prioritize your privacy and security, ensuring that no sensitive personal information is stored in this process."),
      id = "FAQ7", style = "display:none; padding: 10px; background-color: #f0f0f0;"),
    
    h4(actionLink("faq_button", "Can I share the evaluation link with other farmers?"), onclick = "toggleDetails('FAQ9');"),
    p("Yes, we would be delighted if you would like to share the link with other farmers in your area or further afield in Australia",
      id = "FAQ9", style = "display:none; padding: 10px; background-color: #f0f0f0;"),
    
    
    
  )}



page1_content <- function(id){
  start_time <- Sys.time()
  tagList(
    tags$a(name = "top"),
    tags$style(
      HTML("
      .leaflet-overlay-pane svg {
  -webkit-user-select: none; /* Chrome/Safari */
  -moz-user-select: none;    /* Firefox */
  -ms-user-select: none;     /* Internet Explorer/Edge */
  user-select: none;         /* Non-prefixed version, currently supported by modern browsers */
}
 
    ")
    ),
    
    h2("Tell us about yourself", style = "color: #e04b2f;"),
    mainPanel(
      progressBar(id = "progress", value = 1/12*100, display_pct = TRUE)
    ),
    mainPanel(
      p("After completing this 15-minute evaluation, you can access your personalised report to help you manage your soil security. The evaluation is entirely confidential. Please answer all the questions before moving on to ensure your contribution is represented in building a comprehensive national picture of Australian soil connectivity.")
    ),
    mainPanel(
      HTML('<center><img src="Soil connection report graphic.png" width="200"></center>', )
    ),
    radioButtons("Role_Q", "1. What is your role?", 
                 choices = c("Land Manager", "Landowner", "Other"),
                 selected = character(0)),
    
    conditionalPanel(
      condition = "input.Role_Q == 'Other'",
      textInput2("other_role", label = "Please specify your role (required):", maxlength = 150),
      textOutput('other_role_count')
    ),
    
    
    br(),
    
    checkboxGroupInput("Farm_Enterprises", "2. What enterprises do you run? (Multiple selection)", 
                       choices = c("Grain crops", 
                                   "Sugar", 
                                   "Cotton", 
                                   "Dairy",
                                   "Extensive sheep or cattle", 
                                   "Feedlot sheep or cattle", 
                                   "Free range pork or poultry", 
                                   "Horticultural field crops (this refers to potatoes, onions, carrots tomatoes, brassicas, peas, beans etc.)", 
                                   "Grapes", 
                                   "Orchard – fruit, nuts", 
                                   "Other")),
    conditionalPanel(
      condition = "Array.isArray(input.Farm_Enterprises) && input.Farm_Enterprises.includes('Other')",
      textInput2("other_Farm_Enterprises", "Please specify your enterprise(s) (required):",  maxlength = 255),
      textOutput('other_Farm_Enterprises_count')
    ),
    
    br(),
    
    checkboxGroupInput("Perceived_Threats", "3. Across your farm(s) do you experience any of these following soil threats? (Multiple selection)", 
                       choices = c("Erosion", "Acidification", "Structural decline (e.g. caused by soil compaction or sodicity)", 
                                   "Soil carbon loss", "Salinisation", "Habitat loss/degradation of soil biology", "Other")),
    conditionalPanel(
      condition = "Array.isArray(input.Perceived_Threats) && input.Perceived_Threats.includes('Other')",
      textInput2("other_Perceived_Threats", "Please specify your enterprise(s) (required):", maxlength = 150),
      textOutput('other_Perceived_Threats_count')
    ),
    
    br(),
    
    
    radioButtons("Number_soil_types", "4. Different soils require different management. How many different types of soils do you manage?", 
                 choices = c("1", "2-3", "4+"),
                 selected = character(0)),
    br(),
    
    
    tags$h4("Think of one particular soil type you manage. Please keep this particular soil type in mind when answering the remaining questions (which will be referred to as ‘key soil type’ throughout the evaluation)."),
    
    textInput2("Main_Soil_Type", "5. Describe the key soil type you will refer to in this evaluation", width = "100%", maxlength = 350),
    textOutput('Main_Soil_Type_count'),
    
    
    br(),
    
    p("Where is the key soil type"),
    
    numericInput("postal_code", "6. Enter a 4-digit Postal code:", value = NULL, min = 800, max = 9999),
    conditionalPanel(
      condition = "input.postal_code?.length >= 3 && !($.inArray(input.postal_code, valid_codes) > -1)",
      p("Please input valid Postal code")
    ),
    
    br(),
    
    
    conditionalPanel(
      condition = "input.postal_code != null",
      selectInput("town_select", "Select the closest town or city:", choices = NULL),
    ),
    
    br(),
    #conditionalPanel(
    #  condition = "($('#next_page1').is(':disabled'))",
    
    #  p("Please answer all questions. If `Other` is chosen, please specify. Please make sure you enter a valid postal code.", style = "color: red;")
    #),
    
    uiOutput("ans_missing1"),
    
    div(
      class = "d-flex justify-content-center",
      actionButton("next_page1", "Next page", icon = icon("arrow-right"), disable= TRUE),
      p("Next page loads a map and can take an extra second, all over pages load quickly.")  
    ),
    br(),
    br(),
  )
}






page2_content <- function(id){
  tagList(
    tags$a(name = "top"),
    h2("Location of my key soil type", style = "color: #e04b2f;"),
    mainPanel(
      progressBar(id = "progress", value = 2/12*100, display_pct = TRUE)
    ),
    mainPanel(
      p("This page loads a map and can take an extra second, all over pages load quickly."),  
      p("Zoom and ", HTML("<u><b><span style='font-size: 1.2em;'>click</span></b></u>"), " on the location of the key soil type."),
      p("This will be used to unravel the complex relationships between farmers, farming practices and soil security. Coordinates are encrypted and locations are aggregated at a regional scale for reporting."),
      p("You may choose to advance by clicking 'Next' without giving coordinates.")
    ),
    leafletOutput("map", width = "90%", height = "300px"),
    actionButton("clear_coordinates", "Clear Coordinates"), 
    br(),
    br(),
    
    div(
      style = "display: flex; justify-content: space-between;",
      actionButton("go_back_button", "Go Back", icon = icon("arrow-left")),
      actionButton("next_page2", "Next page", icon = icon("arrow-right")),
    ),
    br(),
    br(),
  )}




page3_content <- function(id){
  tagList(
    tags$a(name = "top"),
    tags$script(
      HTML('
       $(document).ready(function() {
    $(document).on("click", "input[type=checkbox][value=\'I am familiar with all these concepts.\']", function() {
      if ($(this).is(":checked")) {
        $("input[type=checkbox][value!=\'I am familiar with all these concepts.\']").prop("checked", false);
        $(this).trigger("change");
      }
    });
           
      $(document).on("click", "input[type=checkbox][value!=\'I am familiar with all these concepts.\']", function() {
  if ($(this).is(":checked")) {
    $("input[type=checkbox][value=\'I am familiar with all these concepts.\']").prop("checked", false);
      $(this).trigger("change");
  }
      });
});

           ')
    ),
    
    h2("Erosion", style = "color: #1760a2;"),
    mainPanel(
      progressBar(id = "progress", value = 3/12*100, display_pct = TRUE)
    ),
    mainPanel(
      br(),
      
      h4(HTML("<strong>Keeping in mind the key soil type you nominated:<strong>")),
      br(),
      
      
      ## Erosion Knowledge
      checkboxGroupInput("E_K", 
                         HTML(paste0("1. Select all the concepts that are ", 
                                     "<strong><em>NEW</em></strong> to you. (Select all that apply)")), 
                         choices = c("Practices that increase soil cover by more than 50% reduce the risk of soil erosion.",
                                     "Wind erosion can be reduced by planting windbreaks at 90 degrees to the prevailing wind.",
                                     "The severity of water erosion is impacted by position in the landscape and can occur below the surface.",
                                     "A shear test is used to measure soil strength, a soil with high shear is less prone to erosion.",
                                     "I am familiar with all these concepts."),
                         width = "100%"),    
      br(),
      ## Action
      radioButtons("E_Ac", "2. For your soils, which statement best describes your approach to soil erosion? (Choose only one)", 
                   choices = c("Soil erosion is not considered in my management practices.",
                               "Practices that maintain soil cover are used when possible.",
                               "Practices that maintain soil cover are a priority.",
                               "We combine information on soil chemical and physical characteristics with topography to identify and manage areas prone to soil erosion."),
                   width = "100%",
                   selected = character(0)),       
      br(),
      ## Attitude
      radioButtons("E_At", "3. Which statement best describes your opinion on managing soil erosion? (Choose only one)", 
                   choices = c("Farming practices do not need to be considered in relation to soil erosion.",
                               "I feel that the expense and complexity of erosion control makes it prohibitive.",
                               "I aim to use production and environmental practices that minimise erosion.",
                               "Our topsoil is our most valuable asset, and we must all work to minimise its loss."),
                   width = "100%",
                   selected = character(0)),
      
      br(),
      textInput2("Legislated_erosion", "4. Are you implementing any management practices to prevent soil erosion because it is legislated by your state or federal government?", 
                 width = "100%", maxlength = 350),
      textOutput('Legislated_erosion_count'),
      
      br(),
      
      uiOutput("ans_missing3"),
      div(
        style = "display: flex; justify-content: space-between;",
        actionButton("go_back_button2", "Go Back", icon = icon("arrow-left")),
        actionButton("next_page3", "Next page", icon = icon("arrow-right"), disabled = TRUE),
      ),
      br(),
      br(),
    )
  )
}






page4_content <- function(id){
  tagList(
    tags$a(name = "top"),
    tags$script(
      HTML('
             $(document).ready(function() {
    $(document).on("click", "input[type=checkbox][value=\'I am familiar with all these concepts.\']", function() {
      if ($(this).is(":checked")) {
        $("input[type=checkbox][value!=\'I am familiar with all these concepts.\']").prop("checked", false);
        $(this).trigger("change");
      }
    });
           
      $(document).on("click", "input[type=checkbox][value!=\'I am familiar with all these concepts.\']", function() {
  if ($(this).is(":checked")) {
    $("input[type=checkbox][value=\'I am familiar with all these concepts.\']").prop("checked", false);
      $(this).trigger("change");
  }
      });
});')
    ),
    h2("Acidification", style = "color: #e04b2f;"),
    mainPanel(
      progressBar(id = "progress", value = 4/12*100, display_pct = TRUE)
    ),
    mainPanel(
      br(),
      
      h4(HTML("<strong>Keeping in mind the key soil type you nominated:<strong>")),
      br(),
      
      ## Acidification Knowledge
      checkboxGroupInput("A_K",                          
                         HTML(paste0("1. Select all the concepts that are ", 
                                     "<strong><em>NEW</em></strong> to you. (Select all that apply)")), 
                         
                         choices = c("A soil with a pH less than 5.5 is considered acidic.",
                                     "Soil pH influences a plant's ability to access nutrients & may cause a deficiency or toxicity.",
                                     "Farm practices can cause soil acidification even in alkaline soils.",
                                     "The buffering capacity of a soil indicates the ability of the soil to resist pH change.",
                                     "I am familiar with all these concepts."),
                         width = "100%"),        
      br(),
      
      ## Action
      radioButtons("A_Ac", "2. For your soils, which statement best describes your approach to soil acidification? (Choose only one)", 
                   choices = c("I have not measured soil pH.",
                               "I have occasionally tested soil pH, but this knowledge rarely impacts my farm practices.",
                               "Soil pH is regularly monitored, and amendments are added if cost effective.",
                               "In my soil testing plan, I geolocate soil pH sampling points within zones and variably apply amendments where required."),
                   width = "100%",
                   selected = character(0)),        
      br(),
      
      ## Attitude
      radioButtons("A_At", "3. Which statement best describes your opinion on managing soil acidification (Choose only one)", 
                   choices = c("I do not need to know the pH of my soil.",
                               "Soil acidification is rarely considered when making management decisions.",
                               "Practices that minimise soil acidification are prioritised if cost effective.",
                               "Everyone should monitor the pH of their surface and subsurface soil and manage acidification."),
                   width = "100%",
                   selected = character(0)),
      br(),
      
      
      
      radioButtons("A_pH", "4. What is the most common pH of topsoil on your soils?", 
                   choices = c("pH less than 5.5",
                               "pH 5.6 to 7.5",
                               "Greater than pH 7.6",
                               "Do not know"),
                   width = "100%",
                   selected = character(0)),
      br(),
      uiOutput("ans_missing4"),
      div(
        style = "display: flex; justify-content: space-between;",
        actionButton("go_back_button", "Go Back", icon = icon("arrow-left")),
        actionButton("next_page4", "Next page", icon = icon("arrow-right"), disabled = TRUE),
      ),
      br(),
      br(),
    ))
}



page5_content <- function(id){
  tagList(
    tags$a(name = "top"),
    h2("Soil Structure Decline", style = "color: #1760a2;"),
    mainPanel(
      progressBar(id = "progress", value = 5/12*100, display_pct = TRUE)
    ),
    mainPanel(
      br(),
      
      h4(HTML("<strong>Keeping in mind the key soil type you nominated:<strong>")),
      br(),
      
      ## Structural Decline Knowledge
      checkboxGroupInput("SD_K", 
                         HTML(paste0("1. Select all the concepts that are ", 
                                     "<strong><em>NEW</em></strong> to you. (Select all that apply)")), 
                         choices = c("Poorly structured soil lacks pores to the hold air and water required for roots and soil organisms to flourish.",
                                     "When the exchangeable sodium percent (ESP) is greater than 6.0, a soil is considered sodic and soil structure declines.",
                                     "Soil structure is quickly assessed  using a slake test.",
                                     "Irrespective of tyre footprint, the weight and engine vibrations of large farm machinery compacts soil reducing soil functions.",
                                     "I am familiar with all these concepts."),
                         width = "100%"),        
      # JavaScript for SD_K checkboxes
      tags$script('
        $(document).ready(function() {
          // Code for handling "I am familiar with all these concepts." checkbox within its question
          $(document).on("click", "input[type=checkbox][name=\'SD_K\'][value=\'I am familiar with all these concepts.\']", function() {
            if ($(this).is(":checked")) {
              // Uncheck other checkboxes within the same group
              $("input[type=checkbox][name=\'SD_K\'][value!=\'I am familiar with all these concepts.\']").prop("checked", false);
            }
            $(this).trigger("change"); // Trigger the change event
          });

          // Code for handling other checkboxes within the same group
          $(document).on("click", "input[type=checkbox][name=\'SD_K\'][value!=\'I am familiar with all these concepts.\']", function() {
            if ($(this).is(":checked")) {
              // Uncheck the "I am familiar with all these concepts." checkbox within the same group
              $("input[type=checkbox][name=\'SD_K\'][value=\'I am familiar with all these concepts.\']").prop("checked", false);
            }
            $(this).trigger("change"); // Trigger the change event
          });
        });
      '),
      br(),
      ## Action
      radioButtons("SD_Ac", "2. For your soils, which statement best describes your approach to managing soil structure? (Choose only one)", 
                   choices = c("Changes in soil structure are not considered.",
                               "Poor and misshapen root growth is used as an indicator of subsurface compaction (hardpan).",
                               "Grazing and machinery practices are designed to minimise soil compaction.",
                               "I have assessed soil structure on my soil by digging soil pits or sending for soil testing to a laboratory."),
                   width = "100%",
                   selected = character(0)),        
      
      br(),
      ## Attitude
      radioButtons("SD_At", "3. Which statement best describes your opinion on managing soil structure (Choose only one)", 
                   choices = c("Changes in soil structure are not measured and recorded.",
                               "My management is about preventing soil compaction rather than improving soil structure.",
                               "Maintaining and improving soil structure is built into our long-term management approaches.",
                               "Soil structural decline has the greatest long-term impact on the viability of farming and its prevention should be central to all farmers management practices."),
                   width = "100%",
                   selected = character(0)),
      br(),
      
      
      
      checkboxGroupInput("SD_Val", "4. Select the practices you use or have used to improve soil structure? (Select all appropriate)",
                         choices = c("None",	"Additions of clay or sand",	"Drainage",	"Deep ripping",	"Minimal tillage",
                                     "Rotational grazing",	"Addition of gypsum",	"Appropriate stocking rates",		"Addition of organic matter",
                                     "Controlled traffic/ raise beds", "Other"),
                         width = "100%"),
      br(),
      conditionalPanel(
        condition = "Array.isArray(input.SD_Val) && input.SD_Val.includes('Other')",
        textInput2("other_SD_Val", "Please specify:", maxlength = 350),
        textOutput('other_SD_Val_count')
      ),
      tags$script('
       setTimeout(function() {
        $(document).ready(function() {
          // Code for handling "None" checkbox within its question
          $(document).on("click", "input[type=checkbox][name=\'SD_Val\'][value=\'None\']", function() {
            if ($(this).is(":checked")) {
              // Uncheck other checkboxes within the same group
              $("input[type=checkbox][name=\'SD_Val\'][value!=\'None\']").prop("checked", false);
            }
            $(this).trigger("change"); // Trigger the change event
          });

          // Code for handling other checkboxes within the same group
          $(document).on("click", "input[type=checkbox][name=\'SD_Val\'][value!=\'None\']", function() {
            if ($(this).is(":checked")) {
              // Uncheck the "None" checkbox within the same group
              $("input[type=checkbox][name=\'SD_Val\'][value=\'None\']").prop("checked", false);
            }
            $(this).trigger("change"); // Trigger the change event
          });
        });
       }, 1000); // Delay in milliseconds
      '),
      
      br(),
      uiOutput("ans_missing5"),
      div(
        style = "display: flex; justify-content: space-between;",
        actionButton("go_back_button", "Go Back", icon = icon("arrow-left")),
        actionButton("next_page5", "Next Page", icon = icon("arrow-right"), disabled = TRUE),
      ),
      br(),
      br(),
      
    ))
}




page6_content <- function(id){
  tagList(  
    tags$a(name = "top"),
    tags$script(
      HTML('
       $(document).ready(function() {
    $(document).on("click", "input[type=checkbox][value=\'I am familiar with all these concepts.\']", function() {
      if ($(this).is(":checked")) {
        $("input[type=checkbox][value!=\'I am familiar with all these concepts.\']").prop("checked", false);
        $(this).trigger("change");
      }
    });
           
      $(document).on("click", "input[type=checkbox][value!=\'I am familiar with all these concepts.\']", function() {
  if ($(this).is(":checked")) {
    $("input[type=checkbox][value=\'I am familiar with all these concepts.\']").prop("checked", false);
      $(this).trigger("change");
  }
      });
});')
    ),
    h2("Salinisation", style = "color: #e04b2f;"),
    mainPanel(
      progressBar(id = "progress", value = 6/12*100, display_pct = TRUE)
    ),
    mainPanel(
      br(),
      
      
      h4(HTML("<strong>Keeping in mind the key soil type you nominated:<strong>")),
      br(),
      
      ## Salinity Knowledge
      checkboxGroupInput("S_K", 
                         HTML(paste0("1. Select all the concepts that are ", 
                                     "<strong><em>NEW</em></strong> to you. (Select all that apply)")), 
                         choices = c("The accumulation of sodium, calcium, magnesium and/or potassium in the soil causes salinity.",
                                     "Applications of saline irrigation water or heavy rates of fertiliser, lime or gypsum can cause salinisation in the rootzone.",
                                     "Plant growth in saline soil is limited by reduced water absorption by roots and increased concentration of sodium chloride in the plant.",
                                     "Increasing soil salinity can negatively impact the accuracy of soil moisture sensors.",
                                     "I am familiar with all these concepts."),
                         width = "100%"),        
      
      br(),
      
      ## Action
      radioButtons("S_Ac", "2. For your soils, which statement best describes your approach to soil salinisation? (Choose only one)", 
                   choices = c("I  do not know if salinity is a problem on my farm.",
                               "I only look for salinity if it is a known problem in my area.",
                               "We grow a range of perennial crops/pastures especially in areas where salinity is a known problem.",
                               "We monitor salts in irrigation water and/or use technology including electromagnetic and biomass maps to locate sampling points to monitor salinisation."),
                   width = "100%",
                   selected = character(0)),        
      br(),
      
      ## Attitude
      radioButtons("S_At", "3. Which statement best describes your opinion on managing soil salinisation (Choose only one)", 
                   choices = c("I do not consider salinity as a production limiter.",
                               "I am concerned that my farming practices will cause the soil to become saline but do not know the solution.",
                               "As my production increases, I pay more attention to monitoring changes in soil salinity.",
                               "I engage with community wide programs to minimise and manage salinisation in our catchment."),
                   width = "100%",
                   selected = character(0)),
      br(),
      
      radioButtons("S_Val", "4. What type of salinity is a problem in your soil? (Choose only one)", 
                   choices = c("None",	"Dryland salinity",	"Saline irrigation water",	"Saline/ marine water ingress", "Salinity in the subsurface",
                               "I do not know"),
                   width = "100%",
                   selected = character(0)),
      br(),
      uiOutput("ans_missing7"),
      div(
        style = "display: flex; justify-content: space-between;",
        actionButton("go_back_button", "Go Back", icon = icon("arrow-left")),
        actionButton("next_page7", "Next Page", icon = icon("arrow-right"), disable=TRUE)
      ),
      br(),
      br(),
      
    ))
}




page7_content <- function(id){
  tagList(
    tags$a(name = "top"),
    tags$script(' 

      $(document).ready(function() {
    // Code for handling "I am familiar with all these concepts." checkbox within its question
    $(document).on("click", "input[type=checkbox][name=\'HL_K\'][value=\'I am familiar with all these concepts.\']", function() {
      if ($(this).is(":checked")) {
        // Uncheck other checkboxes within the same group
        $("input[type=checkbox][name=\'HL_K\'][value!=\'I am familiar with all these concepts.\']").prop("checked", false);
      }
      $(this).trigger("change"); // Trigger the change event
    });



    // Code for handling other checkboxes within the same group
    $(document).on("click", "input[type=checkbox][name=\'HL_K\'][value!=\'I am familiar with all these concepts.\']", function() {
      if ($(this).is(":checked")) {
        // Uncheck the "I am familiar with all these concepts." checkbox within the same group
        $("input[type=checkbox][name=\'HL_K\'][value=\'I am familiar with all these concepts.\']").prop("checked", false);
      }
      $(this).trigger("change"); // Trigger the change event
    })
    });

  '),
    
    h2("Soil Biodiversity", style = "color: #1760a2;"),
    mainPanel(
      progressBar(id = "progress", value = 7/12*100, display_pct = TRUE)
    ),
    mainPanel(
      br(),
      
      
      h4(HTML("<strong>Keeping in mind the key soil type you nominated:<strong>")),
      br(),
      
      ## Habitat Loss Knowledge
      checkboxGroupInput("HL_K", 
                         HTML(paste0("1. Select all the concepts that are ", 
                                     "<strong><em>NEW</em></strong> to you. (Select all that apply)")), 
                         choices = c("Soil biota plays an important role in  soil functions including structural improvement, organic matter turnover and pollutant degradation.",
                                     "More acidic soils favour fungal over bacterial diversity, and support less diverse microbial communities.",
                                     "Long-term experiments show that a combination of organic and inorganic nutrient inputs produces more sustainable crop yields than either kind alone.",
                                     "The symbiotic fungi mycorrhiza is a fundamental part of plant nutrition: as much as 80% of phosphorus and up to 20% of nitrogen can be transferred to plants by these fungi.",
                                     "I am familiar with all these concepts."),
                         width = "100%"),        
      
      br(),
      
      ## Action
      radioButtons("HL_Ac", "2. For your soils, which statement best describes your approach to maintaining soil biodiversity? (Choose only one)", 
                   choices = c("I only think about soil organisms in relation to root disease.",
                               "I would like to implement management practices that increases underground biodiversity but need more information.",
                               "I use practices to build organic matter in my soil to help improve soil health.",
                               "I work with other farmers to learn, test and implement best practice to promote healthy soil biota."),
                   width = "100%",
                   selected = character(0)),        
      
      br(),
      
      ## Attitude
      radioButtons("HL_At", "3. Which statement best describes your opinion on managing soil biodiversity? (Choose only one)", 
                   choices = c("There is no need to monitor or improve the biodiversity on my farm.",
                               "I am aware my practices may be detrimental to soil biota, but maximising productivity is my focus.",
                               "I work in sympathy with soil biota to reduce soil degradation and support productivity.",
                               "It is imperative that we continue to improve understanding and how to work synergistically with below ground biota."),
                   width = "100%",
                   selected = character(0)),
      br(),
      tags$script(' 
    setTimeout(function() {
      $(document).ready(function() {
      // Code for handling "I do not use any practices to support soil biodiversity" checkbox within its question (HL_Val)
        $(document).on("click", "input[type=checkbox][name=\'HL_Val\'][value=\'I do not use any practices to support soil biodiversity\']", function() {
            if ($(this).is(":checked")) {
                // Uncheck other checkboxes within the same group
                $("input[type=checkbox][name=\'HL_Val\'][value!=\'I do not use any practices to support soil biodiversity\']").prop("checked", false);
            }
             $(this).trigger("change"); // Trigger the change event
            
        });

        // Code for handling other checkboxes within the same group (HL_Val)
        $(document).on("click", "input[type=checkbox][name=\'HL_Val\'][value!=\'I do not use any practices to support soil biodiversity\']", function() {
            if ($(this).is(":checked")) {
                // Uncheck the "I do not use any practices to support soil biodiversity" checkbox within the same group
                $("input[type=checkbox][name=\'HL_Val\'][value=\'I do not use any practices to support soil biodiversity\']").prop("checked", false);
            }
             $(this).trigger("change"); // Trigger the change event
        });
    
    });
    }, 1000); // Delay in milliseconds
  '),
      
      br(),
      checkboxGroupInput("HL_Val", "4. Which practices do you use to maintain or increase underground biodiversity in your soils? (Select all appropriate).", 
                         choices = c("I do not use any practices to support soil biodiversity", 
                                     "Minimise soil applied pesticides", 
                                     "Additions of clay",
                                     "Additions of organic matter",
                                     "Maintain soil cover or/and retain stubble",
                                     "Inoculate pulses",	
                                     "Use crop and/or pasture rotations",	
                                     "Other"),
                         width = "100%"), 
      
      conditionalPanel(
        condition = "Array.isArray(input.HL_Val) && input.HL_Val.includes('Other')",
        textInput2("other_HL_Val", "Please specify:", maxlength = 350),
        textOutput('other_SD_Val_count')
      ),
      br(),
      
      tags$script(' 
    setTimeout(function() {
      $(document).ready(function() {
      // Code for handling "Other" checkbox within its question (HL_Val2)
        $(document).on("click", "input[type=checkbox][name=\'HL_Val2\'][value=\'I do not use any practices to support aboveground biodiversity\']", function() {
            if ($(this).is(":checked")) {
                // Uncheck other checkboxes within the same group
                $("input[type=checkbox][name=\'HL_Val2\'][value!=\'I do not use any practices to support aboveground biodiversity\']").prop("checked", false);
            }
             $(this).trigger("change"); // Trigger the change event
            
        });

        // Code for handling other checkboxes within the same group (HL_Val2)
        $(document).on("click", "input[type=checkbox][name=\'HL_Val2\'][value!=\'I do not use any practices to support aboveground biodiversity\']", function() {
            if ($(this).is(":checked")) {
                // Uncheck the "Other" checkbox within the same group
                $("input[type=checkbox][name=\'HL_Val2\'][value=\'I do not use any practices to support aboveground biodiversity\']").prop("checked", false);
            }
             $(this).trigger("change"); // Trigger the change event
        });
    
    });
    }, 1000); // Delay in milliseconds
  '),
      checkboxGroupInput("HL_Val2", "5. Which practices do you use to maintain or increase above-ground biodiversity in your soils? (Select all appropriate).", 
                         choices = c("I do not use any practices to support aboveground biodiversity", 
                                     "Grow cover crops",	
                                     "Plant native vegetation",	
                                     "Planting for pollinators", 	
                                     "Maintain existing native vegetation",	
                                     "Other"),
                         width = "100%"), 
      conditionalPanel(
        condition = "Array.isArray(input.HL_Val2) && input.HL_Val2.includes('Other')",
        textInput2("other_HL_Val2", "Please specify:", maxlength = 350),
        textOutput('other_HL_Val2_count')
      ),
      br(),
      
      
      
      uiOutput("ans_missing8"),
      
      div(
        style = "display: flex; justify-content: space-between;",
        actionButton("go_back_button", "Go Back", icon = icon("arrow-left")),
        actionButton("next_page8", "Next Page", icon = icon("arrow-right"), disabled = TRUE),
      ),
      br(),
      br(),
    ))
}



page8_content <- function(id){
  tagList(
    tags$a(name = "top"),
    tags$script('

    $(document).ready(function() {
        // Code for handling "I am familiar with all these concepts." checkbox within its question
        $(document).on("click", "input[type=checkbox][name=\'NM_K\'][value=\'I am familiar with all these concepts.\']", function() {
            if ($(this).is(":checked")) {
                // Uncheck other checkboxes within the same group
                $("input[type=checkbox][name=\'NM_K\'][value!=\'I am familiar with all these concepts.\']").prop("checked", false);
            }
             $(this).trigger("change"); // Trigger the change event
        });

        // Code for handling other checkboxes within the same group (NM_K)
        $(document).on("click", "input[type=checkbox][name=\'NM_K\'][value!=\'I am familiar with all these concepts.\']", function() {
            if ($(this).is(":checked")) {
                // Uncheck the "I am familiar with all these concepts." checkbox
                $("input[type=checkbox][name=\'NM_K\'][value=\'I am familiar with all these concepts.\']").prop("checked", false);
            }
             $(this).trigger("change"); // Trigger the change event
        });
    });

'),
    
    h2("Nutrient Management", style = "color: #e04b2f;"),
    mainPanel(
      progressBar(id = "progress", value = 8/12*100, display_pct = TRUE)
    ),
    mainPanel(
      br(),
      
      
      h4(HTML("<strong>Keeping in mind the key soil type you nominated:<strong>")),
      br(),
      
      ## Nutrition Management Knowledge
      checkboxGroupInput("NM_K", 
                         HTML(paste0("1. Select all the concepts that are ", 
                                     "<strong><em>NEW</em></strong> to you. (Select all that apply)")), 
                         choices = c("Nitrogen, phosphorus and potassium (N, P, K) are not the only macro nutrients required by plants.",
                                     "Factors that limit root growth, such as toxic layers or disease, reduce nutrient uptake.",
                                     "The critical range for nutrient concentrations varies with soil type.",
                                     "Nutrients applied to soil are rarely taken-up directly by plants but have to be cycled or transported by soil organisms.",
                                     "I am familiar with all these concepts."),
                         width = "100%"),        
      br(),
      
      ## Action
      radioButtons("NM_Ac", "2. For your soils, which statement best describes how you manage soil nutrients? (Choose only one)", 
                   choices = c("If nutrients are applied, rates are not modified by season.",
                               "Fertiliser is only applied based on agronomist or supplier recommendations.",
                               "Fertiliser rates and timings are modified by season, production objective and soil type.",
                               "The latest technologies are used to measure and apply fertiliser in order to minimise loss to the environment."),
                   width = "100%",
                   selected = character(0)),        
      br(),
      ## Attitude
      radioButtons("NM_At", "3. Which statement best describes your opinion on managing soil nutrients? (Choose only one)", 
                   choices = c("I do not apply fertiliser but rely on animal manure and/or leguminous plants.",
                               "Fertiliser is applied because it is the most important source of crop nutrients.",	
                               "I consider soil biology and moisture as important for crop nutrition as the addition of fertiliser or manure.",
                               "Our nutrient inputs are designed to feed the soil biota to support soil health and nutrient cycling to produce healthy crops and animals."),
                   width = "100%",
                   selected = character(0)),
      br(),
      tags$script('
    setTimeout(function() {
    $(document).ready(function() {

        // Code for handling "None" checkbox within its question (NM_Val)
        $(document).on("click", "input[type=checkbox][name=\'NM_Val\'][value=\'None\']", function() {
            if ($(this).is(":checked")) {
                // Uncheck other checkboxes within the same group
                $("input[type=checkbox][name=\'NM_Val\'][value!=\'None\']").prop("checked", false);
            }
             $(this).trigger("change"); // Trigger the change event
        });

        // Code for handling other checkboxes within the same group (NM_Val)
        $(document).on("click", "input[type=checkbox][name=\'NM_Val\'][value!=\'None\']", function() {
            if ($(this).is(":checked")) {
                // Uncheck the "None" checkbox
                $("input[type=checkbox][name=\'NM_Val\'][value=\'None\']").prop("checked", false);
            }
             $(this).trigger("change"); // Trigger the change event
        });
    });
    }, 1000); // Delay in milliseconds
'),
      
      checkboxGroupInput("NM_Val", "4. Which forms of nutrients do you use?", 
                         choices = c("Inorganic",
                                     "Organic",
                                     "Synthetic fertiliser",
                                     "None"),
                         width = "100%"), 
      br(),
      radioButtons("NM_Val2", "5. Which best describes your system?", 
                   choices = c("High input",
                               "Low input"),
                   width = "100%",
                   selected = character(0)), 
      br(),
      
      uiOutput("ans_missing9"),
      
      div(
        style = "display: flex; justify-content: space-between;",
        actionButton("go_back_button", "Go Back", icon = icon("arrow-left")),
        actionButton("next_page9", "Next Page", icon = icon("arrow-right"), disabled=TRUE)
      ),
      br(),
      br(),
      
    ))
}


page9_content <- function(id){
  tagList(
    tags$a(name = "top"),
    tags$script(
      '
         $(document).ready(function() {
    $(document).on("click", "input[type=checkbox][value=\'I am familiar with all these concepts.\']", function() {
      if ($(this).is(":checked")) {
        $("input[type=checkbox][value!=\'I am familiar with all these concepts.\']").prop("checked", false);
        $(this).trigger("change");
      }
      
    });
           
      $(document).on("click", "input[type=checkbox][value!=\'I am familiar with all these concepts.\']", function() {
  if ($(this).is(":checked")) {
    $("input[type=checkbox][value=\'I am familiar with all these concepts.\']").prop("checked", false);
      $(this).trigger("change");
  }
      })
});'
    ),
    h2("Soil Water Management", style = "color: #1760a2;"),
    mainPanel(
      progressBar(id = "progress", value = 9/12*100, display_pct = TRUE)
    ),
    mainPanel(
      br(),
      
      h4(HTML("<strong>Keeping in mind the key soil type you nominated:<strong>")),
      br(),
      
      ## Soil Water Knowledge
      checkboxGroupInput("SW_K", 
                         HTML(paste0("1. Select all the concepts that are ", 
                                     "<strong><em>NEW</em></strong> to you. (Maximum selection 4)")), 
                         choices = c("Plant available water plays a crucial role in determining potential  yield.",
                                     "Soil factors including texture, structure and subsoil constraints influence soil water storage and water uptake by roots.",
                                     "A saturated soil is more vulnerable to soil compaction and water erosion.",
                                     "Under multiple future climate models Australia is predicted to suffer loss of soil moisture between 6 and 15% depending on region between 2030 and 2039.",
                                     "I am familiar with all these concepts."),
                         width = "100%"),        
      br(),
      
      ## Action
      radioButtons("SW_Ac", "2. For your soils, which statement best describes how you manage soil water? (Choose only one)", 
                   choices = c("I do not try to manage my soil water.",
                               "I plan operations based long-term rainfall forecasts as well my knowledge of rain patterns.",
                               "I have soil moisture sensors and/or on-farm weather stations and plan operations based on my soil moisture and local rainfall forecast models.",
                               "Water is a limited resource, and my management plans aim to maximise water use efficiency and minimise run-off."),
                   width = "100%",
                   selected = character(0)),   
      br(),
      
      
      ## Attitude
      radioButtons("SW_At", "3. Which statement best describes your opinion on managing soil water? (Choose only one)", 
                   choices = c("I have no interest in the amount of water stored in my soil.",
                               "Implementing practices to manage water efficiency are difficult or too expensive.",	
                               "I feel confident implementing practices to manage water in order to improve water use efficiency for production.",
                               "Ensuring the long-term viability and water quality of my watershed is important and I want to use the latest technology to monitor and manage change."),
                   width = "100%",
                   selected = character(0)),
      br(),
      
      #Match the ABS: https://www.abs.gov.au/statistics/industry/agriculture/water-use-australian-farms/2020-21
      radioButtons("SW_Val", "4. Which is a greater problem to you? (Select one).", 
                   choices = c("Soil being too wet",	"Soil being too dry",	"Neither",	"Both", "Not sure"),
                   width = "100%",
                   selected = character(0)), 
      br(),
      
      radioButtons("SW_Val2", "5. Which system best describes your system? (Select one).", 
                   choices = c("Rainfed",	"Irrigated",	"Both"),
                   width = "100%",
                   selected = character(0)), 
      br(),
      
      uiOutput("ans_missing10"),
      div(
        style = "display: flex; justify-content: space-between;",
        actionButton("go_back_button", "Go Back", icon = icon("arrow-left")),
        actionButton("next_page10", "Next Page", icon = icon("arrow-right"), disabled= TRUE)
      ),
      br(),
      br(),
      
    ))
}


page10_content <- function(id){
  tagList(
    tags$a(name = "top"),
    tags$script(HTML("
  $(document).ready(function() {
    $('input[type=checkbox][name=\"DC_K\"][value=\"I am familiar with all these concepts.\"]').on('click', function() {
      var isChecked = $(this).is(':checked');
      $('input[type=checkbox][name=\"DC_K\"]').not(this).prop('checked', false).each(function() {
        if (isChecked !== $(this).is(':checked')) {
          $(this).trigger('change');
        }
      });
    });

    $('input[type=checkbox][name=\"DC_K\"]').not('[value=\"I am familiar with all these concepts.\"]').on('click', function() {
      if ($(this).is(':checked')) {
        var specialCheckbox = $('input[type=checkbox][name=\"DC_K\"][value=\"I am familiar with all these concepts.\"]');
        var wasChecked = specialCheckbox.is(':checked');
        specialCheckbox.prop('checked', false);
        if (wasChecked) {
          specialCheckbox.trigger('change');
        }
      }
    });
  });
")),
    
    h2("Soil Carbon", style = "color: #e04b2f;"),
    mainPanel(
      progressBar(id = "progress", value = 10/12*100, display_pct = TRUE)
    ),
    mainPanel(
      br(),
      
      h4(HTML("<strong>Keeping in mind the key soil type you nominated:<strong>")),
      br(),
      
      ## Decarbonisation Knowledge
      checkboxGroupInput("DC_K", 
                         HTML(paste0("1. Select all the concepts that are ", 
                                     "<strong><em>NEW</em></strong> to you. (Select all that apply)")), 
                         choices = c("Topography, drainage, and farm practices have an impact on the amount of carbon in soil at the landscape level.",
                                     "Carbon from decaying roots and fungi are more likely to be retained in soil organic matter than an equivalent mass of aboveground litter after one year.",
                                     "Low levels of nutrients such as nitrogen can lead to poor carbon storage.",
                                     "Usually, more carbon is found between 30 cm and 200 cm below the surface than that of the top 30 cm, with farming practices affecting these deeper levels over decades.",
                                     "I am familiar with all these concepts."),
                         width = "100%"),        
      br(),
      
      
      ## Action
      radioButtons("DC_Ac", "2. For your soils, which statement best describes your approach to monitoring soil carbon? (Choose only one)", 
                   choices = c("Managing soil carbon is not a priority.",
                               "Management practices are not designed particularly to improve soil carbon, but hopefully provide carbon benefits.",
                               "Practices that might improve soil carbon are included in management plans.",
                               "Measuring and improving soil carbon are high priorities."),
                   width = "100%",
                   selected = character(0)),        
      
      br(),
      
      ## Attitude
      radioButtons("DC_At", "3. Which statement best describes your opinion on managing soil carbon (Choose only one)", 
                   choices = c("I don't see the value or benefits of trying to change my soil carbon percentage.",
                               "Implementing practices to increase soil carbon are difficult or too expensive.",
                               "I feel confident implementing practices to increase soil carbon to improve production.",
                               "The long-term security of my soil and soil security in Australia is dependent on the carbon in it."),
                   width = "100%",
                   selected = character(0)),
      br(),
      
      tags$script(' 
    setTimeout(function() {
      $(document).ready(function() {
      // Code for handling "None" checkbox within its question (DC_Val)
        $(document).on("click", "input[type=checkbox][name=\'DC_Val\'][value=\'None\']", function() {
            if ($(this).is(":checked")) {
                // Uncheck other checkboxes within the same group
                $("input[type=checkbox][name=\'DC_Val\'][value!=\'None\']").prop("checked", false);
            }
             $(this).trigger("change"); // Trigger the change event
            
        });

        // Code for handling other checkboxes within the same group (DC_Val)
        $(document).on("click", "input[type=checkbox][name=\'DC_Val\'][value!=\'None\']", function() {
            if ($(this).is(":checked")) {
                // Uncheck the "None" checkbox within the same group
                $("input[type=checkbox][name=\'DC_Val\'][value=\'None\']").prop("checked", false);
            }
             $(this).trigger("change"); // Trigger the change event
        });
    
    });
    }, 1000); // Delay in milliseconds
  '),
      
      checkboxGroupInput("DC_Val", "4. Select the practices you use to maintain or increase carbon in your soil? (Select all appropriate).", 
                         choices = c("None","Stubble retention / rotational grazing", "Minimal tillage",	"Additions of organic manures/fertiliser",
                                     "Additions of clay", "Planting cover crops",	"Other"),
                         width = "100%"), 
      conditionalPanel(
        condition = "Array.isArray(input.DC_Val) && input.DC_Val.includes('Other')",
        textInput2("other_DC_Val", "Please specify:", maxlength = 350),
        textOutput('other_DC_Val_count')
      ),
      br(),
      radioButtons("DC_Val2", 
                   HTML("5. Which statement <strong>best</strong> describes your current attitude towards carbon credits market schemes?"), 
                   choices = c("I don’t see any value in carbon credits market schemes for my business.",
                               "I’m not clear on what carbon market options might be suitable for my business.", 
                               "I can see benefits in carbon markets, but my business is not well positioned to take advantage of these.",
                               "I can see benefits in carbon markets and plan to investigate the available opportunities.",
                               "I participate in carbon credit trading and find it beneficial.", 
                               "I participate in carbon credit trading but have not seen any benefits yet.",	
                               "I participate in carbon credit trading and it’s been a negative experience for me."),
                   width = "100%",
                   selected = character(0)),
      br(),
      br(),
      textInput2("Val_DC_comment", "Do you have anything to add?", maxlength = 500),
      textOutput('Val_DC_count'),
      br(),
      uiOutput("ans_missing6"),
      div(
        style = "display: flex; justify-content: space-between;",
        actionButton("go_back_button", "Go Back", icon = icon("arrow-left")),
        actionButton("next_page6", "Next Page", icon = icon("arrow-right"), disable=TRUE)
      ),
      
      br(),
      br(),
    ))
}



page11_content <- function(id){
  tagList(
    tags$a(name = "top"),
    h3("Rate the importance of these soil functions and threats to your farm business.", style = "color: #1760a2;"),
    p("Comments are optional."),
    mainPanel(
      progressBar(id = "progress", value = 11/12*100, display_pct = TRUE)
    ),
    mainPanel(
      
      radioButtons("Threat_Val_E", "Soil erosion", 
                   choices = c("Very Low", "Low", "Moderate", "High", "Very High", "I do not know"),
                   width = "100%",
                   inline = TRUE,
                   selected = character(0)),
      textInput2("Threat_Val_E_comment", "Comment:" , maxlength = 350),
      conditionalPanel(
        condition = "typeof input.Threat_Val_E_comment !== 'undefined' && input.Threat_Val_E_comment.length > 0",
        textOutput("Threat_Val_E_count")
      ),
      
      
      br(),
      radioButtons("Threat_Val_A", "Soil acidification", 
                   choices = c("Very Low", "Low", "Moderate", "High", "Very High", "I do not know"),
                   width = "100%",
                   inline = TRUE,
                   selected = character(0)),
      textInput2("Threat_Val_A_comment", "Comment:", maxlength = 350),
      conditionalPanel(
        condition = "typeof input.Threat_Val_A_comment !== 'undefined' && input.Threat_Val_A_comment.length > 0",
        textOutput("Threat_Val_A_count")
      ),
      
      
      br(),
      radioButtons("Threat_Val_SD", "Soil structural decline", 
                   choices = c("Very Low", "Low", "Moderate", "High", "Very High", "I do not know"),
                   width = "100%",
                   inline = TRUE,
                   selected = character(0)),
      textInput2("Threat_Val_SD_comment", "Comment:", maxlength = 350),
      conditionalPanel(
        condition = "typeof input.Threat_Val_SD_comment !== 'undefined' && input.Threat_Val_SD_comment.length > 0",
        textOutput("Threat_Val_SD_count")
      ),
      br(),
      radioButtons("Threat_Val_DC", "Soil decarbonisation", 
                   choices = c("Very Low", "Low", "Moderate", "High", "Very High", "I do not know"),
                   width = "100%",
                   inline = TRUE,
                   selected = character(0)),
      textInput2("Threat_Val_DC_comment", "Comment:", maxlength = 350),
      conditionalPanel(
        condition = "typeof input.Threat_Val_DC_comment !== 'undefined' && input.Threat_Val_DC_comment.length > 0",
        textOutput("Threat_Val_DC_count")
      ),
      br(),
      radioButtons("Threat_Val_S", "Soil salinisation", 
                   choices = c("Very Low", "Low", "Moderate", "High", "Very High", "I do not know"),
                   width = "100%",
                   inline = TRUE,
                   selected = character(0)),
      textInput2("Threat_Val_S_comment", "Comment:", maxlength = 350),
      conditionalPanel(
        condition = "typeof input.Threat_Val_S_comment !== 'undefined' && input.Threat_Val_S_comment.length > 0",
        textOutput("Threat_Val_S_count")
      ),
      br(),
      radioButtons("Threat_Val_HL", "Loss of soil biodiversity", 
                   choices = c("Very Low", "Low", "Moderate", "High", "Very High", "I do not know"),
                   width = "100%",
                   inline = TRUE,
                   selected = character(0)),
      textInput2("Threat_Val_HL_comment", "Comment:", maxlength = 350),
      conditionalPanel(
        condition = "typeof input.Threat_Val_HL_comment !== 'undefined' && input.Threat_Val_HL_comment.length > 0",
        textOutput("Threat_Val_HL_count")
      ),
      br(),
      uiOutput("ans_missing11"),
      div(
        style = "display: flex; justify-content: space-between;",
        actionButton("go_back_button", "Go Back", icon = icon("arrow-left")),
        actionButton("next_page11", "Next Page", icon = icon("arrow-right"), disabled= TRUE)
      ),
      br(),
      br(),
      
    ))
}


page12_content <- function(id){
  tagList(
    tags$a(name = "top"),
    h2("Last Questions!", style = "color: #e04b2f;"),
    
    mainPanel(
      progressBar(id = "progress", value = 12/12*100, display_pct = TRUE)
    ),
    mainPanel(
      
      
      #Demographic questions
      ##Agre ranges same as ABS with the option of 75+ which is not in ABS
      radioButtons("Age", "1. How old are you?",
                   choices = c(
                     "15-19",
                     "20-24",
                     "25-34",
                     "35-44",
                     "45-54",
                     "55-64",
                     "65-74",
                     "75+"),
                   width = "100%",
                   selected = character(0)),
      
      
      radioButtons("education_level", "2. What is the highest level of education you have completed?",
                   choices = c(
                     "Year 10 or below (No formal education qualification)",
                     "Year 12 or equivalent (High school completion)",
                     "Certificate II, III, or IV (Vocational qualifications)",
                     "Diploma or Advanced Diploma (Higher vocational qualifications)",
                     "Bachelor's Degree (e.g., Bachelor of Arts, Bachelor of Science)",
                     "Postgraduate Diploma or Certificate (e.g., Graduate Diploma, Graduate Certificate)",
                     "Master's Degree (e.g., Master of Arts, Master of Science)",
                     "Doctoral Degree (e.g., Ph.D., Doctor of Medicine)"),
                   width = "100%",
                   selected = character(0)),
      br(),
      #Should these be from here instead: https://dbr.abs.gov.au/region.html?lyr=ste&rgn=1
      #
      checkboxGroupInput("Land_type", "3. The soil you have been responding about, is it: (select as many as appropriate):",
                         choices = c("Freehold", "Pastoral perpetual lease", "Perpetual lease", "Other lease", "Nature conservation reserve",
                                     "Crown land including multiple use public forest", "Other"),
                         width = "100%",
                         selected = character(0)),
      conditionalPanel(
        condition = "Array.isArray(input.Land_type) && input.Land_type.includes('Other')",
        textInput2("other_land_type", "Please specify:", max=225),
        textOutput("other_land_type_count")
      ),
      
      br(),
      numericInput("Land_area", "4. How many hectares is your farm?", value=NULL, min = 1, max = 100000),
      br(),
      
      checkboxGroupInput("Word_Familiarity", "5. Click on the terms that are familiar to you (Choose all that apply; can be left blank):",
                         choices = c("Soil degradation", "Acidification", "Structural decline","Soil aggregates", "Soil compaction",
                                     "Decarbonisation","Carbon storage", "Salinisation", "Sodicity", "Ecosystem services",
                                     "Soil biodiversity", "Soil ecology", "4 R's of fertilizer management", 
                                     "Water use efficiency", "Eutrophication", "Fertigation"),
                         width = "100%",
                         selected = character(0)),  
      uiOutput("ans_missing12"),
      
      div(
        style = "display: flex; justify-content: space-between;",
        actionButton("go_back_button", "Go Back", icon = icon("arrow-left")),
        actionButton("next_page12", "Next Page", icon = icon("arrow-right"), disabled= TRUE)
      ),
      
      br(),
      br(),
    ))
}





Results_content <- function(id){
  tagList(
    tags$a(name = "top"),
    tags$style(HTML('.Headline-text { color: red; text-align: center; }')),
    
    mainPanel(
      
      h3("YOUR SOIL CONNECTIVITY REPORT"),
      h4("To keep your report, take a screenshot.", style = "color: #1760a2;"),
      div(
        textOutput("Headline"),
        style = "color: #e04b2f;font-size: 20px;",
        class= "title-center"
      ),
      br(),
      uiOutput("Strap"),
      br(),
      uiOutput("Stage_definitions"),
      br(),
      uiOutput("Support"),
      br(),
      tags$p(HTML('<a href="https://www.farminstitute.org.au/research/major-projects/soil-connectivity-resources/" target="_blank" style="text-decoration: underline; font-size: 1.2em;">Click here for more information</a>')),
      
      br(),
      br(),
      
      h3("Background to the evaluation", style = "color: #e04b2f;"),
      
      
      p("Soil is the foundation of farming; it is a precious resource. With good management soils can be sustained, rejuvenated and improved. Without care, the security of soil is threatened."),
      
      p(HTML("Research has identified five indicators against which soil security can be measured:<br>
        &emsp;1.	Productive capacity, <br>
        &emsp;2.	Physical, chemical and biological condition<br>
        &emsp;3.	Financial and cultural values<br>
        &emsp;4.	Regulation and policy<br>
        &emsp;5.	Human connectivity")),
      p(HTML("In this research, this evaluation quantifies your connection to your soil. This relates to your knowledge, actions and attitudes to six key threats to soil security.<br>
        &emsp;1.	soil erosion; <br>
        &emsp;2.	increasing salinity;<br>
        &emsp;3.	decreasing soil pH<br>
        &emsp;4.	degradation due to reduction of carbon content;<br>
        &emsp;5.	loss of soil structure; and<br>
        &emsp;6.	decline in the diversity of soil dwelling organisms.<br>
")),
      p("Our advanced analytics are able to differentiate between a farmer that is disinterested in their soil, and one who farms in an environment where there are few management options to minimise soil threats."),
      p("This is the first-time soil connectivity has tried to be quantified. "),
      p("Thank you for being part of this exciting new frontier in soil science."),
      p(HTML("<i>Richard Heath - AFI, </i>")),
      p(HTML("<i>Professor Alex McBratney - University of Sydney, </i>")),
      p(HTML("<i>Professor Damien Field - University of Sydney, </i>")),
      p(HTML("<i>Dr. Julio Pachon Maldonado - University of Sydney, </i>")),
      p(HTML("<i>Dr. Emma Leonard - AgriKnowHow </i>")),
      
      div(
        style = "display: flex; justify-content: space-between;",
        actionButton("resetLocalStorage", "Want to try a different soil? Start again"),
        actionButton("Close_app", "End", icon = icon("times"))
      ),
      
    ), 
  )
}








#UI#####

#setwd("R:/PRJ-SSCET/Connectivity/Evaluation Tool/Shiny/202307_Mock_EvalTool/")
ui <- fluidPage(
  #theme = shinytheme("flatly"),
  
  useShinyjs(),
  tags$link(href = "https://fonts.googleapis.com/css?family=Lato:400,700", rel = "stylesheet", type = "text/css"),
  tags$head(
    tags$link(rel = "shortcut icon", type = "image/png", href = "https://www.farminstitute.org.au/wp-content/uploads/2020/05/cropped-Favicon-100x100.png"),
    tags$div(id = "disconnectionModal", class = "modal",
             tags$div(class = "modal-content",
                      
                      tags$p("The connection to the server was lost. Please check your internet connection or try refreshing the page."),
                      actionButton("refreshButton", "Refresh")
             )
    ),
    
    tags$style(
      HTML("
                    body, .shiny-output-error, .shiny-output-error:before {
      font-family: 'Lato', sans-serif;
                    }
    
    
    .modal {
    justify-content: center; /* Center horizontally */
  align-items: center; /* Center vertically */

}
    
    
                    #refreshButton {
                      padding: 10px 20px;
                      font-size: 16px;
                      color: white;
                      background-color: #007bff; /* Bootstrap primary color */
                      border: none;
                      border-radius: 5px;
                      cursor: pointer;
                        justify-content: center; /* Center horizontally */
  align-items: center; /* Center vertically */
                    }
                    #refreshButton:hover {
                      background-color: #0056b3; /* Darker on hover */
                    }
 
                
        .logo-column img {
          width: 100%; /* Ensure the logo occupies the available space */
          height: auto; /* Maintain the aspect ratio */
          max-width: 350px; /* Set the maximum width for the logo */
          display: flex;
          justify-content: center;
          align-items: center;
          margin-left: auto;
           margin-right: auto;
        }
        
        
         .title-center {
        max-width: 800px;
        margin: 0 auto;
        text-align: center;
      }
        
        
        .main_body {
          display: flex;
          flex-direction: column;
          align-items: center;
          max-width: 800px;
          margin: 0 auto;
          padding: 20px;
        }

        

        .footer {
          color: #e04b2f;
        }


        .container {
          margin-left: 10px;
          margin-right: 10px;
        }
        
      ")
    ),
    
    
    
    tags$script(HTML("
  // Close modal functionality
  $('.close').on('click', function() {
    $('#disconnectionModal').hide();
  });

  $('#refreshButton').on('click', function(event) {
    event.preventDefault();
    $('#disconnectionModal').hide();
    location.reload(); // This will refresh the page
  });

  // Show the modal on disconnection
  $(document).on('shiny:disconnected', function(event) {
    $('#disconnectionModal').show();
    // Begin attempt to automatically reconnect
    attemptReconnect();
  });

  function clearLocalStorage() {
    // This function will clear local storage
    localStorage.clear();
    location.reload(); // This will refresh the page
  }

  function getFromLocalStorage(key) {
    return localStorage.getItem(key) || '';
  }

  // Define automatic reconnect function with exponential backoff
  function attemptReconnect(attempts = 0) {
    const maxAttempts = 8; // Limit the number of reconnect attempts
    const reconnectDelay = Math.min(Math.pow(2, attempts) * 1000, 32000); // Exponential backoff with a cap
    if (attempts < maxAttempts) {
      setTimeout(function() {
          location.reload(); // Attempt to reconnect using Shiny's built-in function
          attemptReconnect(attempts + 1); // Schedule next reconnect attempt
      }, reconnectDelay);
    }
  }

  // Custom message handlers for Shiny
  Shiny.addCustomMessageHandler('saveToLocalStore', function(message) {
    localStorage.setItem(message['key'], message['value']);
  });

  Shiny.addCustomMessageHandler('setSessionToken', function(message) {
    localStorage.setItem('sessionToken', message);
  });

  Shiny.addCustomMessageHandler('updateCurrentPage', function(message) {
    localStorage.setItem('currentPage', message);
  });

  // Connection event handlers for Shiny
  $(document).on('shiny:connected', function(event) {
    // Reset reconnect attempts upon successful connection
    window.reconnectAttempts = 0;

    // Send stored session token to server
    var sessionToken = getFromLocalStorage('sessionToken');
    if (sessionToken) {
      Shiny.onInputChange('restoredSessionToken', sessionToken);
    }

    // Restore survey page state
    var savedPage = getFromLocalStorage('currentPage');
    if (savedPage !== null) {
      Shiny.onInputChange('restoredPage', savedPage);
    }
  });
")),
    
    
    div(
      class = "logo-column center-vertically",
      shiny::img(src = "header.png", alt="University of Sydney and Australian Farm Instritute logos")
    ),
    
    titlePanel(
      windowTitle = "Soil Security Evaluation Tool",
      title = tagList(
        h1("Soil Security Evaluation Tool", class = "title-center", style = "color: #e04b2f;"),
        
        # Conditional offline mode message
        if (mode == "offline") {
          tags$div(
            "Using offline mode: This mode works based on a local SQLite database.",
            style = "color: red; font-weight: bold; text-align: center; margin-top: 10px;"
          )
        }
      )
    ),
    
    div(
      class = "main_body",
      uiOutput("survey"),
    ),
    
    br(),
    br(),
    
    uiOutput("Intro_no_response"),
    uiOutput("modalContent")
    
    
  ))








server <- function(input, output, session) {
  
  shinyjs::useShinyjs()
  options(shiny.sessionTimeout = 3600)
  code <- NULL
  # Generate session token
  session_token <- reactiveVal(paste0(session$token, "_", Sys.time()))
  
  
  currentPage <- reactiveVal(0)
  
  observeEvent(input$go_back_button, {
    jscode <- "$('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');"
    runjs(jscode)
    
    #browser()
    #print(paste("Go back button clicked. Current page before:", currentPage()))
    newPage <- currentPage() - 1
    currentPage(newPage)
    #session$sendCustomMessage("updateCurrentPage", newPage)
    #print(paste("New page after go back:", currentPage()))
  })
  
  observeEvent(input$go_back_button2, {
    jscode <- "$('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');"
    runjs(jscode)
    
    #browser()
    #print(paste("Go back button clicked. Current page before:", currentPage()))
    currentPage(1)
    #session$sendCustomMessage("updateCurrentPage", newPage)
    #print(paste("New page after go back:", currentPage()))
  })
  

  
  
  observeEvent(input$restoredSessionToken, {
    #print(paste("Received sessionToken:", input$restoredSessionToken))
    # Optionally update the server-side session token
    session_token(input$restoredSessionToken)
  })
  observeEvent(currentPage(), {
    session$sendCustomMessage("updateCurrentPage", currentPage())
    session$sendCustomMessage("setSessionToken", session_token())
  })

  
  
  
  submission_date <- reactiveVal(NULL)
  
  
  
  observeEvent(input$restoredPage, {
    # Debugging: Print the value of input$restoredPage
    #print(paste("restoredPage:", input$restoredPage))
    
    # Safely convert to numeric
    restoredPageNum <- suppressWarnings(as.numeric(input$restoredPage))
    
    # Check if restoredPageNum is a valid number
    if (!is.na(restoredPageNum)) {
      currentPage(restoredPageNum)
    } else {
      print("Invalid restoredPageNum value")
    }
  })
  
  
  
  # Dynamic UI content based on currentPage value
  # 
  output$survey <- renderUI({
    if (currentPage() == 0) {
      
      page0_content()
    } else if (currentPage() == 1) {
      page1_content() 
    } else if (currentPage() == 2) {
      page2_content()
    } else if (currentPage() == 3) {
      page3_content()
    }else if (currentPage() == 4) {
      page4_content()
    }else if (currentPage() == 5) {
      page5_content()
    }else if (currentPage() == 6) {
      page6_content()
    }else if (currentPage() == 7) {
      page7_content()
    }else if (currentPage() == 8) {
      page8_content()
    }else if (currentPage() == 9) {
      page9_content()
    }else if (currentPage() == 10) {
      page10_content()
    }else if (currentPage() == 11) {
      page11_content()
    }else if (currentPage() == 12) {
      page12_content()
    }else if (currentPage() == 13) {
      FAQ_content()
    }else if (currentPage() == 14) {
      Results_content()
    }
  })
  
  #Processes ####
  E_total_score_num <- reactiveVal(0)
  A_total_score_num <- reactiveVal(0)
  SD_total_score_num <- reactiveVal(0)
  S_total_score_num <- reactiveVal(0)
  HL_total_score_num <- reactiveVal(0)
  NM_total_score_num <- reactiveVal(0)
  SW_total_score_num <- reactiveVal(0)
  DC_total_score_num <- reactiveVal(0)
  
  
  
  observeEvent(input$resetLocalStorage, {
    shinyjs::runjs("clearLocalStorage()")
  })
  
  
  ##Page0####
  ##
  observeEvent(currentPage(), {
    # Check if the current page is 1
    if (currentPage() == 0) {
      
      
      observeEvent(input$show_faq_modal, {
        showModal(modalDialog(
          title = "Frequently Asked Questions",
          renderUI({ FAQ_content() })
        ))
      }, ignoreNULL = TRUE)
      
      
      
      
      observeEvent(input$Intro_no, {
        # Show the modal dialog with an additional actionButton
        showModal(modalDialog(
          title = "App Closing",
          "Thank you for your interest in participating. At this stage, we are specifically seeking responses from landowners and soil managers. We acknowledge that soil holds vital significance for all, and your keenness to contribute is greatly appreciated.
We anticipate broadening our outreach to encompass a wider cross-section of participants in the future. We value your support and look forward to the possibility of your involvement in our ongoing research.",
          footer = tagList(
            actionButton("close_and_redirect", "Close")
          ),
          cat("Session", session$token, " intro no..\n"),
          easyClose = FALSE
        ))
      })
      
      # Observe the new button in the modal dialog
      observeEvent(input$close_and_redirect, {
        
        runjs('window.location.href = "https://www.farminstitute.org.au/research/major-projects/soil-connectivity-resources/";')
      })
      
      
      
      # Next Page button click event
      observeEvent(input$next_page0, {
        jscode <- "$('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');"
        runjs(jscode)
        
        
        

        
        start_time <-  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        
        data <- data.frame(
          session_token = session_token(),
          start_time = start_time)
        
        saveData0(data, data)
        
        currentPage(1)
        
      })
      
    }})
  
  ##Page 1 ####
  ##
  observeEvent(currentPage(), {
    # Check if the current page is 1
    if (currentPage() == 1) {
      
      
      output$other_role_count <- reactive({ paste0('Only ', 150-nchar(input$other_role), ' characters remaining.' ) })
      output$other_Farm_Enterprises_count <- reactive({ paste0('Only ', 255-nchar(input$other_Farm_Enterprises), ' characters remaining.' ) })
      output$other_Perceived_Threats_count <- reactive({ paste0('Only ', 150-nchar(input$other_Perceived_Threats), ' characters remaining.' ) })
      output$Main_Soil_Type_count <- reactive({ paste0('Only ', 350-nchar(input$Main_Soil_Type), ' characters remaining.' ) })
      
      valid_codes <- unique(Strata_info$POA_CODE21)
      
      
      observeEvent(input$postal_code, {
        code <<- as.character(input$postal_code)
        
        session$sendCustomMessage("saveToLocalStore", list(key="postal_code", value=as.numeric(code)))
        
        
        if (is.null(code) || is.na(code)) {
          updateSelectInput(session, "town_select", choices = NULL)
          code2 <- NULL
          
          return()
        } else {
          
          code2 <- ifelse(nchar(code) < 4, paste0(strrep("0", 4 - nchar(code)), code), code)
          
          
          names <- Strata_info$SAL_NAME21[Strata_info$POA_CODE21 == code]
          updateSelectInput(session, "town_select", choices = names, selected=NULL)
          
          
        }
        
        
      })
      
      output$output_town_select <- renderPrint({
        selected_name <- input$town_select
        
        if (is.null(selected_name)) {
          "Enter a valid 4-digit Postal code and select a name"
        } else {
          paste("The selected name is:", selected_name)
        }
      })
      
      conditions_met_page1 <- reactiveVal(FALSE)
      
      observe({
        cond1 <- !is.null(input$Role_Q)
        cond2 <- (input$Role_Q != 'Other' || (input$Role_Q == 'Other' && input$other_role != ""))
        cond3 <- (length(input$Farm_Enterprises) != 0)
        cond4 <- !any(input$Farm_Enterprises %in% 'Other') || (any(input$Farm_Enterprises %in% 'Other') && input$other_Farm_Enterprises != "")
        cond5 <- !any(input$Perceived_Threats %in% 'Other') || (any(input$Perceived_Threats %in% 'Other') && input$other_Perceived_Threats != "")
        cond6 <- !is.null(input$Number_soil_types)
        cond7 <- input$Main_Soil_Type != ""
        cond8 <- !is.na(input$postal_code) && input$postal_code != "" && input$town_select != ""  
        
        #print(paste0("1", cond1," 2",cond2," 3",cond3," 4",cond4," 5",cond5," 6",cond6," 7",cond7, " 8",cond8))
        # Update the value of conditions_met_page1 using ()
        conditions_met_page1 <- cond1 && cond2 && cond3 && cond4 && cond5 && cond6 && cond7 && cond8
        
        
        output$ans_missing1 <- renderUI({
          messages <- list()
          
          if (!cond1 || !cond2) {
            messages <- c(messages, "Please answer the 1st question. If `Other`, please specify. <br>")
          }
          if (!cond3 || !cond4) {
            messages <- c(messages, "Please answer the 2nd question. If `Other`, please specify. <br>")
          } 
          if (!cond5) {
            messages <- c(messages, "Please answer the 3rd question. If `Other`, please specify.<br>")
          } 
          if (!cond6) {
            messages <- c(messages, "Please answer the 4th question. <br>")
          } 
          if (!cond7) {
            messages <- c(messages, "Please answer the 5th question. <br>")
          } 
          if (!cond8) {
            messages <- c(messages, "Please answer the 6th question. If no city or town shows up, check that the postal code is correct. <br>")
          } 
          
          if (length(messages) > 0) {
            HTML(paste("<span style='color: #e04b2f;'>", paste(messages, collapse = " "), "</span>"))
          }
          
        })
        
        
        
        shinyjs::toggleState("next_page1", conditions_met_page1)  # Use () to get the current value
      })
      
      
      
      
      observeEvent(input$next_page1, {
        jscode1 <- "
    // Add an overlay to the '#survey' div
    $('#survey').css({'position': 'relative'}).prepend('<div id=\"overlay\" style=\"position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: rgba(211, 211, 211, 0.5); z-index: 10;\"></div>');
    
    // Animate scroll
    $('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');
  "
        runjs(jscode1)
        
        # Delay the removal of the overlay by 1.5 seconds after the scroll animation
        jscode2 <- "
    setTimeout(function() {
      $('#overlay').remove();
    }, 1500); // 700 milliseconds = 1.5 seconds
  "
        # Schedule the execution of jscode2 after a short delay to ensure jscode1 has been initiated
        invalidateLater(200, session)
        observe({
          runjs(jscode2)
        })

        data <- data.frame(
          session_token = session_token(),
          Role_Q = Sanitize(input$Role_Q),
          other_role = Sanitize(input$other_role),
          Farm_Enterprises = Sanitize(paste(input$Farm_Enterprises, collapse = ";")),
          other_Farm_Enterprises = Sanitize(input$other_Farm_Enterprises),
          Perceived_Threats= Sanitize(paste(input$Perceived_Threats, collapse = ";")),
          other_Perceived_Threats= Sanitize(input$other_Perceived_Threats),
          Number_soil_types = Sanitize(input$Number_soil_types),
          Main_Soil_Type = Sanitize(Sanitize(input$Main_Soil_Type)),
          postal_code = input$postal_code,
          town_select = input$town_select
        )
        
        
        saveData1(data, data2=NULL)
        
        currentPage(2)
      })
      
      
      
    }})
  
  
  ##Page 2 ####
  
  observeEvent(currentPage(), {

    if (currentPage() == 2) {
      

      
        if(is.null(code)){
          
          data_return <- return_results_data(session_token(), dbtable)
          
          code <<- as.numeric(data_return$postal_code) 
       
        }
        else {
          code <<- as.numeric(code)
        }
        
        
 
      
      
      #print(paste0("updated", str(code)))
      
      
      
      output$map <- renderLeaflet({
        # Check if 'code' is not NULL and is a numeric value
        if (!is.null(code) && is.numeric(code)) {
          # Ensure the code has more than 2 characters and is a digit
          if (nchar(as.character(code)) > 2 && all(digitize(code))) {
            lat <- Strata_info$lat[Strata_info$POA_CODE21 == code][1]
            lng <- Strata_info$lon[Strata_info$POA_CODE21 == code][1]
            
            # Check if 'lat' and 'lng' are valid
            if (!is.null(lat) && !is.null(lng) && !is.na(lat) && !is.na(lng)) {
              leaflet() %>%
                setView(lng = lng, lat = lat, zoom = 12) %>%
                addProviderTiles("Esri.WorldStreetMap", group = "Street Map") %>%
                addProviderTiles("Esri.WorldImagery", group = "Satellite Image", options = providerTileOptions(opacity = 0.4)) %>% 
                
                addEasyButton(easyButton(
                  icon = "fa-crosshairs",
                  title = "Locate Me",
                  onClick = JS("function(btn, map){ map.locate({setView: true}); }")
                )) %>%
                addLayersControl(
                  overlayGroups = c("Street Map", "Satellite Image"),  # Reference the group names here
                  options = layersControlOptions(collapsed = FALSE)
                )
            }
          }
        }
      })
      
      
      observeEvent(input$map_click, {
        click <- input$map_click
        if (!is.null(click)) {
          lon <- click$lng
          lat <- click$lat
          clicked_loc(list(lon = lon, lat = lat))
        }
      })
      clicked_loc <- reactiveVal(NULL)
      observe({
        loc <- clicked_loc()
        if (!is.null(loc)) {
          lon <- loc$lon
          lat <- loc$lat
          label_text <- paste("Soil you will be refering to: ", round(lat,3), round(lon,3))
          leafletProxy("map") %>%
            clearMarkers() %>%
            addMarkers(lng = lon, lat = lat, label = label_text, labelOptions = labelOptions(noHide = TRUE))
        }
      })
      
      observeEvent(input$clear_coordinates, {
        clicked_loc(NULL)  # Reset the clicked coordinate to NULL
        leafletProxy("map") %>%
          clearMarkers()
      })
      
      
      
      
      observeEvent(input$next_page2, {
        jscode1 <- "
    // Add an overlay to the '#survey' div
    $('#survey').css({'position': 'relative'}).prepend('<div id=\"overlay\" style=\"position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: rgba(211, 211, 211, 0.5); z-index: 10;\"></div>');
    
    // Animate scroll
    $('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');
  "
        runjs(jscode1)
        
        # Delay the removal of the overlay by 1.5 seconds after the scroll animation
        jscode2 <- "
    setTimeout(function() {
      $('#overlay').remove();
    }, 1500); // 700 milliseconds = 1.5 seconds
  "
        # Schedule the execution of jscode2 after a short delay to ensure jscode1 has been initiated
        invalidateLater(200, session)
        observe({
          runjs(jscode2)
        })
        
        loc <- clicked_loc()
        
        if (!is.null(loc)){
          data <- data.frame(
            session_token = session_token(),
            lat = loc$lon + code,
            lon = loc$lat - code
          )
          
          saveData1(data)
        } else {
          data <- data.frame(
            session_token = session_token(),
            lat = FALSE,
            lon = FALSE
          )
          
          saveData1(data)
        }
        currentPage(3)
      })
      
      
    }})
  
  
  
  
  ## Page 3 ####
  
  E_K_score_num_db <- reactiveVal()
  E_Ac_score_num_db <- reactiveVal()
  E_At_score_num_db <- reactiveVal()
  E_total_score_num_db <- reactiveVal()
  
  
  
  observeEvent(currentPage(), {
    
    if (currentPage() == 3) {
      
      
      # print(paste0(E_K_score_num_db(), "  ",E_Ac_score_num_db(), "  ",E_At_score_num_db(), "  ", E_total_score_num_db()))
      
      
      output$Legislated_erosion_count <- reactive({
        req(input$Legislated_erosion) # This ensures that the code waits for the input to not be NULL
        paste0(350 - nchar(input$Legislated_erosion), ' characters remaining.')
      })
      ### Erosion Knowledge
      E_K_score <- observeEvent(input$E_K,  {
        if (is.null(input$E_K) || length(input$E_K) == 0) {
          return(10)
        }
        
        scores <- sapply(input$E_K, function(option) {
          switch(option,
                 "Practices that increase soil cover by more than 50% reduce the risk of soil erosion."=4,
                 "Wind erosion can be reduced by planting windbreaks at 90 degrees to the prevailing wind."=3,
                 "The severity of water erosion is impacted by position in the landscape and can occur below the surface."=2,
                 "A shear test is used to measure soil strength, a soil with high shear is less prone to erosion."=1,
                 "I am familiar with all these concepts."=0)
        })
        
        erosionK_Score <- 10 - sum(unlist(scores))
        
        E_K_score_num_db(erosionK_Score)
        #print(paste0("eros_k: ", erosionK_Score))
        return(erosionK_Score)
      }, ignoreNULL = TRUE)
      
      
      # print("E_K_score_num_db: ", E_K_score_num_db)
      
      
      ### Action
      
      E_Ac_score <- observeEvent(input$E_Ac, {
        if (is.null(input$E_Ac)) {
          return(0)
        }
        scores <- sapply(input$E_Ac, function(option) {
          switch(option,
                 "Soil erosion is not considered in my management practices." = 1,
                 "Practices that maintain soil cover are used when possible." = 2,
                 "Practices that maintain soil cover are a priority." = 3,
                 "We combine information on soil chemical and physical characteristics with topography to identify and manage areas prone to soil erosion." = 4)
        })
        
        erosionAc_Score <- sum(unlist(scores))
        E_Ac_score_num_db(erosionAc_Score)
        #print(paste0("eros_Ac: ", erosionAc_Score))
        return(erosionAc_Score) # # Calculate the sum of scores
      }, ignoreNULL = TRUE)
      
      
      
      #print("E_Ac_score_num_db: ", E_Ac_score_num_db)
      
      ### Attitude
      
      E_At_score <- observeEvent(input$E_At, {
        if (is.null(input$E_At)) {
          return(0)
        }
        scores <- sapply(input$E_At, function(option) {
          switch(option,
                 "Farming practices do not need to be considered in relation to soil erosion." = 1,
                 "I feel that the expense and complexity of erosion control makes it prohibitive." = 2,
                 "I aim to use production and environmental practices that minimise erosion." = 3,
                 "Our topsoil is our most valuable asset, and we must all work to minimise its loss."=4)
        })
        erosionAt_Score <- sum(unlist(scores))
        E_At_score_num_db(erosionAt_Score)
        
        #print(paste0("ErosionAt_Score: ", erosionAt_Score))
        return(erosionAt_Score) # # Calculate the sum of scores
        
      }, ignoreNULL = TRUE)
      
      
      observeEvent({
        req(!is.null(E_K_score_num_db()) && !is.null(E_Ac_score_num_db()) && !is.null(E_At_score_num_db()))
      }, {
        # Calculate erosionScore based on your logic
        erosionScore <- round(100 * ((2 * E_K_score_num_db() + 5 * E_Ac_score_num_db() + 5 * E_At_score_num_db()) / 60), 0)
        
        # Update E_total_score_num with the calculated erosionScore
        E_total_score_num_db(erosionScore)
      })
      
      #print("E_total_score_num_db: ", E_total_score_num_db)
      
      
      conditions_met_page3 <- reactiveVal(FALSE)
      
      observeEvent(list(input$E_K, input$E_Ac, input$E_At, input$Legislated_erosion), {
        # Define conditions
        familiarWithAllConcepts <- "I am familiar with all these concepts."
        selectedConcepts <- input$E_K
        
        # Condition 1: Check if any concept is selected or if the user is familiar with all concepts
        cond1 <- !is.null(selectedConcepts) && (familiarWithAllConcepts %in% selectedConcepts || length(selectedConcepts) > 0)
        cond2 <- !is.null(input$E_Ac)
        cond3 <- !is.null(input$E_At)
        cond4 <- nchar(input$Legislated_erosion) > 0
        
        
        # Combine all conditions
        conditions_met_page3 <- cond1 && cond2 && cond3 && cond4
        
        
        output$ans_missing3 <- renderUI({
          # List to hold messages for unmet conditions
          messages <- list()
          
          # Check each condition and add message if not met
          if (!cond1) {
            messages <- c(messages, "Please select at least one concept in the 1st question or select 'I am familiar with all these concepts.'. <br>")
          }
          if (!cond2) {
            messages <- c(messages, "Please answer 2nd question. <br>")
          }
          if (!cond3) {
            messages <- c(messages, "Please answer 3rd question. <br>")
          }
          if (!cond4) {
            messages <- c(messages, "Please answer 4th question. <br>")
          }
          
          
          if (length(messages) > 0) {
            HTML(paste("<span style='color: #e04b2f;'>", paste(messages, collapse = " "), "</span>"))
          }
          
        })
        
        
        # Update the state of the next button
        shinyjs::toggleState("next_page3", conditions_met_page3)
        
        
      }, ignoreNULL = TRUE)
      
      
      
    }})
  
  
  
  observeEvent(input$next_page3, {
    
    # JavaScript code to add overlay and animate scroll
    jscode1 <- "
    // Add an overlay to the '#survey' div
    $('#survey').css({'position': 'relative'}).prepend('<div id=\"overlay\" style=\"position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: rgba(211, 211, 211, 0.5); z-index: 10;\"></div>');
    
    // Animate scroll
    $('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');
  "
    runjs(jscode1)
    
    # Delay the removal of the overlay by 1.5 seconds after the scroll animation
    jscode2 <- "
    setTimeout(function() {
      $('#overlay').remove();
    }, 1500); // 700 milliseconds = 1.5 seconds
  "
    # Schedule the execution of jscode2 after a short delay to ensure jscode1 has been initiated
    invalidateLater(200, session)
    observe({
      runjs(jscode2)
    })
    
    # R code to handle data
    data <- data.frame(
      session_token = session_token(),
      E_K = Sanitize((paste(input$E_K, collapse = ";"))),
      E_Ac = (input$E_Ac),
      E_At = (input$E_At),
      Legislated_erosion = Sanitize(input$Legislated_erosion)
    )
    
    data2 <- data.frame(
      session_token = session_token(),
      E_K_score_num  = E_K_score_num_db(),
      E_Ac_score_num = E_Ac_score_num_db(),
      E_At_score_num = E_At_score_num_db(),
      E_total_score_num = E_total_score_num_db()
    )
    
    # Save data and update current page
    saveData1(data, data2)
    currentPage(4)
  })
  
  
  
  
  ## Page 4 ####
  
  A_K_score_num_db <- reactiveVal()
  A_Ac_score_num_db <- reactiveVal()
  A_At_score_num_db <- reactiveVal()
  A_total_score_num_db <- reactiveVal()
  
  
  observeEvent(currentPage(), {
    # Check if the current page is 1
    if (currentPage() == 4) {
      
      
      A_K_score <- observeEvent(input$A_K,  {
        if (is.null(input$A_K) || length(input$A_K) == 0) {
          return(10)
        }
        
        scores <- sapply(input$A_K, function(option) {
          switch(option,
                 "A soil with a pH less than 5.5 is considered acidic." =4,
                 "Soil pH influences a plant's ability to access nutrients & may cause a deficiency or toxicity."=3,
                 "Farm practices can cause soil acidification even in alkaline soils."=2,
                 "The buffering capacity of a soil indicates the ability of the soil to resist pH change."=1,
                 "I am familiar with all these concepts."  = 0)
        })
        A_K_Score <- 10 - sum(scores)
        
        A_K_score_num_db(A_K_Score)
        #print(paste0("A_K_Score: ", A_K_Score))
        return(A_K_Score)
      })
      
      ### Action
      
      A_Ac_score <- observeEvent({
        !is.null(input$A_Ac) && length(input$A_Ac) > 0 &&
          !is.null(input$A_pH) && length(input$A_pH) > 0
      }, {
        if (is.null(input$A_Ac)) {
          return(0)
        }
        
        scores <- sapply(input$A_Ac, function(option) {
          case2_score <- if ("Do not know" %in% input$A_pH || is.null(input$A_pH)) {
            0
          } else if (input$A_pH == "Greater than pH 7.6") {
            2
          } else {
            1
          }
          
          case3_score <- if ("Do not know" %in% input$A_pH || is.null(input$A_pH)) {
            0
          } else {
            3
          }
          
          case4_score <- if ("Do not know" %in% input$A_pH || is.null(input$A_pH)) {
            0
          } else {
            4
          }
          
          switch(option, "I have not measured soil pH." = 1,
                 "I have occasionally tested soil pH, but this knowledge rarely impacts my farm practices." = case2_score,
                 "Soil pH is regularly monitored, and amendments are added if cost effective." = case3_score,
                 "In my soil testing plan, I geolocate soil pH sampling points within zones and variably apply amendments where required." = case4_score)
        })
        
        A_Ac_Score <- sum(unlist(scores))
        A_Ac_score_num_db(A_Ac_Score)
        #print(paste0("A_Ac_Score: ", A_Ac_Score))
        return(A_Ac_Score) # # Calculate the sum of scores
      })
      
      
      
      ### Attitude
      
      observeEvent({
        req(!is.null(input$A_At) && length(input$A_At) > 0)
        req(!is.null(input$A_pH) && length(input$A_pH) > 0)
      }, {
        scores <- sapply(input$A_At, function(option) {
          case1_score <- if (input$A_pH == "Greater than pH 7.6") {
            1
          } else {
            0
          }
          
          case2_score <- if (input$A_pH == "pH 5.6 to 7.5") {
            1
          } else if (input$A_pH == "Greater than pH 7.6") {
            2
          } else {
            0
          }
          
          case3_score <- if (input$A_pH == "pH less than 5.5") {
            2
          } else if (input$A_pH == "Do not know") {
            0
          } else {
            3
          }
          
          case4_score <- if (input$A_pH == "Do not know") {
            0
          } else {
            4
          }
          
          switch(
            option,
            "I do not need to know the pH of my soil." = case1_score,
            "Soil acidification is rarely considered when making management decisions." = case2_score,
            "Practices that minimise soil acidification are prioritised if cost effective." = case3_score,
            "Everyone should monitor the pH of their surface and subsurface soil and manage acidification." = case4_score
          )
        })
        
        A_At_Score <- sum(unlist(scores))
        A_At_score_num_db(A_At_Score)
        
        #print(paste0("A_At_Score: ", A_At_Score))
      })
      
      
      
      observeEvent({
        req(!is.null(A_K_score_num_db()) && !is.null(A_Ac_score_num_db()) && !is.null(A_At_score_num_db()))
      }, {
        # Calculate erosionScore based on your logic
        A_Score <- round(100 * ((2 * A_K_score_num_db() + 5 * A_Ac_score_num_db() + 5 * A_At_score_num_db()) / 60), 0)
        
        # Update A_total_score_num with the calculated erosionScore
        A_total_score_num_db(A_Score)
        #print(paste0("A_total_Score: ", A_Score))
      })
      
      conditions_met_page4 <- reactiveVal(FALSE)
      
      observe({
        # Define conditions
        familiarWithAllConcepts <- "I am familiar with all these concepts."
        selectedConcepts <- input$A_K
        
        # Condition 1: Check if any concept is selected or if the user is familiar with all concepts
        cond1 <- !is.null(selectedConcepts) && (familiarWithAllConcepts %in% selectedConcepts || length(selectedConcepts) > 0)
        cond2 <- !is.null(input$A_Ac)
        cond3 <- !is.null(input$A_At)
        cond4 <- !is.null(input$A_pH)
        
        
        # Combine all conditions
        conditions_met_page4 <- cond1 && cond2 && cond3 && cond4
        
        
        output$ans_missing4 <- renderUI({
          # List to hold messages for unmet conditions
          messages <- list()
          
          # Check each condition and add message if not met
          if (!cond1) {
            messages <- c(messages, "Please select at least one concept in the 1st question or select 'I am familiar with all these concepts.'. <br>")
          }
          if (!cond2) {
            messages <- c(messages, "Please answer 2nd question. <br>")
          }
          if (!cond3) {
            messages <- c(messages, "Please answer 3rd question. <br>")
          }
          if (!cond4) {
            messages <- c(messages, "Please answer 4th question. <br>")
          }
          
          
          if (length(messages) > 0) {
            HTML(paste("<span style='color: #e04b2f;'>", paste(messages, collapse = " "), "</span>"))
          }
          
        })
        
        
        # Update the state of the next button
        shinyjs::toggleState("next_page4", conditions_met_page4)
        
      })
      
      
    }})
  
  
  observeEvent(input$next_page4, {
    #print(paste0(A_K_score_num_db(), "  ",A_Ac_score_num_db(), "  ",A_At_score_num_db(), "  ", A_total_score_num_db()))
    
    # JavaScript code to add overlay and animate scroll
    jscode1 <- "
    // Add an overlay to the '#survey' div
    $('#survey').css({'position': 'relative'}).prepend('<div id=\"overlay\" style=\"position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: rgba(211, 211, 211, 0.5); z-index: 10;\"></div>');
    
    // Animate scroll
    $('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');
  "
    runjs(jscode1)
    
    # Delay the removal of the overlay by 1.5 seconds after the scroll animation
    jscode2 <- "
    setTimeout(function() {
      $('#overlay').remove();
    }, 1500); // 700 milliseconds = 1.5 seconds
  "
    # Schedule the execution of jscode2 after a short delay to ensure jscode1 has been initiated
    invalidateLater(200, session)
    observe({
      runjs(jscode2)
    })
    
    data <- data.frame(
      session_token = session_token(),
      A_K = Sanitize(paste(input$A_K, collapse = ";")),
      A_Ac = Sanitize(input$A_Ac),
      A_At = Sanitize(input$A_At),
      A_pH = Sanitize(input$A_pH)
    )
    
    data2 <- data.frame(
      session_token = session_token(),
      A_K_score_num  = A_K_score_num_db(),
      A_Ac_score_num = A_Ac_score_num_db(),
      A_At_score_num = A_At_score_num_db(),
      A_total_score_num = A_total_score_num_db()
    )
    #print(data2)
    saveData1(data, data2)
    
    currentPage(5)
  })
  
  
  
  ## Page 5 ####
  
  
  SD_K_score_num_db <- reactiveVal()
  SD_Ac_score_num_db <- reactiveVal()
  SD_At_score_num_db <- reactiveVal()
  SD_total_score_num_db <- reactiveVal()
  
  
  observeEvent(currentPage(), {
    # Check if the current page is 1
    if (currentPage() == 5) {
      output$other_SD_Val_count <- reactive({ paste0( 350-nchar(input$other_SD_Val), ' characters remaining.' ) })
      
      
      
      SD_K_score <- observeEvent(input$SD_K,  {
        
        if (is.null(input$SD_K) || length(input$SD_K) == 0) {
          return(10)
        }
        
        scores <- sapply(input$SD_K, function(option) {
          switch(option,
                 "Poorly structured soil lacks pores to the hold air and water required for roots and soil organisms to flourish."=4,
                 "When the exchangeable sodium percent (ESP) is greater than 6.0, a soil is considered sodic and soil structure declines."=3,
                 "Soil structure is quickly assessed  using a slake test."=2,
                 "Irrespective of tyre footprint, the weight and engine vibrations of large farm machinery compacts soil reducing soil functions."=1,
                 "I am familiar with all these concepts."  = 0)
          
        })
        
        
        SD_K_Score <- 10 - sum(scores)
        
        SD_K_score_num_db(SD_K_Score)
        #print(paste0("SD_K_Score: ", SD_K_Score))
        return(SD_K_Score)
        
      })
      
      
      
      
      #Action
      
      SD_Ac_score <- observeEvent({
        !is.null(input$SD_Ac) && length(input$SD_Ac) > 0 &&
          !is.null(input$SD_Val) && length(input$SD_Val) > 0
      }, {
        
        if (is.null(input$SD_Ac)) {
          return(0)
        }
        
        scores <- sapply(input$SD_Ac, function(option) {
          case1_score <- if (any(input$SD_Val %in% "None") || is.null(input$SD_Val)) {
            1
          } else {
            0
          }
          
          case2_score <- if ("None" %in% input$SD_Val || (length(input$SD_Val) == 1 && input$SD_Val == "Other") || is.null(input$SD_Val)) {
            1
          } else if (length(input$SD_Val) > 1 && "Other" %in% input$SD_Val) {
            2
          } else {
            2
          }
          
          case3_score <- if (any(input$SD_Val %in% "None") || is.null(input$SD_Val)) {
            0
          } else if (!any(c("Rotational grazing", "Controlled traffic/ raise beds") %in% input$SD_Val)) {
            1
          } else {
            3
          }
          
          
          
          switch(option, "Changes in soil structure are not considered."=case1_score,
                 "Poor and misshapen root growth is used as an indicator of subsurface compaction (hardpan)."=case2_score,
                 "Grazing and machinery practices are designed to minimise soil compaction."=case3_score,
                 "I have assessed soil structure on my soil by digging soil pits or sending for soil testing to a laboratory."=4)
        })
        
        SD_Ac_Score <- sum(unlist(scores))
        SD_Ac_score_num_db(SD_Ac_Score)
        #print(paste0("SD_Ac_Score: ", SD_Ac_Score))
        return(SD_Ac_Score) # # Calculate the sum of scores
        
      })
      
      
      
      
      #Attitude
      
      SD_At_score <- observeEvent({
        !is.null(input$SD_At) && length(input$SD_At) > 0 &&
          !is.null(input$SD_Val) && length(input$SD_Val) > 0
      }, {
        if (is.null(input$SD_At)) {
          return(0)
        }
        
        
        scores <- sapply(input$SD_At, function(option) {
          
          case1_score <- if (any(c("None", "Rotational grazing", "Appropriate stocking rates",
                                   "Controlled traffic/ raise beds", "Other") %in% input$SD_Val)  || is.null(input$SD_Val)) {
            1
          } else {
            0
          }
          
          
          case2_score <- if (is.null(input$SD_Val)){
            0
          } else if (any(c("Rotational grazing","Minimal tillage","Appropriate stocking rates","Controlled traffic/ raise beds") %in% input$SD_Val)) {
            2
          } else if (any(c("Additions of clay or sand","Other") %in% input$SD_Val)){
            1
          } else {
            0
          }
          
          
          case3a_score <- if (any("None" %in% input$SD_Val) || is.null(input$SD_Val)){
            0
          }
          
          case3b_score <-  if (length(input$SD_Val)==1 && any("Other" %in% input$SD_Val)){
            2
          }
          
          case3c_score <- if (any(c("Additions of clay or sand",	"Drainage",	"Deep ripping",	"Minimal tillage",
                                    "Rotational grazing",	"Addition of gypsum",	"Appropriate stocking rates",		"Addition of organic matter",
                                    "Controlled traffic/ raise beds") %in% input$SD_Val)){
            3
          }
          
          case3_score <- max(case3a_score,case3b_score,case3c_score)
          
          
          
          case4a_score <- if (any("None" %in% input$SD_Val) || is.null(input$SD_Val)){
            0
          }
          
          case4b_score <-  if (length(input$SD_Val)==1 && any("Other" %in% input$SD_Val)){
            3
          }
          
          case4c_score <- if (any(c("Additions of clay or sand",	"Drainage",	"Deep ripping",	"Minimal tillage",
                                    "Rotational grazing",	"Addition of gypsum",	"Appropriate stocking rates",		"Addition of organic matter",
                                    "Controlled traffic/ raise beds")%in% input$SD_Val)){
            4
          }
          
          case4_score <- max(case4a_score,case4b_score,case4c_score)
          
          
          
          switch(option, "Changes in soil structure are not measured and recorded."=case1_score,
                 "My management is about preventing soil compaction rather than improving soil structure."=case2_score,
                 "Maintaining and improving soil structure is built into our long-term management approaches."=case3_score,
                 "Soil structural decline has the greatest long-term impact on the viability of farming and its prevention should be central to all farmers management practices."=case4_score)
        })
        
        
        SD_At_Score <- sum(unlist(scores))
        SD_At_score_num_db(SD_At_Score)
        #print(paste0("SD_At_Score: ", SD_At_Score))
        return(SD_At_Score) # # Calculate the sum of scores
        
        
      })
      
      
      observeEvent({
        req(!is.null(SD_K_score_num_db()) && !is.null(SD_Ac_score_num_db()) && !is.null(SD_At_score_num_db()))
        
      }, {
        # Calculate erosionScore based on your logic
        SD_total_score <- round(100 * ((2 * SD_K_score_num_db() + 5 * SD_Ac_score_num_db() + 5 * SD_At_score_num_db()) / 60), 0)
        
        # Update SD_total_score_num with the calculated erosionScore
        SD_total_score_num_db(SD_total_score)
        
        #print(paste0("SD_total_score: ", SD_total_score))
      })
      
      conditions_met_page5 <- reactiveVal(FALSE)
      
      observe({
        # Define conditions
        familiarWithAllConcepts <- "I am familiar with all these concepts."
        selectedConcepts <- input$SD_K
        
        # Condition 1: Check if any concept is selected or if the user is familiar with all concepts
        cond1 <- !is.null(selectedConcepts) && (familiarWithAllConcepts %in% selectedConcepts || length(selectedConcepts) > 0)
        cond2 <- !is.null(input$SD_Ac)
        cond3 <- !is.null(input$SD_At)
        cond4 <- (length(input$SD_Val) != 0 && !('Other' %in% input$SD_Val)) || 
          (('Other' %in% input$SD_Val) && nchar(input$other_SD_Val) > 0)
        
        
        # Combine all conditions
        conditions_met_page5 <- cond1 && cond2 && cond3 && cond4 
        
        
        output$ans_missing5 <- renderUI({
          # List to hold messages for unmet conditions
          messages <- list()
          
          # Check each condition and add message if not met
          if (!cond1) {
            messages <- c(messages, "Please select at least one concept in the 1st question or select 'I am familiar with all these concepts'. <br>")
          }
          if (!cond2) {
            messages <- c(messages, "Please answer 2nd question. <br>")
          }
          if (!cond3) {
            messages <- c(messages, "Please answer 3rd question. <br>")
          }
          if (!cond4) {
            messages <- c(messages, "Please answer 4th question. If `Other` selected, please specify. <br>")
          }
          
          
          if (length(messages) > 0) {
            HTML(paste("<span style='color: #e04b2f;'>", paste(messages, collapse = " "), "</span>"))
          }
          
        })
        
        
        # Update the state of the next button
        shinyjs::toggleState("next_page5", conditions_met_page5)
        
        
      })
      
      
    }})
  
  
  observeEvent(input$next_page5, {
    #print(paste0(SD_K_score_num_db(), "  ",SD_Ac_score_num_db(), "  ",SD_At_score_num_db(), "  ", SD_total_score_num_db()))
    # JavaScript code to add overlay and animate scroll
    jscode1 <- "
    // Add an overlay to the '#survey' div
    $('#survey').css({'position': 'relative'}).prepend('<div id=\"overlay\" style=\"position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: rgba(211, 211, 211, 0.5); z-index: 10;\"></div>');
    
    // Animate scroll
    $('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');
  "
    runjs(jscode1)
    
    # Delay the removal of the overlay by 1.5 seconds after the scroll animation
    jscode2 <- "
    setTimeout(function() {
      $('#overlay').remove();
    }, 1500); // 700 milliseconds = 1.5 seconds
  "
    # Schedule the execution of jscode2 after a short delay to ensure jscode1 has been initiated
    invalidateLater(200, session)
    observe({
      runjs(jscode2)
    })
    
    data <- data.frame(
      session_token = session_token(),
      SD_K = Sanitize(paste(input$SD_K, collapse = ";")),
      SD_Ac = Sanitize(input$SD_Ac),
      SD_At = Sanitize(input$SD_At),
      SD_Val = Sanitize(paste(input$SD_Val, collapse = ";")),
      other_SD_Val = Sanitize(input$other_SD_Val)
    )
    
    
    data2 <- data.frame(
      session_token = session_token(),
      SD_K_score_num  = SD_K_score_num_db(),
      SD_Ac_score_num = SD_Ac_score_num_db(),
      SD_At_score_num = SD_At_score_num_db(),
      SD_total_score_num = SD_total_score_num_db()
    )
    #print(data2)
    
    saveData1(data, data2)
    
    
    saveData1(data, data2)
    currentPage(6)
  })
  
  
  
  
  
  ## Page 6 ####
  
  S_K_score_num_db <- reactiveVal()
  S_Ac_score_num_db <- reactiveVal()
  S_At_score_num_db <- reactiveVal()
  S_total_score_num_db <- reactiveVal()
  
  observeEvent(currentPage(), {
    # Check if the current page is 1
    if (currentPage() == 6) {
      
      S_K_score <- observeEvent(input$S_K,  {
        if (is.null(input$S_K) || length(input$S_K) == 0) {
          return(10)
        }
        
        scores <- sapply(input$S_K, function(option) {
          switch(option,
                 "The accumulation of sodium, calcium, magnesium and/or potassium in the soil causes salinity."=4,
                 "Applications of saline irrigation water or heavy rates of fertiliser, lime or gypsum can cause salinisation in the rootzone."=3,
                 "Plant growth in saline soil is limited by reduced water absorption by roots and increased concentration of sodium chloride in the plant."=2,
                 "Increasing soil salinity can negatively impact the accuracy of soil moisture sensors."=1,
                 "I am familiar with all these concepts."=0)
          
        })
        
        S_K_Score <- 10 - sum(scores)
        
        
        S_K_score_num_db(S_K_Score)
        #print(paste0("S_K_Score: ", S_K_Score))
        return(S_K_Score)
      })
      
      #Action
      
      S_Ac_score <- observeEvent({
        !is.null(input$S_Ac) && length(input$S_Ac) > 0 &&
          !is.null(input$S_Val) && length(input$S_Val) > 0
      },{
        if (is.null(input$S_Ac)) {
          return(0)
        }
        
        scores <- sapply(input$S_Ac, function(option) {
          case1_score <- if (any(input$S_Val %in% c("None", "I do not know")) || is.null(input$S_Val)) {
            1
          } else {
            0
          }
          
          case2_score <- 2
          
          case3_score <- if (any(c("I do not know") %in% input$S_Val ) || is.null(input$S_Val)) {
            0
          } else {
            3
          }
          
          case4_score <- if (any(c("I do not know") %in% input$S_Val ) || is.null(input$S_Val)) {
            0
          } else {
            4
          }
          
          switch(option, "I  do not know if salinity is a problem on my farm."=case1_score,
                 "I only look for salinity if it is a known problem in my area."=case2_score,
                 "We grow a range of perennial crops/pastures especially in areas where salinity is a known problem."=case3_score,
                 "We monitor salts in irrigation water and/or use technology including electromagnetic and biomass maps to locate sampling points to monitor salinisation."=case4_score)
        })
        
        S_Ac_Score <- sum(unlist(scores))
        S_Ac_score_num_db(S_Ac_Score)
        #print(paste0("S_Ac_Score: ", S_Ac_Score))
        return(S_Ac_Score) # # Calculate the sum of scores
        
      })
      
      
      
      #Attitude
      S_At_score <- observeEvent({
        !is.null(input$S_At) && length(input$S_At) > 0 &&
          !is.null(input$S_Val) && length(input$S_Val) > 0
      },{
        if (is.null(input$S_At)) {
          return(0)
        }
        
        
        scores <- sapply(input$S_At, function(option) {
          
          case1_score <- if (any(input$S_Val %in% c("None", "I do not know")) || is.null(input$S_Val)) {
            1
          } else {
            0
          }
          
          case3_score <- if (any(input$S_Val %in% "I do not know") || is.null(input$S_Val)) {
            0
          } else {
            3
          }
          
          case4_score <- if (any(input$S_Val %in% "I do not know") || is.null(input$S_Val)) {
            0
          } else {
            4
          }
          
          
          switch(option, "I do not consider salinity as a production limiter."=case1_score,
                 "I am concerned that my farming practices will cause the soil to become saline but do not know the solution."=2,
                 "As my production increases, I pay more attention to monitoring changes in soil salinity."=case3_score,
                 "I engage with community wide programs to minimise and manage salinisation in our catchment."=case4_score)
        })
        
        S_At_Score <- sum(unlist(scores))
        S_At_score_num_db(S_At_Score)
        #print(paste0("S_At_Score: ", S_At_Score))
        return(S_At_Score) # # Calculate the sum of scores
        
      })
      
      
      
      observeEvent({
        req(!is.null(S_K_score_num_db()) && !is.null(S_Ac_score_num_db()) && !is.null(S_At_score_num_db()))
      }, {
        
        # Calculate erosionScore based on your logic
        S_Score <- round(100 * ((2 * S_K_score_num_db() + 5 * S_Ac_score_num_db() + 5 * S_At_score_num_db()) / 60), 0)
        
        # Update S_total_score_num with the calculated erosionScore
        S_total_score_num_db(S_Score)
        #print(paste0("S_Score: ", S_Score))
        
      })
      
      
      observe({
        conditions_met_page7 <- !is.null(input$S_Ac) && !is.null(input$S_At)  && !is.null(input$S_Val)
        
      })
      
      conditions_met_page7 <- reactiveVal(FALSE)
      
      observe({
        # Define conditions
        familiarWithAllConcepts <- "I am familiar with all these concepts."
        selectedConcepts <- input$S_K
        
        # Condition 1: Check if any concept is selected or if the user is familiar with all concepts
        cond1 <- !is.null(selectedConcepts) && (familiarWithAllConcepts %in% selectedConcepts || length(selectedConcepts) > 0)
        cond2 <- !is.null(input$S_Ac)
        cond3 <- !is.null(input$S_At)
        cond4 <- !is.null(input$S_Val) 
        
        
        
        # Combine all conditions
        conditions_met_page7 <- cond1 && cond2 && cond3 && cond4 
        
        
        
        
        output$ans_missing7 <- renderUI({
          # List to hold messages for unmet conditions
          messages <- list()
          
          # Check each condition and add message if not met
          if (!cond1) {
            messages <- c(messages, "Please select at least one concept in the 1st question or select 'I am familiar with all these concepts'. <br>")
          }
          if (!cond2) {
            messages <- c(messages, "Please answer 2nd question. <br>")
          }
          if (!cond3) {
            messages <- c(messages, "Please answer 3rd question. <br>")
          }
          if (!cond4) {
            messages <- c(messages, "Please answer 4th question. <br>")
          }
          
          
          
          if (length(messages) > 0) {
            HTML(paste("<span style='color: #e04b2f;'>", paste(messages, collapse = " "), "</span>"))
          }
          
        })
        
        
        # Update the state of the next button
        shinyjs::toggleState("next_page7", conditions_met_page7)
        
      })
      
    }})
  
  observeEvent(input$next_page7, {
    #print(paste0(S_K_score_num_db(), "  ",S_Ac_score_num_db(), "  ",S_At_score_num_db(), "  ", S_total_score_num_db()))
    
    # JavaScript code to add overlay and animate scroll
    jscode1 <- "
    // Add an overlay to the '#survey' div
    $('#survey').css({'position': 'relative'}).prepend('<div id=\"overlay\" style=\"position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: rgba(211, 211, 211, 0.5); z-index: 10;\"></div>');
    
    // Animate scroll
    $('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');
  "
    runjs(jscode1)
    
    # Delay the removal of the overlay by 1.5 seconds after the scroll animation
    jscode2 <- "
    setTimeout(function() {
      $('#overlay').remove();
    }, 1500); // 700 milliseconds = 1.5 seconds
  "
    # Schedule the execution of jscode2 after a short delay to ensure jscode1 has been initiated
    invalidateLater(200, session)
    observe({
      runjs(jscode2)
    })
    
    data <- data.frame(
      session_token = session_token(),
      S_K = Sanitize(paste(input$S_K, collapse = ";")),
      S_Ac = Sanitize(input$S_Ac),
      S_At = Sanitize(input$S_At),
      S_Val = Sanitize(input$S_Val)
    )
    
    data2 <- data.frame(
      session_token = session_token(),
      S_K_score_num  = S_K_score_num_db(),
      S_Ac_score_num = S_Ac_score_num_db(),
      S_At_score_num = S_At_score_num_db(),
      S_total_score_num = S_total_score_num_db()
    )
    #print(data2)
    
    saveData1(data, data2)
    currentPage(7)
  })
  
  
  
  
  ## Page 7 ####
  HL_K_score_num_db <- reactiveVal()
  HL_Ac_score_num_db <- reactiveVal()
  HL_At_score_num_db <- reactiveVal()
  HL_total_score_num_db <- reactiveVal()
  
  
  observeEvent(currentPage(), {
    # Check if the current page is 1
    if (currentPage() == 7) {
      output$other_HL_Val_count <- reactive({ paste0( 350-nchar(input$other_HL_Val), ' characters remaining.' ) })
      output$other_HL_Val2_count <- reactive({ paste0( 350-nchar(input$other_HL_Val2), ' characters remaining.' ) })
      
      
      HL_K_score <- observeEvent(input$HL_K,  {
        
        if (is.null(input$HL_K) || length(input$HL_K) == 0) {
          return(10)
        }
        
        scores <- sapply(input$HL_K, function(option) {
          switch(option,
                 "Soil biota plays an important role in  soil functions including structural improvement, organic matter turnover and pollutant degradation."=4,
                 "More acidic soils favour fungal over bacterial diversity, and support less diverse microbial communities."=3,
                 "Long-term experiments show that a combination of organic and inorganic nutrient inputs produces more sustainable crop yields than either kind alone."=2,
                 "The symbiotic fungi mycorrhiza is a fundamental part of plant nutrition: as much as 80% of phosphorus and up to 20% of nitrogen can be transferred to plants by these fungi."=1,
                 "I am familiar with all these concepts."  = 0)
          
        })
        
        HL_K_Score <- 10 - sum(scores)
        
        HL_K_score_num_db(HL_K_Score)
        #print(paste0("HL_K_Score: ", HL_K_Score))
        return(HL_K_Score)
      })
      
      
      
      
      #Action
      
      HL_Ac_score <- observeEvent({
        !is.null(input$HL_Ac) && length(input$HL_Ac) > 0 &&
          !is.null(input$HL_Val) && length(input$HL_Val) > 0
      }, {
        if (is.null(input$HL_Ac)) {
          return(0)
        }
        
        scores <- sapply(input$HL_Ac, function(option) {
          
          case1_score <- 1
          
          case2_score <- 2
          
          case3_score <- if (any(c("I do not use any practices to support soil biodiversity") %in% input$HL_Val ) || is.null(input$HL_Val)) {
            0
          } else  if (!any(c("Maintain soil cover or/and retain stubble", "Additions of organic matter", "Use crop and/or pasture rotations") %in% input$HL_Val)) {
            1
          } else {
            3
          }
          
          case4_score <- if (any(c("I do not use any practices to support soil biodiversity") %in% input$HL_Val ) || is.null(input$HL_Val)) {
            0
          } else {
            4
          }
          
          
          
          switch(option, "I only think about soil organisms in relation to root disease."=case1_score,
                 "I would like to implement management practices that increases underground biodiversity but need more information."=case2_score,
                 "I use practices to build organic matter in my soil to help improve soil health."=case3_score,
                 "I work with other farmers to learn, test and implement best practice to promote healthy soil biota."=case4_score)
        })
        
        HL_Ac_Score <- sum(unlist(scores))
        HL_Ac_score_num_db(HL_Ac_Score)
        #print(paste0("HL_Ac_Score: ", HL_Ac_Score))
        return(HL_Ac_Score) # # Calculate the sum of scores
        
      })
      
      
      
      
      
      #Attitude
      
      HL_At_score <- observeEvent({
        !is.null(input$HL_At) && length(input$HL_At) > 0 &&
          !is.null(input$HL_Val) && length(input$HL_Val) > 0
      }, {
        if (is.null(input$HL_At)) {
          return(0)
        }
        
        
        scores <- sapply(input$HL_At, function(option) {
          
          
          case1_score <- if (any("Minimise soil applied pesticides" %in% input$HL_Val)) {
            0
          } else {
            1
          }
          
          case3_score <- if (any("I do not use any practices to support soil biodiversity" %in% input$HL_Val)) {
            0
          } else {
            3
          }
          
          case4_score <- if (any("I do not use any practices to support soil biodiversity" %in% input$HL_Val)) {
            0
          } else {
            4
          }
          
          
          switch(option, "There is no need to monitor or improve the biodiversity on my farm."=case1_score,
                 "I am aware my practices may be detrimental to soil biota, but maximising productivity is my focus."=2,
                 "I work in sympathy with soil biota to reduce soil degradation and support productivity."=case3_score,
                 "It is imperative that we continue to improve understanding and how to work synergistically with below ground biota."=case4_score)
        })
        
        HL_At_Score <- sum(unlist(scores))
        HL_At_score_num_db(HL_At_Score)
        #print(paste0("HL_At_Score: ", HL_At_Score))
        return(HL_At_Score) # # Calculate the sum of scores
        
        
      })
      
      
      
      observeEvent({
        req(!is.null(HL_K_score_num_db()) && !is.null(HL_Ac_score_num_db()) && !is.null(HL_At_score_num_db()))
      }, {
        # Calculate erosionScore based on your logic
        HL_total_score <- round(100 * ((2 * HL_K_score_num_db() + 5 * HL_Ac_score_num_db() + 5 * HL_At_score_num_db()) / 60), 0)
        
        # Update HL_total_score_num with the calculated erosionScore
        HL_total_score_num_db(HL_total_score)
        
        #print(paste0("HL_total_score: ", HL_total_score))
      })
      
      
      conditions_met_page8 <- reactiveVal(FALSE)
      
      
      observe({
        # Define conditions
        familiarWithAllConcepts <- "I am familiar with all these concepts."
        selectedConcepts <- input$HL_K
        
        # Condition 1: Check if any concept is selected or if the user is familiar with all concepts
        cond1 <- !is.null(selectedConcepts) && (familiarWithAllConcepts %in% selectedConcepts || length(selectedConcepts) > 0)
        cond2 <- !is.null(input$HL_Ac)
        cond3 <- !is.null(input$HL_At)
        cond4 <- (length(input$HL_Val) != 0 && !('Other' %in% input$HL_Val)) || 
          (('Other' %in% input$HL_Val) && nchar(input$other_HL_Val) > 0)
        cond5 <- (length(input$HL_Val2) != 0 && !('Other' %in% input$HL_Val2)) || 
          (('Other' %in% input$HL_Val2) && nchar(input$other_HL_Val2) > 0)
        
        
        # Combine all conditions
        conditions_met_page8 <- cond1 && cond2 && cond3 && cond4 && cond5
        
        
        output$ans_missing8 <- renderUI({
          # List to hold messages for unmet conditions
          messages <- list()
          
          # Check each condition and add message if not met
          if (!cond1) {
            messages <- c(messages, "Please select at least one concept in the 1st question or select 'I am familiar with all these concepts'. <br>")
          }
          if (!cond2) {
            messages <- c(messages, "Please answer 2nd question. <br>")
          }
          if (!cond3) {
            messages <- c(messages, "Please answer 3rd question. <br>")
          }
          if (!cond4) {
            messages <- c(messages, "Please answer 4th question. If `Other` selected, please specify. <br>")
          }
          if (!cond5) {
            messages <- c(messages, "Please answer 5th question. If `Other` selected, please specify. <br>")
          }
          
          
          if (length(messages) > 0) {
            HTML(paste("<span style='color: #e04b2f;'>", paste(messages, collapse = " "), "</span>"))
          }
          
        })
        
        
        # Update the state of the next button
        shinyjs::toggleState("next_page8", conditions_met_page8)
        
        
      })
      
    }})
  
  
  observeEvent(input$next_page8, {
    #print(paste0(HL_K_score_num_db(), "  ",HL_Ac_score_num_db(), "  ",HL_At_score_num_db(), "  ", HL_total_score_num_db()))
    
    # JavaScript code to add overlay and animate scroll
    jscode1 <- "
    // Add an overlay to the '#survey' div
    $('#survey').css({'position': 'relative'}).prepend('<div id=\"overlay\" style=\"position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: rgba(211, 211, 211, 0.5); z-index: 10;\"></div>');
    
    // Animate scroll
    $('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');
  "
    runjs(jscode1)
    
    # Delay the removal of the overlay by 1.5 seconds after the scroll animation
    jscode2 <- "
    setTimeout(function() {
      $('#overlay').remove();
    }, 1500); // 700 milliseconds = 1.5 seconds
  "
    # Schedule the execution of jscode2 after a short delay to ensure jscode1 has been initiated
    invalidateLater(200, session)
    observe({
      runjs(jscode2)
    })
    
    
    data <- data.frame(
      session_token = session_token(),
      HL_K = Sanitize(paste(input$HL_K, collapse = ";")),
      HL_Ac = Sanitize(input$HL_Ac),
      HL_At = Sanitize(input$HL_At),
      HL_Val = Sanitize(paste(input$HL_Val, collapse = ";")),
      other_HL_Val = Sanitize(input$other_HL_Val),
      HL_Val2 = Sanitize(paste(input$HL_Val2, collapse = ";")),
      other_HL_Val2 = Sanitize(input$other_HL_Val2)
      
    )
    
    data2 <- data.frame(
      session_token = session_token(),
      HL_K_score_num  = HL_K_score_num_db(),
      HL_Ac_score_num = HL_Ac_score_num_db(),
      HL_At_score_num = HL_At_score_num_db(),
      HL_total_score_num = HL_total_score_num_db()
    )
    #print(data2)
    
    saveData1(data, data2)
    currentPage(8)
  })
  
  
  
  
  
  ## Page 8 ####
  NM_K_score_num_db <- reactiveVal()
  NM_Ac_score_num_db <- reactiveVal()
  NM_At_score_num_db <- reactiveVal()
  NM_total_score_num_db <- reactiveVal()
  
  
  observeEvent(currentPage(), {
    # Check if the current page is 1
    if (currentPage() == 8) {
      
      NM_K_score <- observeEvent(input$NM_K,  {
        
        if (is.null(input$NM_K) || length(input$NM_K) == 0) {
          return(10)
        }
        
        scores <- sapply(input$NM_K, function(option) {
          switch(option,
                 "Nitrogen, phosphorus and potassium (N, P, K) are not the only macro nutrients required by plants."=4,
                 "Factors that limit root growth, such as toxic layers or disease, reduce nutrient uptake."=3,
                 "The critical range for nutrient concentrations varies with soil type."=2,
                 "Nutrients applied to soil are rarely taken-up directly by plants but have to be cycled or transported by soil organisms."=1,
                 "I am familiar with all these concepts."=0)
          
        })
        
        NM_K_Score <- 10 - sum(scores)
        
        NM_K_score_num_db(NM_K_Score)
        #print(paste0("NM_K_Score: ", NM_K_Score))
        return(NM_K_Score)
      })
      
      
      
      
      #Action
      
      NM_Ac_score <- observeEvent({
        !is.null(input$NM_Ac) && length(input$NM_Ac) > 0 &&
          !is.null(input$NM_Val) && length(input$NM_Val) > 0 &&
          !is.null(input$NM_Val2) && length(input$NM_Val2) > 0
      }, {
        
        if (is.null(input$NM_Ac)) {
          return(0)
        }
        
        scores <- sapply(input$NM_Ac, function(option) {
          
          case1_score <- if (any(c("High input") %in% input$NM_Val2 ) || is.null(input$NM_Val2)) {
            0
          } else {
            1
          }
          
          case2_score <- 2
          
          case3_score <- 3
          
          case4_score <-4
          
          switch(option, "If nutrients are applied, rates are not modified by season."=case1_score,
                 "Fertiliser is only applied based on agronomist or supplier recommendations."=case2_score,
                 "Fertiliser rates and timings are modified by season, production objective and soil type."=case3_score,
                 "The latest technologies are used to measure and apply fertiliser in order to minimise loss to the environment."=case4_score)
        })
        
        NM_Ac_Score <- sum(unlist(scores))
        NM_Ac_score_num_db(NM_Ac_Score)
        #print(paste0("NM_Ac_Score: ", NM_Ac_Score))
        return(NM_Ac_Score) # # Calculate the sum of scores
        
      })
      
      
      
      
      #Attitude
      
      NM_At_score <- observeEvent({
        !is.null(input$NM_At) && length(input$NM_At) > 0 &&
          !is.null(input$NM_Val) && length(input$NM_Val) > 0 &&
          !is.null(input$NM_Val2) && length(input$NM_Val2) > 0
      }, {
        if (is.null(input$NM_At)) {
          return(0)
        }
        
        
        scores <- sapply(input$NM_At, function(option) {
          
          
          case1_score <- if (any(c("High input") %in% input$NM_Val2)||
                             any(c("Synthetic fertiliser") %in% input$NM_Val)) {
            0
          } else {
            1
          }
          case2_score <- if (any(c("None") %in% input$NM_Val)) {
            0
          } else {
            2
          }
          case3_score <- if (any(c("High input") %in% input$NM_Val2)  &&
                             any(c("Synthetic fertiliser") %in% input$NM_Val)) {
            1
          } else {
            3
          }
          
          case4_score <- if (any(c("High input") %in% input$NM_Val2)  &&
                             any(c("Synthetic fertiliser") %in% input$NM_Val)) {
            2
          } else {
            4
          }
          
          
          switch(option, "I do not apply fertiliser but rely on animal manure and/or leguminous plants."=case1_score,
                 "Fertiliser is applied because it is the most important source of crop nutrients."=case2_score,
                 "I consider soil biology and moisture as important for crop nutrition as the addition of fertiliser or manure."=case3_score,
                 "Our nutrient inputs are designed to feed the soil biota to support soil health and nutrient cycling to produce healthy crops and animals."=case4_score)
        })
        
        NM_At_Score <- sum(unlist(scores))
        NM_At_score_num_db(NM_At_Score)
        #print(paste0("NM_At_Score: ", NM_At_Score))
        return(NM_At_Score) # # Calculate the sum of scores
        
      })
      
      
      
      observeEvent({
        req(!is.null(NM_K_score_num_db()) && !is.null(NM_Ac_score_num_db()) && !is.null(NM_At_score_num_db()))
      }, {
        # Calculate erosionScore based on your logic
        NM_total_score <- round(100 * ((2 * NM_K_score_num_db() + 5 * NM_Ac_score_num_db() + 5 * NM_At_score_num_db()) / 60), 0)
        
        # Update NM_total_score_num with the calculated erosionScore
        NM_total_score_num_db(NM_total_score)
        
        #print(paste0("NM_total_score: ", NM_total_score))
      })
      
      conditions_met_page9 <- reactiveVal(FALSE)
      
      observe({
        # Define conditions
        familiarWithAllConcepts <- "I am familiar with all these concepts."
        selectedConcepts <- input$NM_K
        
        # Condition 1: Check if any concept is selected or if the user is familiar with all concepts
        cond1 <- !is.null(selectedConcepts) && (familiarWithAllConcepts %in% selectedConcepts || length(selectedConcepts) > 0)
        cond2 <- !is.null(input$NM_Ac)
        cond3 <- !is.null(input$NM_At)
        cond4 <- !is.null(input$NM_Val)
        cond5 <- !is.null(input$NM_Val2)
        
        # Combine all conditions
        conditions_met_page9 <- cond1 && cond2 && cond3 && cond4 && cond5 
        
        
        
        output$ans_missing9 <- renderUI({
          # List to hold messages for unmet conditions
          messages <- list()
          
          # Check each condition and add message if not met
          if (!cond1) {
            messages <- c(messages, "Please select at least one concept in the 1st question or select 'I am familiar with all these concepts'. <br>")
          }
          if (!cond2) {
            messages <- c(messages, "Please answer 2nd question. <br>")
          }
          if (!cond3) {
            messages <- c(messages, "Please answer 3rd question. <br>")
          }
          if (!cond4) {
            messages <- c(messages, "Please answer 4th question.  <br>")
          }
          if (!cond5) {
            messages <- c(messages, "Please answer 5th question.  <br>")
          }
          
          
          if (length(messages) > 0) {
            HTML(paste("<span style='color: #e04b2f;'>", paste(messages, collapse = " "), "</span>"))
          }
          
        })
        
        # Update the state of the next button
        shinyjs::toggleState("next_page9", conditions_met_page9)
      })
      
      
    }})
  
  observeEvent(input$next_page9, {
    #print(paste0(NM_K_score_num_db(), "  ",NM_Ac_score_num_db(), "  ",NM_At_score_num_db(), "  ", NM_total_score_num_db()))
    
    # JavaScript code to add overlay and animate scroll
    jscode1 <- "
    // Add an overlay to the '#survey' div
    $('#survey').css({'position': 'relative'}).prepend('<div id=\"overlay\" style=\"position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: rgba(211, 211, 211, 0.5); z-index: 10;\"></div>');
    
    // Animate scroll
    $('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');
  "
    runjs(jscode1)
    
    # Delay the removal of the overlay by 1.5 seconds after the scroll animation
    jscode2 <- "
    setTimeout(function() {
      $('#overlay').remove();
    }, 1500); // 700 milliseconds = 1.5 seconds
  "
    # Schedule the execution of jscode2 after a short delay to ensure jscode1 has been initiated
    invalidateLater(200, session)
    observe({
      runjs(jscode2)
    })
    
    data <- data.frame(
      session_token = session_token(),
      NM_K = Sanitize(paste(input$NM_K, collapse = ";")),
      NM_Ac = Sanitize(input$NM_Ac),
      NM_At = Sanitize(input$NM_At),
      NM_Val = Sanitize(paste(input$NM_Val, collapse = ";")),
      NM_Val2 = Sanitize(input$NM_Val2)
    )
    #print(data)
    
    data2 <- data.frame(
      session_token = session_token(),
      NM_K_score_num  = NM_K_score_num_db(),
      NM_Ac_score_num = NM_Ac_score_num_db(),
      NM_At_score_num = NM_At_score_num_db(),
      NM_total_score_num = NM_total_score_num_db()
    )
    #print(data2)
    
    
    saveData1(data, data2)
    currentPage(9)
  })
  
  
  
  
  
  ## Page 9 ####
  
  SW_K_score_num_db <- reactiveVal()
  SW_Ac_score_num_db <- reactiveVal()
  SW_At_score_num_db <- reactiveVal()
  SW_total_score_num_db <- reactiveVal()
  
  observeEvent(currentPage(), {
    # Check if the current page is 1
    if (currentPage() == 9) {
      
      SW_K_score <- observeEvent(input$SW_K,  {
        if (is.null(input$SW_K) || length(input$SW_K) == 0) {
          return(10)
        }
        
        scores <- sapply(input$SW_K, function(option) {
          switch(option,
                 "Plant available water plays a crucial role in determining potential  yield."=4,
                 "Soil factors including texture, structure and subsoil constraints influence soil water storage and water uptake by roots."=3,
                 "A saturated soil is more vulnerable to soil compaction and water erosion."=2,
                 "Under multiple future climate models Australia is predicted to suffer loss of soil moisture between 6 and 15% depending on region between 2030 and 2039."=1,
                 "I am familiar with all these concepts."=0)
          
        })
        SW_K_Score <- 10 - sum(scores)
        
        SW_K_score_num_db(SW_K_Score)
        #print(paste0("SW_K_Score: ", SW_K_Score))
        return(SW_K_Score)
      })
      
      
      
      #Action
      
      SW_Ac_score <- observeEvent({
        !is.null(input$SW_Ac) && length(input$SW_Ac) > 0 &&
          !is.null(input$SW_Val) && length(input$SW_Val) > 0 &&
          !is.null(input$SW_Val2) && length(input$SW_Val2) > 0
      }, {
        if (is.null(input$SW_Ac)) {
          return(0)
        }
        #print(input$SW_Ac)
        scores <- sapply(input$SW_Ac, function(option) {
          
          case1_score <- if (any(c("Soil being too wet") %in% input$SW_Val ) || is.null(input$SW_Val) ||
                             any(c("Irrigated") %in% input$SW_Val2 ) || is.null(input$SW_Val2)) {
            0
          } else {
            1
          }
          
          
          case3_score <- if (any(c("Not sure") %in% input$SW_Val ) || is.null(input$SW_Val)) {
            0
          } else {
            3
          }
          
          case4_score <- if (any(c("Not sure") %in% input$SW_Val ) || is.null(input$SW_Val)) {
            0
          } else {
            4
          }
          
          
          
          switch(option, "I do not try to manage my soil water." = case1_score,
                 "I plan operations based long-term rainfall forecasts as well my knowledge of rain patterns." = 2,
                 "I have soil moisture sensors and/or on-farm weather stations and plan operations based on my soil moisture and local rainfall forecast models." = case3_score,
                 "Water is a limited resource, and my management plans aim to maximise water use efficiency and minimise run-off." = case4_score)
          
          
        })
        
        SW_Ac_Score <- sum(unlist(scores))
        SW_Ac_score_num_db(SW_Ac_Score)
        #print(paste0("SW_Ac_Score: ", SW_Ac_Score))
        return(SW_Ac_Score) # # Calculate the sum of scores # # Calculate the sum of scores
        
      })
      
      
      
      #Attitude
      
      SW_At_score <- observeEvent({
        !is.null(input$SW_At) && length(input$SW_At) > 0 &&
          !is.null(input$SW_Val) && length(input$SW_Val) > 0 &&
          !is.null(input$SW_Val2) && length(input$SW_Val2) > 0
      }, {
        if (is.null(input$SW_At)) {
          return(0)
        }
        
        
        scores <- sapply(input$SW_At, function(option) {
          
          
          case1_score <- if (any("Not sure" %in% input$SW_Val)) {
            1
          } else {
            0
          }
          
          case2_score <- if (any("Not sure" %in% input$SW_Val)) {
            0
          } else {
            2
          }
          
          case3_score <- if (any("Not sure" %in% input$SW_Val)) {
            1
          } else {
            3
          }
          case4_score <- if (any("Not sure" %in% input$SW_Val)) {
            2
          } else {
            4
          }
          
          switch(option, "I have no interest in the amount of water stored in my soil."=case1_score,
                 "Implementing practices to manage water efficiency are difficult or too expensive."=2,
                 "I feel confident implementing practices to manage water in order to improve water use efficiency for production."=case3_score,
                 "Ensuring the long-term viability and water quality of my watershed is important and I want to use the latest technology to monitor and manage change."=case4_score)
        })
        
        SW_At_Score <- sum(unlist(scores))
        SW_At_score_num_db(SW_At_Score)
        #print(paste0("SW_At_Score: ", SW_At_Score))
        return(SW_At_Score) # # Calculate the sum of scores
        
      })
      
      
      
      observeEvent({
        req(!is.null(SW_K_score_num_db()) && !is.null(SW_Ac_score_num_db()) && !is.null(SW_At_score_num_db()))
      }, {
        # Calculate erosionScore based on your logic
        SW_total_score <- round(100 * ((2 * SW_K_score_num_db() + 5 * SW_Ac_score_num_db() + 5 * SW_At_score_num_db()) / 60), 0)
        
        # Update SW_total_score_num with the calculated erosionScore
        SW_total_score_num_db(SW_total_score)
        
        #print(paste0("SW_total_score: ", SW_total_score))
      })
      
      
      
      conditions_met_page10 <- reactiveVal(FALSE)
      observe({
        # Define conditions
        familiarWithAllConcepts <- "I am familiar with all these concepts."
        selectedConcepts <- input$SW_K
        
        # Condition 1: Check if any concept is selected or if the user is familiar with all concepts
        cond1 <- !is.null(selectedConcepts) && (familiarWithAllConcepts %in% selectedConcepts || length(selectedConcepts) > 0)
        cond2 <- !is.null(input$SW_Ac)
        cond3 <- !is.null(input$SW_At)
        cond4 <- !is.null(input$SW_Val)
        cond5 <- !is.null(input$SW_Val2)
        
        
        # Combine all conditions
        conditions_met_page10 <- cond1 && cond2 && cond3 && cond4 && cond5 
        
        
        output$ans_missing10 <- renderUI({
          # List to hold messages for unmet conditions
          messages <- list()
          
          # Check each condition and add message if not met
          if (!cond1) {
            messages <- c(messages, "Please select at least one concept in the 1st question or select 'I am familiar with all these concepts'. <br>")
          }
          if (!cond2) {
            messages <- c(messages, "Please answer 2nd question. <br>")
          }
          if (!cond3) {
            messages <- c(messages, "Please answer 3rd question. <br>")
          }
          if (!cond4) {
            messages <- c(messages, "Please answer 4th question.  <br>")
          }
          if (!cond5) {
            messages <- c(messages, "Please answer 5th question.  <br>")
          }
          
          
          if (length(messages) > 0) {
            HTML(paste("<span style='color: #e04b2f;'>", paste(messages, collapse = " "), "</span>"))
          }
          
        })
        
        # Update the state of the next button
        shinyjs::toggleState("next_page10", conditions_met_page10)
      })
      
      
    }})
  
  
  observeEvent(input$next_page10, {
    #print(paste0(SW_K_score_num_db(), "  ",SW_Ac_score_num_db(), "  ",SW_At_score_num_db(), "  ", SW_total_score_num_db()))
    
    # JavaScript code to add overlay and animate scroll
    jscode1 <- "
    // Add an overlay to the '#survey' div
    $('#survey').css({'position': 'relative'}).prepend('<div id=\"overlay\" style=\"position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: rgba(211, 211, 211, 0.5); z-index: 10;\"></div>');
    
    // Animate scroll
    $('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');
  "
    runjs(jscode1)
    
    # Delay the removal of the overlay by 1.5 seconds after the scroll animation
    jscode2 <- "
    setTimeout(function() {
      $('#overlay').remove();
    }, 1500); // 700 milliseconds = 1.5 seconds
  "
    # Schedule the execution of jscode2 after a short delay to ensure jscode1 has been initiated
    invalidateLater(200, session)
    observe({
      runjs(jscode2)
    })
    
    data_df <- data.frame(
      session_token = session_token(),
      SW_K = Sanitize(paste(input$SW_K, collapse = ";")),
      SW_Ac = Sanitize(input$SW_Ac),
      SW_At = Sanitize(input$SW_At),
      SW_Val = Sanitize(input$SW_Val),
      SW_Val2 = Sanitize(input$SW_Val2)
    )
    
    data_df2 <- data.frame(
      session_token = session_token(),
      SW_K_score_num  = SW_K_score_num_db(),
      SW_Ac_score_num = SW_Ac_score_num_db(),
      SW_At_score_num = SW_At_score_num_db(),
      SW_total_score_num = SW_total_score_num_db()
    )
    
    
    saveData1(data_df, data_df2)
    currentPage(10)
  })
  
  
  
  ## Page 10 ####
  
  DC_K_score_num_db <- reactiveVal()
  DC_Ac_score_num_db <- reactiveVal()
  DC_At_score_num_db <- reactiveVal()
  DC_total_score_num_db <- reactiveVal()     
  
  observeEvent(currentPage(), {
    # Check if the current page is 1
    if (currentPage() == 10) {
      output$other_DC_Val_count <- reactive({ paste0( 350-nchar(input$other_DC_Val), ' characters remaining.' ) })
      
      
      DC_K_score <- observeEvent(input$DC_K,  {
        if (is.null(input$DC_K) || length(input$DC_K) == 0) {
          return(10)
        }
        
        scores <- sapply(input$DC_K, function(option) {
          switch(option,
                 "Topography, drainage, and farm practices have an impact on the amount of carbon in soil at the landscape level."=4,
                 "Carbon from decaying roots and fungi are more likely to be retained in soil organic matter than an equivalent mass of aboveground litter after one year."=3,
                 "Low levels of nutrients such as nitrogen can lead to poor carbon storage."=2,
                 "Usually, more carbon is found between 30 cm and 200 cm below the surface than that of the top 30 cm, with farming practices affecting these deeper levels over decades."=1,
                 "I am familiar with all these concepts."=0)
          
        })
        
        DC_K_Score <- 10 - sum(scores)
        
        DC_K_score_num_db(DC_K_Score)
        #print(paste0("DC_K_Score: ", DC_K_Score))
        return(DC_K_Score)
      })
      
      
      #Action
      
      DC_Ac_score <- observeEvent({
        !is.null(input$DC_Ac) && length(input$DC_Ac) > 0 &&
          !is.null(input$DC_Val) && length(input$DC_Val) > 0
      }, {
        if (is.null(input$DC_Ac)) {
          return(0)
        }
        
        scores <- sapply(input$DC_Ac, function(option) {
          case1_score <- if (any(input$DC_Val %in% "None") || is.null(input$DC_Val)) {
            0
          } else {
            1
          }
          
          case2_score <- if (any(input$DC_Val %in% "None") || is.null(input$DC_Val)) {
            0
          } else {
            2
          }
          
          case3_score <- if (any(input$DC_Val %in% "None") || is.null(input$DC_Val)) {
            0
          } else {
            3
          }
          
          case4_score <- if (any(input$DC_Val %in% "None") || is.null(input$DC_Val)) {
            0
          } else {
            4
          }
          
          switch(option, "Managing soil carbon is not a priority."=case1_score,
                 "Management practices are not designed particularly to improve soil carbon, but hopefully provide carbon benefits."=case2_score,
                 "Practices that might improve soil carbon are included in management plans."=case3_score,
                 "Measuring and improving soil carbon are high priorities."=case4_score)
        })
        
        DC_Ac_Score <- sum(unlist(scores))
        DC_Ac_score_num_db(DC_Ac_Score)
        #print(paste0("DC_Ac_Score: ", DC_Ac_Score))
        return(DC_Ac_Score) # # Calculate the sum of scores
        
      })
      
      
      #Attitude
      
      DC_At_score <- observeEvent({
        !is.null(input$DC_At) && length(input$DC_At) > 0 &&
          !is.null(input$DC_Val) && length(input$DC_Val) > 0
      }, {
        if (is.null(input$DC_At)) {
          return(0)
        }
        
        
        scores <- sapply(input$DC_At, function(option) {
          
          
          case3_score <- if (any(input$DC_Val %in% "None") || is.null(input$DC_Val)) {
            0
          } else {
            3
          }
          
          case4_score <- if (any(input$DC_Val %in% "None") || is.null(input$DC_Val)) {
            0
          } else {
            4
          }
          
          
          switch(option, "I don't see the value or benefits of trying to change my soil carbon percentage."=1,
                 "Implementing practices to increase soil carbon are difficult or too expensive."=2,
                 "I feel confident implementing practices to increase soil carbon in order to improve production."=case3_score,
                 "The long-term security of my soil and soil security in Australia is dependent on the carbon in it."=case4_score)
        })
        
        DC_At_Score <- sum(unlist(scores))
        DC_At_score_num_db(DC_At_Score)
        #print(paste0("DC_At_Score: ", DC_At_Score))
        return(DC_At_Score) # # Calculate the sum of scores
        
      })
      
      
      
      
      observeEvent({
        req(!is.null(DC_K_score_num_db()) && !is.null(DC_Ac_score_num_db()) && !is.null(DC_At_score_num_db()))
      }, {
        
        # Calculate erosionScore based on your logic
        DC_total_score <- round(100 * ((2 * DC_K_score_num_db() + 5 * DC_Ac_score_num_db() + 5 * DC_At_score_num_db()) / 60), 0)
        
        # Update DC_total_score_num with the calculated erosionScore
        DC_total_score_num_db(DC_total_score)
        
        #print(paste0("DC_total_score: ", DC_total_score))
      })
      
      output$Val_DC_count <- reactive({ paste0( 500-nchar(input$Val_DC_comment), ' characters remaining.' ) })
      
      
      conditions_met_page6 <- reactiveVal(FALSE)
      
      observe({
        # Define conditions
        familiarWithAllConcepts <- "I am familiar with all these concepts."
        selectedConcepts <- input$DC_K
        
        # Condition 1: Check if any concept is selected or if the user is familiar with all concepts
        cond1 <- !is.null(selectedConcepts) && (familiarWithAllConcepts %in% selectedConcepts || length(selectedConcepts) > 0)
        cond2 <- !is.null(input$DC_Ac)
        cond3 <- !is.null(input$DC_At)
        cond4 <- (length(input$DC_Val) != 0 && !('Other' %in% input$DC_Val)) ||
          (('Other' %in% input$DC_Val) && nchar(input$other_DC_Val) > 0)
        cond5 <- !is.null(input$DC_Val2)
        
        # Combine all conditions
        conditions_met_page6 <- cond1 && cond2 && cond3 && cond4 && cond5
        
        output$ans_missing6 <- renderUI({
          # List to hold messages for unmet conditions
          messages <- list()
          
          # Check each condition and add message if not met
          if (!cond1) {
            messages <- c(messages, "Please select at least one concept in the 1st question or select 'I am familiar with all these concepts'. <br>")
          }
          if (!cond2) {
            messages <- c(messages, "Please answer 2nd question. <br>")
          }
          if (!cond3) {
            messages <- c(messages, "Please answer 3rd question. <br>")
          }
          if (!cond4) {
            messages <- c(messages, "Please answer 4th question. If `Other` selected, please specify. <br>")
          }
          if (!cond5) {
            messages <- c(messages, "Please answer 5th question. <br>")
          }
          
          if (length(messages) > 0) {
            HTML(paste("<span style='color: #e04b2f;'>", paste(messages, collapse = " "), "</span>"))
          }
          
        })
        
        
        # Update the state of the next button
        shinyjs::toggleState("next_page6", conditions_met_page6)
        
      })
      
      
    }})
  
  
  observeEvent(input$next_page6, {
    #print(paste0(DC_K_score_num_db(), "  ",DC_Ac_score_num_db(), "  ",DC_At_score_num_db(), "  ", DC_total_score_num_db()))
    # JavaScript code to add overlay and animate scroll
    jscode1 <- "
    // Add an overlay to the '#survey' div
    $('#survey').css({'position': 'relative'}).prepend('<div id=\"overlay\" style=\"position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: rgba(211, 211, 211, 0.5); z-index: 10;\"></div>');
    
    // Animate scroll
    $('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');
  "
    runjs(jscode1)
    
    # Delay the removal of the overlay by 1.5 seconds after the scroll animation
    jscode2 <- "
    setTimeout(function() {
      $('#overlay').remove();
    }, 1500); // 700 milliseconds = 1.5 seconds
  "
    # Schedule the execution of jscode2 after a short delay to ensure jscode1 has been initiated
    invalidateLater(200, session)
    observe({
      runjs(jscode2)
    })
    
    # Create the data frame
    data_df <- data.frame(
      session_token = session_token(),
      DC_K = ifelse(!is.null(input$DC_K), Sanitize(paste(input$DC_K, collapse = ";")), "Not found"),
      DC_Ac = ifelse(!is.null(input$DC_Ac), Sanitize(input$DC_Ac), "Not found"),
      DC_At = ifelse(!is.null(input$DC_At), Sanitize(input$DC_At), "Not found"),
      DC_Val = ifelse(!is.null(input$DC_Val), Sanitize(paste(input$DC_Val, collapse = ";")), "Not found"),
      other_DC_Val = ifelse(!is.null(input$other_DC_Val), Sanitize(input$other_DC_Val), "Not found"),
      DC_Val2 = ifelse(!is.null(input$DC_Val2), Sanitize(input$DC_Val2), "Not found"),
      Val_DC_comment= ifelse(!is.null(input$Val_DC_comment), Sanitize(input$Val_DC_comment), "Not found")
    )
    
    data_df2 <- data.frame(
      session_token = session_token(),
      DC_K_score_num  = DC_K_score_num_db(),
      DC_Ac_score_num = DC_Ac_score_num_db(),
      DC_At_score_num = DC_At_score_num_db(),
      DC_total_score_num = DC_total_score_num_db()
    )
    
    saveData1(data_df, data_df2)
    currentPage(11)
  })
  
  
  
  
  
  
  ## Page 11 ####
  observeEvent(currentPage(), {
    # Check if the current page is 1
    if (currentPage() == 11) {
      output$Threat_Val_E_count <- reactive({ paste0( 350-nchar(input$Threat_Val_E_comment), ' characters remaining.' ) })
      output$Threat_Val_A_count <- reactive({ paste0( 350-nchar(input$Threat_Val_A_comment), ' characters remaining.' ) })
      output$Threat_Val_SD_count <- reactive({ paste0( 350-nchar(input$Threat_Val_SD_comment), ' characters remaining.' ) })
      output$Threat_Val_DC_count <- reactive({ paste0( 350-nchar(input$Threat_Val_DC_comment), ' characters remaining.' ) })
      output$Threat_Val_S_count <- reactive({ paste0( 350-nchar(input$Threat_Val_S_comment), ' characters remaining.' ) })
      output$Threat_Val_HL_count <- reactive({ paste0( 350-nchar(input$Threat_Val_HL_comment), ' characters remaining.' ) })
      
      
      
      conditions_met_page11 <- reactiveVal(FALSE)
      
      observe({
        
        cond1 <- !is.null(input$Threat_Val_E)
        cond2 <- !is.null(input$Threat_Val_A)
        cond3 <- !is.null(input$Threat_Val_SD)
        cond4 <- !is.null(input$Threat_Val_DC)
        cond5 <- !is.null(input$Threat_Val_S)
        cond6 <- !is.null(input$Threat_Val_HL)
        
        
        # Combine all conditions
        conditions_met_page11 <- cond1 && cond2 && cond3 && cond4 && cond5 && cond6 
        
        
        
        output$ans_missing11 <- renderUI({
          # List to hold messages for unmet conditions
          messages <- list()
          
          # Check each condition and add message if not met
          if (!cond1) {
            messages <- c(messages, "Please rate importance of soil erosion threat. <br>")
          }
          if (!cond2) {
            messages <- c(messages, "Please Please rate importance of soil acidification threat. <br>")
          }
          if (!cond3) {
            messages <- c(messages, "Please Please rate importance of soil structural decline threat. <br>")
          }
          if (!cond4) {
            messages <- c(messages, "Please Please rate importance of decarbonisation threat. <br>")
          }
          if (!cond5) {
            messages <- c(messages, "Please Please rate importance of salinisation threat. <br>")
          }
          if (!cond6) {
            messages <- c(messages, "Please Please rate importance of soil biodiversity loss threat. <br>")
          }
          
          
          if (length(messages) > 0) {
            HTML(paste("<span style='color: #e04b2f;'>", paste(messages, collapse = " "), "</span>"))
          }
          
        })
        
        
        shinyjs::toggleState("next_page11", conditions_met_page11)
      })
      
      
    }})
  
  
  
  observeEvent(input$next_page11, {
    # JavaScript code to add overlay and animate scroll
    jscode1 <- "
    // Add an overlay to the '#survey' div
    $('#survey').css({'position': 'relative'}).prepend('<div id=\"overlay\" style=\"position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: rgba(211, 211, 211, 0.5); z-index: 10;\"></div>');
    
    // Animate scroll
    $('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');
  "
    runjs(jscode1)
    
    # Delay the removal of the overlay by 1.5 seconds after the scroll animation
    jscode2 <- "
    setTimeout(function() {
      $('#overlay').remove();
    }, 1500); // 700 milliseconds = 1.5 seconds
  "
    # Schedule the execution of jscode2 after a short delay to ensure jscode1 has been initiated
    invalidateLater(200, session)
    observe({
      runjs(jscode2)
    })
    
    End_time <-  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    data <- data.frame(
      session_token = session_token(),
      End_time = End_time,
      Threat_Val_E = Sanitize(input$Threat_Val_E),
      Threat_Val_E_comment = Sanitize(input$Threat_Val_E_comment),
      Threat_Val_A = Sanitize(input$Threat_Val_A),
      Threat_Val_A_comment = Sanitize(input$Threat_Val_A_comment),
      Threat_Val_SD = Sanitize(input$Threat_Val_SD),
      Threat_Val_SD_comment = Sanitize(input$Threat_Val_SD_comment),
      Threat_Val_DC = Sanitize(input$Threat_Val_DC),
      Threat_Val_DC_comment = Sanitize(input$Threat_Val_DC_comment),
      Threat_Val_S = Sanitize(input$Threat_Val_S),
      Threat_Val_S_comment = Sanitize(input$Threat_Val_S_comment),
      Threat_Val_HL = Sanitize(input$Threat_Val_HL),
      Threat_Val_HL_comment = Sanitize(input$Threat_Val_HL_comment)
    )
    
    saveData1(data)
    currentPage(12)
  })
  
  
  
  ## Page 12 ####
  observeEvent(currentPage(), {
    # Check if the current page is 1
    if (currentPage() == 12) {
      output$other_land_type_count <- reactive({ paste0( 225-nchar(input$other_land_type), ' characters remaining.' ) })
      
      
      conditions_met_page12 <- reactiveVal(FALSE)
      
      observe({
        cond1 <- !is.null(input$Age)
        cond2 <- !is.null(input$education_level)
        cond3 <-  (length(input$Land_type) != 0 && !('Other' %in% input$Land_type)) || 
          (('Other' %in% input$Land_type) && nchar(input$other_land_type) > 0)
        cond4 <-  !is.na(input$Land_area) && nchar(input$Land_area) > 0
        
        
        # Combine all conditions
        conditions_met_page12 <- cond1 && cond2 && cond3 && cond4 
        
        output$ans_missing12 <- renderUI({
          # List to hold messages for unmet conditions
          messages <- list()
          
          # Check each condition and add message if not met
          if (!cond1) {
            messages <- c(messages, "Please answer 1st question. <br>")
          }
          if (!cond2) {
            messages <- c(messages, "Please answer 2nd question. <br>")
          }
          if (!cond3) {
            messages <- c(messages, "Please answer 3rd question. If `Other` selected, please specify.  <br>")
          }
          if (!cond4) {
            messages <- c(messages, "Please answer 4th question.  <br>")
          }
          
          
          if (length(messages) > 0) {
            HTML(paste("<span style='color: #e04b2f;'>", paste(messages, collapse = " "), "</span>"))
          }
          
        })
        
        shinyjs::toggleState("next_page12", conditions_met_page12)
      })
      
    }})
  
  
  observeEvent(input$next_page12, {
    jscode <- "$('html, body').animate({ scrollTop: $('.logo-column.center-vertically').offset().top }, 'slow');"
    runjs(jscode)
    
    Submission_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    data <- data.frame(
      session_token = session_token(),
      Submission_time = Submission_time,
      Word_Familiarity = if (length(input$Word_Familiarity) > 0) {
        Sanitize(paste(input$Word_Familiarity, collapse = ";"))
      } else {
        ""
      },
      Age =  if (!is.null(input$Age)) {
        Sanitize(input$Age)
      } else {
        ""
      },
      education_level = if (!is.null(input$education_level)) {
        Sanitize(input$education_level)
      } else {
        ""
      },
      Land_type = if (length(input$Land_type) > 0) {
        Sanitize(paste(input$Land_type, collapse = ";"))
      } else {
        ""
      },
      other_land_type = if (!is.null(input$other_land_type)) {
        Sanitize(input$other_land_type)
      } else {
        ""
      },
      Land_area = if (!is.null(input$Land_area)) {
        as.character(input$Land_area)
      } else {
        ""
      }
    )
    
    saveData1(data= data)
    
    
    currentPage(14)
  })
  
  
  
  
  ##Results Page ####
  observeEvent(currentPage(), {
    # Check if the current page is 1
    if (currentPage() == 14) {
    
        
      data <- return_results_data(session_token(),  dbtable2)
      
      
      E_total_score_num(as.numeric(data$E_total_score_num))
      A_total_score_num(as.numeric(data$A_total_score_num))
      SD_total_score_num(as.numeric(data$SD_total_score_num))  
      S_total_score_num(as.numeric(data$S_total_score_num))
      HL_total_score_num(as.numeric(data$HL_total_score_num)) 
      NM_total_score_num(as.numeric(data$NM_total_score_num)) 
      SW_total_score_num(as.numeric(data$SW_total_score_num)) 
      DC_total_score_num(as.numeric(data$DC_total_score_num))
      
      
      
      Connectivity_Score <- reactive({
        ((E_total_score_num() + 
            A_total_score_num() + 
            SD_total_score_num() + 
            S_total_score_num() +
            HL_total_score_num() +
            NM_total_score_num() +
            SW_total_score_num() +
            DC_total_score_num()) / 8)
      })
      
      
      
      output$Headline <- renderText({
        avg <- Connectivity_Score()
        
        if (avg < 16) {
          "It looks like you are struggling to connect with your soil."
        } else if (avg < 32) {
          "Your score indicates you are poorly connected to your soil."
        } else if (avg < 48) {
          "Congratulations – you are becoming connected to your soil."
        } else if (avg < 65) {
          "Congratulations – you are connected to your soil."
        } else if (avg < 82) {
          "Congratulations – you are well connected to your soil."
        } else {
          "Congratulations – you are super connected to your soil."
        }
      })
      
      
      
      output$Strap <- renderUI({
        avg <- Connectivity_Score()
        values_table <- data.frame(variable=c("E_total_score_num",
                                              "A_total_score_num",
                                              "SD_total_score_num",
                                              "S_total_score_num",
                                              "HL_total_score_num",
                                              "NM_total_score_num",
                                              "SW_total_score_num",
                                              "DC_total_score_num"), 
                                   Score=c(E_total_score_num(),
                                           A_total_score_num(),
                                           SD_total_score_num(),
                                           S_total_score_num(),
                                           HL_total_score_num(),
                                           NM_total_score_num(),
                                           SW_total_score_num(),
                                           DC_total_score_num()),
                                   output=c("preventing erosion", "preventing acidification", "maintaining/increasing soil structure", "preventing salinization",
                                            "working synergistically with soil biodiversity", "maintaining/increasing soil carbon", "nutrient management", "soil water management"))
        
        recommendation <- character(0) # Initialize an empty character vector
        Recommendation <- paste(values_table[values_table$Score < 32, 3], collapse = ", ")
        values_table_sorted <- values_table[order(values_table$Score), ]
        
        Recommendation <- ifelse(Recommendation=="", values_table_sorted[1,3], Recommendation)
        
        
        if (avg < 16) {
          return(HTML("Your score indicates there’s significant opportunity to improve your soils connectivity to achieve future soil security. We encourage you to connect with other farmers and advisers to learn more about implementing best management practice for soils in your region"))
        } else if (avg < 32) {
          return(HTML("You are already aware of the soil threats, but it looks like you could benefit from some assistance in implementing cost-effective best practices for your farming system. Your score indicates you are a little hesitant to change; we encourage you to learn from other farmers already implementing best soil management in your region."))
        } else if (avg < 48) {
          return(HTML(paste0("You know managing soil is important, but it isn’t always your priority. With a little help from other farmers and advisers, you could start to build long-term soil security, benefiting the future of your farm. Your score indicates opportunities to focus attention on improving: <b>", Recommendation, ".</b>")))
        } else if (avg < 65) {
          return(HTML(paste0("You are implementing good soil management practice but there is always room for improvement. Your score indicates you are managing several threats well but now is the time to focus attention on: <b>", Recommendation, ".</b>")))
        } else if (avg < 82) {
          return(HTML("Your score indicates you are implementing best management practices for soil security and adopting new approaches. We encourage you to share your skills and knowledge within your farming networks."))
        } else {
          return(HTML("Your score indicates you are an influencer. We encourage you to keep enthusing others about the importance of their soil and its management."))
        }
      })
      
      
      
      
      output$Stage_definitions <- renderUI({
        avg <- Connectivity_Score()
        values <- c (E_total_score_num(), 
                     A_total_score_num(), 
                     SD_total_score_num(), 
                     S_total_score_num(),
                     HL_total_score_num(),
                     NM_total_score_num(),
                     SW_total_score_num(),
                     DC_total_score_num())
        
        
        Values_less_than_32 <- values[values < 32]
        
        
        if (avg < 16) {
          HTML(paste0("
        Your score indicates the following:<br>
               <b>Knowledge – </b>You are aware of the threats to soil security but lack knowledge of how to minimise the threats. <br>
<b>Action – </b>You may occasionally measure soil properties but take little or no action to reduce threats where they exist.<br>
<b>Attitude – </b>You are indifferent to implementing soil management strategies and prefer to continue current farming practices.
               ")) 
        } else if (avg < 32) {
          HTML(paste0("
        Your score indicates the following:<br>
      <b>Knowledge – </b>You have some knowledge of the threats to soil security and the types of management that can be used to reduce the threat on your soil types. <br>
<b>Action – </b>You measure soil properties infrequently and in reaction to a problem. Sometimes you will implement remediation strategies but often view these as a poor return on investment. <br>
<b>Attitude – </b>You often feel the soil threats are beyond your control or are cautious of practice change or implementing remediation strategies.
"))
        } else if (avg < 48) {
          HTML( paste0("
        Your score indicates the following:<br>
        <b>Knowledge – </b>You have some knowledge of the threats to soil security and the types of management that can be used to reduce the threat on your soil types. <br>
<b>Action – </b>You may measure soil properties and sometimes you will implement remediation strategies but often view these as a poor return on investment. <br>
<b>Attitude – </b>You often feel the soil threats are beyond your control or are cautious of practice change or implementing remediation strategies.
"))
          
        } else if (avg < 65) {
          HTML(paste0("
        Your score indicates the following:<br>
        <b>Knowledge – </b>You have a good understanding of soil threats across your farm and of those that could emerge due to your farming practices.  <br>
<b>Action – </b>You regularly monitor and quantify soil threats and implement appropriate remediation or practice change where required to meet soil quality targets.<br>
<b>Attitude – </b>You are open to new ways of managing soil threats and attend soil related extension activities. 
"))
          
        } else if (avg < 82) {
          HTML(paste0("
        Your score indicates the following:<br>
        <b>Knowledge – </b>You have a solid, practical knowledge of soil threats and how these can be managed to minimise impacts on production across your region.   <br>
<b>Action – </b>Managing soil threats is central to your production management activities. You implement strategies that aim to improve productive potential while maintaining soil health.  <br>
<b>Attitude – </b>You implement best management practices and seek out new ways to minimise soil threats while improving productivity.  
"))
        } else {
          HTML(paste0("
        Your score indicates the following:<br>
        <b>Knowledge – </b>You have a deep knowledge of soil threats and how these can be managed to minimise impacts on production across your region. You seek out the latest information on soil management. <br>
<b>Action – </b>Managing soil threats is a pillar of your production management and you implement strategies that aim to improve the productive potential of your soil without degrading soil quality.<br>
<b>Attitude – </b>You seek out new ways to minimise soil threats while improving productivity. You work with researchers and specialists and are willing to host extension activities to support the sharing of knowledge and experience.
"))
        }
      })
      
      
      output$Support <- renderUI({
        avg <- Connectivity_Score()
        
        if (avg < 16) {
          "To help you further improve your practices we encourage you to check out soils information resources from your State Agricultural and Environment Departments and your industry Research Development Corporation (RDC). You can also reach out to local farming groups in your area for advice and resources."
        } else if (avg < 32) {
          "To help you further improve your practices we encourage you to check out soils information resources from your State Agricultural and Environment Departments and your industry Research Development Corporation (RDC). You can also reach out to local farming groups in your area for advice and resources."
        } else if (avg < 48) {
          "To help you further improve your practices we encourage you to check out soils information resources from your State Agricultural and Environment Departments and your industry Research Development Corporation (RDC). You can also reach out to local farming groups in your area for advice and resources."
        } else if (avg < 65) {
          HTML("To help you further improve your practices, you may be interested in these resources from <a href='https://www.soilscienceaustralia.org.au/smartsoils' target='_blank'>Soils Science Australia.</a>")
        } else if (avg < 82) {
          HTML("As an implementer of best soil management practices, you may be interested in these resources from <a href='https://www.soilscienceaustralia.org.au/smartsoils' target='_blank'>Soils Science Australia.</a>")
        } else {
          HTML("As a seeker of information, you may be interested in the latest news from the <a href='https://soilcrc.com.au/news/' target='_blank'>CRC for High Performance Soils.</a>")
        }
      })
    }})
  
  # Closing action
  # 
  #    
  
  
  
  
  
  observeEvent(input$Close_app, {
    # Show the modal dialog
    showModal(modalDialog(
      title = "App Closing",
      "Your responses have been recorded! Thank you for using our soil connectivity evaluation tool. Please close this tab or window to exit.",
      footer = tagList(
        actionButton("close_and_redirect", "Close")
      ),
      easyClose = FALSE,
      
    ))
    cat("Session", session$token, "ended in Results page.\n")
    # Use shinyjs to run JavaScript for redirecting the user
    
  })
  
  
  
  
  session$onSessionEnded(function() {
    # Code to run when session ends
    cat("Session", session$token, "ended by user.\n")
  })
  
  
  
  
  
  
  
  
  
  #Idling (Keep-Alive)####
  # Set up Keep-Alive interval using shinyjs
  shinyjs::runjs('
    setInterval(function() {
      Shiny.setInputValue("keep_alive", Math.random());
    }, 60000); // 1 minute
  ')
  
  
  
  
  # Reactively handle Keep-Alive requests
  # observeEvent(input$keep_alive, {
  #   cat("Keep-Alive request received at", Sys.time(), "\n")
  # })
  
}

# Run the application
shinyApp(ui = ui, server = server)




