library(shiny)
library(terradactyl)

#### UI ####
# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Indicator Calculator"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "search_type",
                        label = "LDC search method",
                        choices = c("Project Key", "Ecological Site ID"),
                        selected = "Ecological Site ID"),
            textInput(inputId = "search_string",
                      label = "Search string",
                      value = "",
                      placeholder = "R036XB006NM"),
            actionButton(inputId = "search_button",
                         label = "Search!"),
            
            selectInput(inputId = "grouping_vars",
                        label = "Grouping variable(s)",
                        choices = c("Plot ID" = "PlotID",
                                    "Project Name" = "ProjectName",
                                    "State" = "State",
                                    "County" = "County",
                                    "Ecological Site ID" = "EcologicalSiteId"),
                        multiple = TRUE),
            selectInput(inputId = "indicator_type",
                        label = "Indicator calculation",
                        choices = c("Percent cover by species (first hit)" = "first_hit",
                                    "Percent cover by species (any hit)" = "any_hit",
                                    "Percent cover (between plant)" = "between_plant",
                                    "Percent cover (bare soil)" = "bare_soil",
                                    "Percent cover (litter)" = "litter")),
            conditionalPanel(condition = "input.search_button >= 1",
                             actionButton(inputId = "calculate_button",
                                          label = "Calculate!")),
            
            # Only show if there are results to download
            conditionalPanel(condition = "input.calculate_button >= 1",
                             downloadButton(outputId = 'downloadData',
                                            label = 'Download current results'))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id = "maintabs",
                        tabPanel(title = "Instructions")
                        ,
                        tabPanel(title = "Data",
                                 dataTableOutput("data_table")
                        ),
                        tabPanel(title = "Results",
                                 dataTableOutput("results_table")
                        )
            )
        )
    )
)

#### SERVER ####
server <- function(input, output, session) {

    workspace <- reactiveValues(placeholder = TRUE,
                                temp_directory = tempdir(),
                                original_directory = getwd())
    

    #### When the search button is pressed, do this ####
    observeEvent(eventExpr = input$search_button,
                 handlerExpr = {
                     output$query_error <- renderText("")
                     # Only do anything if there's an search string
                     if (input$search_string != "") {
                         # Make sure it's uppercase
                         search_string <- toupper(input$search_string)
                         
                         # Handle multiple requested ecosites at once!
                         search_string_vector <- stringr::str_split(string = search_string,
                                                                 pattern = ",",
                                                                 simplify = TRUE)
                         search_string_vector <- trimws(search_string_vector)
                         
                         query_results_list <- lapply(X = search_string_vector,
                                                      FUN = function(X,
                                                                     search_type = input$search_type){
                                                          # Build the query
                                                          query <- switch(search_type,
                                                                          "Project Key" = {
                                                                              paste0("http://api.landscapedatacommons.org/api/",
                                                                                     "datalpi?",
                                                                                     "ProjectKey=",
                                                                                     X)
                                                                          },
                                                                          "Ecological Site ID" = {
                                                                              paste0("http://api.landscapedatacommons.org/api/",
                                                                                     "datalpi?",
                                                                                     "EcologicalSiteId=",
                                                                                     X)
                                                                          })
                                                          
                                                          # Getting the data via curl
                                                          # connection <- curl::curl(query)
                                                          # results_raw <- readLines(connection)
                                                          # results <- jsonlite::fromJSON(results_raw)
                                                          print("Attempting to query LDC")
                                                          # Full query results for geoindicators based on ecosite
                                                          full_results <- httr::GET(query,
                                                                                    config = httr::timeout(60))
                                                          # Grab only the data portion
                                                          results_raw <- full_results[["content"]]
                                                          # Convert from raw to character
                                                          results_character <- rawToChar(results_raw)
                                                          # Convert from character to data frame
                                                          results <- jsonlite::fromJSON(results_character)
                                                      })
                         
                         results <- do.call(rbind,
                                            query_results_list)
                         
                         # So we can tell the user later which actually got queried
                         workspace$queried_ecosites <- unique(results$EcologicalSiteId)
                         workspace$missing_ecosites <- ecosite_id_vector[!(ecosite_id_vector %in% workspace$queried_ecosites)]
                         
                         # Only keep going if there are results!!!!
                         if (length(results) > 0) {
                             # Convert from character to numeric variables where possible
                             data_corrected <- lapply(X = names(results),
                                                      data = results,
                                                      FUN = function(X, data){
                                                          # Get the current variable values as a vector
                                                          vector <- data[[X]]
                                                          # Try to coerce into numeric
                                                          numeric_vector <- as.numeric(vector)
                                                          # If that works without introducing NAs, return the numeric vector
                                                          # Otherwise, return the original character vector
                                                          if (all(!is.na(numeric_vector))) {
                                                              return(numeric_vector)
                                                          } else {
                                                              return(vector)
                                                          }
                                                      })
                             
                             # From some reason co.call(cbind, data_corrected) was returning a list not a data frame
                             # so I'm resorting to using dplyr
                             data <- dplyr::bind_cols(data_corrected)
                             # Correct the names of the variables
                             names(data) <- names(results)
                             
                             # Put it in the workspace list
                             workspace$raw_data <- data
                         } else {
                             output$query_error <- renderText(paste("The following are not valid ecological site IDs recognized by EDIT:",
                                                                      paste(workspace$missing_ecosites,
                                                                            collapse = ", ")))
                         }
                     }
                 })
    
    #### When the calculate button is pressed, do this ####
    observeEvent(eventExpr = input$calculate_button,
                 handlerExpr = {
                     terradactyl::pct_cover()
                 })
    
    
    ##### Download handler for the .zip file created with plots ####
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0("indicator_results_",
                   paste0(format(Sys.Date(), "%Y-%m-%d"), "_",
                          format(Sys.time(), "%H%M", tz = "GMT")),
                   ".csv")
        },
        content = function(file) {
            file.copy(paste0(workspace$temp_directory, "/results.csv"), file)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
