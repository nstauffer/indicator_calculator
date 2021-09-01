library(shiny)
library(terradactyl)

options(shiny.maxRequestSize = 30*1024^2)

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
            
            fileInput(inputId = "uploaded_data",
                      label = "Upload data",
                      accept = "CSV"),
            
            selectInput(inputId = "lookup_table",
                        label = "Species information lookup table",
                        choices = c("AIM" = "aim",
                                    "USDA Plants" = "usda_plants",
                                    "Custom" = "custom")),
            # checkboxInput(inputId = "by_state",
            #               label = "Join lookup table to data by state"),
            
            conditionalPanel(condition = "input.lookup_table == 'custom'",
                             fileInput(inputId = "custom_lut",
                                       label = "Upload custom lookup table",
                                       accept = "CSV")),
            
            selectInput(inputId = "indicator_type",
                        label = "Indicator calculation",
                        choices = c("Percent cover by custom groups (first hit)" = "first_hit",
                                    "Percent cover by custom groups (any hit)" = "any_hit",
                                    "Percent cover by species (first hit)" = "species_first_hit",
                                    "Percent cover by species (any hit)" = "species_any_hit",
                                    "Percent cover (between plant)" = "between_plant",
                                    "Percent cover (bare soil)" = "bare_soil",
                                    "Percent cover (litter)" = "litter")),
            conditionalPanel(condition = "input.indicator_type == 'first_hit' | input.indicator_type == 'any_hit'",
                             selectInput(inputId = "grouping_vars",
                                         label = "Grouping variable(s)",
                                         choices = c("Species Code" = "code",
                                                     "Project Name" = "ProjectName",
                                                     "State" = "State",
                                                     "County" = "County",
                                                     "Ecological Site ID" = "EcologicalSiteId"),
                                         multiple = TRUE)),
            radioButtons(inputId = "output_format",
                         label = "Results table format",
                         choices = c("Tall" = "tall",
                                     "Wide" = "wide"),
                         selected = "wide"),
            
            actionButton(inputId = "calculate_button",
                         label = "Calculate!"),
            
            # Only show if there are results to download
            conditionalPanel(condition = "input.calculate_button >= 1",
                             downloadButton(outputId = 'downloadData',
                                            label = 'Download current results'))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id = "maintabs",
                        tabPanel(title = "Instructions"
                        ),
                        tabPanel(title = "Lookup Table",
                                 textOutput("missing_codes_error"),
                                 downloadButton(outputId = 'download_lut',
                                                label = 'Download current lookup table'),
                                 dataTableOutput("current_lut_table"),
                        ),
                        tabPanel(title = "Data",
                                 textOutput("query_error"),
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
                                original_directory = getwd(),
                                current_lut = read.csv("aim_state_species_list_2020.csv",
                                                       stringsAsFactors = FALSE),
                                aim_lut = read.csv("aim_state_species_list_2020.csv",
                                                   stringsAsFactors = FALSE),
                                usda_lut = read.csv("usda_plants_characteristics_lookup.csv",
                                                    stringsAsFactors = FALSE),
                                required_data_vars = c("PrimaryKey", "layer", "code"),
                                illegal_grouping_vars = c("PrimaryKey", "DBKey", 
                                                          "Latitude_NAD83", "Longitude_NAD83",
                                                          "DateEstablished", "DateLoadedInDb", "ProjectName", 
                                                          "ProjectKey", "LocationType", "DateVisited", "PercentCoveredByEcoSite", 
                                                          "wkb_geometry", "RecKey", "DateModified", "FormType", 
                                                          "FormDate", "Direction", "Measure", "LineLengthAmount", "SpacingIntervalAmount", 
                                                          "SpacingType", "ShowCheckbox", "CheckboxLabel", "PointLoc", "PointNbr", 
                                                          "ShrubShape", "layer", "chckbox"),
                                nonspecies_codes = c("L", "HL", "WL", "NL",
                                                     "DS", "W", "VL", "GR", "CB", "ST",
                                                     "S", "LC", "M", "D", "W", "R",
                                                     "CY", "EL", "BY", "BR"))
    
    #### When a custom lookup table is uploaded, do this ####
    observeEvent(eventExpr = input$custom_lut,
                 handlerExpr = {
                     workspace[["custom_lut"]] <- read.csv(input$custom_lut$datapath,
                                                           stringsAsFactors = FALSE)
                     
                     workspace[["current_lut"]] <- workspace$custom_lut
                     output$current_lut_table <- renderDataTable(workspace$current_lut)
                 })
    
    #### When a lookup table type is selected, do this ####
    observeEvent(eventExpr = input$lookup_table,
                 handlerExpr = {
                     # We'll only do this if it's not a custom table
                     if (input$lookup_table == "aim") {
                         workspace[["current_lut"]] <- workspace$aim_lut
                     } else if (input$lookup_table == "usda_plants") {
                         workspace[["current_lut"]] <- workspace$usda_lut
                     } else if (!is.null(workspace$custom_lut) & input$lookup_table == "custom") {
                         workspace[["current_lut"]] <- workspace$custom_lut
                     }
                 })
    
    #### When the lookup table or raw data is updated, do this ####
    observeEvent(eventExpr = list(workspace$current_lut,
                                  workspace$raw_data),
                 handlerExpr = {
                     # Only proceed if there are data already
                     if (!is.null(workspace[["raw_data"]])) {
                         current_data <- workspace$raw_data
                         current_data <- terradactyl::species_join(data = current_data,
                                                                   data_code = "code",
                                                                   species_file = workspace$current_lut,
                                                                   species_code = "SpeciesCode",
                                                                   species_duration = "Duration",
                                                                   growth_habit_file = "",
                                                                   by_state = "SpeciesState" %in% names(workspace$current_lut))
                                                                   
                         output$data_table <- renderDataTable(current_data)
                         workspace$current_data <- current_data
                         
                         # Time to make a version of the lookup table that has the missing codes
                         # Let's get the missing codes first
                         missing_codes <- unique(current_data[["code"]][!(current_data[["code"]] %in% workspace$current_lut[["SpeciesCode"]])])
                         
                         missing_codes <- missing_codes[!(missing_codes %in% workspace$nonspecies_codes)]
                         
                         if (length(missing_codes) > 0) {
                             # Make a copy of the current lookup table that we can make blank
                             lut_missing <- workspace$current_lut[seq_len(length(missing_codes)), ]
                             
                             # Replace the codes in that with the missing ones
                             lut_missing[["SpeciesCode"]] <- missing_codes
                             
                             # Change the values to NA for all non-code variables
                             for (var in names(lut_missing)[!(names(lut_missing) %in% c("SpeciesCode"))]) {
                                 lut_missing[[var]] <- NA
                             }
                             
                             output$missing_codes_error <- renderText("WARNING: There are codes in your data not currently in your lookup table. Please download the current lookup table, populate values for the missing codes, and reupload the corrected table as a custom lookup table to calculate indicators correctly.")
                             
                             workspace$current_lut <- rbind(lut_missing,
                                                            workspace$current_lut)

                         } else {
                             output$missing_codes_error <- renderText("")
                         }
                         
                         # Write the lookup table out so it can be downloaded
                         write.csv(workspace$current_lut,
                                   file = paste0(workspace$temp_directory, "/code_lookup_table.csv"),
                                   row.names = FALSE)
                         
                         output$current_lut_table <- renderDataTable(workspace$current_lut)
                     } 
                 })
    
    #### When workspace$current_data gets updated ####
    observeEvent(eventExpr = workspace$current_data,
                 handlerExpr = {
                     all_vars <- names(workspace$current_data)
                     acceptable_vars <- all_vars[!(all_vars %in% workspace$illegal_grouping_vars)]
                     updateSelectInput(inputId = "grouping_vars",
                                       choices = acceptable_vars)
                 })
    

    #### When data are uploaded, do this ####
    observeEvent(eventExpr = input$uploaded_data,
                 handlerExpr = {
                     workspace$raw_data <- input$uploaded_data
                     # Render the data to present to the user
                     print(workspace$raw_data)
                     workspace$display_data <- workspace$raw_data
                     message("Rendering data table")
                     output$data_table <- renderDataTable(workspace$display_data)
                     
                     updateTabsetPanel(session,
                                       inputId = "maintabs",
                                       selected = "Data")
                 })    
    
    
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
                         
                         query_results <- do.call(rbind,
                                                  query_results_list)
                         
                         # So we can tell the user later which actually got queried
                         workspace$queried_ecosites <- unique(query_results$EcologicalSiteId)
                         workspace$missing_ecosites <- search_string_vector[!(search_string_vector %in% workspace$queried_ecosites)]
                         
                         # Only keep going if there are results!!!!
                         if (length(query_results) > 0) {
                             message("Correcting numeric variables")
                             # Convert from character to numeric variables where possible
                             data_corrected <- lapply(X = names(query_results),
                                                      data = query_results,
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
                             names(data) <- names(query_results)
                             
                             # Put it in the workspace list
                             workspace$raw_data <- data
                         } else {
                             output$query_error <- renderText(paste("The following are not valid ecological site IDs recognized by EDIT:",
                                                                    paste(workspace$missing_ecosites,
                                                                          collapse = ", ")))
                         }
                         
                         # Render the data to present to the user
                         print(workspace$raw_data)
                         workspace$display_data <- workspace$raw_data
                         message("Rendering data table")
                         output$data_table <- renderDataTable(workspace$display_data)
                         
                         updateTabsetPanel(session,
                                           inputId = "maintabs",
                                           selected = "Data")
                     }
                 })
    
    #### When the calculate button is pressed, do this ####
    observeEvent(eventExpr = input$calculate_button,
                 handlerExpr = {
                     if (!is.null(input$current_data)) {
                         # Build a string to parse
                         # This is because the pct_cover_* functions take bare variable names
                         var_string <- paste0(input$grouping_vars,
                                              collapse = ", ")
                         
                         
                         argument_string <- paste0("lpi_tall = workspace$current_data, tall = ",
                                                   switch(input$output_format,
                                                          wide = {"FALSE"},
                                                          tall = {"TRUE"}),
                                                   ", by_line = FALSE, ")
                         
                         command_string <- switch(input$indicator_type,
                                                  "first_hit" = {
                                                      gsub(paste0("pct_cover(hit = 'first', ",
                                                                  argument_string,
                                                                  var_string,
                                                                  ")"),
                                                           pattern = ", )$",
                                                           replacement = ")")
                                                  },
                                                  "any_hit" = {
                                                      gsub(paste0("pct_cover(hit = 'any', ",
                                                                  argument_string,
                                                                  var_string,
                                                                  ")"),
                                                           pattern = ", )$",
                                                           replacement = ")")
                                                  },
                                                  "species_first_hit" = {
                                                      gsub(paste0("pct_cover_species(hit = 'first', ",
                                                                  argument_string,
                                                                  ")"),
                                                           pattern = ", )$",
                                                           replacement = ")")
                                                  },
                                                  "species_any_hit" = {
                                                      gsub(paste0("pct_cover_species(hit = 'any', ",
                                                                  argument_string,
                                                                  ")"),
                                                           pattern = ", )$",
                                                           replacement = ")")
                                                  },
                                                  "between_plant" = {
                                                      gsub(paste0("pct_cover_between_plant(",
                                                                  argument_string,
                                                                  ")"),
                                                           pattern = ", )$",
                                                           replacement = ")")
                                                  },
                                                  "bare_soil" = {
                                                      gsub(paste0("pct_cover_bare_soil(",
                                                                  argument_string,
                                                                  ")"),
                                                           pattern = ", )$",
                                                           replacement = ")")
                                                  },
                                                  "litter" = {
                                                      gsub(paste0("pct_cover_litter(",
                                                                  argument_string,
                                                                  ")"),
                                                           pattern = ", )$",
                                                           replacement = ")")
                                                  })
                         message(paste0("Command string is:", command_string))
                         workspace$results <- eval(parse(text = command_string))
                         
                         # Render the results
                         output$results_table <- renderDataTable(workspace$results)
                         
                         # Write the results in case they want to doanload them
                         write.csv(x = workspace$results,
                                   file = paste0(workspace$temp_directory,
                                                 "/results.csv"),
                                   row.names = FALSE)
                         
                         updateTabsetPanel(session = session,
                                           inputId = "maintabs",
                                           "Results")
                     }
                     
                 })
    
    
    #### Download handler for the calculation results ####
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
    
    #### Download handler for the lookup table ####
    output$download_lut <- downloadHandler(
        filename = function() {
            paste0("code_lookup_table.csv")
        },
        content = function(file) {
            file.copy(paste0(workspace$temp_directory, "/code_lookup_table.csv"), file)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
