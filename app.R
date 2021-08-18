library(shiny)

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
                      placeholder = "R036XB006NM")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

#### SERVER ####
server <- function(input, output, session) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
