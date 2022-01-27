#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "tufte_shiny.css")
    ),

    # Application title
    # titlePanel("This is a title"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Spectrum:",
                        min = 1,
                        max = 100,
                        value = 50)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    df <- reactive({
        
        test <- tribble(
            ~ Line, ~ Original, ~ start_val, ~ end_val, ~Translation,
            "1", "C'est un petit peu du francien", 1, 30, "It's French",
            "1", "C'est un petit peu du francien", 31, 45, "It's a little French",
            "1", "C'est un petit peu du francien", 46, 89, "It's a little bit of French",
            "1", "C'est un petit peu du francien", 90, 100, "It's a little bit of Old French"
        )
        
        return(test)
    })

    output$text <- renderTable(
        df() %>% 
            filter(input$bins >= df()$start_val,
                   input$bins <= df()$end_val) %>% 
            select(-start_val, -end_val)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
