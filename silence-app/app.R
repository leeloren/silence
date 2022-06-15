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
            radioButtons("button",
                        "What do you want to prioritize in the translation?",
                        choices = c("Meaning" = "My Translation",
                                    "Syntax" = "Word Order Priority Translation",
                                    "Sound" = "Sound Priority Translation"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("text")
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    df <- reactive({
        
        df_translations <- read_csv("FREN 8510 McGrady Silence Translations - Sheet1.csv", 
                                            col_types = cols(.default = "c",
                                                             Line = col_integer())) %>% 
            #make rows with Notes empty strings
            mutate(across(.cols = c(Notes, `Sound Priority Translation Notes`), 
                          ~ifelse(is.na(.), "", .))) %>%
            #fill in Word Order Priority Translation with My Translation
            mutate(`Word Order Priority Translation` = case_when(
                is.na(`Word Order Priority Translation`) ~ `My Translation`,
                TRUE ~ `Word Order Priority Translation`
            )) %>% 
            #create line numbers that only appear every 5th row
            rename(Line_all = Line) %>%
            mutate(Line = case_when((Line_all + 1 )%% 5 == 0 ~ as.character(Line_all),
                                    TRUE ~ ""))
        
        return(df_translations)
    })

    output$text <- renderTable(
        df() %>% 
            select(all_of(c(
                "Line",
                "Roche-Mahdi (2007) OF Edition",
                input$button
            )
            ))
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
