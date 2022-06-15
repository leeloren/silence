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
    
    output$text <- renderTable({
        
        #choose correct column for margin notes
        if (input$button == "Sound Priority Translation") notes_column_name <- "Sound Priority Translation Notes"
        else notes_column_name <- "Notes"
        
        df() %>% 
            #select relevant columns
            select(all_of(c(
                "Line",
                "Roche-Mahdi (2007) OF Edition",
                "My Translation" = input$button,
                "Notes" = notes_column_name
            )
            )) %>% 
            mutate(
                #convert notes to margin notes (only when there is something in the Notes column, otherwise leave blank)
                Notes = case_when(
                    # Notes != "" ~ margin_note(paste0(
                    #     #paste the manicule to the margin note
                    #     '<img src="manicule.png" style="height: 1.3em; vertical-align: bottom; display: inline-block; margin-left: -10%" /> ',
                    #     Notes),
                    #     # icon = "&#9756;"
                    #     #make the icon to expand the margin note when the window is narrow also a manicule
                    #     icon = '<img src="manicule.png" style="height: 1.3em; vertical-align: bottom; display: inline-block; " />'
                    # ),
                    Notes != "" ~ paste0(
                        #open span with manicule
                        '<span class="marginnote"><img src="manicule.png" style="height: 1.3em; vertical-align: bottom; display: inline-block; margin-left: -10%">',
                        Notes,
                        #close span
                        "</span>"
                        )
                    ,
                    TRUE ~ ""
                ),
                #paste the margin notes to the last column
                `My Translation` = paste(`My Translation`, Notes)) %>% 
            #remove notes column
            select(-Notes)
        
        },
        sanitize.text.function = function(x) x)
}

# Run the application 
shinyApp(ui = ui, server = server)
