library(tufte)
library(tidyverse)
library(knitr)

## functions --------------


##custom margin note wrangling function based on tufte::margin_note()
# number_column is a column of the dataframe that contains some sort of line number (needed so that unique labels are created for each margin note, which is needed for toggling to work)
margin_note_custom <- function (text, icon = "&#8853;", number_column) {
  
  marginnote_html_custom <- function (text = "", icon = "&#8853;") 
  {
    sprintf(paste0("<label for=\"tufte-mn-", {{number_column}}, 
                   "\" class=\"margin-toggle\">%s</label>", 
                   "<input type=\"checkbox\" id=\"tufte-mn-", {{number_column}}, 
                   "\" class=\"margin-toggle\">%s"), 
            icon, text)
  }
  
  
  if (is_html_output()) {
    marginnote_html_custom(sprintf("<span class=\"marginnote\">%s</span>", 
                                   text), icon)
  }  else {
    warning("margin_note_custom() only works for HTML output", 
            call. = FALSE)
    text
  }
}



## data ----------------

df_translations <- read_csv("materials/FREN 8510 McGrady Silence Translations - Sheet1.csv", 
                            col_types = cols(.default = "c",
                                             Line = col_integer())) %>% 
  #convert NAs to empty strings
  mutate(across(.cols = c(Line, `My Translation`, Notes, `Sound Priority Translation Notes`), 
                ~ifelse(is.na(.), " ", .))) %>%
  #fill in Word Order Priority Translation with My Translation
  mutate(`Word Order Priority Translation` = case_when(
    is.na(`Word Order Priority Translation`) ~ `My Translation`,
    TRUE ~ `Word Order Priority Translation`
  )) %>% 
  #create line number column with number every 5 lines
  rename(Line_all = Line) %>%
  mutate(Line = case_when((Line_all + 1 )%% 5 == 0 ~ as.character(Line_all),
                          TRUE ~ " ")) %>% 
  mutate(
    #convert notes to margin notes (only when there is something in the Notes column, otherwise leave blank)
    Notes_html = case_when(
      Notes != " " ~ margin_note_custom(paste0(
        #paste the manicule to the margin note
        '<img src="materials/manicule.png" style="height: 1.3em; vertical-align: bottom; display: inline-block; margin-left: -10%" /> ',
        Notes),
        # icon = "&#9756;"
        #make the icon to expand the margin note when the window is narrow also a manicule
        icon = '<img src="materials/manicule.png" style="height: 1.3em; vertical-align: bottom; display: inline-block; " />',
        number_column = Line_all
      ),
      TRUE ~ ""
    ),
    #convert notes to margin notes (only when there is something in the Notes column, otherwise leave blank)
    `Sound Priority Translation Notes html` = case_when(
      `Sound Priority Translation Notes` != " " ~ margin_note_custom(paste0(
        #paste the manicule to the margin note
        '<img src="materials/manicule.png" style="height: 1.3em; vertical-align: bottom; display: inline-block; margin-left: -10%" /> ',
        `Sound Priority Translation Notes`),
        # icon = "&#9756;"
        #make the icon to expand the margin note when the window is narrow also a manicule
        icon = '<img src="materials/manicule.png" style="height: 1.3em; vertical-align: bottom; display: inline-block; " />',
        number_column = Line_all
      ),
      TRUE ~ ""
    ),
    #paste the margin notes to the last column
    `My Translation w/ notes` = paste(`My Translation`, Notes_html),
    `Word Order Priority Translation w/ notes` = paste(`Word Order Priority Translation`, Notes_html),
    `Sound Priority Translation w/ notes` = paste(`Sound Priority Translation`, `Sound Priority Translation Notes html`)
    
    )


df_translations_old <- df_translations %>% 
  select(Line, `Psaki (1991) Translation`, `Roche-Mahdi (2007) Translation`)



