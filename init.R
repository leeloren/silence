library(tufte)
library(tidyverse)
library(knitr)

df_translations <- read_csv("materials/FREN 8510 McGrady Silence Translations - Sheet1.csv", 
                            col_types = cols(.default = "c",
                                             Line = col_integer())) %>% 
  #convert NAs to empty strings
  mutate(across(.cols = c(Line, `My Translation`, Notes, `Sound Priority Translation Notes`), 
                ~ifelse(is.na(.), "", .))) %>%
  #fill in Word Order Priority Translation with My Translation
  mutate(`Word Order Priority Translation` = case_when(
    is.na(`Word Order Priority Translation`) ~ `My Translation`,
    TRUE ~ `Word Order Priority Translation`
  )) %>% 
  #create line number column with number every 5 lines
  rename(Line_all = Line) %>%
  mutate(Line = case_when((Line_all + 1 )%% 5 == 0 ~ as.character(Line_all),
                          TRUE ~ ""))


df_translations_old <- df_translations %>% 
  select(Line, `Psaki (1991) Translation`, `Roche-Mahdi (2007) Translation`)



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

