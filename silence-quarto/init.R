library(tufte)
library(tidyverse)
library(knitr)

df_translations <- read_csv("materials/FREN 8510 McGrady Silence Translations - Sheet1.csv", 
                            col_types = cols(.default = "c",
                                             Line = col_integer())) %>% 
  mutate(across(.cols = c(Line, `My Translation`, Notes), 
                ~ifelse(is.na(.), "", .))) %>%
  rename(Line_all = Line) %>%
  mutate(Line = case_when((Line_all + 1 )%% 5 == 0 ~ as.character(Line_all),
                          TRUE ~ ""))

df_translations_old <- df_translations %>% 
  select(Line, `Psaki (1991) Translation`, `Roche-Mahdi (2007) Translation`)
