library(shiny); library(dplyr); library(tidyr);
library(ggplot2); library(readr); library(cowplot); 
library(shinythemes);library(lubridate); library(kableExtra)
library(scales); library(broom); library(forcats);
library(lme4); library(stringr); library(plotly);
library(writexl); library(DT)
theme_set(theme_classic(base_size = 12) + 
              background_grid(color.major = "grey90", 
                              color.minor = "grey95", 
                              minor = "xy", major = "xy") +
              theme(legend.position = "none"))

url.exists <- RCurl::url.exists

fileurl <- local({
    today <- Sys.Date()
    baseurl <- "https://raw.githubusercontent.com/bgautijonsson/covid19/master/Output/Public/Iceland_Predictions/"
    url <- paste0(baseurl, today, ".csv")
    # Ef er komin inn spá fyrir daginn, annars prófa frá í gær
    if (url.exists(url)) {
        url
    } else {
        paste0(baseurl, "Iceland_Predictions_", "2020-03-24", ".csv")
    }
})


d_spa <- read_csv(
    fileurl
) %>% 
    mutate_at(vars(median, upper), round)

d <- read_csv("https://raw.githubusercontent.com/bgautijonsson/covid19/master/Input/Test/ECDC_Data.csv")
sidast_uppfaert <- "Síðast uppfært 23. mars 2020 klukkan 19:30"