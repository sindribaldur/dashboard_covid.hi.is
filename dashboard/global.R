# Packages ----
library(cowplot)
library(data.table)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(kableExtra)
library(lme4)
library(plotly)
library(scales)
library(shiny)
library(shinythemes)
library(writexl)
url.exists <- RCurl::url.exists


# Global settings ----
theme_set(
  theme_classic(base_size = 12) +
    background_grid(color.major = "grey90", color.minor = "grey95", minor = "xy", major = "xy") +
    theme(legend.position = "none")
)

# Constants ----
sidast_uppfaert <- "Síðast uppfært 05. apríl 2020 klukkan 18:53"
nordic_countries <- c("Denmark", "Norway", "Finland", "Sweden", "Iceland", "Faroe Islands")

# Load data ----
baseurl <- "https://raw.githubusercontent.com/bgautijonsson/covid19/master/"
d_spa <- local({
  day <- Sys.Date()
  url <- paste0(baseurl, "Output/Iceland_Predictions/Iceland_Predictions_", day, ".csv")
  
  # Ef ekki komin spá, leita aftur í tímann eftir síðustu spá.
  while (!url.exists(url)) {
    day <- day - 1
    url <- paste0(baseurl, "Output/Iceland_Predictions/Iceland_Predictions_", day, ".csv")
  }
  tointeger <- c("median", "upper")
  fread(url, colClasses = c("Date", rep("character", 3), rep("numeric", 3), "character"), encoding = "UTF-8")[,
                                                                                                              (tointeger) := lapply(.SD, function(x) as.integer(round(x))),
                                                                                                              .SDcols = tointeger]
}) 

d_spa <- setDF(d_spa) %>% 
  filter(aldursdreifing == "gögn") %>% 
  select(-aldursdreifing)

d <- fread(
  paste0(baseurl, "Input/ECDC_Data.csv"), 
  encoding = "UTF-8")[, date := as.Date(date)][, !c("region", "new_deaths")]
setDF(d)
date_range <- range(d$date)

iceland_d <- fread("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/export?format=csv&id=1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA&gid=1788393542") %>% 
  select(date = Dagsetning, cumulative_cases = Smit_Samtals, active_cases = Virk_Smit, 
         active_hospital = Inniliggjandi, active_icu = Gjorgaesla, cumulative_hospital = Spitali_Samtals, cumulative_icu = Gjorgaesla_Samtals) %>% 
  pivot_longer(-date, names_pattern = "(.+)_(.+)", names_to = c("type", "name")) %>%
  mutate(date = as.Date(date))

# Info in sidebar:
sidebar_info <-
  paste0(
    '<h6>Höfundar:</h6>
       <h6>Brynjófur Gauti Jónsson og Sindri Baldur Sævarsson</h6>
       <h6>Tölfræðiráðgjöf Heilbrigðisvísindasviðs Háskóla Íslands</h6>
       <div align="middle" class="center">
       <img src="hi_hvs_horiz.png" width="80%"/>
       </div>
       <h6>Byggt á daglega uppfærðum gögnum ECDC</h6>
       <a href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide">Hlekkur á gögn</a>
       <h6>', sidast_uppfaert, '</h6>
       <a href="https://github.com/sindribaldur/dashboard_covid.hi.is/">Allan kóða má nálgast hér</a>'
  )
