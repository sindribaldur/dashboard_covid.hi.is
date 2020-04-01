# Packages ----
library(cowplot)
library(data.table)
library(dplyr)
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
library(glue)

# Global settings ----
theme_set(
    theme_classic(base_size = 12) +
    background_grid(color.major = "grey90", color.minor = "grey95", minor = "xy", major = "xy") +
    theme(legend.position = "none")
)

# Constants ----
sidast_uppfaert <- "Síðast uppfært 01. apríl 2020 klukkan 19:45"
nordic_countries <- c("Denmark", "Norway", "Finland", "Sweden", "Iceland", "Faroe Islands")

# Load data ----
baseurl <- "https://raw.githubusercontent.com/bgautijonsson/covid19/master/"
d_spa <- local({
    today <- Sys.Date()
    url <- paste0(baseurl, "Output/Public/Iceland_Predictions/Iceland_Predictions_", today, ".csv")
    # Ef er komin inn spá fyrir daginn, annars
    day <- "2020-03-24"
    url <- if (url.exists(url)) url else sub(today, day, url, fixed = TRUE)
    tointeger <- c("median", "upper")
    fread(url, colClasses = c("Date", rep("character", 3), "numeric", "numeric"))[,
      (tointeger) := lapply(.SD, function(x) as.integer(round(x))),
      .SDcols = tointeger]
})
setDF(d_spa)
d <- fread(
    paste0(baseurl, "Input/ECDC_Data.csv"), 
    encoding = "UTF-8")[, date := as.Date(date)][, !c("region", "total_deaths", "new_deaths")]
setDF(d)
date_range <- range(d$date)


# Info in sidebar:
sidebar_info <- glue(
    '<h6>Höfundar:</h6>
     <h6>Brynjófur Gauti Jónsson og Sindri Baldur Sævarsson</h6>
     <h6>Tölfræðiráðgjöf Heilbrigðisvísindasviðs Háskóla Íslands</h6>
     <div align="middle" class="center">
         <img src="hi_hvs_horiz.png" width="80%"/>
     </div>
     <h6>Byggt á daglega uppfærðum gögnum ECDC</h6>
     <a href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide">Hlekkur á gögn</a>
     <h6>Síðast uppfært 1. apríl 2020 klukkan 19:06</h6>
     <a href="https://github.com/sindribaldur/dashboard_covid.hi.is/">Allan kóða má nálgast hér</a>'
)