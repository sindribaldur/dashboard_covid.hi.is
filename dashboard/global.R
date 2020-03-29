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

# Global settings ----
theme_set(
    theme_classic(base_size = 12) +
    background_grid(color.major = "grey90", color.minor = "grey95", minor = "xy", major = "xy") +
    theme(legend.position = "none")
)

# Constants ----
sidast_uppfaert <- "Síðast uppfært 28. mars 2020 klukkan 13:30"
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
d <- fread(paste0(baseurl, "Input/ECDC_Data.csv"), encoding = "UTF-8")[, date := as.Date(date)]
setDF(d)