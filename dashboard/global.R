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
sidast_uppfaert <- "Síðast uppfært 12. ágúst 2020"
default_countries <- "Iceland"

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


# Load data ----
d <- local({
  d <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")
  to_rename <- c(country = "location", pop = "population")
  setnames(d, to_rename, names(to_rename))
  # Keep only relevant columns
  to_select <- c(
    "continent", "country", "pop", "date", 
    "new_cases", "new_deaths", "total_cases", "total_deaths"
  ) 
  d[, setdiff(names(d), to_select) := NULL]
  # Keep only relevant rows (actual countries) and at least one case
  d <- d[continent != "" & total_cases > 0]
  to_integer <- c("pop", "new_cases", "new_deaths", "total_cases", "total_deaths")
  d[, (to_integer) := lapply(.SD, as.integer), .SDcols = to_integer]
  # Add case and death rate
  d[, case_rate  := total_cases / pop * 1000]
  d[, death_rate := fifelse(total_cases == 0L, 0, total_deaths / total_cases)]
  # Convert to data.frame
  setDF(d)
})
date_range <- range(d$date)
