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
sidast_uppfaert <- "Síðast uppfært 04. maí 2020"
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
baseurl <- "https://raw.githubusercontent.com/bgautijonsson/covid19/master/"

d <- fread(
  paste0(baseurl, "Input/ECDC_Data.csv"), 
  encoding = "UTF-8")[, date := as.Date(date)][, !c("region")]
setDF(d)
date_range <- range(d$date)

