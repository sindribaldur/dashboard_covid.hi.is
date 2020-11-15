# Packages ----
library(cowplot)
library(data.table)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(lme4)
library(plotly)
library(scales)
library(shiny)
library(shinythemes)
library(writexl)


# Global settings ----
theme_set(
  theme_classic(base_size = 12) +
    background_grid(color.major = "grey90", color.minor = "grey95", minor = "xy", major = "xy") +
    theme(legend.position = "none")
)


# Constants ----
default_countries <- "Ísland" # Has to be in Europe

sidebar_info <-
  paste0(
    '<h6>Höfundar: Brynjófur Gauti Jónsson og Sindri Baldur Sævarsson</h6>
       <h6>Tölfræðiráðgjöf Heilbrigðisvísindasviðs Háskóla Íslands</h6>
       <div align="middle" class="center">
       <img src="hi_hvs_horiz.png" width="80%"/>
       </div>
       <h6>Byggt á daglega uppfærðum 
       <a href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide">gögnum ECDC.</a>
       Allan kóða má nálgast <a href="https://github.com/sindribaldur/dashboard_covid.hi.is/">hér.</a></h6>'
  )


# Load data ----
d <- readRDS("./data/data.rds")
date_range <- c(min(d$date), max(d$date)) # Faster than range()
