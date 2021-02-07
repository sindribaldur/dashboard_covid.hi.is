# Packages ----
library(cowplot)
library(data.table)
library(dplyr)
library(ggplot2)
library(DT)
library(lme4)
library(plotly)
library(scales)
library(shiny)
library(shinythemes)


# Global settings ----
theme_set(
  theme_classic(base_size = 12) +
    background_grid(color.major = "grey90", color.minor = "grey95", minor = "xy", major = "xy") +
    theme(legend.position = "none")
)
options(OutDec = ",") # For DT


# Constants ----
default_countries <- "Ísland" # Has to be in Europe

sidebar_info <-
  paste0(
    '<h6>Höfundar: Brynjófur Gauti Jónsson og Sindri Baldur Sævarsson</h6>
       <h6>Tölfræðiráðgjöf Heilbrigðisvísindasviðs Háskóla Íslands</h6>
       <div align="middle" class="center">
       <img src="hi_hvs_horiz.png" width="80%"/>
       </div>
       <h6>Byggt á daglega uppfærðum gögnum frá 
       <a href = "https://ourworldindata.org/coronavirus">Our World in Data.</a>
       Allan kóða má nálgast <a href="https://github.com/sindribaldur/dashboard_covid.hi.is/">hér.</a></h6>'
  )


# Load data ----
load("./data/data.rdata")
setkey(d, country)

# Functions -----
get_count_per_cont <- function(cont) {
   names(count_cont_vec)[count_cont_vec %chin% cont]
}