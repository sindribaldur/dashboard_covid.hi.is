# Packages ----
library(cowplot)
library(data.table)
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

if (.Platform$OS.type == "unix") {
  Sys.setlocale("LC_TIME", "is_IS.utf8")
}

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
date_range <- as.Date(date_range)
# Functions -----
get_count_per_cont <- function(cont) {
   names(count_cont_vec)[count_cont_vec %chin% cont]
}

format_date <- function() function(x) sub("^0", "", format(x, "%d. %b %Y"))
format_perc <- function(x, digits = 2L) {
    sub(".", ",", sprintf('%.0*f%%', digits, x*100), fixed = TRUE)
}

general_plot <- function(df = throun_df(), yvar = 'y_var_n', ylab = '', xdags = TRUE,
                         logscale = FALSE, perc = FALSE) {
    prefix_title = if (perc) '' else 'Þróun fjölda'
    prefix_ylab  = if (perc) '' else 'Fjöldi'
    ggplot(
        df,
        aes(
            text = paste0(
              country, ", ", format_date()(date), "<br>",
              if (is.integer(get(yvar))) {
                  get(yvar) 
              } else if (perc) {
                  paste0(round(get(yvar) * 100, 2), '%')
              } else {
                  round(get(yvar), 2)
              }
            )
        )
    ) +
    geom_line(
        aes_string(
            if (xdags) "date" else "days",  
            yvar, 
            col = "chosen",
            group = "country"
        ),
        show.legend = FALSE
    ) +
    scale_colour_manual(values = c(comp = "Blue", rest = "Black")) +
    {
        if (xdags) {
            scale_x_date(labels = format_date(), breaks = pretty_breaks(8))
        } else {
            NULL
        }
    } + 
    labs(
        title = paste(prefix_title, ylab),
        y = paste(prefix_ylab, ylab),
        x = if (xdags) NULL else "Dagar síðan skilyrði var náð"
    ) + 
    if (logscale) {
        scale_y_log10(labels = if (perc) scales::percent else label_number(accuracy = 1, big.mark = "\U202F"))
    } else {
        scale_y_continuous(labels = if (perc) scales::percent else label_number(accuracy = 1, big.mark = "\U202F"))
    }
}