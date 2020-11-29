# Packages ----
library(data.table)
setDTthreads(0L)
library(fst)

# Download data ----
d <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")


# Prepare data ----

to_rename <- c(country = "location", pop = "population")
setnames(d, to_rename, names(to_rename))

# Keep only relevant columns
to_select <- c(
  "continent", "country", "pop", "date", "new_cases", "new_deaths"
)
d[, setdiff(names(d), to_select) := NULL]

# Keep only relevant rows (actual countries)
d <- d[continent != ""]

# Integer columns as integers
to_integer <- c("pop", "new_cases", "new_deaths")
d[, (to_integer) := lapply(.SD, as.integer), .SDcols = to_integer]

# All countries should start at the same point
start <- min(d$date)
d <- d[d[, .(date = seq.Date(from = start, to = max(date), by = 1L)), by = country], 
       on = .(country, date)]
# Fill in missing values
d[, pop := last(pop) , by = country]
d[, continent := last(continent), by = country]
d[is.na(new_cases), new_cases := 0L]
d[is.na(new_deaths), new_deaths := 0L]

# Add total 
d[, total_cases := cumsum(new_cases), by = country]
d[, total_deaths := cumsum(new_deaths), by = country]

# Add case and death rate
d[, case_rate  := total_cases / pop * 100000]
d[, death_rate := fifelse(total_cases == 0L, 0, total_deaths / total_cases)]
# Calculate rolling sums
d[, 
  `:=`(
    cases_n_weekly = as.integer(frollsum(new_cases, n = 7)),
    deaths_n_weekly = as.integer(frollsum(new_deaths, n = 7)),
    cases_p_biweekly = as.integer(frollsum(new_cases, n = 14)) / pop * 100000,
    deaths_p_biweekly = NA_integer_ # Not used at the moment
  ), by = country]
# Country/continent names to Icelandic
count_tr <- fread("../translation_tables/country_translation.csv", encoding = "UTF-8")
d[country %chin% count_tr$en, country := count_tr[.SD, on = .(en = country), is]]
conti_tr <- fread("../translation_tables/cont_translation.csv", encoding = "UTF-8")
d[continent %chin% conti_tr$en, continent := conti_tr[.SD, on = .(en = continent), is]]


# Helper objects ----
date_range <- c(min(d$date), max(d$date)) # Faster than range()
count_cont_vec <- x <- d[, unique(.SD), .SDcols = c("continent", "country")
  ][, setNames(continent, country)]


# Export data ----

# Convert to data.frame
save(d, date_range, count_cont_vec, file = "data.rdata", compress = FALSE)
