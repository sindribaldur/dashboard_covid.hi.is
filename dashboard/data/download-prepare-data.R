# Packages ----
library(data.table)

# Download data ----
d <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Prepare data ----
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
# Idate fails in some context
d[, date := as.Date(date)]
# Add case and death rate
d[, case_rate  := total_cases / pop * 100000]
d[, death_rate := fifelse(total_cases == 0L, 0, total_deaths / total_cases)]
# Calculate rolling sums
d[, 
  `:=`(
    cases_n_weekly = as.integer(frollsum(new_cases, n = 7)),
    deaths_n_weekly = as.integer(frollsum(new_deaths, n = 7)),
    cases_p_biweekly = as.integer(frollsum(new_cases, n = 14)) / max(pop) * 100000,
    deaths_p_biweekly = NA_integer_ # Not used at the moment
  ), by = country]
# Country/continent names to Icelandic
count_tr <- fread("../translation_tables/country_translation.csv", encoding = "UTF-8")
d[country %chin% count_tr$en, country := count_tr[.SD, on = .(en = country), is]]
conti_tr <- fread("../translation_tables/cont_translation.csv", encoding = "UTF-8")
d[continent %chin% conti_tr$en, continent := conti_tr[.SD, on = .(en = continent), is]]
# Convert to data.frame
setDF(d)

# Export data ----
saveRDS(d, "data.rds", compress = FALSE)
