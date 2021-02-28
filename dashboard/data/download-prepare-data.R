# Packages ----
library(data.table)
setDTthreads(0L)

# Import data ----
# the complete Our World in Data COVID-19 dataset
d <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")
count_tr <- fread("../translation_tables/country_translation.csv", encoding = "UTF-8")
conti_tr <- fread("../translation_tables/cont_translation.csv", encoding = "UTF-8")

# Prepare data ----
to_rename <- c(country = "location", pop = "population", total_vaccines = "people_fully_vaccinated")
setnames(d, to_rename, names(to_rename))

# Keep only relevant rows (actual countries)
d <- d[continent != ""]

# Country/continent names to Icelandic
d[country %chin% count_tr$en, country := count_tr[.SD, on = .(en = country), is]]
d[continent %chin% conti_tr$en, continent := conti_tr[.SD, on = .(en = continent), is]]


# Helper objects
date_range <- c(min(d$date), max(d$date)) # Faster than range()
count_cont_vec <- d[, unique(.SD), .SDcols = c("continent", "country")
  ][, setNames(continent, country)]
count_pop <- d[, as.integer(max(pop)), by = country][, setNames(V1, country)]



# Keep only relevant columns
to_select <- c(
  "country", "date", "new_cases", "new_deaths", "total_vaccines"
)
d[, setdiff(names(d), to_select) := NULL]


# Setkey
setkey(d, "country")

# Integer columns as integers
to_integer <- c("new_cases", "new_deaths", "total_vaccines")
d[, (to_integer) := lapply(.SD, as.integer), .SDcols = to_integer]

# All countries should start at the same point
d[, date := as.Date(date)]
start <- min(d$date)
d <- d[d[, .(date = seq.Date(from = start, to = max(date), by = 1L)), by = country], 
       on = .(country, date)]
# Fill in missing values
d[is.na(new_cases), new_cases := 0L]
d[is.na(new_deaths), new_deaths := 0L]
# For vaccine, data is sparse,
# We can safely fill first obs with 0, and then use LOCF
d[d[, .I[1L], by = country]$V1, total_vaccines := 0L]
d[, 
  total_vaccines := nafill(total_vaccines, "locf"),
  by = country]


# Add other total 
d[, total_cases := cumsum(new_cases), by = country]
d[, total_deaths := cumsum(new_deaths), by = country]

# New 
d[, new_vaccines := total_vaccines - shift(total_vaccines, fill = 0), by = country]


# Total per 100k
total_cols <- grep("^total", names(d), value = TRUE)
d[, paste0(total_cols, "_per100k") := lapply(.SD, function(x) x / count_pop[country] * 100000),
  .SDcols = total_cols]


# New last week
new_cols <- grep("^new", names(d), value = TRUE)
d[, paste0(new_cols, "_lw") := lapply(.SD, function(x) frollsum(x, n = 7)),
  .SDcols = new_cols, 
  by = country]

# New last two weeks per 100k
d[, paste0(new_cols, "_l2w_per100k") := lapply(.SD, function(x) frollsum(x, n = 14) / count_pop[country] * 100000 ),
  .SDcols = new_cols, 
  by = country]

# Death cases
d[, 
  death_rate := fifelse(total_cases == 0L, 0, total_deaths / total_cases), 
  by = country]



# Export data ----
setorder(d, country, date)
save(d, date_range, count_cont_vec, count_pop, file = "data.rdata", compress = FALSE)
