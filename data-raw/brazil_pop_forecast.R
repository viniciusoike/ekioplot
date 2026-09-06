# Brazilian Population Projections by Age Group
# Data source: United Nations World Population Prospects 2024

library(dplyr)
library(wpp2024)

# Configuration ----

age_band_levels <- c(
  "0-4",
  "5-9",
  "10-14",
  "15-19",
  "20-24",
  "25-29",
  "30-34",
  "35-39",
  "40-44",
  "45-49",
  "50-54",
  "55-59",
  "60-64",
  "65-69",
  "70-74",
  "75-79",
  "80-84",
  "85-89",
  "90-94",
  "95-99",
  "100+"
)

ibge_age_group_levels <- c("0-14", "15-64", "65+")

age_group_levels <- c(
  "0-19",
  "20-29",
  "30-39",
  "40-49",
  "50-59",
  "60-69",
  "70-79",
  "80+"
)

# Age-group lookup ----

age_group_lookup <- tibble::tibble(age = age_band_levels) |>
  mutate(
    age_min = as.integer(sub("[-+].*$", "", age)),
    age_max = if_else(
      endsWith(age, "+"),
      Inf,
      as.numeric(gsub("\\D", "", age))
    ),
    ibge_age_group = as.character(cut(
      age_min,
      breaks = c(-Inf, 14, 64, Inf),
      labels = ibge_age_group_levels
    )),
    age_group = as.character(cut(
      age_min,
      breaks = c(-Inf, 19, 29, 39, 49, 59, 69, 79, Inf),
      labels = age_group_levels
    ))
  )

# Load five-year age-group projections from WPP 2024.
data("popprojAge5dt", package = "wpp2024")

# Build dataset ----

brazil_population_forecast <- popprojAge5dt |>
  as_tibble() |>
  filter(country_code == 76) |>
  mutate(
    year = as.integer(year),
    age = as.character(age),
    age_min = as.integer(sub("[-+].*$", "", age)),
    age_max = if_else(
      endsWith(age, "+"),
      Inf,
      as.numeric(gsub("\\D", "", age))
    )
  ) |>
  left_join(
    age_group_lookup,
    by = c("age", "age_min", "age_max"),
    relationship = "many-to-one"
  ) |>
  mutate(
    age = factor(age, levels = age_band_levels, ordered = TRUE),
    ibge_age_group = factor(
      ibge_age_group,
      levels = ibge_age_group_levels,
      ordered = TRUE
    ),
    age_group = factor(age_group, levels = age_group_levels, ordered = TRUE)
  ) |>
  transmute(
    country_code,
    name,
    year,
    age,
    ibge_age_group,
    age_group,
    population_m = round(popM * 1000),
    population_f = round(popF * 1000),
    population = round(pop * 1000),
    population_m_low = round(popM_low * 1000),
    population_m_high = round(popM_high * 1000),
    population_f_low = round(popF_low * 1000),
    population_f_high = round(popF_high * 1000),
    population_low = round(pop_low * 1000),
    population_high = round(pop_high * 1000)
  ) |>
  arrange(year, age)

# Validate and save ----

if (nrow(brazil_population_forecast) != 16 * length(age_band_levels)) {
  cli::cli_abort("Unexpected number of rows in brazil_population_forecast.")
}

if (
  anyNA(brazil_population_forecast$ibge_age_group) ||
    anyNA(brazil_population_forecast$age_group)
) {
  cli::cli_abort("Age-group classifications contain missing values.")
}

cli::cli_inform(
  paste0(
    "Built ",
    nrow(brazil_population_forecast),
    " rows for Brazil (",
    min(brazil_population_forecast$year),
    "-",
    max(brazil_population_forecast$year),
    ")."
  )
)

usethis::use_data(brazil_population_forecast, overwrite = TRUE)
