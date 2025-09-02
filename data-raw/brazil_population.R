# Brazilian Municipal Population Data Processing
# Data source: IBGE SIDRA Table 6579 - Municipal population estimates

library(dplyr)
library(geobr)

import::from(sf, st_drop_geometry)
import::from(sidrar, get_sidra)
import::from(janitor, clean_names)
import::from(here, here)

# Get official Brazilian municipality data from geobr
municipalities <- read_municipality(year = 2022, simplified = TRUE)
dim_muni <- as_tibble(st_drop_geometry(municipalities))

city_size_categories <- tibble::tibble(
  min_pop = c(1000000, 500000, 200000, 100000, 0),
  max_pop = c(Inf, 1000000, 500000, 200000, 100000),
  category = c(
    "Metropolis (1M+)",
    "Large city (500K-1M)",
    "Medium city (200K-500K)",
    "Small city (100K-200K)",
    "Other"
  )
)

# Get population data for largest municipalities (2018-2022)
# Using table 6579 - População residente estimada
raw_population <- get_sidra(
  x = 6579,
  variable = 9324, # População residente estimada
  period = "2025",
  geo = "City"
)

# Clean and standardize raw data
population_base <- raw_population |>
  clean_names() |>
  as_tibble() |>
  select(
    code_muni = municipio_codigo,
    municipality_raw = municipio,
    year = ano,
    population = valor
  )

# Convert columns
population <- population_base |>
  mutate(
    code_muni = as.numeric(code_muni),
    year = as.numeric(year),
    population = as.numeric(population),
    # Rank cities by population
    rank = rank(-population)
  )

# Get only big cities
population <- population |>
  # Remove cities with less than 100,000 inhabitants
  filter(population > 1e5) |>
  # Join with city identifiers
  left_join(dim_muni, by = "code_muni") |>
  # Format table
  select(rank, name_muni, abbrev_state, population) |>
  arrange(desc(population))

# Classify cities by size
population <- population |>
  mutate(
    category = case_when(
      population > 1e6 ~ "Metropolis (1M+)",
      population > 5e5 ~ "Large city (500K-1M)",
      population > 2e5 ~ "Medium city (200K-500K)",
      population > 1e5 ~ "Small city (100K-200K)",
      TRUE ~ "Other"
    ),
    # Convert category to ordered factor
    category = factor(
      category,
      levels = c("Metropolis (1M+)", "Large city (500K-1M)", 
                 "Medium city (200K-500K)", "Small city (100K-200K)", "Other"),
      ordered = TRUE
    )
  )

# Create final dataset
brazil_population <- population
usethis::use_data(brazil_population, overwrite = TRUE)
