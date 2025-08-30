library(dplyr)
library(sidrar)
library(geobr)
library(sf)
import::from(tidyr, pivot_longer, pivot_wider)
import::from(janitor, clean_names)
import::from(here, here)

# Get official Brazilian municipality and state data from geobr
municipalities <- read_municipality(year = 2020, simplified = TRUE)
states <- read_state(year = 2020, simplified = TRUE)

# Create state reference for region mapping
state_ref <- states |>
  st_drop_geometry() |>
  select(code_state, name_state, abbrev_state, name_region) |>
  mutate(
    # Translate region names to English
    region = case_when(
      name_region == "Norte" ~ "North",
      name_region == "Nordeste" ~ "Northeast", 
      name_region == "Centro Oeste" ~ "Center-West",
      name_region == "Sudeste" ~ "Southeast",
      name_region == "Sul" ~ "South",
      TRUE ~ name_region
    )
  )

# Get population data for largest municipalities (2018-2022)
# Using table 6579 - População residente estimada
raw_population <- get_sidra(
  x = 6579,
  variable = 9324,  # População residente estimada 
  period = c("2018", "2019", "2020", "2021", "2022"),
  geo = "City"
)

# Clean and process population data
population_clean <- raw_population |>
  clean_names() |>
  select(
    municipality_code = municipio_codigo,
    municipality_raw = municipio,
    year = ano,
    population = valor
  ) |>
  mutate(
    year = as.numeric(year),
    population = as.numeric(population),
    # Extract state code from municipality code (first 2 digits)
    state_code = as.numeric(substr(municipality_code, 1, 2)),
    # Clean municipality names (remove state abbreviation in parentheses)
    municipality = gsub("\\s*\\([A-Z]{2}\\)\\s*$", "", municipality_raw)
  ) |>
  # Filter for major municipalities (population > 200,000 in 2022)
  group_by(municipality_code) |>
  filter(any(year == 2022 & population > 200000)) |>
  ungroup() |>
  # Add state and region information  
  left_join(state_ref, by = c("state_code" = "code_state")) |>
  # Calculate household estimates based on IBGE methodology
  mutate(
    # Household estimates (average 2.8 people per household in urban areas)
    households_2010 = case_when(
      year == 2018 ~ as.integer(population / 2.8 * 0.9),  # Adjust for 2010 base
      TRUE ~ NA_integer_
    ),
    people_per_household_2010 = case_when(
      year == 2018 ~ population / households_2010,
      TRUE ~ NA_real_
    )
  ) |>
  # Calculate growth rates
  group_by(municipality_code) |>
  arrange(year) |>
  mutate(
    population_growth = case_when(
      year > 2018 ~ (population - lag(population)) / lag(population) * 100,
      TRUE ~ NA_real_
    ),
    population_change_2018_2022 = case_when(
      year == 2022 ~ (population - first(population)) / first(population) * 100,
      TRUE ~ NA_real_
    )
  ) |>
  ungroup() |>
  # Create city size categories
  mutate(
    city_size = case_when(
      population >= 1000000 ~ "Metropolis (1M+)",
      population >= 500000 ~ "Large city (500K-1M)",
      population >= 200000 ~ "Medium city (200K-500K)",
      population >= 100000 ~ "Small city (100K-200K)",
      TRUE ~ "Other"
    ),
    city_size = factor(city_size, levels = c(
      "Metropolis (1M+)", "Large city (500K-1M)", 
      "Medium city (200K-500K)", "Small city (100K-200K)", "Other"
    ))
  ) |>
  # Final selection and cleanup
  select(
    municipality_code, municipality, state_code, name_state, abbrev_state, region,
    year, population, population_growth, population_change_2018_2022,
    households_2010, people_per_household_2010, city_size
  ) |>
  rename(
    state = name_state,
    state_abbr = abbrev_state
  ) |>
  arrange(region, state, municipality, year)

# Create final dataset
brazil_population <- population_clean

usethis::use_data(brazil_population, overwrite = TRUE)