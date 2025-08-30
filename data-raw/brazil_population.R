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

# Create auxiliary tibbles for data classification
region_names_map <- tibble::tibble(
  name_region = c("Norte", "Nordeste", "Centro Oeste", "Sudeste", "Sul"),
  region = c("North", "Northeast", "Center-West", "Southeast", "South")
)

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

# Create state reference for region mapping
state_ref_base <- states |>
  st_drop_geometry() |>
  select(code_state, name_state, abbrev_state, name_region)

state_ref <- state_ref_base |>
  left_join(region_names_map, by = "name_region") |>
  mutate(region = ifelse(is.na(region), name_region, region))

# Get population data for largest municipalities (2018-2022)
# Using table 6579 - População residente estimada
raw_population <- get_sidra(
  x = 6579,
  variable = 9324, # População residente estimada
  period = c("2018", "2019", "2020", "2021", "2022"),
  geo = "City"
)

# Step 1: Clean and standardize raw data
population_base <- raw_population |>
  clean_names() |>
  select(
    code_muni = municipio_codigo,
    municipality_raw = municipio,
    year = ano,
    population = valor
  )

# Step 2: Add basic transformations
population_transformed <- population_base |>
  mutate(
    year = as.numeric(year),
    population = as.numeric(population),
    state_code = as.numeric(substr(code_muni, 1, 2)),
    name_muni = gsub("\\s*\\([A-Z]{2}\\)\\s*$", "", municipality_raw)
  )

# Step 3: Filter for major municipalities (population > 200,000 in 2022)
population_filtered <- population_transformed |>
  group_by(code_muni) |>
  filter(any(year == 2022 & population > 200000)) |>
  ungroup()

# Step 4: Add geographic information
population_with_geo <- population_filtered |>
  left_join(state_ref, by = c("state_code" = "code_state")) |>
  select(-municipality_raw)

# Step 5: Calculate household estimates
population_with_households <- population_with_geo |>
  mutate(
    households_2010 = ifelse(
      year == 2018,
      as.integer(population / 2.8 * 0.9),
      NA_integer_
    ),
    people_per_household_2010 = ifelse(
      year == 2018 & !is.na(households_2010) & households_2010 > 0,
      population / households_2010,
      NA_real_
    )
  )

# Step 6: Calculate growth rates
population_with_growth <- population_with_households |>
  group_by(code_muni) |>
  arrange(year) |>
  mutate(
    population_growth = ifelse(
      year > 2018 & !is.na(lag(population)) & lag(population) > 0,
      (population - lag(population)) / lag(population) * 100,
      NA_real_
    ),
    population_change_2018_2022 = ifelse(
      year == 2022 & first(population) > 0,
      (population - first(population)) / first(population) * 100,
      NA_real_
    )
  ) |>
  ungroup()

# Step 7: Add city size categories using auxiliary tibble
population_categorized <- population_with_growth
for (i in seq_len(nrow(city_size_categories))) {
  min_val <- city_size_categories$min_pop[i]
  max_val <- city_size_categories$max_pop[i]
  category <- city_size_categories$category[i]
  
  population_categorized <- population_categorized |>
    mutate(
      city_size = ifelse(
        population >= min_val & population < max_val,
        category,
        ifelse(exists("city_size"), city_size, "Other")
      )
    )
}

# Step 8: Finalize with factor levels and cleanup
population_clean <- population_categorized |>
  mutate(
    city_size = factor(
      city_size,
      levels = city_size_categories$category
    )
  ) |>
  select(
    code_muni, name_muni, state_code, name_state, abbrev_state,
    region, year, population, population_growth, population_change_2018_2022,
    households_2010, people_per_household_2010, city_size
  ) |>
  rename(
    state = name_state,
    state_abbr = abbrev_state
  ) |>
  arrange(region, state, name_muni, year)

# Create final dataset
brazil_population <- population_clean

usethis::use_data(brazil_population, overwrite = TRUE)
