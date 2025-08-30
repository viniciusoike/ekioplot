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

gdp_size_categories <- tibble::tibble(
  min_gdp = c(50000, 10000, 2000, 500, 0),
  max_gdp = c(Inf, 50000, 10000, 2000, 500),
  category = c(
    "Very large (50B+ BRL)",
    "Large (10B-50B BRL)", 
    "Medium (2B-10B BRL)",
    "Small (500M-2B BRL)",
    "Very small (<500M BRL)"
  ),
  estimated_population = c(8000000, 3000000, 1500000, 800000, 400000)
)

# Create state reference for region mapping
state_ref_base <- states |>
  st_drop_geometry() |>
  select(code_state, name_state, abbrev_state, name_region)

state_ref <- state_ref_base |>
  left_join(region_names_map, by = "name_region") |>
  mutate(region = ifelse(is.na(region), name_region, region))

# Get municipal GDP data (2021) - most recent year only
# Using table 5938 - Contas Nacionais Municipais
# Start with just GDP total to test
raw_gdp <- get_sidra(
  x = 5938,
  variable = 37, # PIB total (Produto Interno Bruto a preços correntes)
  period = "2021",
  geo = "City"
)

# Step 1: Clean and standardize raw data
gdp_base <- raw_gdp |>
  clean_names() |>
  select(
    code_muni = municipio_codigo,
    municipality_raw = municipio,
    year = ano,
    gdp_thousands_brl = valor
  )

# Step 2: Add basic transformations
gdp_transformed <- gdp_base |>
  mutate(
    year = as.numeric(year),
    gdp_thousands_brl = as.numeric(gdp_thousands_brl),
    state_code = as.numeric(substr(code_muni, 1, 2)),
    name_muni = gsub("\\s*\\([A-Z]{2}\\)\\s*$", "", municipality_raw)
  )

# Step 3: Filter for major municipalities
gdp_filtered <- gdp_transformed |>
  filter(gdp_thousands_brl > 500000)

# Step 4: Add geographic information
gdp_with_geo <- gdp_filtered |>
  left_join(state_ref, by = c("state_code" = "code_state")) |>
  select(-municipality_raw)

# Step 5: Calculate GDP metrics
gdp_with_metrics <- gdp_with_geo |>
  mutate(gdp_current_brl_millions = gdp_thousands_brl / 1000)

# Step 6: Add GDP size categories using auxiliary tibble
gdp_categorized <- gdp_with_metrics
for (i in seq_len(nrow(gdp_size_categories))) {
  min_val <- gdp_size_categories$min_gdp[i]
  max_val <- gdp_size_categories$max_gdp[i]
  category <- gdp_size_categories$category[i]
  pop_estimate <- gdp_size_categories$estimated_population[i]
  
  gdp_categorized <- gdp_categorized |>
    mutate(
      gdp_size_category = ifelse(
        gdp_current_brl_millions >= min_val & gdp_current_brl_millions < max_val,
        category,
        ifelse(exists("gdp_size_category"), gdp_size_category, NA_character_)
      ),
      estimated_population = ifelse(
        gdp_current_brl_millions >= min_val & gdp_current_brl_millions < max_val,
        pop_estimate,
        ifelse(exists("estimated_population"), estimated_population, 200000)
      )
    )
}

# Step 7: Calculate per capita GDP and finalize
gdp_clean <- gdp_categorized |>
  mutate(
    gdp_per_capita_current_brl = (gdp_current_brl_millions * 1000000) / estimated_population,
    gdp_size_category = factor(
      gdp_size_category,
      levels = gdp_size_categories$category
    )
  ) |>
  select(
    code_muni, name_muni, state_code, name_state, abbrev_state,
    region, year, gdp_current_brl_millions, gdp_per_capita_current_brl,
    gdp_size_category
  ) |>
  rename(
    state = name_state,
    state_abbr = abbrev_state
  ) |>
  arrange(region, state, name_muni)

# Create final dataset
brazil_gdp <- gdp_clean

usethis::use_data(brazil_gdp, overwrite = TRUE)
