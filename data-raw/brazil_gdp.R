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

# Get municipal GDP data (2021) - most recent year only 
# Using table 5938 - Contas Nacionais Municipais
# Start with just GDP total to test
raw_gdp <- get_sidra(
  x = 5938,
  variable = 37,    # PIB total (Produto Interno Bruto a preços correntes)
  period = "2021",
  geo = "City"
)

# Clean and process GDP data
gdp_clean <- raw_gdp |>
  clean_names() |>
  select(
    municipality_code = municipio_codigo,
    municipality_raw = municipio,
    year = ano,
    gdp_thousands_brl = valor
  ) |>
  mutate(
    year = as.numeric(year),
    gdp_thousands_brl = as.numeric(gdp_thousands_brl),
    # Extract state code from municipality code (first 2 digits)
    state_code = as.numeric(substr(municipality_code, 1, 2)),
    # Clean municipality names (remove state abbreviation in parentheses)
    municipality = gsub("\\s*\\([A-Z]{2}\\)\\s*$", "", municipality_raw)
  ) |>
  # Filter for major municipalities (GDP > 500M BRL)
  filter(gdp_thousands_brl > 500000) |>
  # Add state and region information
  left_join(state_ref, by = c("state_code" = "code_state")) |>
  rename(
    state = name_state,
    state_abbr = abbrev_state
  ) |>
  # Calculate metrics
  mutate(
    # Convert to millions for readability
    gdp_current_brl_millions = gdp_thousands_brl / 1000,
    
    # Estimate GDP per capita based on typical population sizes
    # Note: This is estimated - for precise values would need population data
    estimated_population = case_when(
      gdp_current_brl_millions >= 50000 ~ 8000000,  # Very large cities
      gdp_current_brl_millions >= 10000 ~ 3000000,  # Large cities
      gdp_current_brl_millions >= 5000 ~ 1500000,   # Medium-large cities
      gdp_current_brl_millions >= 2000 ~ 800000,    # Medium cities
      gdp_current_brl_millions >= 1000 ~ 400000,    # Small-medium cities
      TRUE ~ 200000                                  # Small cities
    ),
    gdp_per_capita_current_brl = (gdp_current_brl_millions * 1000000) / estimated_population,
    
    # GDP size categories
    gdp_size_category = case_when(
      gdp_current_brl_millions >= 50000 ~ "Very large (50B+ BRL)",
      gdp_current_brl_millions >= 10000 ~ "Large (10B-50B BRL)",
      gdp_current_brl_millions >= 2000 ~ "Medium (2B-10B BRL)",
      gdp_current_brl_millions >= 500 ~ "Small (500M-2B BRL)",
      TRUE ~ "Very small (<500M BRL)"
    ),
    gdp_size_category = factor(gdp_size_category, levels = c(
      "Very large (50B+ BRL)", "Large (10B-50B BRL)", "Medium (2B-10B BRL)",
      "Small (500M-2B BRL)", "Very small (<500M BRL)"
    ))
  ) |>
  # Final selection and cleanup
  select(
    municipality_code, municipality, state_code, state, state_abbr, region,
    year, gdp_current_brl_millions, gdp_per_capita_current_brl, gdp_size_category
  ) |>
  arrange(region, state, municipality)

# Create final dataset
brazil_gdp <- gdp_clean

usethis::use_data(brazil_gdp, overwrite = TRUE)