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

# Define major Brazilian crops (temporary crops from PAM)
major_crops <- c(
  "2713",  # Soybeans (Soja em grão)
  "2711",  # Corn (Milho em grão) 
  "2696",  # Sugarcane (Cana-de-açúcar)
  "2689",  # Cotton (Algodão herbáceo em caroço)
  "2692",  # Rice (Arroz em casca)
  "2716",  # Wheat (Trigo em grão)
  "2702"   # Beans (Feijão em grão)
)

# Get agricultural production data (2022) - most recent year only
# Using table 1612 - PAM (Produção Agrícola Municipal)
# Reduce to fewer variables and crops to avoid API limits
raw_agriculture <- get_sidra(
  x = 1612,
  variable = c(
    214,   # Quantidade produzida (toneladas)
    216    # Área colhida (hectares)
  ),
  period = "2022",
  geo = "City",
  classific = "c81",
  category = list(c81 = c("2713", "2711", "2696"))  # Just soybeans, corn, sugarcane
)

# Clean and process agriculture data
agriculture_clean <- raw_agriculture |>
  clean_names() |>
  select(
    municipality_code = municipio_codigo,
    municipality_raw = municipio,
    year = ano,
    variable = variavel,
    crop_raw = produto_das_lavouras_temporarias,
    value = valor
  ) |>
  mutate(
    year = as.numeric(year),
    value = as.numeric(value),
    # Extract state code from municipality code (first 2 digits)
    state_code = as.numeric(substr(municipality_code, 1, 2)),
    # Clean municipality names (remove state abbreviation in parentheses)
    municipality = gsub("\\s*\\([A-Z]{2}\\)\\s*$", "", municipality_raw),
    # Create standardized crop names
    crop = case_when(
      grepl("Soja", crop_raw, ignore.case = TRUE) ~ "soybeans",
      grepl("Milho", crop_raw, ignore.case = TRUE) ~ "corn",
      grepl("Cana", crop_raw, ignore.case = TRUE) ~ "sugarcane",
      TRUE ~ tolower(crop_raw)
    ),
    # Create variable identifier
    variable_id = case_when(
      grepl("Área colhida", variable, ignore.case = TRUE) ~ "area_harvested_ha",
      grepl("Quantidade produzida", variable, ignore.case = TRUE) ~ "production_tonnes",
      TRUE ~ "other"
    )
  ) |>
  # Filter out zero/missing production
  filter(
    !is.na(value),
    value > 0,
    # Keep only municipalities with significant production
    !(variable_id == "production_tonnes" & value < 1000)
  ) |>
  # Add state and region information
  left_join(state_ref, by = c("state_code" = "code_state")) |>
  rename(
    state = name_state,
    state_abbr = abbrev_state
  ) |>
  # Pivot to wide format for analysis
  select(-municipality_raw, -crop_raw, -variable) |>
  pivot_wider(
    names_from = variable_id,
    values_from = value,
    values_fill = 0
  ) |>
  # Filter for meaningful data (municipalities with actual production)
  filter(production_tonnes >= 1000) |>
  # Calculate additional metrics
  mutate(
    # Calculate basic yield
    yield_tonnes_per_ha = case_when(
      area_harvested_ha > 0 ~ production_tonnes / area_harvested_ha,
      TRUE ~ NA_real_
    ),
    
    # Add crop classifications
    crop_type = case_when(
      crop %in% c("soybeans", "corn") ~ "annual",
      crop %in% c("sugarcane") ~ "semi-perennial",
      TRUE ~ "other"
    ),
    crop_category = case_when(
      crop %in% c("soybeans", "corn") ~ "grains",
      crop %in% c("sugarcane") ~ "industrial",
      TRUE ~ "other"
    ),
    crop_importance = case_when(
      crop %in% c("soybeans", "sugarcane", "corn") ~ "major",
      TRUE ~ "other"
    ),
    # Production scale categories
    production_scale = case_when(
      production_tonnes >= 1000000 ~ "Very large (1M+ tonnes)",
      production_tonnes >= 100000 ~ "Large (100K-1M tonnes)",
      production_tonnes >= 10000 ~ "Medium (10K-100K tonnes)",
      production_tonnes >= 1000 ~ "Small (1K-10K tonnes)",
      TRUE ~ "Very small (<1K tonnes)"
    ),
    production_scale = factor(production_scale, levels = c(
      "Very large (1M+ tonnes)", "Large (100K-1M tonnes)", "Medium (10K-100K tonnes)",
      "Small (1K-10K tonnes)", "Very small (<1K tonnes)"
    ))
  ) |>
  # Calculate rankings within each crop
  group_by(crop) |>
  mutate(
    production_rank = rank(-production_tonnes, ties.method = "min"),
    area_rank = rank(-area_harvested_ha, ties.method = "min"),
    yield_rank = rank(-yield_tonnes_per_ha, ties.method = "min", na.last = TRUE)
  ) |>
  ungroup() |>
  # Final selection and cleanup
  select(
    municipality_code, municipality, state_code, state, state_abbr, region,
    year, crop, crop_type, crop_category, crop_importance,
    area_harvested_ha, production_tonnes, yield_tonnes_per_ha,
    production_rank, area_rank, yield_rank, production_scale
  ) |>
  arrange(region, state, municipality, crop)

# Create final dataset
brazil_agriculture <- agriculture_clean

usethis::use_data(brazil_agriculture, overwrite = TRUE)