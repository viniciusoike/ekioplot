library(dplyr)
library(sidrar)
library(geobr)
library(sf)
library(stringr)
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
  "2713", # Soybeans (Soja em grão)
  "2711", # Corn (Milho em grão)
  "2696", # Sugarcane (Cana-de-açúcar)
  "2689", # Cotton (Algodão herbáceo em caroço)
  "2692", # Rice (Arroz em casca)
  "2716", # Wheat (Trigo em grão)
  "2702" # Beans (Feijão em grão)
)

# Create auxiliary tibbles for data classification
crop_names_map <- tibble::tibble(
  crop_raw_pattern = c(
    "Soja", "Milho", "Cana", "Algodão", 
    "Arroz", "Trigo", "Feijão"
  ),
  crop_clean = c(
    "soybeans", "corn", "sugarcane", "cotton",
    "rice", "wheat", "beans"
  )
)

variable_map <- tibble::tibble(
  variable_pattern = c(
    "Área colhida", "Quantidade produzida", 
    "Rendimento", "Valor"
  ),
  variable_id = c(
    "area_harvested_ha", "production_tonnes",
    "yield_kg_per_ha", "production_value_brl_thousands"
  )
)

crop_classifications <- tibble::tibble(
  crop = c(
    "soybeans", "corn", "rice", "wheat", "beans", "cotton", "sugarcane"
  ),
  crop_type = c(
    "annual", "annual", "annual", "annual", "annual", "annual", "semi-perennial"
  ),
  crop_category = c(
    "grains", "grains", "grains", "grains", "grains", "fiber", "industrial"
  ),
  crop_importance = c(
    "major", "major", "food-security", "food-security", "food-security", "high-value", "major"
  )
)

# Get agricultural production data (2022) - most recent year only
# Using table 1612 - PAM (Produção Agrícola Municipal)
# Reduce to fewer variables and crops to avoid API limits
raw_agriculture <- get_sidra(
  x = 1612,
  variable = c(
    214, # Quantidade produzida (toneladas)
    216 # Área colhida (hectares)
  ),
  period = "2022",
  geo = "City",
  classific = "c81",
  category = list(c81 = c("2713", "2711", "2696")) # Just soybeans, corn, sugarcane
)

# Get state-level time series data (2018-2022) for the same crops
raw_agriculture_states <- get_sidra(
  x = 1612,
  variable = c(
    214, # Quantidade produzida (toneladas)
    216, # Área colhida (hectares)
    112, # Rendimento médio (kg/ha)
    215 # Valor da produção (mil reais)
  ),
  period = c("2018", "2019", "2020", "2021", "2022"),
  geo = "State",
  classific = "c81",
  category = list(
    c81 = c("2713", "2711", "2696", "2689", "2692", "2716", "2702")
  ) # All major crops
)

# Step 1: Clean and standardize raw data
agriculture_base <- raw_agriculture |>
  clean_names() |>
  select(
    code_muni = municipio_codigo,
    municipality_raw = municipio,
    year = ano,
    variable = variavel,
    crop_raw = produto_das_lavouras_temporarias,
    value = valor
  )

# Step 2: Add basic transformations
agriculture_transformed <- agriculture_base |>
  mutate(
    year = as.numeric(year),
    value = as.numeric(value),
    state_code = as.numeric(substr(code_muni, 1, 2)),
    name_muni = str_remove(municipality_raw, "\\s*\\([A-Z]{2}\\)\\s*$")
  )

# Step 3: Map crop names using auxiliary tibble
agriculture_with_crops <- agriculture_transformed
for (i in seq_len(nrow(crop_names_map))) {
  pattern <- crop_names_map$crop_raw_pattern[i]
  clean_name <- crop_names_map$crop_clean[i]
  agriculture_with_crops <- agriculture_with_crops |>
    mutate(
      crop = ifelse(
        str_detect(crop_raw, regex(pattern, ignore_case = TRUE)),
        clean_name,
        ifelse(exists("crop"), crop, str_to_lower(crop_raw))
      )
    )
}

# Step 4: Map variable identifiers
agriculture_with_vars <- agriculture_with_crops
for (i in seq_len(nrow(variable_map))) {
  pattern <- variable_map$variable_pattern[i]
  var_id <- variable_map$variable_id[i]
  agriculture_with_vars <- agriculture_with_vars |>
    mutate(
      variable_id = ifelse(
        str_detect(variable, regex(pattern, ignore_case = TRUE)),
        var_id,
        ifelse(exists("variable_id"), variable_id, "other")
      )
    )
}

# Step 5: Filter valid data
agriculture_filtered <- agriculture_with_vars |>
  filter(
    !is.na(value),
    value > 0,
    !(variable_id == "production_tonnes" & value < 1000)
  )

# Step 6: Add geographic information
agriculture_with_geo <- agriculture_filtered |>
  left_join(state_ref, by = c("state_code" = "code_state")) |>
  select(-municipality_raw, -crop_raw, -variable)

# Step 7: Pivot to wide format
agriculture_wide <- agriculture_with_geo |>
  group_by(code_muni, year, state_code, name_muni, crop, name_state, abbrev_state, name_region, region, variable_id) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = variable_id,
    values_from = value,
    values_fill = 0
  ) |>
  filter(production_tonnes >= 1000)

# Step 8: Ensure all expected columns exist
if (!"area_harvested_ha" %in% names(agriculture_wide)) {
  agriculture_wide$area_harvested_ha <- 0
}
if (!"production_tonnes" %in% names(agriculture_wide)) {
  agriculture_wide$production_tonnes <- 0
}

# Add crop classifications using auxiliary tibble
agriculture_classified <- agriculture_wide |>
  left_join(crop_classifications, by = "crop")

# Step 9: Calculate metrics and scales
agriculture_with_metrics <- agriculture_classified |>
  mutate(
    yield_tonnes_per_ha = ifelse(
      area_harvested_ha > 0,
      production_tonnes / area_harvested_ha,
      NA_real_
    ),
    production_scale = case_when(
      production_tonnes >= 1000000 ~ "Very large (1M+ tonnes)",
      production_tonnes >= 100000 ~ "Large (100K-1M tonnes)",
      production_tonnes >= 10000 ~ "Medium (10K-100K tonnes)",
      production_tonnes >= 1000 ~ "Small (1K-10K tonnes)",
      TRUE ~ "Very small (<1K tonnes)"
    )
  )

# Step 10: Add factor levels and rankings
agriculture_clean <- agriculture_with_metrics |>
  mutate(
    production_scale = factor(
      production_scale,
      levels = c(
        "Very large (1M+ tonnes)",
        "Large (100K-1M tonnes)",
        "Medium (10K-100K tonnes)",
        "Small (1K-10K tonnes)",
        "Very small (<1K tonnes)"
      )
    )
  ) |>
  group_by(crop) |>
  mutate(
    production_rank = rank(-production_tonnes, ties.method = "min"),
    area_rank = rank(-area_harvested_ha, ties.method = "min"),
    yield_rank = rank(-yield_tonnes_per_ha, ties.method = "min", na.last = TRUE)
  ) |>
  ungroup() |>
  select(
    code_muni, name_muni, state_code, name_state, abbrev_state,
    region, year, crop, crop_type, crop_category, crop_importance,
    area_harvested_ha, production_tonnes, yield_tonnes_per_ha,
    production_rank, area_rank, yield_rank, production_scale
  ) |>
  rename(
    state = name_state,
    state_abbr = abbrev_state
  ) |>
  arrange(region, state, name_muni, crop)

# State-level data processing - Step 1: Base cleaning
states_base <- raw_agriculture_states |>
  clean_names() |>
  select(
    state_code = unidade_da_federacao_codigo,
    state_raw = unidade_da_federacao,
    year = ano,
    variable = variavel,
    crop_raw = produto_das_lavouras_temporarias,
    value = valor
  )

# Step 2: Basic transformations
states_transformed <- states_base |>
  mutate(
    year = as.numeric(year),
    value = as.numeric(value),
    state_code = as.numeric(state_code),
    state = str_remove(state_raw, "\\s*\\([A-Z]{2}\\)\\s*$")
  )

# Step 3: Map crop names using auxiliary tibble
states_with_crops <- states_transformed
for (i in seq_len(nrow(crop_names_map))) {
  pattern <- crop_names_map$crop_raw_pattern[i]
  clean_name <- crop_names_map$crop_clean[i]
  states_with_crops <- states_with_crops |>
    mutate(
      crop = ifelse(
        str_detect(crop_raw, regex(pattern, ignore_case = TRUE)),
        clean_name,
        ifelse(exists("crop"), crop, str_to_lower(crop_raw))
      )
    )
}

# Step 4: Map variable identifiers
states_with_vars <- states_with_crops
for (i in seq_len(nrow(variable_map))) {
  pattern <- variable_map$variable_pattern[i]
  var_id <- variable_map$variable_id[i]
  states_with_vars <- states_with_vars |>
    mutate(
      variable_id = ifelse(
        str_detect(variable, regex(pattern, ignore_case = TRUE)),
        var_id,
        ifelse(exists("variable_id"), variable_id, "other")
      )
    )
}

# Step 5: Filter valid data
states_filtered <- states_with_vars |>
  filter(
    !is.na(value),
    value > 0,
    !(variable_id == "production_tonnes" & value < 10000)
  )

# Step 6: Add geographic information
states_with_geo <- states_filtered |>
  left_join(state_ref, by = c("state_code" = "code_state")) |>
  select(-state_raw, -crop_raw, -variable)

# Step 7: Pivot to wide format
states_wide <- states_with_geo |>
  group_by(state_code, year, crop, name_state, abbrev_state, name_region, region, variable_id) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = variable_id,
    values_from = value,
    values_fill = 0
  ) |>
  filter(production_tonnes >= 10000)

# Step 8: Ensure all expected columns exist
if (!"area_harvested_ha" %in% names(states_wide)) {
  states_wide$area_harvested_ha <- 0
}
if (!"production_tonnes" %in% names(states_wide)) {
  states_wide$production_tonnes <- 0
}
if (!"yield_kg_per_ha" %in% names(states_wide)) {
  states_wide$yield_kg_per_ha <- 0
}
if (!"production_value_brl_thousands" %in% names(states_wide)) {
  states_wide$production_value_brl_thousands <- 0
}

# Add crop classifications
states_classified <- states_wide |>
  left_join(crop_classifications, by = "crop")

# Step 9: Calculate basic metrics
states_with_metrics <- states_classified |>
  mutate(
    yield_tonnes_per_ha = case_when(
      yield_kg_per_ha > 0 ~ yield_kg_per_ha / 1000,
      area_harvested_ha > 0 ~ production_tonnes / area_harvested_ha,
      TRUE ~ NA_real_
    ),
    production_value_brl_millions = production_value_brl_thousands / 1000,
    value_per_ha_brl = ifelse(
      area_harvested_ha > 0,
      (production_value_brl_thousands * 1000) / area_harvested_ha,
      NA_real_
    ),
    value_per_tonne_brl = ifelse(
      production_tonnes > 0,
      (production_value_brl_thousands * 1000) / production_tonnes,
      NA_real_
    )
  )

# Step 10: Add production scale categories
states_with_scale <- states_with_metrics |>
  mutate(
    production_scale = case_when(
      production_tonnes >= 10000000 ~ "Very large (10M+ tonnes)",
      production_tonnes >= 1000000 ~ "Large (1M-10M tonnes)",
      production_tonnes >= 100000 ~ "Medium (100K-1M tonnes)",
      production_tonnes >= 10000 ~ "Small (10K-100K tonnes)",
      TRUE ~ "Very small (<10K tonnes)"
    ),
    production_scale = factor(
      production_scale,
      levels = c(
        "Very large (10M+ tonnes)",
        "Large (1M-10M tonnes)",
        "Medium (100K-1M tonnes)",
        "Small (10K-100K tonnes)",
        "Very small (<10K tonnes)"
      )
    )
  )

# Step 11: Calculate growth rates
states_with_growth <- states_with_scale |>
  group_by(state_code, crop) |>
  arrange(year) |>
  mutate(
    production_growth_rate = ifelse(
      year > 2018 & !is.na(lag(production_tonnes)) & lag(production_tonnes) > 0,
      (production_tonnes - lag(production_tonnes)) / lag(production_tonnes) * 100,
      NA_real_
    ),
    area_growth_rate = ifelse(
      year > 2018 & !is.na(lag(area_harvested_ha)) & lag(area_harvested_ha) > 0,
      (area_harvested_ha - lag(area_harvested_ha)) / lag(area_harvested_ha) * 100,
      NA_real_
    ),
    value_growth_rate = ifelse(
      year > 2018 & !is.na(lag(production_value_brl_millions)) & lag(production_value_brl_millions) > 0,
      (production_value_brl_millions - lag(production_value_brl_millions)) / lag(production_value_brl_millions) * 100,
      NA_real_
    ),
    yield_growth_rate = ifelse(
      year > 2018 & !is.na(lag(yield_tonnes_per_ha)) & lag(yield_tonnes_per_ha) > 0,
      (yield_tonnes_per_ha - lag(yield_tonnes_per_ha)) / lag(yield_tonnes_per_ha) * 100,
      NA_real_
    ),
    production_change_2018_2022 = ifelse(
      year == 2022 & first(production_tonnes) > 0,
      (production_tonnes - first(production_tonnes)) / first(production_tonnes) * 100,
      NA_real_
    )
  ) |>
  ungroup()

# Step 12: Add rankings and finalize
agriculture_states_clean <- states_with_growth |>
  group_by(crop, year) |>
  mutate(
    production_rank = rank(-production_tonnes, ties.method = "min"),
    area_rank = rank(-area_harvested_ha, ties.method = "min"),
    value_rank = rank(-production_value_brl_millions, ties.method = "min"),
    yield_rank = rank(-yield_tonnes_per_ha, ties.method = "min", na.last = TRUE)
  ) |>
  ungroup() |>
  select(
    state_code, name_state, abbrev_state, region, year, crop,
    crop_type, crop_category, crop_importance, area_harvested_ha,
    production_tonnes, yield_tonnes_per_ha, yield_kg_per_ha,
    production_value_brl_millions, production_value_brl_thousands,
    value_per_ha_brl, value_per_tonne_brl, production_growth_rate,
    area_growth_rate, value_growth_rate, yield_growth_rate,
    production_change_2018_2022, production_rank, area_rank,
    value_rank, yield_rank, production_scale
  ) |>
  rename(
    state = name_state,
    state_abbr = abbrev_state
  ) |>
  arrange(region, state, crop, year)

# Create final datasets
brazil_agriculture <- agriculture_clean
brazil_agriculture_states <- agriculture_states_clean

usethis::use_data(brazil_agriculture, overwrite = TRUE)
usethis::use_data(brazil_agriculture_states, overwrite = TRUE)
