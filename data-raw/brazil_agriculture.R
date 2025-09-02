# Brazilian Agricultural Production Data Processing
# Data source: IBGE SIDRA Table 1612 - PAM (Produção Agrícola Municipal)

library(dplyr)
library(geobr)
library(stringr)

import::from(sf, st_drop_geometry)
import::from(sidrar, get_sidra)
import::from(tidyr, pivot_longer, pivot_wider)
import::from(janitor, clean_names)
import::from(here, here)

# Get official Brazilian municipality and state data from geobr
municipalities <- read_municipality(year = 2020, simplified = TRUE)
states <- read_state(year = 2020, simplified = TRUE)

dim_muni <- as_tibble(st_drop_geometry(municipalities))
dim_state <- as_tibble(st_drop_geometry(states))

# Create state reference for region mapping
dim_state <- states |>
  st_drop_geometry() |>
  select(code_state, name_state, abbrev_state, name_region) |>
  mutate(
    # Translate region names to English
    region = case_when(
      name_region == "Norte" ~ "North",
      name_region == "Nordeste" ~ "Northeast",
      name_region == "Centro Oeste" ~ "Mid-West",
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
crop_names_map <- tibble(
  crop_ibge = c(
    "Soja (em grão)",
    "Milho (em grão)",
    "Cana-de-açúcar",
    "Algodão herbáceo (em caroço)",
    "Arroz (em casca)",
    "Trigo (em grão)",
    "Feijão (em grão)"
  ),
  crop_pt = c(
    "Soja",
    "Milho",
    "Cana",
    "Algodão",
    "Arroz",
    "Trigo",
    "Feijão"
  ),
  crop_en = c(
    "soybeans",
    "corn",
    "sugarcane",
    "cotton",
    "rice",
    "wheat",
    "beans"
  )
)

variable_map <- tibble(
  variable_pattern = c(
    "Área colhida",
    "Quantidade produzida",
    "Rendimento médio da produção",
    "Valor da produção"
  ),
  variable_id = c(
    "area_harvested_ha",
    "production_tonnes",
    "yield_kg_per_ha",
    "production_value_brl_k"
  )
)

crop_classifications <- tibble(
  crop = c(
    "soybeans",
    "corn",
    "rice",
    "wheat",
    "beans",
    "cotton",
    "sugarcane"
  ),
  crop_type = c(
    "annual",
    "annual",
    "annual",
    "annual",
    "annual",
    "annual",
    "semi-perennial"
  ),
  crop_category = c(
    "grains",
    "grains",
    "grains",
    "grains",
    "grains",
    "fiber",
    "industrial"
  ),
  crop_importance = c(
    "major",
    "major",
    "food-security",
    "food-security",
    "food-security",
    "high-value",
    "major"
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
  period = c("1974-2023"),
  geo = "State",
  classific = "c81",
  # All major crops
  category = list(
    c81 = c("2713", "2711", "2696", "2689", "2692", "2716", "2702")
  )
)

# Clean and standardize raw data
agri_muni <- raw_agriculture |>
  clean_names() |>
  as_tibble() |>
  select(
    code_muni = municipio_codigo,
    year = ano,
    variable = variavel,
    crop_ibge = produto_das_lavouras_temporarias,
    value = valor
  )

agri_muni <- agri_muni |>
  # Convert columns
  mutate(
    code_muni = as.numeric(code_muni),
    year = as.numeric(year)
  ) |>
  # Join with crop names
  left_join(crop_names_map, by = "crop_ibge") |>
  # Join with variable names
  left_join(variable_map, by = c("variable" = "variable_pattern")) |>
  # Join with city identifiers
  left_join(dim_muni, by = "code_muni")

# fmt: skip
agri_wide <- agri_muni |>
  pivot_wider(
    id_cols = c(
      "code_muni", "name_muni", "name_state", "name_region", "crop_en"
    ),
    names_from = "variable_id",
    values_from = "value"
  ) |>
  # Add yield column
  mutate(yield = production_tonnes / area_harvested_ha)

agri_wide <- left_join(
  agri_wide,
  crop_classifications,
  by = c("crop_en" = "crop")
)

# Helper function to classify data according to Jenks algorithm
get_jenks_breaks <- function(x, k = 7) {
  breaks <- BAMMtools::getJenksBreaks(x, k = 7)
  groups <- findInterval(x, breaks)
  return(groups)
}

# Classify production scale
agri_wide <- agri_wide |>
  mutate(
    production_scale = get_jenks_breaks(production_tonnes, k = 7),
    .by = "crop_en"
  )


# State-level data processing - Step 1: Base cleaning
states_base <- raw_agriculture_states |>
  clean_names() |>
  as_tibble() |>
  select(
    code_state = unidade_da_federacao_codigo,
    name_state = unidade_da_federacao,
    year = ano,
    variable = variavel,
    crop_ibge = produto_das_lavouras_temporarias,
    value = valor
  )

# Step 2: Basic transformations
states_transformed <- states_base |>
  mutate(
    year = as.numeric(year),
    value = as.numeric(value),
    code_state = as.numeric(code_state)
  )

# Step 3: Join with identfiers and convert to wide
states_clean <- states_transformed |>
  left_join(crop_names_map, by = "crop_ibge") |>
  left_join(variable_map, by = c("variable" = "variable_pattern")) |>
  pivot_wider(
    id_cols = c("code_state", "name_state", "year", "crop_en"),
    names_from = "variable_id",
    values_from = "value"
  )

agri_wide <- rename(agri_wide, crop = crop_en)
states_clean <- rename(states_clean, crop = crop_en)

# Create final datasets
brazil_agriculture <- agri_wide
brazil_agriculture_states <- states_clean

usethis::use_data(brazil_agriculture, overwrite = TRUE)
usethis::use_data(brazil_agriculture_states, overwrite = TRUE)
