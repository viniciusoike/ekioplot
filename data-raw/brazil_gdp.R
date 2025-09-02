# Brazilian Municipal GDP Data Processing
# Data source: IBGE SIDRA Table 5938 - Municipal GDP (2021)

library(dplyr)
library(geobr)

import::from(sf, st_drop_geometry)
import::from(sidrar, get_sidra)
import::from(janitor, clean_names)
import::from(here, here)

# Get official Brazilian municipality data from geobr
municipalities <- read_municipality(year = 2022, simplified = TRUE)
dim_muni <- as_tibble(st_drop_geometry(municipalities))

# Get municipal GDP data (2021) - most recent year only
# Using table 5938 - Contas Nacionais Municipais
raw_gdp <- get_sidra(
  x = 5938,
  variable = 37, # PIB total (Produto Interno Bruto a preços correntes)
  period = "2021",
  geo = "City"
)

# Clean and standardize raw data
gdp_base <- raw_gdp |>
  clean_names() |>
  as_tibble() |>
  select(
    code_muni = municipio_codigo,
    year = ano,
    gdp_brl_k = valor
  )

# Convert columns and join with city identifiers
gdp_clean <- gdp_base |>
  mutate(
    code_muni = as.numeric(code_muni),
    year = as.numeric(year)
  ) |>
  left_join(dim_muni, by = "code_muni")

# Format table
brazil_gdp <- gdp_clean |>
  # Create GDP in millions
  mutate(gdp_brl_m = gdp_brl_k / 1000) |>
  # Select columns
  select(
    code_muni,
    name_muni,
    code_state,
    name_state,
    year,
    gdp_brl_k,
    gdp_brl_m
  )

usethis::use_data(brazil_gdp, overwrite = TRUE)
