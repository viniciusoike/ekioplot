# Brazilian State-Level Agricultural Production Time Series (1974-2023)

A time series dataset containing agricultural production data for
Brazil's major crops by state, based on IBGE's Municipal Agricultural
Production survey (PAM).

## Usage

``` r
brazil_agriculture_states
```

## Format

A tibble with 9,450 rows and 8 variables:

- code_state:

  IBGE state code (numeric)

- name_state:

  State name (character)

- year:

  Year of observation (1974-2023, numeric)

- crop:

  Crop name in English (character)

- production_tonnes:

  Production quantity in tonnes (numeric)

- area_harvested_ha:

  Area harvested in hectares (numeric)

- yield_kg_per_ha:

  Productivity in kg per hectare (numeric)

- production_value_brl_k:

  Production value in thousands of BRL (numeric)

## Source

IBGE - Instituto Brasileiro de Geografia e Estatística Table 1612: Area,
production, yield and value of agricultural production (PAM - Produção
Agrícola Municipal) <https://www.ibge.gov.br/>

## Details

This dataset provides state-level time series for Brazil's seven most
important crops from 1974 to 2023, enabling analysis of long-term
agricultural trends and regional specialization patterns.

**Crops included:**

- **Soybeans**: Brazil's top agricultural export

- **Corn**: Major grain crop for domestic and export markets

- **Sugarcane**: Industrial crop for sugar and ethanol

- **Cotton**: Key fiber crop and export commodity

- **Rice**: Important food security crop

- **Wheat**: Food grain crop, mainly in southern states

- **Beans**: Traditional protein source and food security crop

**Time coverage:** Nearly 50 years of data (1974-2023) providing
comprehensive historical perspective on Brazilian agricultural
development.

## References

IBGE. (2023). Produção Agrícola Municipal - PAM 2022. Rio de Janeiro:
IBGE.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(brazil_agriculture_states)

# Soybean production trends by top states (recent years)
brazil_agriculture_states |>
  filter(crop == "soybeans", year >= 2010, !is.na(production_tonnes)) |>
  slice_max(production_tonnes, n = 50) |>
  ggplot(aes(year, production_tonnes, color = name_state)) +
  geom_line()

# View structure
str(brazil_agriculture_states)
} # }
```
