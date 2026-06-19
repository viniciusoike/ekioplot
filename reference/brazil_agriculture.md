# Brazilian Municipal Agricultural Production (2022)

A comprehensive dataset containing agricultural production data for
Brazil's three major crops by municipality, based on IBGE's Municipal
Agricultural Production survey (PAM - Produção Agrícola Municipal).

## Usage

``` r
brazil_agriculture
```

## Format

A tibble with 16,689 rows and 12 variables:

- code_muni:

  IBGE municipality code (numeric)

- name_muni:

  Municipality name (character)

- name_state:

  State name (character)

- name_region:

  Region name in Portuguese (character)

- crop:

  Crop name in English (character)

- production_tonnes:

  Production quantity in tonnes (numeric)

- area_harvested_ha:

  Area harvested in hectares (numeric)

- yield:

  Calculated productivity in tonnes per hectare (numeric)

- crop_type:

  Crop cultivation type: annual, semi-perennial (character)

- crop_category:

  Crop category: grains, industrial (character)

- crop_importance:

  Economic importance: major (character)

- production_scale:

  Production scale category (integer)

## Source

IBGE - Instituto Brasileiro de Geografia e Estatística Table 1612: Area,
production, yield and value of agricultural production (PAM - Produção
Agrícola Municipal) <https://www.ibge.gov.br/>

## Details

The dataset covers Brazil's three major crops by municipality in 2022:
soybeans, corn, and sugarcane. This represents municipal-level
agricultural production data for analysis of regional agricultural
patterns.

**Crops included:**

- **Soybeans**: Brazil's top agricultural export (annual grain crop)

- **Corn**: Major grain crop for domestic and export markets (annual)

- **Sugarcane**: Industrial crop for sugar and ethanol (semi-perennial)

**Geographic coverage:** All Brazilian municipalities with production
data for these crops across all regions.

## References

IBGE. (2023). Produção Agrícola Municipal - PAM 2022. Rio de Janeiro:
IBGE.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(brazil_agriculture)

# View top soybean producing municipalities
brazil_agriculture |>
  filter(crop == "soybeans") |>
  slice_max(production_tonnes, n = 10)

# Check available crops
unique(brazil_agriculture$crop)

# View structure
str(brazil_agriculture)
} # }
```
