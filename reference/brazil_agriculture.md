# Brazilian municipal crop production, 2022

Municipal production of soybeans, corn and sugarcane from IBGE's
Municipal Agricultural Production Survey (PAM).

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

  Brazilian region (character)

- crop:

  Crop name (character)

- production_tonnes:

  Production volume, tonnes (numeric)

- area_harvested_ha:

  Harvested area, hectares (numeric)

- yield:

  Productivity, tonnes per hectare (numeric)

- crop_type:

  Crop cycle (character)

- crop_category:

  Crop category (character)

- crop_importance:

  Economic importance (character)

- production_scale:

  Production-scale category (integer)

## Source

IBGE, Table 1612: Municipal Agricultural Production (PAM).
<https://sidra.ibge.gov.br/tabela/1612>

## References

IBGE. (2023). Produção Agrícola Municipal - PAM 2022.
