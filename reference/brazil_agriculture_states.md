# Brazilian state crop production, 1974–2023

Annual production of seven major crops by Brazilian state from IBGE's
Municipal Agricultural Production Survey (PAM).

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

  Observation year (numeric)

- crop:

  Crop name (character)

- production_tonnes:

  Production volume, tonnes (numeric)

- area_harvested_ha:

  Harvested area, hectares (numeric)

- yield_kg_per_ha:

  Productivity, kilograms per hectare (numeric)

- production_value_brl_k:

  Production value, thousands of Brazilian reais (numeric)

## Source

IBGE, Table 1612: Municipal Agricultural Production (PAM).
<https://sidra.ibge.gov.br/tabela/1612>

## Details

Crops are beans, corn, cotton, rice, soybeans, sugarcane and wheat.

## References

IBGE. (2023). Produção Agrícola Municipal - PAM 2022.
