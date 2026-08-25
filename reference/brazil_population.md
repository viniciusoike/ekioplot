# Brazilian municipal population, 2025

Population estimates for Brazil's 338 municipalities with more than
100,000 inhabitants.

## Usage

``` r
brazil_population
```

## Format

A tibble with 338 rows and 5 variables:

- rank:

  Population rank (numeric)

- name_muni:

  Municipality name (character)

- abbrev_state:

  State abbreviation (character)

- population:

  2025 population estimate (numeric)

- category:

  Ordered city-size category (factor)

## Source

IBGE, Table 6579: Municipal population estimates.
<https://sidra.ibge.gov.br/tabela/6579>

## Details

Categories are Metropolis (1M+), Large city (500K–1M), Medium city
(200K–500K) and Small city (100K–200K).
