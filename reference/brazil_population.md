# Brazilian Municipal Population Data (2025)

A dataset containing population data for Brazilian municipalities with
over 100,000 inhabitants, based on IBGE population estimates for 2025.

## Usage

``` r
brazil_population
```

## Format

A tibble with 338 rows and 5 variables:

- rank:

  Population ranking among all municipalities (numeric)

- name_muni:

  Municipality name (character)

- abbrev_state:

  State abbreviation (character)

- population:

  Total population in 2025 (numeric)

- category:

  City size category based on population (ordered factor)

## Source

IBGE - Instituto Brasileiro de Geografia e Estatística Table 6579:
Municipal population estimates <https://www.ibge.gov.br/>

## Details

The dataset focuses on Brazil's 338 largest municipalities (population
\> 100,000) and provides population ranking and size classification for
2025.

**City size categories:**

- **Metropolis (1M+)**: 1 million+ inhabitants

- **Large city (500K-1M)**: 500,000 to 1 million inhabitants

- **Medium city (200K-500K)**: 200,000 to 500,000 inhabitants

- **Small city (100K-200K)**: 100,000 to 200,000 inhabitants

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(brazil_population)

# View the largest cities
head(brazil_population)

# Count cities by category
table(brazil_population$category)
} # }
```
