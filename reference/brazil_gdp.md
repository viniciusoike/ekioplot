# Brazilian Municipal GDP and Economic Structure (2021)

A dataset containing municipal GDP data for all Brazilian municipalities
based on IBGE's Municipal National Accounts (Contas Nacionais
Municipais).

## Usage

``` r
brazil_gdp
```

## Format

A tibble with 5,570 rows and 7 variables:

- code_muni:

  IBGE municipality code (numeric)

- name_muni:

  Municipality name (character)

- code_state:

  IBGE state code (numeric)

- name_state:

  State name (character)

- year:

  Year of observation (2021, numeric)

- gdp_brl_k:

  GDP in thousands of Brazilian reais (numeric)

- gdp_brl_m:

  GDP in millions of Brazilian reais (numeric)

## Source

IBGE - Instituto Brasileiro de Geografia e Estatística Table 5938: GDP
and other aggregates by municipality (Contas Nacionais Municipais)
<https://www.ibge.gov.br/>

## Details

The dataset includes GDP data for all 5,570 Brazilian municipalities for
2021, the most recent year available in IBGE's Municipal National
Accounts. GDP values are provided in both thousands (gdp_brl_k) and
millions (gdp_brl_m) of Brazilian reais at current prices.

## References

IBGE. (2023). Produto Interno Bruto dos Municípios - 2021. Rio de
Janeiro: IBGE.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(brazil_gdp)

# Top 10 municipalities by GDP in 2021
brazil_gdp |>
  slice_max(gdp_brl_m, n = 10)

# View structure
str(brazil_gdp)
} # }
```
