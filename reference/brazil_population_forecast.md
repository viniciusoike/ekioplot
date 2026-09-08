# Brazilian population projections by age and sex, 2025–2100

Population projections for Brazil at five-year intervals, by five-year
age group and sex. The data are a transformed subset of the United
Nations World Population Prospects 2024, Online Edition.

## Usage

``` r
brazil_population_forecast
```

## Format

A tibble with 336 rows and 15 variables:

- country_code:

  WPP country or area code (numeric)

- name:

  Country or area name (character)

- year:

  Projection year (integer)

- age:

  Five-year age band, from 0–4 to 100+ (ordered factor)

- ibge_age_group:

  Age group derived by ekioplot: 0–14, 15–64 or 65+ (ordered factor)

- age_group:

  Age group derived by ekioplot: 0–19, 20–29, 30–39, 40–49, 50–59,
  60–69, 70–79 or 80+ (ordered factor)

- population_m:

  Medium-variant projected male population, in persons (numeric)

- population_f:

  Medium-variant projected female population, in persons (numeric)

- population:

  Medium-variant projected total population, in persons (numeric)

- population_m_low:

  Low-variant projected male population, in persons (numeric)

- population_m_high:

  High-variant projected male population, in persons (numeric)

- population_f_low:

  Low-variant projected female population, in persons (numeric)

- population_f_high:

  High-variant projected female population, in persons (numeric)

- population_low:

  Low-variant projected total population, in persons (numeric)

- population_high:

  High-variant projected total population, in persons (numeric)

## Source

United Nations, Department of Economic and Social Affairs, Population
Division (2024). *World Population Prospects 2024, Online Edition*.
<https://www.un.org/development/desa/pd/content/world-population-prospects-2024>

## Details

The dataset contains 16 projection years (2025–2100) and 21 age bands.
WPP reports values in thousands; ekioplot converts them to persons and
rounds them to the nearest person. The two aggregate age-group variables
are derived by ekioplot from the WPP five-year bands.

The `*_low` and `*_high` columns are WPP low- and high-fertility
variants, respectively. They are alternative scenarios, not probability
intervals.

Data © United Nations, 2024. Source data are made available under [CC BY
3.0 IGO](https://creativecommons.org/licenses/by/3.0/igo/). This is a
transformed subset prepared by ekioplot; the United Nations does not
endorse ekioplot or this dataset.

## References

United Nations Population Division (2024). *wpp2024: World Population
Prospects 2024*. R package. <https://github.com/PPgp/wpp2024>
