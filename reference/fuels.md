# Global Fuel Consumption by Source

Historical primary energy consumption data by fuel type and entity,
spanning from 1800 to the present.

## Usage

``` r
fuels
```

## Format

A tibble with 4 variables:

- entity:

  Country, region, or aggregated group name (character)

- year:

  Year of observation (numeric)

- fuel:

  Fuel type: `"gas"`, `"oil"`, or `"coal"` (character)

- consumption_gwh:

  Primary energy consumption in gigawatt-hours (numeric)

## Source

Our World in Data — Energy <https://ourworldindata.org/energy>

## Examples

``` r
if (FALSE) { # \dontrun{
data(fuels)

# Global consumption over time by fuel type
fuels |>
  dplyr::filter(entity == "World") |>
  ggplot2::ggplot(ggplot2::aes(year, consumption_gwh, color = fuel)) +
  ggplot2::geom_line()
} # }
```
