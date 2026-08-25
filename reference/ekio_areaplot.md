# EKIO Area Plot

Professional area plot with smart aesthetic detection. Supports stacked
and filled (proportional) area charts.

## Usage

``` r
ekio_areaplot(
  data,
  x,
  y,
  fill = NULL,
  palette = NULL,
  position = "stack",
  alpha = 1,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ...
)
```

## Arguments

- data:

  A data frame

- x:

  X-axis variable (supports data-masking)

- y:

  Y-axis variable (supports data-masking)

- fill:

  Fill aesthetic. A color string or a discrete variable. A continuous
  variable is an error: bin it or wrap it in
  [`factor()`](https://rdrr.io/r/base/factor.html).

- palette:

  Character. Palette name for variable mappings.

- position:

  Character. Stacking method: `"stack"` (default) or `"fill"` for
  proportional areas.

- alpha:

  Numeric. Fill transparency (default: 1).

- title, subtitle, caption:

  Plot labels. NULL (default) draws none.

- ...:

  Additional arguments passed to
  [`ggplot2::geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html).
  These override the recipe's own geom defaults.

## Value

ggplot2 object

## Examples

``` r
ekio_areaplot(ggplot2::economics, date, unemploy)


# Stacked area with normalized economic series
economic_series <- subset(
  ggplot2::economics_long,
  variable %in% c("pce", "psavert", "uempmed")
)
ekio_areaplot(economic_series, date, value01, fill = variable)
```
