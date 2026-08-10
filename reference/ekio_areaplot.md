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
  alpha = 0.8,
  add_zero = TRUE,
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

  Numeric. Fill transparency (default: 0.8).

- add_zero:

  Logical. Add horizontal line at y=0 (default: TRUE).

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
if (FALSE) { # rlang::is_interactive()
DONTSHOW({.op <- options(ekioplot.font_title = "serif", ekioplot.font_text = "sans")})
ekio_areaplot(ggplot2::economics, date, unemploy)

# Stacked area with groups
data(fuels)
world_fuels <- fuels[fuels$entity == "World" & fuels$year >= 1950, ]
ekio_areaplot(world_fuels, year, consumption_gwh, fill = fuel)
DONTSHOW({options(.op)})
}
```
