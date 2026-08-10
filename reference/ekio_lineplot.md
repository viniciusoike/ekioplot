# EKIO Line Plot

Professional line plot with smart aesthetic detection.

## Usage

``` r
ekio_lineplot(
  data,
  x,
  y,
  color = NULL,
  palette = NULL,
  add_zero = TRUE,
  line_width = 0.8,
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

- color:

  Color aesthetic. A color string or a discrete variable. A continuous
  variable is an error: bin it or wrap it in
  [`factor()`](https://rdrr.io/r/base/factor.html).

- palette:

  Character. Palette name for variable mappings.

- add_zero:

  Logical. Add horizontal line at y=0 (default: TRUE)

- line_width:

  Line thickness (default: 0.8)

- title, subtitle, caption:

  Plot labels. NULL (default) draws none.

- ...:

  Additional arguments passed to
  [`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html).
  These override the recipe's own geom defaults.

## Value

ggplot2 object

## Examples

``` r
if (FALSE) { # rlang::is_interactive()
DONTSHOW({.op <- options(ekioplot.font_title = "serif", ekioplot.font_text = "sans")})
ekio_lineplot(ggplot2::economics, date, unemploy)
DONTSHOW({options(.op)})
}
```
