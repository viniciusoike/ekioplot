# EKIO Histogram

Professional histogram with smart aesthetic detection.

## Usage

``` r
ekio_histogram(
  data,
  x,
  fill = NULL,
  palette = NULL,
  bins = "sturges",
  binwidth = NULL,
  add_zero = TRUE,
  border_color = "white",
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

  Variable to plot (supports data-masking)

- fill:

  Fill aesthetic. A color string or variable name. NULL uses EKIO blue.

- palette:

  Character. Palette name for variable mappings.

- bins:

  Binning method: "sturges", "FD", "scott", or numeric.

- binwidth:

  Width of bins (overrides bins if specified)

- add_zero:

  Logical. Add horizontal line at y=0 (default: TRUE)

- border_color:

  Color for histogram outline (default: "white")

- title, subtitle, caption:

  Plot labels. NULL (default) draws none.

- ...:

  Additional arguments passed to
  [`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)

## Value

ggplot2 object

## Examples

``` r
if (FALSE) { # rlang::is_interactive()
DONTSHOW({.op <- options(ekioplot.font_title = "serif", ekioplot.font_text = "sans")})
ekio_histogram(mtcars, mpg)
ekio_histogram(mtcars, mpg, fill = "steelblue")
ekio_histogram(mtcars, mpg, fill = factor(cyl), palette = "cool")
DONTSHOW({options(.op)})
}
```
