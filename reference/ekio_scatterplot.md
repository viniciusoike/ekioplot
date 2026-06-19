# EKIO Scatter Plot

Professional scatter plot with smart aesthetic detection.

## Usage

``` r
ekio_scatterplot(
  data,
  x,
  y,
  color = NULL,
  size = NULL,
  palette = NULL,
  add_zero = TRUE,
  add_smooth = FALSE,
  smooth_method = "lm",
  point_size = 2.5,
  point_alpha = 0.8,
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

  Color aesthetic. A color string or variable name.

- size:

  Size aesthetic (optional variable)

- palette:

  Character. Palette name for variable mappings.

- add_zero:

  Logical. Add horizontal line at y=0 (default: TRUE)

- add_smooth:

  Logical. Add smooth trend line (default: FALSE)

- smooth_method:

  Smoothing method: "lm", "gam", "loess" (default: "lm")

- point_size:

  Base point size (default: 2.5)

- point_alpha:

  Point transparency (default: 0.8)

- ...:

  Additional arguments passed to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)

## Value

ggplot2 object

## Examples

``` r
if (FALSE) { # rlang::is_interactive()
ekio_scatterplot(mtcars, wt, mpg)
ekio_scatterplot(mtcars, wt, mpg, color = factor(cyl))
}
```
