# EKIO Bar Plot

Professional bar plot with smart aesthetic detection.

## Usage

``` r
ekio_barplot(
  data,
  x,
  y,
  fill = NULL,
  palette = NULL,
  add_zero = TRUE,
  horizontal = FALSE,
  bar_width = 0.8,
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

  Fill aesthetic. A color string or variable name.

- palette:

  Character. Palette name for variable mappings.

- add_zero:

  Logical. Add horizontal line at y=0 (default: TRUE)

- horizontal:

  Logical. Create horizontal bar plot (default: FALSE)

- bar_width:

  Bar width (default: 0.8)

- ...:

  Additional arguments passed to
  [`ggplot2::geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.html)

## Value

ggplot2 object

## Examples

``` r
if (FALSE) { # rlang::is_interactive()
cyl_counts <- as.data.frame(table(cyl = mtcars$cyl))
names(cyl_counts)[2] <- "n"
ekio_barplot(cyl_counts, cyl, n)
}
```
