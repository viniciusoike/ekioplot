# Apply EKIO Theme to GT Tables

Professional EKIO branding and styling for gt table objects.

## Usage

``` r
gt_theme_ekio(
  data,
  table_width = "100%",
  font_size = 14,
  stripe = TRUE,
  add_footer = TRUE
)
```

## Arguments

- data:

  A gt table object

- table_width:

  Character. Width of the table (default: "100%")

- font_size:

  Numeric. Base font size in pixels (default: 14)

- stripe:

  Logical. Apply alternating row striping (default: TRUE)

- add_footer:

  Logical. Add automatic EKIO footer (default: TRUE)

## Value

A styled gt table object

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)
head(mtcars, 10) |>
  gt() |>
  gt_theme_ekio()
} # }
```
