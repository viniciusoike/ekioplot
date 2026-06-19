# Apply EKIO Theme to ggplot2 Plots

A minimal, professional theme for EKIO visualizations built on
[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Usage

``` r
theme_ekio(base_size = 11, base_family = "", grid = "y")
```

## Arguments

- base_size:

  Numeric. Base font size in points (default: 11)

- base_family:

  Character. Font family. Defaults to the platform-appropriate EKIO font
  via `.get_ekio_font()`.

- grid:

  Character. Which major grid lines to show: `"y"` (default), `"x"`,
  `"xy"`, or `"none"`.

## Value

A ggplot2 theme object
