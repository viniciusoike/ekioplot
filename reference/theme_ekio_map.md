# Apply EKIO Map Theme to ggplot2 Plots

A variant of
[`theme_ekio()`](https://viniciusoike.github.io/ekioplot/reference/theme_ekio.md)
with axes and grid removed, suited for choropleth and spatial maps.

## Usage

``` r
theme_ekio_map(base_size = 11, base_family = "")
```

## Arguments

- base_size:

  Numeric. Base font size in points (default: 11)

- base_family:

  Character. Font family. Defaults to the platform-appropriate EKIO font
  via `.get_ekio_font()`.

## Value

A ggplot2 theme object
