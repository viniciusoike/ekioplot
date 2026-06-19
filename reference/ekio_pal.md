# Get Color Palette

Returns colors for data visualization. Includes EKIO brand palettes,
curated small-group variants, and standard scientific palettes.

## Usage

``` r
ekio_pal(palette = "contrast", n = NULL, reverse = FALSE)
```

## Arguments

- palette:

  Character. Name of the palette. See
  [`list_ekio_palettes()`](https://viniciusoike.github.io/ekioplot/reference/list_ekio_palettes.md)
  for all available options.

- n:

  Integer or NULL. Number of colors to return. If NULL, returns all.

- reverse:

  Logical. If TRUE, reverses the palette order.

## Value

Character vector of hex color codes

## Examples

``` r
ekio_pal("contrast")
#> [1] "#1E3A5F" "#DD6B20" "#2C7A7B" "#D69E2E" "#805AD5" "#C53030"
ekio_pal("contrast", n = 4)
#> [1] "#1E3A5F" "#DD6B20" "#2C7A7B" "#D69E2E"
ekio_pal("binary", reverse = TRUE)
#> [1] "#DD6B20" "#1E3A5F"
ekio_pal("okabe_ito")
#> [1] "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7"
#> [8] "#000000"
```
