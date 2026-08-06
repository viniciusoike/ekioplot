# Get Color Palette

Returns colors for data visualization. Includes EKIO brand palettes,
curated small-group variants, and standard scientific palettes. When
printed interactively, displays the palette as a colored swatch with hex
labels.

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

Object of class `ekio_palette` (a character vector of hex codes).
Printing displays a visual swatch. Use
[`as.character()`](https://rdrr.io/r/base/character.html) to strip the
class.

## Examples

``` r
ekio_pal("contrast")

ekio_pal("contrast", n = 4)

ekio_pal("binary", reverse = TRUE)

ekio_pal("okabe_ito")
```
