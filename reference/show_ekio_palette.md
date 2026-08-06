# Display a Palette

Deprecated. Use
[`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md)
in its place.

## Usage

``` r
show_ekio_palette(palette, n = NULL, labels = TRUE)
```

## Arguments

- palette:

  Character or vector. Either a palette name or a vector of hex colors.

- n:

  Integer. Number of colors (used for interpolation on sequential
  palettes).

- labels:

  Logical. Show hex codes as labels (default: TRUE).

## Value

A ggplot2 object (invisibly)

## Examples

``` r
ekio_pal("contrast")
```
