# Display a Palette

Visualizes a color palette as a horizontal bar chart with hex labels.

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
if (FALSE) { # rlang::is_interactive()
show_ekio_palette("contrast")
show_ekio_palette(c("#1E3A5F", "#DD6B20", "#2C7A7B"))
}
```
