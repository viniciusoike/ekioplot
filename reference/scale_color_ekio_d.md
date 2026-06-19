# Discrete Color Scale

Apply qualitative palettes to discrete/categorical data.

## Usage

``` r
scale_color_ekio_d(palette = "contrast", reverse = FALSE, ...)

scale_colour_ekio_d(palette = "contrast", reverse = FALSE, ...)

scale_fill_ekio_d(palette = "contrast", reverse = FALSE, ...)
```

## Arguments

- palette:

  Character. Palette name (default: "contrast"). See
  [`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md)
  for options.

- reverse:

  Logical. If TRUE, reverses the palette order.

- ...:

  Additional arguments passed to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)

## Value

A ggplot2 scale object

## See also

[`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md),
`scale_fill_ekio_d()`

## Examples

``` r
if (FALSE) { # rlang::is_interactive()
library(ggplot2)
ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_ekio_d()
}
```
