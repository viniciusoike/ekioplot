# Continuous Color Scale

Apply sequential or diverging palettes to continuous/numeric data.

## Usage

``` r
scale_color_ekio_c(palette = "blue", reverse = FALSE, ...)

scale_colour_ekio_c(palette = "blue", reverse = FALSE, ...)

scale_fill_ekio_c(palette = "blue", reverse = FALSE, ...)
```

## Arguments

- palette:

  Character. Palette name (default: "blue"). See
  `list_ekio_palettes("sequential")` and
  `list_ekio_palettes("diverging")` for options.

- reverse:

  Logical. If TRUE, reverses the color order.

- ...:

  Additional arguments passed to
  [`ggplot2::scale_color_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)

## Value

A ggplot2 scale object

## See also

`scale_fill_ekio_c()`,
[`list_ekio_palettes()`](https://viniciusoike.github.io/ekioplot/reference/list_ekio_palettes.md)

## Examples

``` r
if (FALSE) { # rlang::is_interactive()
library(ggplot2)
ggplot(mtcars, aes(wt, mpg, color = hp)) +
  geom_point(size = 3) +
  scale_color_ekio_c()

ggplot(mtcars, aes(wt, mpg, color = hp)) +
  geom_point(size = 3) +
  scale_color_ekio_c("purple")
}
```
