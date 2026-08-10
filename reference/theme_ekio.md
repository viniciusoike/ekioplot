# Apply EKIO Theme to ggplot2 Plots

A minimal, professional theme for EKIO visualizations built on
[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Usage

``` r
theme_ekio(
  base_size = 11,
  font_title = "Lora",
  font_text = "Lato",
  title_align = "plot",
  grid = "y",
  ticks = "x",
  background = "offwhite",
  ...
)
```

## Arguments

- base_size:

  Numeric. Base font size in points (default: 11).

- font_title:

  Character. Font family passed only to the chart title. Defaults to
  'Lora'.

- font_text:

  Character. Font family passed to all textual elements except the
  title. Defaults to 'Lato'.

- title_align:

  Argument passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).
  Can be one of 'plot' or 'panel'.

- grid:

  Character. Which major grid lines to show: `"y"` (default), `"x"`,
  `"xy"`, or `"none"`. Only the requested grid themes are added.

- ticks:

  Character. Which axis ticks and lines to show: `"x"` (default), `"y"`,
  `"xy"`, or `"none"`. This is independent of `grid`.

- background:

  Character. Plot and panel background: `"offwhite"` (default,
  `#FEFEFE`), `"white"` (`#FFFFFF`), `"gray"` (the brand `gray.100`), or
  `"transparent"`.

- ...:

  Additional arguments passed to
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Value

A ggplot2 theme object

## Examples

``` r
if (FALSE) { # rlang::is_interactive()
ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point() +
  theme_ekio(font_title = "serif", font_text = "sans")
}
```
