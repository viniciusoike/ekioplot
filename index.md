# ekioplot

## Overview

`ekioplot` implements EKIO’s visual identity system for data
visualization in R.

``` r

library(ekioplot)
library(ggplot2)

ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_ekio_d("contrast") +
  labs(
    title = "Fuel Efficiency vs. Weight",
    subtitle = "Motor Trend Car Road Tests (1974)",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon",
    color = "Cylinders"
  ) +
  theme_ekio()
```

![](reference/figures/README-hero.png)

## Installation

`ekioplot` is not on CRAN. Install from
[r-universe](https://viniciusoike.r-universe.dev/ekioplot):

``` r

install.packages("ekioplot", repos = "https://viniciusoike.r-universe.dev")
```

Or install the development version from GitHub.

``` r

# install.packages("pak")
pak::pak("viniciusoike/ekioplot")
```

## Themes

[`theme_ekio()`](https://viniciusoike.github.io/ekioplot/reference/theme_ekio.md)
applies EKIO’s visual identity to any ggplot2 plot, building on
[`theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
with curated typography, spacing, and color. The `grid` argument
controls which major grid lines are drawn (`"y"`, `"x"`, `"xy"`, or
`"none"`).

``` r

theme_ekio(grid = "xy")
```

## Color palettes

ekioplot ships palettes across five categories, all accessible through a
single function.

``` r

ekio_pal()
```

![](reference/figures/README-palettes.png)

## Scales

Discrete and continuous scales are provided for both `color` and `fill`.

``` r

# Discrete (categorical palettes)
scale_color_ekio_d("contrast")
scale_fill_ekio_d("cool")

# Continuous (sequential / diverging palettes)
scale_color_ekio_c("blue")
scale_fill_ekio_c("blue_orange")
```

## Recipe functions

High-level builders create complete, publication-ready plots with smart
defaults.

![](reference/figures/README-recipes.png)

## See more

See
[`vignette("getting-started", package = "ekioplot")`](https://viniciusoike.github.io/ekioplot/articles/getting-started.md)
and the package website at <https://viniciusoike.github.io/ekioplot/>.

------------------------------------------------------------------------
