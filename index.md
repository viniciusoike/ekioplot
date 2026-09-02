# ekioplot

## Overview

`ekioplot` implements EKIO’s visual identity system for data
visualization in R.

``` r

library(ekioplot)
library(ggplot2)

ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_ekio_d("full") +
  labs(
    title = "Fuel Efficiency vs. Weight",
    subtitle = "Motor Trend Car Road Tests (1974)",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon",
    color = "Cylinders"
  ) +
  theme_ekio()
```

![Scatter plot of fuel economy against weight for the mtcars data,
points coloured by cylinder count with the full palette, drawn in
theme_ekio().](reference/figures/README-hero.png)

## Installation

Install the released version from CRAN:

``` r

install.packages("ekioplot")
```

Install the development version from
[r-universe](https://viniciusoike.r-universe.dev/ekioplot):

``` r

install.packages(
  "ekioplot",
  repos = c(
    "https://viniciusoike.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

Alternatively, install the development version from GitHub.

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

ekioplot ships palettes across six groups, all accessible through a
single function. The seven brand scales are generated from one OKLCH
specification, so a given shade carries the same visual weight in every
family.

Compact categorical palettes (`"cool3"`, `"cool4"`) cover small groups;
`"full_muted"` provides a quieter eight-color alternative. Variable-size
accent palettes keep blue or orange prominent against two to six series.
The named `"ekio_brand"` palette collects EKIO identity colors for brand
assets; it is not intended for general data visualization.

``` r

ekio_pal()
ekio_pal("accent_blue", n = 5)
```

![Ten selected EKIO palettes drawn as horizontal colour strips,
including the new compact categorical and variable-size accent
palettes.](reference/figures/README-palettes.png)

## Scales

Discrete and continuous scales are provided for both `color` and `fill`.

``` r

# Discrete (categorical palettes)
scale_color_ekio_d("full")
scale_fill_ekio_d("full")

# Continuous (sequential / diverging palettes)
scale_color_ekio_c("blue")
scale_fill_ekio_c("blue_orange")
```

## Recipe functions

High-level builders create complete, publication-ready plots with smart
defaults.

![Four panels showing ekio_scatterplot(), ekio_barplot(),
ekio_lineplot() and ekio_areaplot(), each labelled with the function
that drew it.](reference/figures/README-recipes.png)

## See more

See
[`vignette("getting-started", package = "ekioplot")`](https://viniciusoike.github.io/ekioplot/articles/getting-started.md),
the [palette
gallery](https://viniciusoike.github.io/ekioplot/articles/palettes.html),
and the package website at <https://viniciusoike.github.io/ekioplot/>.

------------------------------------------------------------------------
