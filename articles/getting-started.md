# Getting Started with ekioplot

``` r

library(ekioplot)
library(ggplot2)
```

## EKIO Theme

[`theme_ekio()`](https://viniciusoike.github.io/ekioplot/reference/theme_ekio.md)
applies EKIO’s visual identity to any ggplot2 plot. It builds on
[`theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
with curated typography, spacing, and color choices.

``` r

ggplot(mtcars, aes(wt, mpg)) +
  geom_point(color = ekio_pal("blue")["700"], size = 2.5) +
  labs(
    title = "Fuel Efficiency vs. Weight",
    subtitle = "Motor Trend Car Road Tests (1974)",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon"
  ) +
  theme_ekio()
```

![](getting-started_files/figure-html/theme-basic-1.png)

The `grid` parameter controls which major grid lines are drawn:

``` r

ggplot(mtcars, aes(wt, mpg)) +
  geom_point(color = ekio_pal("blue")["700"]) +
  theme_ekio(grid = "xy")
```

![](getting-started_files/figure-html/theme-grid-1.png)

## Color Palettes

ekioplot ships 22 palettes across six groups. Use
[`list_ekio_palettes()`](https://viniciusoike.github.io/ekioplot/reference/list_ekio_palettes.md)
to explore them:

``` r

str(list_ekio_palettes())
#> List of 6
#>  $ accent     : chr [1:3] "gold" "accent_blue" "accent_orange"
#>  $ brand      : chr "ekio_brand"
#>  $ categorical: chr [1:4] "full" "full_muted" "cool3" "cool4"
#>  $ sequential : chr [1:7] "blue" "gray" "stone" "teal" ...
#>  $ diverging  : chr [1:3] "blue_orange" "blue_red" "teal_orange"
#>  $ scientific : chr [1:4] "okabe_ito" "viridis" "inferno" "plasma"
```

Access any palette with
[`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md):

``` r

ekio_pal("full")
```

![](getting-started_files/figure-html/palette-access-1.png)

``` r

ekio_pal("cool3")
```

![](getting-started_files/figure-html/palette-access-2.png)

``` r

ekio_pal("accent_blue", n = 5)
```

![](getting-started_files/figure-html/palette-access-3.png)

``` r

ekio_pal("blue", n = 5)
```

![](getting-started_files/figure-html/palette-access-4.png)

### Palette types

- **Categorical**: `full`, `full_muted`, `cool3`, `cool4`
- **Brand**: `ekio_brand` for EKIO identity assets, not general data
  visualization
- **Scientific**: `okabe_ito`, `viridis`, `inferno`, `plasma`
- **Sequential**: `blue`, `gray`, `stone`, `teal`, `green`, `orange`,
  `red`
- **Accent**: `gold`, `accent_blue`, `accent_orange`
- **Diverging**: `blue_orange`, `blue_red`, `teal_orange`

`accent_blue` and `accent_orange` return four colors by default. Set `n`
from 2 to 6 to match the number of series while retaining the accent as
the first color. `gold` remains a fixed three-color named palette.

[`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md)
displays a swatch when printed:

``` r

ekio_pal("full")
```

![](getting-started_files/figure-html/show-palette-1.png)

## Scale Functions

ekioplot provides ggplot2 scales for both discrete and continuous data.

### Discrete scales

``` r

ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_ekio_d("full") +
  labs(color = "Cylinders") +
  theme_ekio(grid = "xy")
```

![](getting-started_files/figure-html/scale-discrete-1.png)

### Continuous scales

Sequential and diverging palettes work with continuous data:

``` r

ggplot(mtcars, aes(wt, mpg, color = hp)) +
  geom_point(size = 3) +
  scale_color_ekio_c("blue") +
  labs(color = "Horsepower") +
  theme_ekio(grid = "xy")
```

![](getting-started_files/figure-html/scale-continuous-1.png)

Fill variants are available as
[`scale_fill_ekio_d()`](https://viniciusoike.github.io/ekioplot/reference/scale_color_ekio_d.md)
and
[`scale_fill_ekio_c()`](https://viniciusoike.github.io/ekioplot/reference/scale_color_ekio_c.md).

## Recipe Functions

Recipe functions are high-level wrappers that create complete,
publication-ready plots with smart defaults.

### Histogram

``` r

ekio_histogram(mtcars, mpg)
```

![](getting-started_files/figure-html/recipe-histogram-1.png)

### Bar plot

``` r

cyl_counts <- as.data.frame(table(cyl = mtcars$cyl))
names(cyl_counts)[2] <- "n"
ekio_barplot(cyl_counts, cyl, n)
```

![](getting-started_files/figure-html/recipe-barplot-1.png)

### Scatter plot

``` r

ekio_scatterplot(mtcars, wt, mpg, color = factor(cyl))
```

![](getting-started_files/figure-html/recipe-scatter-1.png)

### Area plot

``` r

economic_series <- subset(
  ggplot2::economics_long,
  variable %in% c("pce", "psavert", "uempmed")
)
ekio_areaplot(economic_series, date, value01, fill = variable)
```

![](getting-started_files/figure-html/recipe-area-1.png)

### Smart aesthetic detection

Recipe functions automatically detect whether the color/fill argument
is:

- **Missing** — uses EKIO blue as default
- **A color string** (e.g., `"steelblue"`) — uses that color directly
- **A variable** — maps it and applies the appropriate EKIO scale

``` r

ekio_histogram(mtcars, mpg, fill = "coral")
```

![](getting-started_files/figure-html/recipe-static-color-1.png)

## Brand Scales

Every brand color is reached through
[`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md).
The seven brand scales — `"blue"`, `"gray"`, `"stone"`, `"teal"`,
`"green"`, `"orange"`, and `"red"` — are nine-step ramps running light
to dark, named by shade. All seven sit on one lightness spine, so a
given shade carries the same visual weight in every family:

``` r

ekio_pal("gray")
```

![](getting-started_files/figure-html/brand-scale-1.png)

Position and shade are aligned by construction, so element `i` is always
shade `i * 100`. Index whichever way reads better:

``` r

ekio_pal("blue")["700"]
#>       700 
#> "#1E3A5F"
ekio_pal("blue")[7]
#>       700 
#> "#1E3A5F"
```

Because these are the same objects used for continuous fills, asking for
fewer colors interpolates across the whole ramp rather than returning
the lightest few:

``` r

ekio_pal("blue", n = 3)
```

![](getting-started_files/figure-html/brand-interpolate-1.png)
