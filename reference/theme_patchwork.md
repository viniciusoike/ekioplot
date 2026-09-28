# Theme for patchwork annotations

Returns the plot-level parts of
[`theme_ekio()`](https://viniciusoike.github.io/ekioplot/reference/theme_ekio.md)
for use with `patchwork::plot_annotation(theme = )`. The theme colors
the outer canvas and styles titles without changing the panels or axes
of child plots.

## Usage

``` r
theme_patchwork(background = "offwhite", ...)
```

## Arguments

- background:

  Character. Background surface accepted by
  [`theme_ekio()`](https://viniciusoike.github.io/ekioplot/reference/theme_ekio.md).

- ...:

  Additional arguments passed to
  [`theme_ekio()`](https://viniciusoike.github.io/ekioplot/reference/theme_ekio.md).

## Value

A partial ggplot2 theme.

## See also

[`theme_ekio()`](https://viniciusoike.github.io/ekioplot/reference/theme_ekio.md)

## Examples

``` r
theme_patchwork(background = "offwhite")
#> <theme> List of 5
#>  $ plot.background: <ggplot2::element_rect>
#>   ..@ fill         : chr "#FBFBF6"
#>   ..@ colour       : logi NA
#>   ..@ linewidth    : NULL
#>   ..@ linetype     : NULL
#>   ..@ linejoin     : NULL
#>   ..@ inherit.blank: logi FALSE
#>  $ plot.title     : <ggplot2::element_text>
#>   ..@ family       : chr "serif"
#>   ..@ face         : NULL
#>   ..@ italic       : chr NA
#>   ..@ fontweight   : num NA
#>   ..@ fontwidth    : num NA
#>   ..@ colour       : chr "#191A1C"
#>   ..@ size         : 'rel' num 1.2
#>   ..@ hjust        : num 0
#>   ..@ vjust        : NULL
#>   ..@ angle        : NULL
#>   ..@ lineheight   : NULL
#>   ..@ margin       : <ggplot2::margin> num [1:4] 0 0 4 0
#>   ..@ debug        : NULL
#>   ..@ inherit.blank: logi FALSE
#>  $ plot.subtitle  : <ggplot2::element_text>
#>   ..@ family       : chr "Lato"
#>   ..@ face         : NULL
#>   ..@ italic       : chr NA
#>   ..@ fontweight   : num NA
#>   ..@ fontwidth    : num NA
#>   ..@ colour       : chr "#505358"
#>   ..@ size         : 'rel' num 0.9
#>   ..@ hjust        : num 0
#>   ..@ vjust        : NULL
#>   ..@ angle        : NULL
#>   ..@ lineheight   : NULL
#>   ..@ margin       : <ggplot2::margin> num [1:4] 0 0 8 0
#>   ..@ debug        : NULL
#>   ..@ inherit.blank: logi FALSE
#>  $ plot.caption   : <ggplot2::element_text>
#>   ..@ family       : chr "Lato"
#>   ..@ face         : NULL
#>   ..@ italic       : chr NA
#>   ..@ fontweight   : num NA
#>   ..@ fontwidth    : num NA
#>   ..@ colour       : chr "#6A6E74"
#>   ..@ size         : 'rel' num 0.7
#>   ..@ hjust        : num 0
#>   ..@ vjust        : NULL
#>   ..@ angle        : NULL
#>   ..@ lineheight   : NULL
#>   ..@ margin       : <ggplot2::margin> num [1:4] 8 0 0 0
#>   ..@ debug        : NULL
#>   ..@ inherit.blank: logi FALSE
#>  $ plot.margin    : <ggplot2::margin> num [1:4] 15 10 15 10
#>  @ complete: logi FALSE
#>  @ validate: logi TRUE
```
