# Pick a Readable Text Color for a Background

Returns the text color (dark or light) with the higher WCAG contrast
ratio against each background color. Useful for labels placed on colored
fills, e.g. in
[`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html).

## Usage

``` r
ekio_text_on(background, dark = "black", light = "white")
```

## Arguments

- background:

  Character. Background color(s) as hex codes or R color names.

- dark:

  Character. Dark text color candidate (default: `"black"`).

- light:

  Character. Light text color candidate (default: `"white"`).

## Value

Character vector of text colors, one per background. Names of
`background` are preserved.

## See also

[`ekio_contrast()`](https://viniciusoike.github.io/ekioplot/reference/ekio_contrast.md)
for the underlying contrast ratios

## Examples

``` r
ekio_text_on(ekio_pal("blue")["700"])
#>     700 
#> "white" 
ekio_text_on(ekio_pal("blue"))
#>     100     200     300     400     500     600     700     800     900 
#> "black" "black" "black" "black" "white" "white" "white" "white" "white" 
ekio_text_on(ekio_pal("full"), dark = ekio_pal("gray")["900"])
#> [1] "white"   "#191A1C" "white"   "#191A1C" "white"   "white"   "white"  
#> [8] "#191A1C"
```
