# Pick a Readable Text Color for a Background

Returns the text color (dark or light) with the higher WCAG contrast
ratio against each background color. Useful for labels placed on colored
fills, e.g. in
[`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
or gt table cells.

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
ekio_text_on(ekio_blue["700"])
#>     700 
#> "white" 
ekio_text_on(ekio_blue)
#>     900     800     700     600     500     400     300     200     100      50 
#> "white" "white" "white" "white" "white" "black" "black" "black" "black" "black" 
ekio_text_on(ekio_accent, dark = ekio_gray["900"])
#>      blue    orange      teal     amber    purple       red     green      gray 
#>   "white" "#1A202C"   "white" "#1A202C"   "white"   "white" "#1A202C" "#1A202C" 
```
