# WCAG Contrast Ratio Between Two Colors

Computes the contrast ratio between foreground and background colors as
defined by WCAG 2.1. Ratios range from 1 (no contrast) to 21 (black on
white). WCAG requires at least 4.5 for normal text (level AA), 3.0 for
large text (AA), and 7.0 for normal text at level AAA.

## Usage

``` r
ekio_contrast(color, background = "white")
```

## Arguments

- color:

  Character. Foreground color(s) as hex codes or R color names.

- background:

  Character. Background color(s) (default: `"white"`). Recycled against
  `color` if needed.

## Value

Numeric vector of contrast ratios between 1 and 21

## See also

[`ekio_text_on()`](https://viniciusoike.github.io/ekioplot/reference/ekio_text_on.md)
to pick a readable text color for a background

## Examples

``` r
ekio_contrast("black", "white")
#> [1] 21
ekio_contrast(ekio_pal("blue")["700"])
#> [1] 11.50262
ekio_contrast("white", ekio_pal("blue"))
#> [1]  1.101930  1.502798  2.125206  3.141442  4.781517  7.482243 11.502620
#> [8] 14.517373 17.393901
```
