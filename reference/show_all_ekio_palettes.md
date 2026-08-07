# Show All Palettes

Deprecated. Use
[`list_ekio_palettes()`](https://viniciusoike.github.io/ekioplot/reference/list_ekio_palettes.md)
with `verbose = TRUE` instead.

## Usage

``` r
show_all_ekio_palettes()
```

## Value

The palette list, invisibly (as returned by
[`list_ekio_palettes()`](https://viniciusoike.github.io/ekioplot/reference/list_ekio_palettes.md)
with `verbose = TRUE`).

## Examples

``` r
list_ekio_palettes(verbose = TRUE)
#> 
#> ── Available Palettes ──────────────────────────────────────────────────────────
#> 
#> ── Categorical ──
#> 
#> "cool", "minimal", "contrast", "full", "muted", "binary", and "political"
#> 
#> ── Small Group Variants ──
#> 
#> "duo_warm", "duo_cool", "trio_bold", "trio_cool", "quad_earth", and
#> "quad_vivid"
#> 
#> ── Sequential (brand scales, for continuous fills) ──
#> 
#> "blue", "gray", "teal", "orange", "purple", "red", "green", and "amber"
#> 
#> ── Diverging (for continuous scales) ──
#> 
#> "blue_orange", "blue_red", and "teal_orange"
#> 
#> ── Scientific ──
#> 
#> "okabe_ito", "viridis", "inferno", and "plasma"
#> 
#> ℹ Print `ekio_pal()` to see the palette swatch
```
