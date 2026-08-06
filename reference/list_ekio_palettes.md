# List Available Palettes

Returns names of all available palettes, optionally filtered by type.
When `verbose = TRUE`, prints a formatted summary to the console.

## Usage

``` r
list_ekio_palettes(type = "all", verbose = FALSE)
```

## Arguments

- type:

  Character. Type of palettes to list: "categorical", "small_group",
  "scientific", "sequential", "diverging", or "all" (default).

- verbose:

  Logical. If TRUE, prints a formatted summary of the selected type(s)
  and returns the result invisibly (default: FALSE).

## Value

Character vector of palette names, or named list if type = "all".
Invisibly returned when `verbose = TRUE`.

## Examples

``` r
list_ekio_palettes()
#> $categorical
#> [1] "cool"      "minimal"   "contrast"  "full"      "muted"     "binary"   
#> [7] "political"
#> 
#> $small_group
#> [1] "duo_warm"   "duo_cool"   "trio_bold"  "trio_cool"  "quad_earth"
#> [6] "quad_vivid"
#> 
#> $scientific
#> [1] "okabe_ito" "viridis"   "inferno"   "plasma"   
#> 
#> $sequential
#> [1] "blue"   "teal"   "gray"   "orange" "purple" "red"    "green"  "amber" 
#> 
#> $diverging
#> [1] "blue_orange" "blue_red"    "teal_orange"
#> 
list_ekio_palettes("categorical")
#> [1] "cool"      "minimal"   "contrast"  "full"      "muted"     "binary"   
#> [7] "political"
list_ekio_palettes("diverging")
#> [1] "blue_orange" "blue_red"    "teal_orange"
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
#> ── Scientific ──
#> 
#> "okabe_ito", "viridis", "inferno", and "plasma"
#> 
#> ── Sequential (for continuous scales) ──
#> 
#> "blue", "teal", "gray", "orange", "purple", "red", "green", and "amber"
#> 
#> ── Diverging (for continuous scales) ──
#> 
#> "blue_orange", "blue_red", and "teal_orange"
#> 
#> ℹ Print `ekio_pal()` to see the palette swatch
```
