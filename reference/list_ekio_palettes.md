# List Available Palettes

Returns names of all available palettes, optionally filtered by type.
When `verbose = TRUE`, prints a formatted summary to the console.

## Usage

``` r
list_ekio_palettes(type = "all", verbose = FALSE)
```

## Arguments

- type:

  Character. Type of palettes to list: "accent", "categorical",
  "highlight", "small_group", "scientific", "sequential", "diverging",
  or "all" (default).

- verbose:

  Logical. If TRUE, prints a formatted summary of the selected type(s)
  and returns the result invisibly (default: FALSE).

## Value

Character vector of palette names, or named list if type = "all".
Invisibly returned when `verbose = TRUE`.

## Examples

``` r
list_ekio_palettes()
#> $accent
#> [1] "gold"
#> 
#> $categorical
#> [1] "cool"       "minimal"    "contrast"   "full"       "muted"     
#> [6] "muted_warm" "binary"     "political" 
#> 
#> $highlight
#> [1] "highlight_blue"   "highlight_orange" "highlight_teal"   "highlight_red"   
#> 
#> $small_group
#> [1] "duo_warm"   "duo_cool"   "trio_bold"  "trio_cool"  "quad_earth"
#> [6] "quad_vivid"
#> 
#> $sequential
#> [1] "blue"   "gray"   "stone"  "teal"   "green"  "orange" "red"   
#> 
#> $diverging
#> [1] "blue_orange" "blue_red"    "teal_orange"
#> 
#> $scientific
#> [1] "okabe_ito" "viridis"   "inferno"   "plasma"   
#> 
list_ekio_palettes("categorical")
#> [1] "cool"       "minimal"    "contrast"   "full"       "muted"     
#> [6] "muted_warm" "binary"     "political" 
list_ekio_palettes("highlight")
#> [1] "highlight_blue"   "highlight_orange" "highlight_teal"   "highlight_red"   
list_ekio_palettes(verbose = TRUE)
#> 
#> ── Available Palettes ──────────────────────────────────────────────────────────
#> 
#> ── Accent (named tokens, not ramps) ──
#> 
#> "gold"
#> 
#> ── Categorical ──
#> 
#> "cool", "minimal", "contrast", "full", "muted", "muted_warm", "binary", and
#> "political"
#> 
#> ── Highlight (one accent against receding grays) ──
#> 
#> "highlight_blue", "highlight_orange", "highlight_teal", and "highlight_red"
#> 
#> ── Small Group Variants ──
#> 
#> "duo_warm", "duo_cool", "trio_bold", "trio_cool", "quad_earth", and
#> "quad_vivid"
#> 
#> ── Sequential (brand scales, for continuous fills) ──
#> 
#> "blue", "gray", "stone", "teal", "green", "orange", and "red"
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
