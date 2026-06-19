# List Available Palettes

Returns names of all available palettes, optionally filtered by type.

## Usage

``` r
list_ekio_palettes(type = "all")
```

## Arguments

- type:

  Character. Type of palettes to list: "categorical", "small_group",
  "scientific", "sequential", "diverging", or "all" (default).

## Value

Character vector of palette names, or named list if type = "all"

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
```
