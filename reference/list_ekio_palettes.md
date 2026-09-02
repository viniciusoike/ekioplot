# List Available Palettes

Returns names of all available palettes, optionally filtered by type.

## Usage

``` r
list_ekio_palettes(type = "all")
```

## Arguments

- type:

  Character. Type of palettes to list: "accent", "brand", "categorical",
  "scientific", "sequential", "diverging", or "all" (default).

## Value

Character vector of palette names, or named list if type = "all".

## Examples

``` r
list_ekio_palettes()
#> $accent
#> [1] "gold"          "accent_blue"   "accent_orange"
#> 
#> $brand
#> [1] "ekio_brand"
#> 
#> $categorical
#> [1] "full"       "full_muted" "cool3"      "cool4"     
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
#> [1] "full"       "full_muted" "cool3"      "cool4"     
```
