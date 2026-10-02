# List Available Palettes

Returns names of all available palettes, optionally filtered by type.

## Usage

``` r
list_ekio_palettes(type = "all")
```

## Arguments

- type:

  Character. Type of palettes to list: "accent", "brand", "academy",
  "categorical", "scientific", "sequential", "diverging", or "all"
  (default).

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
#> $academy
#> [1] "yoro_blue"  "yoro_green"
#> 
#> $categorical
#> [1] "full"           "full_muted"     "full_light"     "spectrum_light"
#> [5] "cool3"          "cool4"         
#> 
#> $sequential
#> [1] "blue"   "gray"   "stone"  "teal"   "green"  "orange" "red"    "purple"
#> 
#> $diverging
#> [1] "purple_orange" "blue_red"      "teal_orange"   "purple_green" 
#> 
#> $scientific
#> [1] "okabe_ito" "viridis"   "inferno"   "plasma"   
#> 
list_ekio_palettes("categorical")
#> [1] "full"           "full_muted"     "full_light"     "spectrum_light"
#> [5] "cool3"          "cool4"         
```
