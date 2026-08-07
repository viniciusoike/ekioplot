# EKIO Font Family

Returns the platform-appropriate EKIO font family. EKIO chart and table
output uses a sans stack only: Helvetica Neue on macOS, Arial on
Windows, and the generic device font elsewhere.

## Usage

``` r
ekio_font(type = c("primary", "mono"))
```

## Arguments

- type:

  Character. `"primary"` for the sans stack (default) or `"mono"` for
  the monospace stack.

## Value

Character. A font family name.

## Details

This is the canonical accessor for EKIO brand type. Packages that style
other output (for example gt tables) should call it rather than
repeating the platform logic.

## Examples

``` r
ekio_font()
#> [1] "sans"
ekio_font("mono")
#> [1] "mono"
```
