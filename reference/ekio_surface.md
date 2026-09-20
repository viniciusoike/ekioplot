# Brand surfaces

Resolve an EKIO brand surface name to a hex code. `ekio_surface()` takes
the same vocabulary as `theme_ekio(background = )`: `"offwhite"`
(`#FBFBF6`), `"white"` (`#FFFFFF`) and `"cold"` (`#F6F7F8`). A hex code
such as `"#F0EAD6"` is passed through unchanged, and `"transparent"`
returns `NA_character_` so
[`ggplot2::element_rect()`](https://ggplot2.tidyverse.org/reference/element.html)
draws nothing.

## Usage

``` r
ekio_surface(surface = "offwhite", arg = "surface", call = current_env())
```

## Arguments

- surface:

  Character. A brand surface name or a hex code.

- arg, call:

  The argument name and the calling environment used to build error
  messages. See
  [`rlang::args_error_context()`](https://rlang.r-lib.org/reference/args_error_context.html).

## Value

A single hex code, or `NA_character_` for `"transparent"`.

## See also

[`theme_ekio()`](https://viniciusoike.github.io/ekioplot/reference/theme_ekio.md)

## Examples

``` r
ekio_surface("cold")
#> [1] "#F6F7F8"
ekio_surface("#F0EAD6")
#> [1] "#F0EAD6"
```
