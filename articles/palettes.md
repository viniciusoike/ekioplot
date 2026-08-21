# Palette Gallery

Every palette `ekioplot` ships, at full size. Colors are read live from
the package, so this page is always in sync with
`inst/ekio-palettes.yaml` — the source of truth for the brand.

Each chip is labelled with its hex code, drawn in black or white by
[`ekio_text_on()`](https://viniciusoike.github.io/ekioplot/reference/ekio_text_on.md),
the same WCAG contrast helper the palette swatches use.

## All colors

Every color in one searchable table. Click a hex to copy it; expand a
palette to see its colors, or click its **copy** button to take the
whole thing as an R vector.

## Categorical

For unordered groups. `contrast` is the general-purpose default; `cool`
and `minimal` stay within the primary blues and grays for restrained
charts, and `binary` and `political` are fixed two-color pairings.

`cool`3 colors

\#1E3A5F

\#4A90C2

\#2C7A7B

`minimal`3 colors

\#1E3A5F

\#4A5568

\#A0AEC0

`contrast`6 colors

\#1E3A5F

\#DD6B20

\#2C7A7B

\#D69E2E

\#805AD5

\#C53030

`full`8 colors

\#1E3A5F

\#DD6B20

\#2C7A7B

\#D69E2E

\#805AD5

\#C53030

\#38A169

\#718096

`muted`5 colors

\#4A5568

\#718096

\#A0AEC0

\#CBD5E0

\#E2E8F0

`binary`2 colors

\#1E3A5F

\#DD6B20

`political`2 colors

\#1E3A5F

\#C53030

## Small group

Tuned for a known number of series — reach for these instead of
truncating a longer palette, which can leave you with two neighbouring
hues.

`duo_warm`2 colors

\#DD6B20

\#D69E2E

`duo_cool`2 colors

\#1E3A5F

\#2C7A7B

`trio_bold`3 colors

\#1E3A5F

\#DD6B20

\#2C7A7B

`trio_cool`3 colors

\#3A6EA5

\#2C7A7B

\#805AD5

`quad_earth`4 colors

\#1E3A5F

\#DD6B20

\#2C7A7B

\#38A169

`quad_vivid`4 colors

\#3A6EA5

\#DD6B20

\#805AD5

\#C53030

## Sequential

The eight brand scales, light to dark. Position `i` is shade `i * 100`,
so `ekio_pal("blue")["700"]` is the primary blue. These work as discrete
palettes for ordered categories and as the ramp behind
[`scale_color_ekio_c()`](https://viniciusoike.github.io/ekioplot/reference/scale_color_ekio_c.md).

`blue`9 colors

\#D4E8F5

\#A8D0E8

\#7EB6D8

\#4A90C2

\#3A6EA5

\#2B4C7E

\#1E3A5F

\#152A44

\#0D1B2A

`gray`9 colors

\#F7FAFC

\#EDF2F7

\#E2E8F0

\#CBD5E0

\#A0AEC0

\#718096

\#4A5568

\#2D3748

\#1A202C

`teal`9 colors

\#E6FFFA

\#B2F5EA

\#81E6D9

\#4FD1C5

\#38B2AC

\#319795

\#2C7A7B

\#285E61

\#234E52

`orange`9 colors

\#FFFAF0

\#FEEBC8

\#FBD38D

\#F6AD55

\#ED8936

\#DD6B20

\#C05621

\#9C4221

\#7B341E

`purple`9 colors

\#E9D8FD

\#D6BCFA

\#B794F4

\#9F7AEA

\#805AD5

\#6B46C1

\#553C9A

\#44337A

\#332A5A

`red`9 colors

\#FED7D7

\#FEB2B2

\#FC8181

\#F56565

\#E53E3E

\#C53030

\#9B2C2C

\#742A2A

\#4D2828

`green`9 colors

\#C6F6D5

\#9AE6B4

\#68D391

\#48BB78

\#38A169

\#2F855A

\#276749

\#22543D

\#1D4131

`amber`9 colors

\#FEFCBF

\#FAF089

\#F6E05E

\#ECC94B

\#D69E2E

\#B7791F

\#975A16

\#744210

\#512A0A

## Diverging

For values with a meaningful midpoint. The pivot is a near-neutral tone,
lighter than both arms, so the visual center of the scale lands on zero.

`blue_orange`11 colors

\#0D1B2A

\#1E3A5F

\#3A6EA5

\#7EB6D8

\#D4E8F5

\#FCFAF7

\#FEEBC8

\#F6AD55

\#DD6B20

\#9C4221

\#7B341E

`blue_red`11 colors

\#0D1B2A

\#1E3A5F

\#3A6EA5

\#7EB6D8

\#D4E8F5

\#F5F0F0

\#FED7D7

\#FC8181

\#E53E3E

\#9B2C2C

\#4D2828

`teal_orange`11 colors

\#234E52

\#2C7A7B

\#38B2AC

\#81E6D9

\#B2F5EA

\#FCFAF7

\#FEEBC8

\#F6AD55

\#DD6B20

\#9C4221

\#7B341E

## Scientific

Established palettes included for accessibility and continuity with
published work: `okabe_ito` is colorblind-safe, and the three matplotlib
ramps are perceptually uniform.

`okabe_ito`8 colors

\#E69F00

\#56B4E9

\#009E73

\#F0E442

\#0072B2

\#D55E00

\#CC79A7

\#000000

`viridis`9 colors

\#440154

\#482677

\#3F4A8A

\#31678E

\#26838F

\#1F9D8A

\#6CCE5A

\#B6DE2B

\#FEE825

`inferno`10 colors

\#000004

\#1B0C41

\#4A0C6B

\#781C6D

\#A52C60

\#CF4446

\#ED6925

\#FB9B06

\#F7D03C

\#FCFFA4

`plasma`10 colors

\#0D0887

\#46039F

\#7201A8

\#9C179E

\#BD3786

\#D8576B

\#ED7953

\#FB9F3A

\#FDCA26

\#F0F921

## Using a palette

Pass any name above to a scale function, or call
[`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md)
directly to pull hex codes out.

``` r

library(ggplot2)

ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_ekio_d("contrast") +
  theme_ekio()

# Named access to any brand scale
ekio_pal("teal")["700"]
#>     700
#> "#2C7A7B"

# Interpolated to an arbitrary length
ekio_pal("blue_orange", n = 15)
```

[`list_ekio_palettes()`](https://viniciusoike.github.io/ekioplot/reference/list_ekio_palettes.md)
returns these names programmatically, and
[`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md)
prints a swatch when called at the console.
