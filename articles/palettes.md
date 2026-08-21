# Palette Gallery

Every palette `ekioplot` ships, at full size. Colors are read live from
the package, so this page is always in sync with
`inst/ekio-palettes.yaml` — the source of truth for the brand.

Each chip is labelled with its hex code, drawn in black or white by
[`ekio_text_on()`](https://viniciusoike.github.io/ekioplot/reference/ekio_text_on.md),
the same WCAG contrast helper the palette swatches use.

The seven brand scales are generated from one OKLCH specification: a
shared lightness spine anchored on the brand navy, a shared chroma arc,
and a hue path per family. A given shade therefore carries the same
visual weight in every family. The scientific palettes are not EKIO
color — they come from matplotlib and from Okabe & Ito, and
`inst/COPYRIGHTS` carries the notices.

## All colors

Every color in one searchable table. Click a hex to copy it; expand a
palette to see its colors, or click its **copy** button to take the
whole thing as an R vector.

## Accent

Named tokens rather than a ramp. Gold has no nine-step scale because
dark yellow is brown — past the middle of the spine a gold ramp stops
being gold. The three sit on the same lightness rungs as scale shades
300, 400 and 500, and `deep` is the one that can carry type.

`gold`3 colors

\#D5AA48

\#B88715

\#966800

## Categorical

For unordered groups. Each series sits on its own rung of the lightness
spine, so `contrast` stays readable in grayscale and under deuteranopia.
Past five categories `full` separates by hue rather than lightness, so
it does not survive grayscale printing. `cool` and `minimal` stay within
the primary blues and grays; `muted` and `muted_warm` are gray-only;
`binary` and `political` are fixed two-color pairings.

`cool`3 colors

\#1E3A5F

\#5597CC

\#006261

`minimal`3 colors

\#1E3A5F

\#6E7378

\#AEB1B5

`contrast`5 colors

\#1E3A5F

\#D3742A

\#006261

\#D5AA48

\#B44D47

`full`8 colors

\#1E3A5F

\#D3742A

\#006261

\#D5AA48

\#B44D47

\#448255

\#373A3D

\#B4B0AB

`muted`4 colors

\#191A1C

\#373A3D

\#6E7378

\#AEB1B5

`muted_warm`4 colors

\#1B1A18

\#3C3935

\#76716B

\#B4B0AB

`binary`2 colors

\#1E3A5F

\#D3742A

`political`2 colors

\#1E3A5F

\#8C3431

## Highlight

One accent against three receding grays. The series carrying the
argument speaks and the rest stay legible context — order the factor so
the level you want emphasised comes first.

`highlight_blue`4 colors

\#1E3A5F

\#6E7378

\#8D9197

\#AEB1B5

`highlight_orange`4 colors

\#D3742A

\#6E7378

\#8D9197

\#AEB1B5

`highlight_teal`4 colors

\#006261

\#6E7378

\#8D9197

\#AEB1B5

`highlight_red`4 colors

\#B44D47

\#6E7378

\#8D9197

\#AEB1B5

## Small group

Tuned for a known number of series — reach for these instead of
truncating a longer palette, which can leave you with two neighbouring
hues.

`duo_warm`2 colors

\#D3742A

\#D5AA48

`duo_cool`2 colors

\#1E3A5F

\#158281

`trio_bold`3 colors

\#1E3A5F

\#D3742A

\#006261

`trio_cool`3 colors

\#1E3A5F

\#158281

\#8D9197

`quad_earth`4 colors

\#1E3A5F

\#D3742A

\#006261

\#D5AA48

`quad_vivid`4 colors

\#3E76AC

\#D3742A

\#8C3431

\#D5AA48

## Sequential

The seven brand scales, light to dark. Position `i` is shade `i * 100`,
so `ekio_pal("blue")["700"]` is the primary blue. These work as discrete
palettes for ordered categories and as the ramp behind
[`scale_color_ekio_c()`](https://viniciusoike.github.io/ekioplot/reference/scale_color_ekio_c.md).

`blue`9 colors

\#E8F6FF

\#B1D8F2

\#84B8DD

\#5597CC

\#3E76AC

\#305687

\#1E3A5F

\#152A44

\#0D1B2A

`gray`9 colors

\#F2F3F5

\#CFD2D5

\#AEB1B5

\#8D9197

\#6E7378

\#52555A

\#373A3D

\#28292C

\#191A1C

`stone`9 colors

\#F5F3F1

\#D4D1CD

\#B4B0AB

\#959089

\#76716B

\#59544F

\#3C3935

\#2B2926

\#1B1A18

`teal`9 colors

\#E2F9FA

\#AFDCDD

\#7EBEC0

\#40A2A3

\#158281

\#006261

\#004342

\#013031

\#051F20

`green`9 colors

\#E7F9EC

\#BADCC3

\#8FBE9C

\#62A274

\#448255

\#2E623B

\#1C4326

\#14301C

\#0D1F12

`orange`9 colors

\#FFF1E5

\#F7C7A0

\#E49F6C

\#D3742A

\#B15400

\#893A00

\#612400

\#471904

\#2D1106

`red`9 colors

\#FFEFED

\#FDC1B7

\#E9998E

\#D76D63

\#B44D47

\#8C3431

\#622020

\#461818

\#2D1010

## Diverging

For values with a meaningful midpoint. The pivot is a near-neutral tone,
lighter than both arms, so the visual center of the scale lands on zero.

`blue_orange`9 colors

\#152A44

\#305687

\#5597CC

\#B1D8F2

\#F5F3EF

\#F7C7A0

\#D3742A

\#893A00

\#471904

`blue_red`9 colors

\#152A44

\#305687

\#5597CC

\#B1D8F2

\#F5F3EF

\#FDC1B7

\#D76D63

\#8C3431

\#461818

`teal_orange`9 colors

\#013031

\#006261

\#40A2A3

\#AFDCDD

\#F5F3EF

\#F7C7A0

\#D3742A

\#893A00

\#471904

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
ekio_pal("teal")["600"]
#>     600
#> "#006261"

# Interpolated to an arbitrary length
ekio_pal("blue_orange", n = 15)
```

[`list_ekio_palettes()`](https://viniciusoike.github.io/ekioplot/reference/list_ekio_palettes.md)
returns these names programmatically, and
[`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md)
prints a swatch when called at the console.
