# Palette Gallery

Every palette `ekioplot` ships, at full size. Colors are read from the
package registry, which is generated from `inst/ekio-palettes.yaml` —
the source of truth for the brand.

Each chip is labelled with its hex code, drawn in black or white by
[`ekio_text_on()`](https://viniciusoike.github.io/ekioplot/reference/ekio_text_on.md),
the same WCAG contrast helper the palette swatches use.

The brand scales are generated from one OKLCH specification: a shared
lightness spine anchored on the brand navy, a shared chroma arc, and a
hue path per family. A given shade therefore carries the same visual
weight in every family. The scientific palettes are not EKIO color —
they come from matplotlib and from Okabe & Ito, and `inst/COPYRIGHTS`
carries the notices.

## All colors

Every color in one searchable table. Click a hex to copy it; expand a
palette to see its colors, or click its **copy** button to take the
whole thing as an R vector.

## Accent

Gold is a named token rather than a ramp. It has no nine-step scale
because dark yellow is brown — past the middle of the spine a gold ramp
stops being gold. The three tokens sit on the same lightness rungs as
scale shades 300, 400 and 500, and `deep` is the one that can carry
type.

`accent_blue` and `accent_orange` put one main color before receding
grays. They return the original four-color form by default; use `n` from
2 to 6 when the number of series changes. The gallery shows all six
available positions.

`gold`3 colors

\#D2A745

\#B4840C

\#916400

`accent_blue`6 colors

\#1E3A5F

\#6A6E74

\#898E93

\#ABAEB3

\#CED0D4

\#F2F3F5

`accent_orange`6 colors

\#CF7126

\#6A6E74

\#898E93

\#ABAEB3

\#CED0D4

\#F2F3F5

## Categorical

For unordered groups. `cool3` and `cool4` are compact cool palettes,
while `full_muted` follows the same sequence as `full` with each color
one rung darker. `full` separates by hue rather than lightness past five
categories, so it does not survive grayscale printing.

`full`8 colors

\#1E3A5F

\#CF7126

\#00605E

\#D2A745

\#AF4942

\#407D51

\#373A3D

\#B2AEA8

`full_muted`8 colors

\#152A44

\#AB5000

\#004342

\#B4840C

\#89322F

\#2C6039

\#28292C

\#928C85

`cool3`3 colors

\#1E3A5F

\#5194C8

\#00605E

`cool4`4 colors

\#1E3A5F

\#5194C8

\#00605E

\#407D51

## EKIO brand

`ekio_brand` contains the colors extracted from Hokusai’s *Inume Pass in
Kai Province*, together with white and black. It is intended for EKIO
identity assets and other brand work, rather than general data
visualization.

`ekio_brand`7 colors

\#225A7E

\#D4DED9

\#EFE8DC

\#517A90

\#F2EDE2

\#FFFFFF

\#000000

## Sequential

The brand scales, light to dark. Position `i` is shade `i * 100`, so
`ekio_pal("blue")["700"]` is the primary blue. These work as discrete
palettes for ordered categories and as the ramp behind
[`scale_color_ekio_c()`](https://viniciusoike.github.io/ekioplot/reference/scale_color_ekio_c.md).

`blue`9 colors

\#E8F6FF

\#B0D6F0

\#82B5DA

\#5194C8

\#3A71A8

\#2E5485

\#1E3A5F

\#152A44

\#0D1B2A

`gray`9 colors

\#F2F3F5

\#CED0D4

\#ABAEB3

\#898E93

\#6A6E74

\#505358

\#373A3D

\#28292C

\#191A1C

`stone`9 colors

\#F5F3F1

\#D3D0CC

\#B2AEA8

\#928C85

\#726D66

\#56524D

\#3C3935

\#2B2926

\#1B1A18

`teal`9 colors

\#E2F9FA

\#ADDADC

\#7BBBBD

\#3C9E9F

\#097E7D

\#00605E

\#004342

\#013031

\#051F20

`green`9 colors

\#E7F9EC

\#B8DAC2

\#8DBC9A

\#5F9E70

\#407D51

\#2C6039

\#1C4326

\#14301C

\#0D1F12

`orange`9 colors

\#FFF1E5

\#F6C59F

\#E19D6A

\#CF7126

\#AB5000

\#863900

\#612400

\#471904

\#2D1106

`red`9 colors

\#FFEFED

\#FBC0B6

\#E6968B

\#D3695F

\#AF4942

\#89322F

\#622020

\#461818

\#2D1010

`purple`9 colors

\#FDEEFF

\#E2C4E9

\#C49ED2

\#A877BD

\#86589C

\#684079

\#4B2A55

\#361F3C

\#231425

## Diverging

For values with a meaningful midpoint. The pivot is a near-neutral tone,
lighter than both arms, so the visual center of the scale lands on zero.

`purple_orange`9 colors

\#684079

\#86589C

\#C49ED2

\#E2C4E9

\#F5F3EF

\#F6C59F

\#E19D6A

\#AB5000

\#863900

`blue_red`9 colors

\#2E5485

\#3A71A8

\#82B5DA

\#B0D6F0

\#F5F3EF

\#FBC0B6

\#E6968B

\#AF4942

\#89322F

`teal_orange`9 colors

\#00605E

\#097E7D

\#7BBBBD

\#ADDADC

\#F5F3EF

\#F6C59F

\#E19D6A

\#AB5000

\#863900

`purple_green`9 colors

\#684079

\#86589C

\#C49ED2

\#E2C4E9

\#F5F3EF

\#B8DAC2

\#8DBC9A

\#407D51

\#2C6039

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
  scale_color_ekio_d("full") +
  theme_ekio()

# Compact and accent alternatives
ekio_pal("cool3")
ekio_pal("full_muted")
ekio_pal("accent_orange", n = 5)

# Named access to any brand scale
ekio_pal("teal")["600"]
#>     600
#> "#006261"

# Interpolated to an arbitrary length
ekio_pal("purple_orange", n = 15)
```

[`list_ekio_palettes()`](https://viniciusoike.github.io/ekioplot/reference/list_ekio_palettes.md)
returns these names programmatically, and
[`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md)
prints a swatch when called at the console.
