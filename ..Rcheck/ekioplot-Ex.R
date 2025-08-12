pkgname <- "ekioplot"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ekioplot')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("check_ekio_fonts")
### * check_ekio_fonts

flush(stderr()); flush(stdout())

### Name: check_ekio_fonts
### Title: Check Font Availability
### Aliases: check_ekio_fonts

### ** Examples

check_ekio_fonts()
check_ekio_fonts(detailed = TRUE)




cleanEx()
nameEx("ekio_barplot")
### * ekio_barplot

flush(stderr()); flush(stdout())

### Name: ekio_barplot
### Title: EKIO Bar Plot with Professional Styling
### Aliases: ekio_barplot

### ** Examples

library(ggplot2)
library(dplyr)

# Basic bar plot
ekio_barplot(mtcars |> count(cyl), cyl, n)

# With fill grouping
ekio_barplot(mtcars |> count(cyl, gear), cyl, n, fill = factor(gear))

# Horizontal with labels
ekio_barplot(mtcars |> count(cyl), cyl, n,
             horizontal = TRUE,
             add_labels = TRUE)

# Custom styling
ekio_barplot(mtcars |> count(cyl), cyl, n,
             palette = "sophisticated_unique",
             single_color_index = 6,
             add_labels = TRUE)




cleanEx()
nameEx("ekio_colors")
### * ekio_colors

flush(stderr()); flush(stdout())

### Name: ekio_colors
### Title: Enhanced EKIO Color System
### Aliases: ekio_colors

### ** Examples

# EKIO palettes
ekio_colors("modern_premium")

# Mapping palettes
ekio_colors("RdBu")
ekio_colors("viridis")

# Scientific palettes
ekio_colors("hokusai1")

# Custom EKIO combinations
ekio_colors("midnight_steel")




cleanEx()
nameEx("ekio_hexmap")
### * ekio_hexmap

flush(stderr()); flush(stdout())

### Name: ekio_hexmap
### Title: EKIO Hexbin Map with Professional Styling
### Aliases: ekio_hexmap

### ** Examples

library(ggplot2)
library(dplyr)

# Basic hexbin map
ekio_hexmap(faithful, waiting, eruptions)

# With point overlay
ekio_hexmap(faithful, waiting, eruptions, show_points = TRUE)




cleanEx()
nameEx("ekio_histogram")
### * ekio_histogram

flush(stderr()); flush(stdout())

### Name: ekio_histogram
### Title: EKIO Histogram with Professional Styling
### Aliases: ekio_histogram

### ** Examples

library(ggplot2)

# Basic histogram
ekio_histogram(mtcars, mpg)

# With custom binning
ekio_histogram(mtcars, mpg, bins = "FD")
ekio_histogram(mtcars, mpg, binwidth = 2)

# Custom styling
ekio_histogram(mtcars, mpg,
               fill_palette = "academic_authority",
               fill_index = 6)




cleanEx()
nameEx("ekio_lineplot")
### * ekio_lineplot

flush(stderr()); flush(stdout())

### Name: ekio_lineplot
### Title: EKIO Line Plot with Professional Styling
### Aliases: ekio_lineplot

### ** Examples

library(ggplot2)
library(dplyr)

# Single line plot
ekio_lineplot(economics, date, unemploy)

# Multiple lines with grouping
ekio_lineplot(mtcars, wt, mpg, color = factor(cyl))

# Custom styling
ekio_lineplot(economics, date, unemploy,
              palette = "academic_authority",
              single_color_index = 6)




cleanEx()
nameEx("ekio_lolli")
### * ekio_lolli

flush(stderr()); flush(stdout())

### Name: ekio_lolli
### Title: EKIO Lollipop Plot with Professional Styling
### Aliases: ekio_lolli

### ** Examples

library(ggplot2)
library(dplyr)

# Basic lollipop plot
mtcars |> slice_head(n = 10) |>
  ekio_lolli(reorder(rownames(.), mpg), mpg)

# Horizontal with colors
mtcars |> slice_head(n = 8) |>
  ekio_lolli(reorder(rownames(.), mpg), mpg,
             color = factor(cyl), horizontal = TRUE)




cleanEx()
nameEx("ekio_palette")
### * ekio_palette

flush(stderr()); flush(stdout())

### Name: ekio_palette
### Title: Generate EKIO Color Palettes with Custom Indexing
### Aliases: ekio_palette

### ** Examples

# Generate discrete palette
ekio_palette("modern_premium", n = 5)

# Generate categorical colors
ekio_palette("categorical", n = 4)

# Continuous palette
ekio_palette("academic_authority", n = 20, type = "continuous")

# Custom indexing for non-obvious orders
ekio_palette("modern_premium", indices = c(9, 7, 5, 4))




cleanEx()
nameEx("ekio_parallel")
### * ekio_parallel

flush(stderr()); flush(stdout())

### Name: ekio_parallel
### Title: EKIO Parallel Coordinates Plot with Professional Styling
### Aliases: ekio_parallel

### ** Examples

library(ggplot2)
library(dplyr)

# Basic parallel coordinates
ekio_parallel(iris, c("Sepal.Length", "Sepal.Width", "Petal.Length"))

# Grouped with bumps
ekio_parallel(iris, c("Sepal.Length", "Sepal.Width", "Petal.Length"),
              group = Species, use_bumps = TRUE)




cleanEx()
nameEx("ekio_scatterplot")
### * ekio_scatterplot

flush(stderr()); flush(stdout())

### Name: ekio_scatterplot
### Title: EKIO Scatter Plot with Professional Styling
### Aliases: ekio_scatterplot

### ** Examples

library(ggplot2)
library(dplyr)

# Basic scatter plot
ekio_scatterplot(mtcars, wt, mpg)

# With color grouping
ekio_scatterplot(mtcars, wt, mpg, color = factor(cyl))

# With size and smooth line
ekio_scatterplot(mtcars, wt, mpg,
                 color = factor(cyl),
                 size = hp,
                 add_smooth = TRUE)

# Custom styling
ekio_scatterplot(mtcars, wt, mpg,
                 palette = "categorical_warm",
                 point_shape = 19,
                 add_smooth = TRUE,
                 smooth_method = "gam")




cleanEx()
nameEx("get_ekio_font")
### * get_ekio_font

flush(stderr()); flush(stdout())

### Name: get_ekio_font
### Title: Get Best Available EKIO Font
### Aliases: get_ekio_font

### ** Examples

fonts <- load_ekio_fonts()
get_ekio_font("primary", fonts)




cleanEx()
nameEx("import_fira_code")
### * import_fira_code

flush(stderr()); flush(stdout())

### Name: import_fira_code
### Title: Import Fira Code Font Family
### Aliases: import_fira_code

### ** Examples

## Not run: 
##D import_fira_code()
## End(Not run)



cleanEx()
nameEx("import_lato")
### * import_lato

flush(stderr()); flush(stdout())

### Name: import_lato
### Title: Import Lato Font Family
### Aliases: import_lato

### ** Examples

## Not run: 
##D import_lato()
## End(Not run)



cleanEx()
nameEx("import_roboto_condensed")
### * import_roboto_condensed

flush(stderr()); flush(stdout())

### Name: import_roboto_condensed
### Title: Import Roboto Condensed Font Family
### Aliases: import_roboto_condensed

### ** Examples

## Not run: 
##D import_roboto_condensed()
## End(Not run)



cleanEx()
nameEx("list_ekio_palettes")
### * list_ekio_palettes

flush(stderr()); flush(stdout())

### Name: list_ekio_palettes
### Title: List All Available EKIO Palettes
### Aliases: list_ekio_palettes

### ** Examples

# See all available palettes
list_ekio_palettes()

# Filter by category
list_ekio_palettes("categorical")
list_ekio_palettes("accent")
list_ekio_palettes("diverging")




cleanEx()
nameEx("load_ekio_fonts")
### * load_ekio_fonts

flush(stderr()); flush(stdout())

### Name: load_ekio_fonts
### Title: Load EKIO Fonts
### Aliases: load_ekio_fonts

### ** Examples

## Not run: 
##D # Load fonts at start of session
##D load_ekio_fonts()
##D 
##D # Install missing fonts and load
##D load_ekio_fonts(install_missing = TRUE)
## End(Not run)



cleanEx()
nameEx("scale_color_ekio_c")
### * scale_color_ekio_c

flush(stderr()); flush(stdout())

### Name: scale_color_ekio_c
### Title: EKIO Continuous Color Scale
### Aliases: scale_color_ekio_c scale_colour_ekio_c

### ** Examples

library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg, color = hp)) +
  geom_point() +
  scale_color_ekio_c("modern_premium")




cleanEx()
nameEx("scale_color_ekio_d")
### * scale_color_ekio_d

flush(stderr()); flush(stdout())

### Name: scale_color_ekio_d
### Title: Enhanced EKIO Discrete Color Scale with Custom Indexing
### Aliases: scale_color_ekio_d scale_colour_ekio_d

### ** Examples

library(ggplot2)

# Standard usage
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  scale_color_ekio_d("categorical")

# Custom indexing for specific colors
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  scale_color_ekio_d("modern_premium", indices = c(9, 6, 3))




cleanEx()
nameEx("scale_fill_ekio_c")
### * scale_fill_ekio_c

flush(stderr()); flush(stdout())

### Name: scale_fill_ekio_c
### Title: EKIO Continuous Fill Scale
### Aliases: scale_fill_ekio_c

### ** Examples

library(ggplot2)
ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_tile() +
  scale_fill_ekio_c("academic_authority")




cleanEx()
nameEx("scale_fill_ekio_d")
### * scale_fill_ekio_d

flush(stderr()); flush(stdout())

### Name: scale_fill_ekio_d
### Title: Enhanced EKIO Discrete Fill Scale with Custom Indexing
### Aliases: scale_fill_ekio_d

### ** Examples

library(ggplot2)
ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_ekio_d("categorical_cool", indices = c(7, 4, 1))




cleanEx()
nameEx("show_ekio_palette")
### * show_ekio_palette

flush(stderr()); flush(stdout())

### Name: show_ekio_palette
### Title: Display EKIO Palette Visually
### Aliases: show_ekio_palette

### ** Examples

## Not run: 
##D # Basic palette display
##D show_ekio_palette("modern_premium")
##D 
##D # Show specific colors
##D show_ekio_palette("modern_premium", indices = c(9, 7, 5, 3))
##D 
##D # Show with color codes
##D show_ekio_palette("categorical", show_names = TRUE)
## End(Not run)




cleanEx()
nameEx("theme_ekio")
### * theme_ekio

flush(stderr()); flush(stdout())

### Name: theme_ekio
### Title: Enhanced EKIO ggplot2 Theme with Font Management
### Aliases: theme_ekio

### ** Examples

library(ggplot2)

# Basic usage
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_ekio() +
  labs(title = "Brazilian Vehicle Analysis")

# With font loading
## Not run: 
##D load_ekio_fonts()
##D ggplot(mtcars, aes(x = wt, y = mpg)) +
##D   geom_point() +
##D   theme_ekio("academic_authority", use_ekio_fonts = TRUE)
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
