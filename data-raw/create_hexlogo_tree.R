# Hexagon Logo Generator for ekioplot ----
#
# Generates the package hexagon logo (man/figures/logo.png). The logo features
# a treemap of EKIO brand colors arranged using Fibonacci proportions, on a
# hex sticker following EKIO brand colors.
#
# Requirements:
#   - ggplot2, treemapify, hexSticker, showtext, magick
#   - ekioplot (devtools::load_all())
#
# Output:
#   - man/figures/logo.png       (light: white fill, blue border)
#   - man/figures/logo_dark.png  (dark:  blue fill, transparent corners)
#
# Usage:
#   1. From the package root, run: devtools::load_all()
#   2. Source this entire script.

# ---- Packages ----
library(ggplot2)
library(ekioplot)
library(treemapify)
library(hexSticker)
library(showtext)

# Setup fonts for high-quality rendering. Avenir is a system font on macOS;
# adjust the path if registering it fails on your platform.
font_add("Avenir", regular = "/System/Library/Fonts/Avenir.ttc")
showtext_opts(dpi = 400)
showtext_auto()

# ---- Color Showcase ----
# Pull colors from across the EKIO palette families to highlight the range:
# the core brand blue, contrasting accents, and a teal/orange pairing.
colors_showcase <- c(
  ekio_blue["700"], # core brand blue
  ekio_pal("contrast")[2], # orange accent
  ekio_pal("contrast")[3], # teal
  ekio_pal("contrast")[4], # amber
  ekio_pal("contrast")[5], # purple
  ekio_blue["400"], # mid blue
  ekio_orange["400"] # soft orange
)
colors_showcase <- unname(colors_showcase)

# ---- Treemap Data ----
# Fibonacci-based areas give visually pleasing relative block sizes.
fib <- c(1, 1, 2, 3, 5, 8, 13, 21)
fib <- 1 / fib
fib <- fib[-1]

treemap_data <- data.frame(
  area = fib[seq_along(colors_showcase)],
  color_id = factor(seq_along(colors_showcase)),
  fill_color = colors_showcase
)

# ---- Treemap Subplot ----
# White borders separate the color blocks for clarity.
subplot <- ggplot(treemap_data, aes(area = area, fill = color_id)) +
  geom_treemap(color = "#FFFFFF", size = 2, start = "topleft") +
  scale_fill_manual(values = colors_showcase) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

# ---- Light Sticker ----
# White background with EKIO blue border.
sticker(
  subplot = subplot,
  s_x = 1,
  s_y = 1,
  s_width = 2,
  s_height = 2.5,
  package = "ekioplot",
  p_x = 0.72,
  p_y = 1.33,
  p_color = "#FFFFFF",
  p_family = "Avenir",
  p_size = 7,
  h_fill = "#FFFFFF",
  h_color = "#000000",
  h_size = 1.2,
  filename = "man/figures/logo.png",
  dpi = 400,
  spotlight = FALSE,
  white_around_sticker = TRUE
)

# ---- Dark Sticker ----
# EKIO blue background with darker border, for dark contexts.
sticker(
  subplot = subplot,
  s_x = 1,
  s_y = 0.8,
  s_width = 1.05,
  s_height = 1.05,
  package = "ekioplot",
  p_x = 1,
  p_y = 1.52,
  p_color = "#FFFFFF",
  p_family = "Avenir",
  p_size = 8,
  h_fill = ekio_blue[["700"]],
  h_color = ekio_blue[["900"]],
  h_size = 1.2,
  filename = "man/figures/logo_dark.png",
  dpi = 300,
  spotlight = FALSE,
  white_around_sticker = TRUE
)

# ---- Transparent Corners (dark version) ----
# Make the white corners around the hexagon transparent.
library(magick)

p <- image_read("man/figures/logo_dark.png")
info <- image_info(p)
w <- info$width
h <- info$height

pp <- p |>
  image_fill("transparent", refcolor = "white", fuzz = 4, point = "+1+1") |>
  image_fill(
    "transparent",
    refcolor = "white",
    fuzz = 4,
    point = paste0("+", w - 1, "+1")
  ) |>
  image_fill(
    "transparent",
    refcolor = "white",
    fuzz = 4,
    point = paste0("+1+", h - 1)
  ) |>
  image_fill(
    "transparent",
    refcolor = "white",
    fuzz = 4,
    point = paste0("+", w - 1, "+", h - 1)
  )

image_write(pp, path = "man/figures/logo_dark.png")

cli::cli_alert_success("Hex logo created")
cli::cli_ul(c(
  "man/figures/logo.png (light)",
  "man/figures/logo_dark.png (dark, transparent corners)"
))
