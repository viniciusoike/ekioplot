# Hexagon Logo Generator for ekioplot ----
#
# Generates the package hexagon logo (man/figures/logo.png). The logo is a
# Voronoi treemap of EKIO brand colors with Fibonacci-based weights, clipped
# to a hexagon, built with ggvmap (https://github.com/loukesio/ggvmap).
# The previous treemap logo lives in data-raw/archive/create_hexlogo_tree.R.
#
# Requirements:
#   - ggplot2, ggvmap, showtext, sysfonts
#   - ekioplot (devtools::load_all())
#
# Output:
#   - man/figures/logo.png  (black border, transparent corners)
#
# Usage:
#   1. From the package root, run: devtools::load_all()
#   2. Source this entire script.

# ---- Packages ----
library(ggplot2)
library(ggvmap)
library(ekioplot)

sysfonts::font_add_google("Host Grotesk", "Host Grotesk")
showtext::showtext_opts(dpi = 400)
showtext::showtext_auto()

# ---- Colors and Weights ----
blue <- ekio_pal("blue")

colors_showcase <- unname(c(
  blue["700"], # core brand blue
  ekio_pal("full")[2:5], # orange, teal, gold, red
  blue["400"], # mid blue
  ekio_pal("orange")["300"] # soft orange
))

fib <- 1 / c(1, 2, 3, 5, 8, 13, 21)

# ---- Voronoi Layout ----
# The seed fixes the cell layout; seed 2 puts the largest cell at the top.
vm <- voronoi_map(fib, clip = clip_hexagon(), seed = 2)

# Wordmark sits on the largest cell's centroid
cent <- vm_centroids(vm)
big <- cent[which.max(cent$data_weight), ]

hex <- as.data.frame(clip_hexagon())
names(hex) <- c("x", "y")

# ---- Plot ----
logo <- ggvmap(
  vm,
  palette = colors_showcase,
  show_labels = FALSE,
  border_col = "#FFFFFF",
  border_size = 1.6
) +
  geom_polygon(
    data = hex,
    aes(x, y),
    fill = NA,
    color = "#000000",
    linewidth = 1.6,
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x = big$cx,
    y = big$cy,
    label = "ekioplot",
    family = "Host Grotesk",
    color = "#FFFFFF",
    size = 7
  ) +
  suppressMessages(coord_fixed(expand = FALSE, clip = "off")) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.position = "none",
    plot.margin = margin(4, 4, 4, 4)
  )

# ---- Save ----
# Same size as a hexSticker sticker
ggsave(
  "man/figures/logo.png",
  logo,
  width = 43.9,
  height = 50.8,
  units = "mm",
  dpi = 400,
  bg = "transparent"
)

cli::cli_alert_success("Hex logo created: man/figures/logo.png")
