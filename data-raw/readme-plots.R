# Generate README showcase images ----
#
# Renders the plots embedded in README.Rmd to `man/figures/` so the README
# (and the pkgdown home page) can reference static images instead of running
# ggplot2 at knit time. Run from the package root after changing themes,
# palettes, or recipe functions:
#
#   Rscript data-raw/readme-plots.R
#
# then re-knit the README with `devtools::build_readme()`.

# ---- Setup ----
pkgload::load_all(".", quiet = TRUE)

library(ggplot2)
library(patchwork)

fig_dir <- "man/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Consistent export: ragg for crisp system-font rendering.
save_fig <- function(plot, name, width = 8, height = 4.5, dpi = 200) {
  ggsave(
    file.path(fig_dir, name),
    plot = plot,
    width = width, height = height, dpi = dpi,
    device = ragg::agg_png, bg = "white"
  )
}

# ---- Hero: themed scatter ----
hero <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_ekio_d("contrast") +
  labs(
    title = "Fuel Efficiency vs. Weight",
    subtitle = "Motor Trend Car Road Tests (1974)",
    x = "Weight (1000 lbs)", y = "Miles per Gallon", color = "Cylinders"
  ) +
  theme_ekio()

save_fig(hero, "README-hero.png", width = 8, height = 5)

# ---- Palette overview ----
# One row per palette, each normalised to the same width so palettes of
# different lengths stay visually comparable.
pal_names <- c(
  "contrast", "cool", "muted", "okabe_ito",
  "blue", "teal", "orange", "blue_orange", "blue_red"
)

pal_rows <- lapply(seq_along(pal_names), function(i) {
  cols <- ekio_pal(pal_names[i])
  n <- length(cols)
  data.frame(
    ord = i, idx = seq_len(n), n = n, color = cols,
    stringsAsFactors = FALSE
  )
})
pal_df <- do.call(rbind, pal_rows)
pal_df$xmin <- (pal_df$idx - 1) / pal_df$n
pal_df$xmax <- pal_df$idx / pal_df$n

palettes <- ggplot(pal_df) +
  geom_rect(
    aes(
      xmin = xmin, xmax = xmax,
      ymin = ord - 0.42, ymax = ord + 0.42, fill = color
    ),
    color = "white", linewidth = 0.6
  ) +
  scale_fill_identity() +
  scale_y_continuous(
    breaks = seq_along(pal_names), labels = pal_names,
    trans = "reverse", expand = expansion(add = 0.3)
  ) +
  scale_x_continuous(expand = expansion(0)) +
  labs(title = "A few of the ~30 EKIO palettes") +
  theme_ekio(grid = "none") +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "mono", hjust = 0)
  )

save_fig(palettes, "README-palettes.png", width = 8, height = 5)

# ---- Recipe grid ----
cyl_counts <- as.data.frame(table(cyl = mtcars$cyl))
world_fuels <- fuels[fuels$entity == "World" & fuels$year >= 1950, ]

recipes <- (
  ekio_histogram(mtcars, mpg) + labs(title = "ekio_histogram()") |
  ekio_barplot(cyl_counts, cyl, Freq) + labs(title = "ekio_barplot()")
) / (
  ekio_lineplot(world_fuels[world_fuels$fuel == "oil", ], year, consumption_gwh) +
    labs(title = "ekio_lineplot()") |
  ekio_areaplot(world_fuels, year, consumption_gwh, fill = fuel) +
    labs(title = "ekio_areaplot()")
)

save_fig(recipes, "README-recipes.png", width = 9, height = 6.5)

cli::cli_alert_success("README figures written to {.path {fig_dir}}")
