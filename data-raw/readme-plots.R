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

# Setup ------------------------------------------------------------------

library(ekioplot)
library(ggplot2)
library(patchwork)

fig_dir <- "man/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Consistent export: ragg for crisp system-font rendering.
save_fig <- function(plot, name, width = 8, height = 5, dpi = 300) {
  ggsave(
    file.path(fig_dir, name),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    device = ragg::agg_png,
    bg = "white"
  )
}


# Plots ------------------------------------------------------------------

## Main plot --------------------------------------------------------------

hero <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_ekio_d("full") +
  labs(
    title = "Fuel Efficiency vs. Weight",
    subtitle = "Motor Trend Car Road Tests (1974)",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon",
    color = "Cylinders"
  ) +
  theme_ekio()

save_fig(hero, "README-hero.png", width = 8, height = 5)

## Palette overview -------------------------------------------------------

# One row per palette, each normalised to the same width so palettes of
# different lengths stay visually comparable.
pal_names <- c(
  "full",
  "full_muted",
  "cool3",
  "cool4",
  "accent_blue",
  "accent_orange",
  "gold",
  "blue",
  "blue_orange",
  "okabe_ito"
)

pal_rows <- lapply(seq_along(pal_names), function(i) {
  cols <- ekio_pal(pal_names[i])
  n <- length(cols)
  data.frame(
    ord = i,
    idx = seq_len(n),
    n = n,
    color = cols,
    stringsAsFactors = FALSE
  )
})
pal_df <- do.call(rbind, pal_rows)
pal_df$xmin <- (pal_df$idx - 1) / pal_df$n
pal_df$xmax <- pal_df$idx / pal_df$n

palettes <- ggplot(pal_df) +
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ord - 0.42,
      ymax = ord + 0.42,
      fill = color
    ),
    color = "white",
    linewidth = 0.6
  ) +
  scale_fill_identity() +
  scale_y_continuous(
    breaks = seq_along(pal_names),
    labels = pal_names,
    trans = "reverse",
    expand = expansion(add = 0.3)
  ) +
  scale_x_continuous(expand = expansion(0)) +
  labs(title = "Selected EKIO palettes") +
  theme_ekio(grid = "none") +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "mono", hjust = 0)
  )

save_fig(palettes, "README-palettes.png", width = 8, height = 6)

## Recipe grid ------------------------------------------------------------

cyl_counts <- as.data.frame(table(cyl = mtcars$cyl))
economic_series <- subset(
  ggplot2::economics_long,
  variable %in% c("pce", "psavert", "uempmed")
)

ek_scatterplot <- ekio_scatterplot(mtcars, wt, mpg, color = factor(cyl)) +
  labs(title = "ekio_scatterplot()") +
  theme(
    plot.title = element_text(family = "Fira Code", face = "bold", size = 16)
  )

ek_barplot <- ekio_barplot(cyl_counts, cyl, Freq) +
  labs(title = "ekio_barplot()") +
  theme(
    plot.title = element_text(family = "Fira Code", face = "bold", size = 16)
  )

ek_lineplot <- ekio_lineplot(
  ggplot2::economics,
  date,
  unemploy
) +
  labs(title = "ekio_lineplot()") +
  theme(
    plot.title = element_text(family = "Fira Code", face = "bold", size = 16)
  )

ek_areaplot <- ekio_areaplot(
  economic_series,
  date,
  value01,
  fill = variable
) +
  labs(title = "ekio_areaplot()") +
  theme(
    plot.title = element_text(family = "Fira Code", face = "bold", size = 16)
  )

recipes <- (ek_scatterplot | ek_barplot) / (ek_lineplot | ek_areaplot)

save_fig(recipes, "README-recipes.png", width = 9, height = 6.5)

cli::cli_alert_success("README figures written to {.path {fig_dir}}")
