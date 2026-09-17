# Diverging palette choropleths ----

devtools::load_all()

library(dplyr)
library(geobr)
library(ggplot2)
library(sf)

municipalities <- geobr::read_municipality(
  year = 2020,
  simplified = TRUE,
  showProgress = FALSE
) |>
  select(code_muni, geometry)

agriculture_map <- brazil_agriculture |>
  mutate(code_muni = as.numeric(code_muni)) |>
  left_join(municipalities, by = "code_muni") |>
  st_as_sf()

plot_agriculture <- function(data, crop_name, palette, title) {
  plot_data <- data |>
    filter(crop == crop_name, production_tonnes > 0) |>
    mutate(
      production_index = log2(
        production_tonnes / median(production_tonnes, na.rm = TRUE)
      )
    )

  limit <- max(abs(plot_data$production_index), na.rm = TRUE)

  ggplot(plot_data, aes(fill = production_index)) +
    geom_sf(color = NA) +
    scale_fill_ekio_c(
      palette,
      limits = c(-limit, limit),
      oob = scales::squish,
      name = "Production\nvs. median"
    ) +
    coord_sf(datum = NA) +
    labs(title = title, subtitle = crop_name) +
    theme_ekio() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
}

maps <- list(
  plot_agriculture(
    agriculture_map,
    "soybeans",
    "purple_orange",
    "Brazil"
  ),
  plot_agriculture(
    filter(agriculture_map, name_region == "Sul"),
    "corn",
    "blue_red",
    "South"
  ),
  plot_agriculture(
    filter(agriculture_map, name_region == "Nordeste"),
    "sugarcane",
    "teal_orange",
    "Northeast"
  ),
  plot_agriculture(
    filter(agriculture_map, name_region == "Centro Oeste"),
    "soybeans",
    "purple_green",
    "Mid-West"
  ),
  plot_agriculture(
    filter(agriculture_map, name_state %in% c("São Paulo", "Minas Gerais")),
    "corn",
    "purple_orange",
    "São Paulo and Minas Gerais"
  )
)

invisible(lapply(maps, print))
