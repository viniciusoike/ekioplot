load_all()

ekioplot::list_ekio_palettes()

# Let's make some nice examples and tests

install.packages("maddison")

library(maddison)

maddison::maddison

library(dplyr)
library(ggplot2)

brazil <- maddison |>
  filter(country == "Brazil", year >= 1950)

ggplot(brazil, aes(year, rgdpnapc)) +
  geom_line(color = ekio_palette()[9]) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")
  ) +
  theme_ekio()

latam <- maddison |>
  filter(
    country %in% c("Argentina", "Brazil", "Mexico", "Peru", "Uruguay"),
    year >= 1980
  )

ggplot(latam, aes(year, rgdpnapc, color = country)) +
  geom_line() +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")
  ) +
  scale_color_ekio_d("categorical_extended") +
  theme_ekio() +
  theme(
    axis.title.x = element_blank()
  )

brazil_2000 <- subset(brazil, year %in% 2000:2008)

# Simple barplot (no data labels)
ggplot(brazil_2000, aes(year, rgdpnapc)) +
  geom_col(width = 0.8, fill = ekio_palette("sophisticated_unique")[7]) +
  geom_hline(yintercept = 0, linewidth = 0.8) +
  scale_x_continuous(
    breaks = seq(min(brazil_2000$year), max(brazil_2000$year), by = 1)
  ) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = "."),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_ekio() +
  theme(
    panel.grid.major.x = element_blank()
  )

# Barplot with data labels
ggplot(brazil_2000, aes(year, rgdpnapc)) +
  geom_col(width = 0.8, fill = ekio_palette("sophisticated_unique")[7]) +
  geom_text(
    aes(label = scales::number(rgdpnapc, big.mark = ".", accuracy = 1e2)),
    vjust = -0.5,
    size = 3,
    family = "Avenir"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.8) +
  scale_x_continuous(
    breaks = seq(min(brazil_2000$year), max(brazil_2000$year), by = 1)
  ) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = "."),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_ekio() +
  theme(
    panel.grid.major.x = element_blank()
  )


latam_countries <- maddison |>
  filter(region == "South America", year == 2000) |>
  pull(iso3c)

avg_latam_growth <- maddison |>
  filter(
    iso3c %in% latam_countries,
    year >= 2000,
    year <= 2013
  ) |>
  mutate(growth = (rgdpnapc / lag(rgdpnapc) - 1) * 100, .by = "country") |>
  summarise(
    avg = mean(growth, na.rm = TRUE),
    .by = "country"
  ) |>
  mutate(
    country = factor(country),
    country = forcats::fct_reorder(country, avg)
  )


# Horizontal barplot with data labels
ggplot(avg_latam_growth, aes(country, avg)) +
  geom_col(width = 0.8, fill = ekio_palette("sophisticated_unique")[7]) +
  geom_hline(yintercept = 0, linewidth = 0.8) +
  geom_text(
    aes(label = scales::number(avg, accuracy = 0.1, decimal.mark = ",")),
    hjust = -0.5,
    size = 3,
    family = "Avenir"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = "."),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_ekio() +
  theme(
    panel.grid.major.y = element_blank()
  )

# Horizontal barplot with data labels + integrated axis title
ggplot(avg_latam_growth, aes(country, avg)) +
  geom_col(width = 0.8, fill = ekio_palette("sophisticated_unique")[7]) +
  geom_hline(yintercept = 0, linewidth = 0.8) +
  geom_text(
    aes(label = scales::number(avg, accuracy = 0.1, decimal.mark = ",")),
    hjust = -0.5,
    size = 10,
    size.unit = "pt",
    family = "Avenir"
  ) +
  geom_text(
    aes(x = country, y = 0, label = country),
    size = 10,
    size.unit = "pt",
    family = "Avenir",
    color = "white",
    hjust = 0,
    nudge_y = 0.1
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = "."),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_ekio() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  )


# Barplot with labels (both positive and negative values)

brazil_contemp <- subset(brazil, year >= 2000, year <= 2015)

brazil_contemp <- brazil_contemp |>
  mutate(
    growth = (rgdpnapc / lag(rgdpnapc) - 1) * 100,
    is_growth = factor(if_else(growth > 0, 1L, 0L))
  ) |>
  filter(!is.na(growth))

ggplot(brazil_contemp, aes(year, growth)) +
  geom_col(aes(fill = is_growth), width = 0.8) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = "."),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_ekio_d("RdBu", indices = c(2, 10)) +
  theme_ekio()

# Very bad tone of red
ggplot(brazil_contemp, aes(year, growth)) +
  geom_col(aes(fill = is_growth), width = 0.8) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = "."),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_ekio_d(indices = c(4, 1)) +
  theme_ekio()

# This red a little too dark but goes well with academic authority
ggplot(brazil_contemp, aes(year, growth)) +
  geom_col(aes(fill = is_growth), width = 0.8) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = "."),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(
    values = c(ekio_palette("RdBu")[2], ekio_palette("academic_authority")[7])
  ) +
  theme_ekio()

# Simple histogram

brazil <- brazil |>
  mutate(
    growth = (rgdpnapc / lag(rgdpnapc) - 1) * 100
  )

ggplot(brazil, aes(growth)) +
  geom_histogram(
    fill = ekio_palette("institutional_oxford")[7],
    color = "white",
    bins = nclass.Sturges(na.omit(brazil$rgdpnapc))
  ) +
  geom_hline(yintercept = 0, linewidth = 0.8) +
  scale_x_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")
  ) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = "."),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_ekio()

# Simple scatter plot

brazil_recent <- brazil |>
  filter(year >= 1980, year <= 2015) |>
  mutate(
    pop_growth = (pop / lag(pop) - 1) * 100,
    gdp_growth = (rgdpnapc / lag(rgdpnapc) - 1) * 100,
    decade = floor(year / 10) * 10
  )

ggplot(brazil_recent, aes(pop_growth, gdp_growth)) +
  geom_point(aes(color = as.factor(decade))) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")
  ) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")
  ) +
  scale_color_ekio_d("categorical_extended") +
  theme_ekio()

ggplot(brazil_recent, aes(pop_growth, gdp_growth)) +
  geom_point(aes(fill = as.factor(decade)), shape = 21, color = "gray20") +
  geom_hline(yintercept = 0) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = ekio_palette()[9]
  ) +
  scale_x_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")
  ) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")
  ) +
  scale_fill_ekio_d("categorical_extended") +
  theme_ekio()

ggplot(brazil_recent, aes(pop_growth, gdp_growth)) +
  geom_point(aes(color = as.factor(decade))) +
  geom_hline(yintercept = 0) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = ekio_palette()[9]
  ) +
  scale_x_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")
  ) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")
  ) +
  scale_color_ekio_d("categorical_extended") +
  theme_ekio()

ggplot(brazil_recent, aes(pop_growth, gdp_growth)) +
  geom_point(aes(color = as.factor(decade), size = pop), alpha = 0.75) +
  geom_hline(yintercept = 0) +
  geom_smooth(
    method = "gam",
    se = FALSE,
    color = ekio_palette()[9]
  ) +
  scale_x_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")
  ) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = ".")
  ) +
  scale_color_ekio_d("categorical_extended") +
  guides(size = "none") +
  theme_ekio()


ggplot(brazil_recent, aes(as.factor(decade), gdp_growth, group = decade)) +
  geom_boxplot(fill = ekio_palette()[8]) +
  theme_ekio()


list_ekio_palettes()

# Seems to be off
ekio_palette("academic authority")

ekio_palette("categorical_cool")
ekio_palette("categorical_warm")
ekio_palette("categorical")
ekio_palette("categorical_extended")
