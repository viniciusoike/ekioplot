library(dplyr)
library(readr)

col_names <- c("entity", "code", "year", "gas", "oil", "coal")

dat <- read_csv(
  "data-raw/global-fossil-fuel-consumption.csv",
  skip = 1,
  col_names = col_names,
  show_col_types = FALSE
)

fuels <- dat |>
  select(-code) |>
  tidyr::pivot_longer(
    cols = c(gas, oil, coal),
    names_to = "fuel",
    values_to = "consumption_gwh"
  )

usethis::use_data(fuels, overwrite = TRUE)
