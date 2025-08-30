library(dplyr)
import::from(tidyr, pivot_longer)
import::from(here, here)

dat <- readr::read_csv(here("data-raw/ips_brasil_municipios.csv"))

dict <- tibble(
  original_names = names(dat),
  col_names = janitor::make_clean_names(original_names)
)

inds <- c(
  "indice_de_progresso_social",
  "pib_per_capita_2021",
  "agua_e_saneamento",
  "moradia",
  "seguranca_pessoal",
  "saude_e_bem_estar",
  "nota_media_no_enem",
  "empregados_com_ensino_superior"
)

labels <- c(
  "Social Progress Index",
  "GDP per capita",
  "Water and Sanitation",
  "Housing Conditions",
  "Safety",
  "Healthcare and Wellbeing",
  "Avg. ENEM scores(*)",
  "Share College Educ."
)

subdat <- dat |>
  janitor::clean_names() |>
  select(
    codigo_ibge,
    municipio,
    uf,
    populacao_2022,
    all_of(inds)
  )

ranked <- subdat |>
  slice_max(populacao_2022, n = 25) |>
  mutate(
    across(all_of(inds), ~ rank(-.x))
  )

codes <- c(3550308, 5300108, 3304557, 1501402, 4314902, 2304400, 2611606)

ips_brasil <- ranked |>
  pivot_longer(all_of(inds), names_to = "measure", values_to = "rank") |>
  mutate(
    measure = factor(measure, levels = inds),
    highlight = if_else(codigo_ibge %in% codes, municipio, ""),
    is_highlight = factor(if_else(codigo_ibge %in% codes, 1L, 0L)),
    rank_labels = if_else(rank %in% c(1, 5, 10, 15, 20, 25), rank, NA),
    is_highlight = factor(ifelse(codigo_ibge %in% codes, 1L, 0L)),
    rank_labels = if_else(
      rank %in% c(1, 5, 10, 15, 20, 25),
      paste0(rank, "°"),
      NA
    )
  )

usethis::use_data(ips_brasil, overwrite = TRUE)
