#' Brazilian municipal population, 2025
#'
#' Population estimates for Brazil's 338 municipalities with more than 100,000
#' inhabitants.
#'
#' @format A tibble with 338 rows and 5 variables:
#' \describe{
#'   \item{rank}{Population rank (numeric)}
#'   \item{name_muni}{Municipality name (character)}
#'   \item{abbrev_state}{State abbreviation (character)}
#'   \item{population}{2025 population estimate (numeric)}
#'   \item{category}{Ordered city-size category (factor)}
#' }
#'
#' @details Categories are Metropolis (1M+), Large city (500K–1M), Medium city
#' (200K–500K) and Small city (100K–200K).
#'
#' @source IBGE, Table 6579: Municipal population estimates.
#' \url{https://sidra.ibge.gov.br/tabela/6579}
"brazil_population"


#' Brazilian municipal GDP, 2021
#'
#' Municipal GDP for all 5,570 Brazilian municipalities, at current prices.
#'
#' @format A tibble with 5,570 rows and 7 variables:
#' \describe{
#'   \item{code_muni}{IBGE municipality code (numeric)}
#'   \item{name_muni}{Municipality name (character)}
#'   \item{code_state}{IBGE state code (numeric)}
#'   \item{name_state}{State name (character)}
#'   \item{year}{Observation year (numeric)}
#'   \item{gdp_brl_k}{GDP, thousands of Brazilian reais (numeric)}
#'   \item{gdp_brl_m}{GDP, millions of Brazilian reais (numeric)}
#' }
#'
#' @source IBGE, Table 5938: GDP and other aggregates by municipality.
#' \url{https://sidra.ibge.gov.br/tabela/5938}
#'
#' @references IBGE. (2023). Produto Interno Bruto dos Municípios - 2021.
"brazil_gdp"


#' Brazilian municipal crop production, 2022
#'
#' Municipal production of soybeans, corn and sugarcane from IBGE's Municipal
#' Agricultural Production Survey (PAM).
#'
#' @format A tibble with 16,689 rows and 12 variables:
#' \describe{
#'   \item{code_muni}{IBGE municipality code (numeric)}
#'   \item{name_muni}{Municipality name (character)}
#'   \item{name_state}{State name (character)}
#'   \item{name_region}{Brazilian region (character)}
#'   \item{crop}{Crop name (character)}
#'   \item{production_tonnes}{Production volume, tonnes (numeric)}
#'   \item{area_harvested_ha}{Harvested area, hectares (numeric)}
#'   \item{yield}{Productivity, tonnes per hectare (numeric)}
#'   \item{crop_type}{Crop cycle (character)}
#'   \item{crop_category}{Crop category (character)}
#'   \item{crop_importance}{Economic importance (character)}
#'   \item{production_scale}{Production-scale category (integer)}
#' }
#'
#' @source IBGE, Table 1612: Municipal Agricultural Production (PAM).
#' \url{https://sidra.ibge.gov.br/tabela/1612}
#'
#' @references IBGE. (2023). Produção Agrícola Municipal - PAM 2022.
"brazil_agriculture"


#' Brazilian state crop production, 1974–2023
#'
#' Annual production of seven major crops by Brazilian state from IBGE's
#' Municipal Agricultural Production Survey (PAM).
#'
#' @format A tibble with 9,450 rows and 8 variables:
#' \describe{
#'   \item{code_state}{IBGE state code (numeric)}
#'   \item{name_state}{State name (character)}
#'   \item{year}{Observation year (numeric)}
#'   \item{crop}{Crop name (character)}
#'   \item{production_tonnes}{Production volume, tonnes (numeric)}
#'   \item{area_harvested_ha}{Harvested area, hectares (numeric)}
#'   \item{yield_kg_per_ha}{Productivity, kilograms per hectare (numeric)}
#'   \item{production_value_brl_k}{Production value, thousands of Brazilian reais (numeric)}
#' }
#'
#' @details Crops are beans, corn, cotton, rice, soybeans, sugarcane and wheat.
#'
#' @source IBGE, Table 1612: Municipal Agricultural Production (PAM).
#' \url{https://sidra.ibge.gov.br/tabela/1612}
#'
#' @references IBGE. (2023). Produção Agrícola Municipal - PAM 2022.
"brazil_agriculture_states"
