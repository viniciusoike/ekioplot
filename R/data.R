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


#' Brazilian population projections by age and sex, 2025–2100
#'
#' Population projections for Brazil at five-year intervals, by five-year age
#' group and sex. The data are a transformed subset of the United Nations
#' World Population Prospects 2024, Online Edition.
#'
#' @format A tibble with 336 rows and 15 variables:
#' \describe{
#'   \item{country_code}{WPP country or area code (numeric)}
#'   \item{name}{Country or area name (character)}
#'   \item{year}{Projection year (integer)}
#'   \item{age}{Five-year age band, from 0–4 to 100+ (ordered factor)}
#'   \item{ibge_age_group}{Age group derived by ekioplot: 0–14, 15–64 or 65+
#'   (ordered factor)}
#'   \item{age_group}{Age group derived by ekioplot: 0–19, 20–29, 30–39,
#'   40–49, 50–59, 60–69, 70–79 or 80+ (ordered factor)}
#'   \item{population_m}{Medium-variant projected male population, in persons
#'   (numeric)}
#'   \item{population_f}{Medium-variant projected female population, in persons
#'   (numeric)}
#'   \item{population}{Medium-variant projected total population, in persons
#'   (numeric)}
#'   \item{population_m_low}{Low-variant projected male population, in persons
#'   (numeric)}
#'   \item{population_m_high}{High-variant projected male population, in persons
#'   (numeric)}
#'   \item{population_f_low}{Low-variant projected female population, in persons
#'   (numeric)}
#'   \item{population_f_high}{High-variant projected female population, in persons
#'   (numeric)}
#'   \item{population_low}{Low-variant projected total population, in persons
#'   (numeric)}
#'   \item{population_high}{High-variant projected total population, in persons
#'   (numeric)}
#' }
#'
#' @details The dataset contains 16 projection years (2025–2100) and 21 age
#' bands. WPP reports values in thousands; ekioplot converts them to persons and
#' rounds them to the nearest person. The two aggregate age-group variables are
#' derived by ekioplot from the WPP five-year bands.
#'
#' The `*_low` and `*_high` columns are WPP low- and high-fertility variants,
#' respectively. They are alternative scenarios, not probability intervals.
#'
#' Data © United Nations, 2024. Source data are made available under
#' [CC BY 3.0 IGO](https://creativecommons.org/licenses/by/3.0/igo/). This is a
#' transformed subset prepared by ekioplot; the United Nations does not endorse
#' ekioplot or this dataset.
#'
#' @source United Nations, Department of Economic and Social Affairs,
#' Population Division (2024). *World Population Prospects 2024, Online
#' Edition*.
#' \url{https://www.un.org/development/desa/pd/content/world-population-prospects-2024}
#'
#' @references United Nations Population Division (2024). *wpp2024: World
#' Population Prospects 2024*. R package.
#' \url{https://github.com/PPgp/wpp2024}
#' @keywords datasets
"brazil_population_forecast"


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
