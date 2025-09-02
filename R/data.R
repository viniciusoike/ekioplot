#' IPS Brasil 2025 - Social Progress Index for Brazilian Municipalities
#'
#' A dataset containing the Social Progress Index
#' rankings for the top 25 most populated Brazilian municipalities in 2025.
#' The IPS Brasil is a comprehensive index that measures social and environmental
#' progress across all 5,570 Brazilian municipalities using 57 indicators.
#'
#' @format A data frame with 200 rows (25 municipalities � 8 measures) and 8 variables:
#' \describe{
#'   \item{codigo_ibge}{IBGE municipality code (numeric)}
#'   \item{municipio}{Municipality name (character)}
#'   \item{uf}{State abbreviation (character)}
#'   \item{populacao_2022}{Population in 2022 (numeric)}
#'   \item{measure}{Indicator measured, one of 8 key social progress indicators (factor)}
#'   \item{rank}{Ranking position among the 25 municipalities for each measure (1-25, numeric)}
#'   \item{highlight}{Municipality name if it's one of the 7 highlighted cities, empty string otherwise (character)}
#'   \item{is_highlight}{Factor indicating if municipality is highlighted (0 or 1)}
#'   \item{rank_labels}{Formatted rank labels showing only positions 1, 5, 10, 15, 20, 25 with "�" suffix (character)}
#' }
#'
#' @details
#' The dataset focuses on the 25 most populated Brazilian municipalities and includes
#' rankings across 8 key social progress indicators:
#'
#' \strong{Indicators included:}
#' \itemize{
#'   \item \strong{Social Progress Index}: Overall composite score
#'   \item \strong{GDP per capita}: Economic indicator (2021 data)
#'   \item \strong{Water and Sanitation}: Access to basic services
#'   \item \strong{Housing Conditions}: Quality of housing infrastructure
#'   \item \strong{Safety}: Personal security measures
#'   \item \strong{Healthcare and Wellbeing}: Health system performance
#'   \item \strong{Avg. ENEM scores}: Educational outcomes (national exam)
#'   \item \strong{Share College Educ.}: Percentage of population with higher education
#' }
#'
#' \strong{Highlighted municipalities} (7 cities with special focus):
#' The dataset highlights 7 specific municipalities for comparison purposes:
#' S�o Paulo (SP), Bras�lia (DF), Rio de Janeiro (RJ), Bel�m (PA),
#' Porto Alegre (RS), Fortaleza (CE), and Recife (PE).
#'
#' \strong{Data transformation}:
#' Rankings are calculated where rank 1 = best performance and rank 25 = worst
#' performance among the top 25 most populated cities. The data is in long format
#' with one row per municipality-indicator combination.
#'
#' @source
#' IPS Brasil 2025 - Indice de Progresso Social Brasil
#' \url{https://ipsbrasil.org.br/pt}
#'
#' The IPS Brasil is developed by Instituto Imazon and follows the methodology
#' of the Social Progress Imperative, using 57 indicators across three dimensions:
#' Basic Human Needs, Foundations of Wellbeing, and Opportunity.
#'
#' @references
#' Imazon. (2025). Indice de Progresso Social Brasil 2025.
#' \url{https://imazon.org.br/indice_de_progresso_social_brasil_2025/}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(ips_brasil)
#'
#' # View structure
#' str(ips_brasil)
#' }
"ips_brasil"


#' Brazilian Municipal Population Data (2025)
#'
#' A dataset containing population data for Brazilian municipalities with over 
#' 100,000 inhabitants, based on IBGE population estimates for 2025.
#'
#' @format A tibble with 338 rows and 5 variables:
#' \describe{
#'   \item{rank}{Population ranking among all municipalities (numeric)}
#'   \item{name_muni}{Municipality name (character)}
#'   \item{abbrev_state}{State abbreviation (character)}
#'   \item{population}{Total population in 2025 (numeric)}
#'   \item{category}{City size category based on population (ordered factor)}
#' }
#'
#' @details
#' The dataset focuses on Brazil's 338 largest municipalities (population > 100,000)
#' and provides population ranking and size classification for 2025.
#'
#' \strong{City size categories:}
#' \itemize{
#'   \item \strong{Metropolis (1M+)}: 1 million+ inhabitants
#'   \item \strong{Large city (500K-1M)}: 500,000 to 1 million inhabitants
#'   \item \strong{Medium city (200K-500K)}: 200,000 to 500,000 inhabitants
#'   \item \strong{Small city (100K-200K)}: 100,000 to 200,000 inhabitants
#' }
#'
#' @source
#' IBGE - Instituto Brasileiro de Geografia e Estatística
#' Table 6579: Municipal population estimates
#' \url{https://www.ibge.gov.br/}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(brazil_population)
#' 
#' # View the largest cities
#' head(brazil_population)
#' 
#' # Count cities by category
#' table(brazil_population$category)
#' }
"brazil_population"

#' Brazilian Municipal GDP and Economic Structure (2021)
#'
#' A dataset containing municipal GDP data for all Brazilian municipalities
#' based on IBGE's Municipal National Accounts (Contas Nacionais Municipais).
#'
#' @format A tibble with 5,570 rows and 7 variables:
#' \describe{
#'   \item{code_muni}{IBGE municipality code (numeric)}
#'   \item{name_muni}{Municipality name (character)}
#'   \item{code_state}{IBGE state code (numeric)}
#'   \item{name_state}{State name (character)}
#'   \item{year}{Year of observation (2021, numeric)}
#'   \item{gdp_brl_k}{GDP in thousands of Brazilian reais (numeric)}
#'   \item{gdp_brl_m}{GDP in millions of Brazilian reais (numeric)}
#' }
#'
#' @details
#' The dataset includes GDP data for all 5,570 Brazilian municipalities for 2021,
#' the most recent year available in IBGE's Municipal National Accounts.
#' GDP values are provided in both thousands (gdp_brl_k) and millions (gdp_brl_m)
#' of Brazilian reais at current prices.
#'
#' @source
#' IBGE - Instituto Brasileiro de Geografia e Estatística
#' Table 5938: GDP and other aggregates by municipality
#' (Contas Nacionais Municipais)
#' \url{https://www.ibge.gov.br/}
#'
#' @references
#' IBGE. (2023). Produto Interno Bruto dos Municípios - 2021.
#' Rio de Janeiro: IBGE.
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(brazil_gdp)
#'
#' # Top 10 municipalities by GDP in 2021
#' brazil_gdp |>
#'   slice_max(gdp_brl_m, n = 10)
#'
#' # View structure
#' str(brazil_gdp)
#' }
"brazil_gdp"

#' Brazilian Municipal Agricultural Production (2022)
#'
#' A comprehensive dataset containing agricultural production data for Brazil's
#' three major crops by municipality, based on IBGE's Municipal Agricultural Production
#' survey (PAM - Produção Agrícola Municipal).
#'
#' @format A tibble with 16,689 rows and 12 variables:
#' \describe{
#'   \item{code_muni}{IBGE municipality code (numeric)}
#'   \item{name_muni}{Municipality name (character)}
#'   \item{name_state}{State name (character)}
#'   \item{name_region}{Region name in Portuguese (character)}
#'   \item{crop}{Crop name in English (character)}
#'   \item{production_tonnes}{Production quantity in tonnes (numeric)}
#'   \item{area_harvested_ha}{Area harvested in hectares (numeric)}
#'   \item{yield}{Calculated productivity in tonnes per hectare (numeric)}
#'   \item{crop_type}{Crop cultivation type: annual, semi-perennial (character)}
#'   \item{crop_category}{Crop category: grains, industrial (character)}
#'   \item{crop_importance}{Economic importance: major (character)}
#'   \item{production_scale}{Production scale category (integer)}
#' }
#'
#' @details
#' The dataset covers Brazil's three major crops by municipality in 2022:
#' soybeans, corn, and sugarcane. This represents municipal-level agricultural
#' production data for analysis of regional agricultural patterns.
#'
#' \strong{Crops included:}
#' \itemize{
#'   \item \strong{Soybeans}: Brazil's top agricultural export (annual grain crop)
#'   \item \strong{Corn}: Major grain crop for domestic and export markets (annual)
#'   \item \strong{Sugarcane}: Industrial crop for sugar and ethanol (semi-perennial)
#' }
#'
#' \strong{Geographic coverage:}
#' All Brazilian municipalities with production data for these crops across
#' all regions.
#'
#' @source
#' IBGE - Instituto Brasileiro de Geografia e Estatística
#' Table 1612: Area, production, yield and value of agricultural production
#' (PAM - Produção Agrícola Municipal)
#' \url{https://www.ibge.gov.br/}
#'
#' @references
#' IBGE. (2023). Produção Agrícola Municipal - PAM 2022.
#' Rio de Janeiro: IBGE.
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(brazil_agriculture)
#'
#' # View top soybean producing municipalities
#' brazil_agriculture |>
#'   filter(crop == "soybeans") |>
#'   slice_max(production_tonnes, n = 10)
#'
#' # Check available crops
#' unique(brazil_agriculture$crop)
#'
#' # View structure
#' str(brazil_agriculture)
#' }
"brazil_agriculture"

#' Brazilian State-Level Agricultural Production Time Series (1974-2023)
#'
#' A time series dataset containing agricultural production data for Brazil's
#' major crops by state, based on IBGE's Municipal Agricultural Production
#' survey (PAM).
#'
#' @format A tibble with 9,450 rows and 8 variables:
#' \describe{
#'   \item{code_state}{IBGE state code (numeric)}
#'   \item{name_state}{State name (character)}
#'   \item{year}{Year of observation (1974-2023, numeric)}
#'   \item{crop}{Crop name in English (character)}
#'   \item{production_tonnes}{Production quantity in tonnes (numeric)}
#'   \item{area_harvested_ha}{Area harvested in hectares (numeric)}
#'   \item{yield_kg_per_ha}{Productivity in kg per hectare (numeric)}
#'   \item{production_value_brl_k}{Production value in thousands of BRL (numeric)}
#' }
#'
#' @details
#' This dataset provides state-level time series for Brazil's seven most
#' important crops from 1974 to 2023, enabling analysis of long-term
#' agricultural trends and regional specialization patterns.
#'
#' \strong{Crops included:}
#' \itemize{
#'   \item \strong{Soybeans}: Brazil's top agricultural export
#'   \item \strong{Corn}: Major grain crop for domestic and export markets
#'   \item \strong{Sugarcane}: Industrial crop for sugar and ethanol
#'   \item \strong{Cotton}: Key fiber crop and export commodity
#'   \item \strong{Rice}: Important food security crop
#'   \item \strong{Wheat}: Food grain crop, mainly in southern states
#'   \item \strong{Beans}: Traditional protein source and food security crop
#' }
#'
#' \strong{Time coverage:}
#' Nearly 50 years of data (1974-2023) providing comprehensive historical
#' perspective on Brazilian agricultural development.
#'
#' @source
#' IBGE - Instituto Brasileiro de Geografia e Estatística
#' Table 1612: Area, production, yield and value of agricultural production
#' (PAM - Produção Agrícola Municipal)
#' \url{https://www.ibge.gov.br/}
#'
#' @references
#' IBGE. (2023). Produção Agrícola Municipal - PAM 2022.
#' Rio de Janeiro: IBGE.
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(brazil_agriculture_states)
#'
#' # Soybean production trends by top states (recent years)
#' brazil_agriculture_states |>
#'   filter(crop == "soybeans", year >= 2010, !is.na(production_tonnes)) |>
#'   slice_max(production_tonnes, n = 50) |>
#'   ggplot(aes(year, production_tonnes, color = name_state)) +
#'   geom_line()
#'
#' # View structure
#' str(brazil_agriculture_states)
#' }
"brazil_agriculture_states"
