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

#' World Development Indicators - Key Global Economic and Social Metrics
#'
#' A dataset containing key development indicators from the World Bank for major
#' economies and regions worldwide. This dataset provides a comprehensive view
#' of global economic and social development trends from 2010 to 2023.
#'
#' @format A data frame with approximately 2,500+ rows and 8 variables:
#' \describe{
#'   \item{country_code}{ISO3 country code (character)}
#'   \item{country}{Country name (character)}
#'   \item{region}{Geographic region classification (character)}
#'   \item{income_group}{World Bank income classification (character)}
#'   \item{year}{Year of observation (2010-2023, numeric)}
#'   \item{indicator}{Development indicator name (character)}
#'   \item{indicator_id}{World Bank indicator code (character)}
#'   \item{value}{Indicator value (numeric)}
#' }
#'
#' @details
#' The dataset includes 8 key development indicators for 42 major economies:
#'
#' \strong{Indicators included:}
#' \itemize{
#'   \item \strong{GDP per capita}: GDP per capita in current US dollars
#'   \item \strong{Population}: Total population
#'   \item \strong{Life expectancy}: Life expectancy at birth (years)
#'   \item \strong{Unemployment rate}: Unemployment as % of total labor force
#'   \item \strong{Inflation rate}: Consumer price inflation (annual %)
#'   \item \strong{Trade openness}: Trade as % of GDP
#'   \item \strong{Urban population share}: Urban population as % of total
#'   \item \strong{CO2 emissions per capita}: CO2 emissions in metric tons per capita
#' }
#'
#' \strong{Geographic coverage:}
#' Includes G20 countries plus selected major economies across all regions:
#' North America, South America, Europe, Asia, Eurasia, Oceania, Middle East, and Africa.
#'
#' \strong{Income group classification:}
#' Countries are classified into World Bank income groups: High income, Upper middle income,
#' Lower middle income, and Low income.
#'
#' @source
#' World Bank Open Data
#' \url{https://data.worldbank.org/}
#'
#' Data retrieved using the wbstats R package which provides access to the
#' World Bank's World Development Indicators API.
#'
#' @references
#' World Bank. (2024). World Development Indicators.
#' \url{https://datatopics.worldbank.org/world-development-indicators/}
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(world_indicators)
#'
#' # View available indicators
#' unique(world_indicators$indicator)
#'
#' # Filter for Brazil's GDP per capita over time
#' brazil_gdp <- world_indicators |>
#'   filter(country == "Brazil", indicator == "GDP per capita")
#' }
"world_indicators"

#' Brazilian Municipal Population and Demographics (2018-2022)
#'
#' A comprehensive dataset containing population data and demographic indicators
#' for Brazilian municipalities with over 100,000 inhabitants, based on IBGE
#' Census data and inter-census estimates.
#'
#' @format A data frame with approximately 1,200+ rows and 12 variables:
#' \describe{
#'   \item{code_muni}{IBGE municipality code (character)}
#'   \item{name_muni}{Municipality name (character)}
#'   \item{state}{Full state name (character)}
#'   \item{state_abbr}{State abbreviation (character)}
#'   \item{region}{Brazilian geographic region (character)}
#'   \item{year}{Year of observation (2018-2022, numeric)}
#'   \item{population}{Total population (numeric)}
#'   \item{population_growth}{Annual population growth rate (%, numeric)}
#'   \item{population_change_2018_2022}{Total population change 2018-2022 (%, numeric)}
#'   \item{households_2010}{Number of households in 2010 Census (numeric)}
#'   \item{people_per_household_2010}{Average people per household in 2010 (numeric)}
#'   \item{city_size}{City size category based on population (factor)}
#' }
#'
#' @details
#' This dataset focuses on Brazil's larger municipalities (population > 100,000)
#' and provides insights into demographic trends and household composition.
#'
#' \strong{Geographic coverage:}
#' All five Brazilian regions: North, Northeast, Center-West, Southeast, and South.
#'
#' \strong{City size categories:}
#' \itemize{
#'   \item \strong{Metropolis}: 1 million+ inhabitants
#'   \item \strong{Large city}: 500,000 to 1 million inhabitants
#'   \item \strong{Medium city}: 200,000 to 500,000 inhabitants
#'   \item \strong{Small city}: 100,000 to 200,000 inhabitants
#' }
#'
#' \strong{Data sources:}
#' Population estimates are based on IBGE's annual municipal population estimates,
#' while household data comes from the 2010 Demographic Census.
#'
#' @source
#' IBGE - Instituto Brasileiro de Geografia e Estatística
#' \itemize{
#'   \item Table 6579: Population by municipality (2018-2022 estimates)
#'   \item Table 3175: Households by municipality (2010 Census)
#' }
#' \url{https://www.ibge.gov.br/}
#'
#' @references
#' IBGE. (2023). Estimativas da população residente no Brasil e unidades da federação
#' com data de referência em 1º de julho de 2022.
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(brazil_population)
#'
#' # View largest cities in 2022
#' brazil_population |>
#'   filter(year == 2022) |>
#'   slice_max(population, n = 10)
#' }
"brazil_population"

#' Brazilian Municipal GDP and Economic Structure (2017-2021)
#'
#' A dataset containing municipal GDP data and economic sector composition for
#' Brazilian municipalities with significant economic activity, based on IBGE's
#' Municipal National Accounts (Contas Nacionais Municipais).
#'
#' @format A data frame with approximately 250+ rows and 10 variables:
#' \describe{
#'   \item{code_muni}{IBGE municipality code (character)}
#'   \item{name_muni}{Municipality name (character)}
#'   \item{state}{Full state name (character)}
#'   \item{state_abbr}{State abbreviation (character)}
#'   \item{region}{Brazilian geographic region (character)}
#'   \item{year}{Year of observation (2021, numeric)}
#'   \item{gdp_current_brl_millions}{GDP in millions of Brazilian reais (numeric)}
#'   \item{gdp_per_capita_current_brl}{GDP per capita in Brazilian reais (numeric)}
#'   \item{gdp_size_category}{GDP size classification (factor)}
#' }
#'
#' @details
#' The dataset includes municipalities with GDP above 500 million BRL,
#' covering Brazil's major urban centers and economic hubs.
#'
#' \strong{GDP size categories:}
#' \itemize{
#'   \item \strong{Very large}: 50+ billion BRL
#'   \item \strong{Large}: 10-50 billion BRL
#'   \item \strong{Medium}: 2-10 billion BRL
#'   \item \strong{Small}: 500M-2 billion BRL
#'   \item \strong{Very small}: <500 million BRL
#' }
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
#' # Top 10 municipalities by GDP per capita in 2021
#' brazil_gdp |>
#'   filter(year == 2021) |>
#'   slice_max(gdp_per_capita_current_brl, n = 10)
#' }
"brazil_gdp"

#' Brazilian Agricultural Production by State and Crop (2018-2022)
#'
#' A comprehensive dataset containing agricultural production data for Brazil's
#' eight major crops by state, based on IBGE's Municipal Agricultural Production
#' survey (PAM - Produção Agrícola Municipal).
#'
#' @format A data frame with approximately 1,400+ rows and 21 variables:
#' \describe{
#'   \item{state_code}{IBGE state code (character)}
#'   \item{state}{Full state name (character)}
#'   \item{state_abbr}{State abbreviation (character)}
#'   \item{region}{Brazilian geographic region (character)}
#'   \item{year}{Year of observation (2018-2022, numeric)}
#'   \item{crop}{Crop name in English (character)}
#'   \item{crop_type}{Crop cultivation type: annual, perennial, semi-perennial (character)}
#'   \item{crop_category}{Crop category: grains, fiber, beverage, industrial (character)}
#'   \item{crop_importance}{Economic importance: major, high-value, food-security (character)}
#'   \item{area_planted_ha}{Area planted in hectares (numeric)}
#'   \item{area_harvested_ha}{Area harvested in hectares (numeric)}
#'   \item{production_tonnes}{Production quantity in tonnes (numeric)}
#'   \item{production_value_brl_millions}{Production value in millions of BRL (numeric)}
#'   \item{yield_tonnes_per_ha}{Productivity in tonnes per hectare (numeric)}
#'   \item{value_per_ha_brl}{Value per hectare in BRL (numeric)}
#'   \item{value_per_tonne_brl}{Value per tonne in BRL (numeric)}
#'   \item{production_growth_rate}{Annual production growth rate (%, numeric)}
#'   \item{area_growth_rate}{Annual area growth rate (%, numeric)}
#'   \item{value_growth_rate}{Annual value growth rate (%, numeric)}
#'   \item{production_rank}{State ranking by production volume (numeric)}
#'   \item{area_rank}{State ranking by planted area (numeric)}
#'   \item{value_rank}{State ranking by production value (numeric)}
#'   \item{yield_rank}{State ranking by productivity (numeric)}
#' }
#'
#' @details
#' The dataset covers Brazil's eight most important crops by production value
#' and economic significance in global markets.
#'
#' \strong{Crops included:}
#' \itemize{
#'   \item \strong{Soybeans}: Brazil's top agricultural export
#'   \item \strong{Sugarcane}: Major industrial crop for sugar and ethanol
#'   \item \strong{Corn}: Second most important grain crop
#'   \item \strong{Cotton}: Key fiber crop and export commodity
#'   \item \strong{Coffee}: Traditional Brazilian export crop
#'   \item \strong{Rice}: Important food security crop
#'   \item \strong{Wheat}: Food grain crop, mainly in southern states
#'   \item \strong{Beans}: Traditional protein source and food security crop
#' }
#'
#' \strong{Geographic coverage:}
#' All Brazilian states and regions, with focus on states producing
#' minimum quantities (1,000+ tonnes) of each crop.
#'
#' \strong{Economic importance classification:}
#' \itemize{
#'   \item \strong{Major crops}: Soybeans, sugarcane, corn (highest value/volume)
#'   \item \strong{High-value crops}: Cotton, coffee (export-oriented, high value per unit)
#'   \item \strong{Food-security crops}: Rice, wheat, beans (domestic consumption focus)
#' }
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
#' # View soybean production leaders in 2022
#' brazil_agriculture |>
#'   filter(crop == "soybeans", year == 2022) |>
#'   slice_min(production_rank, n = 5)
#'
#' # Compare productivity across crops
#' brazil_agriculture |>
#'   filter(year == 2022) |>
#'   group_by(crop) |>
#'   summarize(avg_yield = mean(yield_tonnes_per_ha, na.rm = TRUE))
#' }
"brazil_agriculture"

#' Brazilian State-Level Agricultural Production Time Series (2018-2022)
#'
#' A time series dataset containing agricultural production data for Brazil's
#' major crops by state, including production metrics, value, and growth rates
#' based on IBGE's Municipal Agricultural Production survey (PAM).
#'
#' @format A data frame with approximately 1,400+ rows and 25 variables:
#' \describe{
#'   \item{state_code}{IBGE state code (numeric)}
#'   \item{state}{Full state name (character)}
#'   \item{state_abbr}{State abbreviation (character)}
#'   \item{region}{Brazilian geographic region (character)}
#'   \item{year}{Year of observation (2018-2022, numeric)}
#'   \item{crop}{Crop name in English (character)}
#'   \item{crop_type}{Crop cultivation type (character)}
#'   \item{crop_category}{Crop category (character)}
#'   \item{crop_importance}{Economic importance classification (character)}
#'   \item{area_harvested_ha}{Area harvested in hectares (numeric)}
#'   \item{production_tonnes}{Production quantity in tonnes (numeric)}
#'   \item{yield_tonnes_per_ha}{Productivity in tonnes per hectare (numeric)}
#'   \item{yield_kg_per_ha}{Productivity in kg per hectare (numeric)}
#'   \item{production_value_brl_millions}{Production value in millions of BRL (numeric)}
#'   \item{production_value_brl_thousands}{Production value in thousands of BRL (numeric)}
#'   \item{value_per_ha_brl}{Value per hectare in BRL (numeric)}
#'   \item{value_per_tonne_brl}{Value per tonne in BRL (numeric)}
#'   \item{production_growth_rate}{Annual production growth rate (%, numeric)}
#'   \item{area_growth_rate}{Annual area growth rate (%, numeric)}
#'   \item{value_growth_rate}{Annual value growth rate (%, numeric)}
#'   \item{yield_growth_rate}{Annual productivity growth rate (%, numeric)}
#'   \item{production_change_2018_2022}{Total production change 2018-2022 (%, numeric)}
#'   \item{production_rank}{State ranking by production volume (numeric)}
#'   \item{area_rank}{State ranking by harvested area (numeric)}
#'   \item{value_rank}{State ranking by production value (numeric)}
#'   \item{yield_rank}{State ranking by productivity (numeric)}
#'   \item{production_scale}{Production scale category (factor)}
#' }
#'
#' @details
#' This dataset provides state-level time series for Brazil's seven most
#' important crops, enabling analysis of agricultural trends and regional
#' specialization patterns.
#'
#' \strong{Crops included:}
#' \itemize{
#'   \item \strong{Soybeans}: Brazil's top agricultural export
#'   \item \strong{Sugarcane}: Major industrial crop for sugar and ethanol
#'   \item \strong{Corn}: Second most important grain crop
#'   \item \strong{Cotton}: Key fiber crop and export commodity
#'   \item \strong{Rice}: Important food security crop
#'   \item \strong{Wheat}: Food grain crop, mainly in southern states
#'   \item \strong{Beans}: Traditional protein source and food security crop
#' }
#'
#' \strong{Economic importance classification:}
#' \itemize{
#'   \item \strong{Major crops}: Soybeans, sugarcane, corn
#'   \item \strong{High-value crops}: Cotton
#'   \item \strong{Food-security crops}: Rice, wheat, beans
#' }
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
#' # Soybean production trends by top states
#' brazil_agriculture_states |>
#'   filter(crop == "soybeans", production_rank <= 5) |>
#'   ggplot(aes(year, production_tonnes, color = state)) +
#'   geom_line()
#' }
"brazil_agriculture_states"
