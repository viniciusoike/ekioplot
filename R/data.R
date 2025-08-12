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
