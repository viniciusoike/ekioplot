library(dplyr)
import::from(tidyr, pivot_longer, pivot_wider)
import::from(here, here)

# Create sample world indicators data with realistic values
set.seed(789)

# Select key countries for demonstration
countries <- data.frame(
  country_code = c("USA", "CHN", "JPN", "DEU", "GBR", "FRA", "IND", "ITA", 
                   "BRA", "CAN", "RUS", "KOR", "AUS", "MEX", "ESP", "IDN",
                   "NLD", "SAU", "TUR", "ARG", "ZAF"),
  country = c("United States", "China", "Japan", "Germany", "United Kingdom", 
              "France", "India", "Italy", "Brazil", "Canada", "Russian Federation",
              "Korea, Rep.", "Australia", "Mexico", "Spain", "Indonesia", 
              "Netherlands", "Saudi Arabia", "Turkey", "Argentina", "South Africa"),
  region = c("North America", "Asia", "Asia", "Europe", "Europe", "Europe", 
             "Asia", "Europe", "South America", "North America", "Eurasia", 
             "Asia", "Oceania", "North America", "Europe", "Asia", "Europe", 
             "Middle East", "Middle East", "South America", "Africa"),
  income_group = c("High income", "Upper middle income", "High income", "High income",
                   "High income", "High income", "Lower middle income", "High income",
                   "Upper middle income", "High income", "Upper middle income", 
                   "High income", "High income", "Upper middle income", "High income",
                   "Lower middle income", "High income", "High income", 
                   "Upper middle income", "Upper middle income", "Upper middle income")
)

# Base values for 2010
base_values <- data.frame(
  country_code = countries$country_code,
  gdp_per_capita_2010 = c(48400, 4560, 44500, 44700, 39800, 40700, 1340, 35800,
                           11300, 47400, 10700, 23100, 51800, 9240, 31000, 3120,
                           50800, 20400, 10500, 9930, 7280),
  population_2010 = c(309326000, 1340910000, 128070000, 81780000, 63180000, 
                      64720000, 1224614000, 59190000, 194950000, 34000000, 
                      142960000, 49410000, 22300000, 113420000, 46560000, 
                      239870000, 16610000, 27450000, 72330000, 40370000, 50130000)
)

# Generate time series data
years <- 2010:2023
world_indicators <- expand.grid(
  country_code = countries$country_code,
  year = years,
  stringsAsFactors = FALSE
) |>
  left_join(countries, by = "country_code") |>
  left_join(base_values, by = "country_code") |>
  group_by(country_code) |>
  arrange(year) |>
  mutate(
    # GDP per capita with realistic growth patterns
    gdp_growth_factor = case_when(
      year <= 2010 ~ 1.0,
      income_group == "High income" ~ cumprod(c(1, runif(n()-1, 0.98, 1.04))),
      income_group == "Upper middle income" ~ cumprod(c(1, runif(n()-1, 1.00, 1.08))),
      income_group == "Lower middle income" ~ cumprod(c(1, runif(n()-1, 1.02, 1.10))),
      TRUE ~ cumprod(c(1, runif(n()-1, 0.99, 1.05)))
    ),
    gdp_per_capita = gdp_per_capita_2010 * gdp_growth_factor,
    
    # Population with country-specific growth
    pop_growth_rate = case_when(
      country_code %in% c("JPN", "DEU", "ITA") ~ runif(n(), -0.002, 0.005),
      country_code %in% c("CHN", "RUS") ~ runif(n(), 0.001, 0.008),
      country_code %in% c("IND", "IDN") ~ runif(n(), 0.010, 0.020),
      country_code %in% c("USA", "CAN", "AUS") ~ runif(n(), 0.005, 0.012),
      TRUE ~ runif(n(), 0.003, 0.015)
    ),
    population = as.integer(population_2010 * cumprod(c(1, 1 + pop_growth_rate[-1]))),
    
    # Other indicators with realistic ranges
    life_expectancy = case_when(
      income_group == "High income" ~ runif(n(), 78, 85),
      income_group == "Upper middle income" ~ runif(n(), 70, 78),
      income_group == "Lower middle income" ~ runif(n(), 65, 72),
      TRUE ~ runif(n(), 60, 70)
    ),
    
    unemployment_rate = case_when(
      year %in% 2008:2010 ~ runif(n(), 4, 12),
      year %in% 2020:2021 ~ runif(n(), 6, 15),
      country_code == "ZAF" ~ runif(n(), 25, 30),
      TRUE ~ runif(n(), 2, 10)
    ),
    
    inflation_rate = case_when(
      year %in% 2020:2023 ~ runif(n(), 1, 8),
      year %in% 2008:2009 ~ runif(n(), -2, 5),
      TRUE ~ runif(n(), 0, 4)
    ),
    
    trade_openness = case_when(
      country_code %in% c("NLD", "DEU") ~ runif(n(), 140, 180),
      country_code %in% c("USA", "BRA", "IND") ~ runif(n(), 20, 40),
      TRUE ~ runif(n(), 40, 100)
    ),
    
    urban_population = case_when(
      income_group == "High income" ~ runif(n(), 75, 90),
      income_group == "Upper middle income" ~ runif(n(), 60, 85),
      income_group == "Lower middle income" ~ runif(n(), 30, 70),
      TRUE ~ runif(n(), 40, 75)
    ),
    
    co2_emissions = case_when(
      country_code %in% c("SAU", "USA", "CAN", "AUS") ~ runif(n(), 12, 20),
      country_code %in% c("CHN", "RUS", "KOR") ~ runif(n(), 6, 12),
      country_code %in% c("DEU", "GBR", "FRA", "JPN") ~ runif(n(), 7, 11),
      country_code %in% c("IND", "IDN") ~ runif(n(), 1, 3),
      TRUE ~ runif(n(), 2, 8)
    )
  ) |>
  ungroup() |>
  select(-gdp_per_capita_2010, -population_2010, -gdp_growth_factor, -pop_growth_rate) |>
  # Pivot to long format
  pivot_longer(
    cols = c(gdp_per_capita, population, life_expectancy, unemployment_rate, 
             inflation_rate, trade_openness, urban_population, co2_emissions),
    names_to = "indicator_id",
    values_to = "value"
  ) |>
  # Create readable indicator names
  mutate(
    indicator = case_when(
      indicator_id == "gdp_per_capita" ~ "GDP per capita",
      indicator_id == "population" ~ "Population",
      indicator_id == "life_expectancy" ~ "Life expectancy",
      indicator_id == "unemployment_rate" ~ "Unemployment rate",
      indicator_id == "inflation_rate" ~ "Inflation rate",
      indicator_id == "trade_openness" ~ "Trade openness",
      indicator_id == "urban_population" ~ "Urban population share",
      indicator_id == "co2_emissions" ~ "CO2 emissions per capita",
      TRUE ~ indicator_id
    )
  ) |>
  filter(!is.na(value)) |>
  arrange(country, year, indicator)

usethis::use_data(world_indicators, overwrite = TRUE)