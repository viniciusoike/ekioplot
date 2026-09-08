test_that("brazil_population_forecast has complete age classifications", {
  data(brazil_population_forecast, package = "ekioplot")

  expect_equal(nrow(brazil_population_forecast), 336)
  expect_equal(
    sort(unique(brazil_population_forecast$year)),
    seq(2025, 2100, by = 5)
  )
  expect_equal(
    levels(brazil_population_forecast$ibge_age_group),
    c("0-14", "15-64", "65+")
  )
  expect_equal(
    levels(brazil_population_forecast$age_group),
    c("0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  )
  expect_false(anyNA(brazil_population_forecast$ibge_age_group))
  expect_false(anyNA(brazil_population_forecast$age_group))
  expect_true(all(brazil_population_forecast$population > 0))
})

test_that("brazil_population_forecast classifies age-group boundaries", {
  data(brazil_population_forecast, package = "ekioplot")

  expected_groups <- tibble::tribble(
    ~age, ~ibge_age_group, ~age_group,
    "10-14", "0-14", "0-19",
    "15-19", "15-64", "0-19",
    "20-24", "15-64", "20-29",
    "60-64", "15-64", "60-69",
    "65-69", "65+", "60-69",
    "80-84", "65+", "80+",
    "100+", "65+", "80+"
  )

  actual_groups <- brazil_population_forecast |>
    dplyr::filter(age %in% expected_groups$age) |>
    dplyr::distinct(age, ibge_age_group, age_group) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::arrange(age)

  expect_equal(actual_groups, dplyr::arrange(expected_groups, age))
})
