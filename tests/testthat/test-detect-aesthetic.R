# =============================================================================
# Tests for Aesthetic Detection
# =============================================================================

test_that("is_valid_color detects hex colors", {

  # Valid hex colors
  expect_true(is_valid_color("#FF0000"))
  expect_true(is_valid_color("#ff0000"))
  expect_true(is_valid_color("#F00"))
  expect_true(is_valid_color("#1E3A5F"))

  # Invalid hex colors
  expect_false(is_valid_color("#GG0000"))
  expect_false(is_valid_color("#12345"))
  expect_false(is_valid_color("FF0000"))  # missing #
})

test_that("is_valid_color detects named colors", {

  # Valid named colors
  expect_true(is_valid_color("red"))
  expect_true(is_valid_color("steelblue"))
  expect_true(is_valid_color("darkorange"))
  expect_true(is_valid_color("white"))
  expect_true(is_valid_color("black"))

  # Invalid named colors
  expect_false(is_valid_color("notacolor"))
  expect_false(is_valid_color("sometext"))
  expect_false(is_valid_color("category"))
})

test_that("detect_aesthetic_type identifies missing values", {

  # Test with a truly missing/NULL quosure
  quo_missing <- rlang::quo(NULL)
  result <- detect_aesthetic_type(quo_missing, "param")

  # NULL is treated as missing
 expect_equal(result$type, "missing")
})

test_that("detect_aesthetic_type identifies static colors", {

  test_fn <- function(param) {
    quo <- rlang::enquo(param)
    detect_aesthetic_type(quo, "param")
  }

  # Test with hex color
  result_hex <- test_fn("#FF0000")
  expect_equal(result_hex$type, "static_color")
  expect_equal(result_hex$value, "#FF0000")

  # Test with named color
  result_named <- test_fn("steelblue")
  expect_equal(result_named$type, "static_color")
  expect_equal(result_named$value, "steelblue")
})

test_that("detect_aesthetic_type identifies variable mappings", {

  test_fn <- function(param, data = NULL) {
    quo <- rlang::enquo(param)
    detect_aesthetic_type(quo, "param", data)
  }

  df <- data.frame(
    x = 1:5,
    category = letters[1:5],
    value = rnorm(5)
  )

  # Test with bare column name
  result_col <- test_fn(category, df)
  expect_equal(result_col$type, "variable_mapping")

  # Test with expression
  result_expr <- test_fn(factor(category), df)
  expect_equal(result_expr$type, "variable_mapping")
})

test_that("detect_aesthetic_type determines continuous vs discrete", {

  test_fn <- function(param, data = NULL) {
    quo <- rlang::enquo(param)
    detect_aesthetic_type(quo, "param", data)
  }

  df <- data.frame(
    category = factor(letters[1:5]),
    value = rnorm(5),
    integer_col = 1:5
  )

  # Factor should not be continuous
  result_factor <- test_fn(category, df)
  expect_false(result_factor$is_continuous)

  # Numeric should be continuous
  result_num <- test_fn(value, df)
  expect_true(result_num$is_continuous)
})
