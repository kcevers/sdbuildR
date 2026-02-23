# Tests for core helper functions

test_that("has_internet() returns logical", {
  result <- has_internet()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("last() returns last element", {
  expect_equal(last(c(1, 2, 3)), 3)
  expect_equal(last(c("a", "b", "c")), "c")
})

test_that("last() returns last n elements", {
  expect_equal(last(c(1, 2, 3, 4, 5), n = 2), c(4, 5))
  expect_equal(last(c(1, 2, 3), n = 3), c(1, 2, 3))
})

test_that("last() handles empty vector", {
  expect_null(last(c()))
  expect_equal(last(c(), default = NA), NA)
})

test_that("near() compares numeric values within tolerance", {
  expect_true(near(1.0, 1.00001, tol = 1e-4))
  expect_false(near(1.0, 1.1))
  expect_true(near(0.1 + 0.2, 0.3)) # floating point comparison
})

test_that("bind_rows_() combines data frames", {
  df1 <- data.frame(a = 1:2, b = 3:4)
  df2 <- data.frame(a = 5:6, b = 7:8)

  result <- bind_rows_(df1, df2)
  expect_equal(nrow(result), 4)
  expect_equal(result$a, c(1, 2, 5, 6))
})

test_that("bind_rows_() handles lists of data frames", {
  df1 <- data.frame(a = 1:2)
  df2 <- data.frame(a = 3:4)

  result <- bind_rows_(list(df1, df2))
  expect_equal(nrow(result), 4)
})

test_that("bind_rows_() fills missing columns with NA", {
  df1 <- data.frame(a = 1:2, b = 3:4)
  df2 <- data.frame(a = 5:6, c = 7:8)

  result <- bind_rows_(df1, df2)
  expect_true(any(is.na(result$b)))
  expect_true(any(is.na(result$c)))
})

test_that("compact_() removes NULL values", {
  expect_equal(compact_(list(a = 1, b = NULL, c = 3)), list(a = 1, c = 3))
  expect_equal(compact_(list(NULL)), list())
  expect_equal(compact_(list()), list())
})

test_that("clean_name() creates syntactically valid names", {
  expect_true(all(make.names(clean_name(c("TRUE", "T"))) == clean_name(c("TRUE", "T"))))
  expect_true(all(make.names(clean_name(c("a-1", "b!2"))) == clean_name(c("a-1", "b!2"))))
})

test_that("clean_name() ensures unique names", {
  result <- clean_name(c("a", "a", "b"))
  expect_equal(length(unique(result)), 3)
})

# Validation functions ----

test_that("is_defined() returns FALSE for empty vectors", {
  expect_false(is_defined(c()))
  expect_false(is_defined(character(0)))
  expect_false(is_defined(numeric(0)))
})

test_that("is_defined() returns FALSE for NA values", {
  expect_false(is_defined(NA))
  expect_false(is_defined(NA_character_))
  expect_false(is_defined(c(NA, NA)))
})

test_that("is_defined() returns FALSE for empty strings", {
  expect_false(is_defined(""))
  expect_false(is_defined(c("", "")))
})

test_that("is_defined() returns TRUE for defined values", {
  expect_true(is_defined("value"))
  expect_true(is_defined(c("a", "b")))
  expect_true(is_defined(123))
  expect_true(is_defined(c(1, 2, 3)))
})

test_that("is_defined() handles mixed cases", {
  expect_true(is_defined(c("a", "")))
  expect_true(is_defined(c(1, NA)))
})

# String/language functions ----

test_that("clean_language() accepts valid languages", {
  expect_equal(clean_language("r"), "R")
  expect_equal(clean_language("R"), "R")
  expect_equal(clean_language("julia"), "Julia")
  expect_equal(clean_language("Julia"), "Julia")
  expect_equal(clean_language("jl"), "Julia")
})

test_that("clean_language() handles whitespace", {
  expect_equal(clean_language("  r  "), "R")
  expect_equal(clean_language(" julia "), "Julia")
})

test_that("clean_language() rejects invalid languages", {
  expect_error(clean_language("python"))
  expect_error(clean_language("cpp"))
  expect_error(clean_language(""))
})

test_that("clean_type() normalizes variable types", {
  expect_equal(clean_type("stock"), "stock")
  expect_equal(clean_type("Stock"), "stock")
  expect_equal(clean_type("STOCK"), "stock")
})

test_that("clean_type() handles auxiliaries", {
  expect_equal(clean_type("auxiliary"), "aux")
  expect_equal(clean_type("auxiliaries"), "aux")
})

test_that("clean_type() removes trailing s", {
  expect_equal(clean_type("stocks"), "stock")
  expect_equal(clean_type("flows"), "flow")
  expect_equal(clean_type("constants"), "constant")
})

test_that("clean_type() handles whitespace", {
  expect_equal(clean_type("  stock  "), "stock")
  expect_equal(clean_type(c(" flow ", "constant")), c("flow", "constant"))
})

test_that("clean_type() filters empty strings", {
  result <- clean_type(c("stock", "", "flow"))
  expect_equal(length(result), 2)
  expect_true("stock" %in% result)
  expect_true("flow" %in% result)
})

# List manipulation functions ----

test_that("switch_list() swaps names and values", {
  input <- list(a = "x", b = "y")
  result <- switch_list(input)
  expect_equal(result$x, "a")
  expect_equal(result$y, "b")
})

test_that("switch_list() handles multiple values", {
  input <- list(a = c("x", "y"), b = "z")
  result <- switch_list(input)
  expect_equal(result$x, "a")
  expect_equal(result$y, "a")
  expect_equal(result$z, "b")
})

# Error message tests ----

cli::test_that_cli(configs = c("plain", "ansi"), "clean_language() error messages", {
  expect_snapshot(clean_language("python"), error = TRUE)
})

cli::test_that_cli(configs = c("plain", "ansi"), "clean_type() error messages", {
  expect_snapshot(clean_type(123), error = TRUE)
})

