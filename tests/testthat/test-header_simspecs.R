# Tests for header and sim_specs manipulation

test_that("header() modifies model name", {
  sfm <- xmile() |>
    header(name = "Test Model")
  
  expect_equal(sfm$header$name, "Test Model")
})

test_that("header() modifies author", {
  sfm <- xmile() |>
    header(author = "Test Author")
  
  expect_equal(sfm$header$author, "Test Author")
})

test_that("header() modifies version", {
  sfm <- xmile() |>
    header(version = "2.0")
  
  expect_equal(sfm$header$version, "2.0")
})

test_that("header() preserves other fields", {
  sfm <- xmile() |>
    header(name = "New Name")
  
  expect_true("author" %in% names(sfm$header))
  expect_true("version" %in% names(sfm$header))
})

test_that("header() returns sdbuildR_xmile object", {
  sfm <- xmile() |>
    header(name = "Test")
  
  expect_s3_class(sfm, "sdbuildR_xmile")
})

test_that("sim_specs() modifies start time", {
  sfm <- xmile() |>
    sim_specs(start = 10)
  
  expect_equal(as.numeric(sfm$sim_specs$start), 10)
})

test_that("sim_specs() modifies stop time", {
  sfm <- xmile() |>
    sim_specs(stop = 200)
  
  expect_equal(as.numeric(sfm$sim_specs$stop), 200)
})

test_that("sim_specs() modifies dt", {
  sfm <- xmile() |>
    sim_specs(dt = 0.1)
  
  expect_equal(as.numeric(sfm$sim_specs$dt), 0.1)
})

test_that("sim_specs() modifies time_units", {
  sfm <- xmile() |>
    sim_specs(time_units = "days")
  
  # Note: time_units get converted to abbreviations internally
  expect_true(sfm$sim_specs$time_units %in% c("d", "days"))
})

test_that("sim_specs() modifies method", {
  sfm <- xmile() |>
    sim_specs(method = "rk4")
  
  expect_equal(sfm$sim_specs$method, "rk4")
})

test_that("sim_specs() preserves other fields", {
  sfm <- xmile() |>
    sim_specs(start = 5)
  
  expect_true("stop" %in% names(sfm$sim_specs))
  expect_true("dt" %in% names(sfm$sim_specs))
})

test_that("sim_specs() returns sdbuildR_xmile object", {
  sfm <- xmile() |>
    sim_specs(start = 0)
  
  expect_s3_class(sfm, "sdbuildR_xmile")
})

test_that("sim_specs() handles language parameter", {
  sfm <- xmile() |>
    sim_specs(language = "R")
  
  expect_equal(sfm$sim_specs$language, "R")
  
  sfm <- xmile() |>
    sim_specs(language = "Julia")
  
  expect_equal(sfm$sim_specs$language, "Julia")
})

test_that("sim_specs() modifies seed", {
  sfm <- xmile() |>
    sim_specs(seed = 123)
  
  expect_equal(as.numeric(sfm$sim_specs$seed), 123)
})

# Error message tests ----

cli::test_that_cli(configs = c("plain", "ansi"), "header() requires sfm argument", {
  expect_snapshot(header(name = "Test"), error = TRUE)
})

cli::test_that_cli(configs = c("plain", "ansi"), "clean_language() rejects invalid language", {
  expect_snapshot(clean_language("python"), error = TRUE)
  expect_snapshot(clean_language("cpp"), error = TRUE)
})

