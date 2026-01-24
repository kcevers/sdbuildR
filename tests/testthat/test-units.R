# Tests for unit specification functions

test_that("u() returns unit string", {
  result <- u("10 seconds")
  expect_type(result, "character")
  expect_equal(result, "10 seconds")
})

test_that("u() handles different unit formats", {
  expect_equal(u("kilograms"), "kilograms")
  expect_equal(u("kg"), "kg")
  expect_equal(u("kilograms per meter squared"), "kilograms per meter squared")
})

test_that("drop_u() removes units", {
  result <- drop_u("x")
  expect_type(result, "character")
  expect_equal(result, "x")
})

test_that("convert_u() converts units", {
  result <- convert_u("rate", u("hours"))
  expect_type(result, "character")
})

test_that("u() works in equations", {
  # Basic unit specification
  expect_true(grepl("u\\(", "u('10kilometers')"))
})

