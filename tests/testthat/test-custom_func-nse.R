test_that("custom_func() accepts unquoted function expressions", {
  sfm <- sdbuildR()

  sfm1 <- custom_func(sfm, hills, eqn = function(x, slope, midpoint = 0.5) {
    x^slope / (midpoint^slope + x^slope)
  })

  df <- as.data.frame(sfm1, type = "func", properties = c("name", "eqn"))
  expect_equal(nrow(df), 1)
  expect_equal(df[["name"]], "hills")
  # eqn should be stored as a single string containing the function text
  expect_length(df[["eqn"]], 1)
  expect_true(grepl("function\\s*\\(", df[["eqn"]]))
})

test_that("update()/custom_func() handles bang-bang injection of simple values", {
  sfm <- sdbuildR()

  a <- 1
  sfm1 <- custom_func(sfm, fval, eqn = !!a)
  df <- as.data.frame(sfm1, type = "func", properties = c("name", "eqn"))
  expect_equal(df[["eqn"]], "1")
})

test_that("update()/custom_func() handles bang-bang injection of function objects", {
  sfm <- sdbuildR()

  a <- function(x) x^2
  sfm1 <- custom_func(sfm, fobj, eqn = !!a)
  df <- as.data.frame(sfm1, type = "func", properties = c("name", "eqn"))
  expect_length(df[["eqn"]], 1)
  expect_true(grepl("function", df[["eqn"]]))
})

test_that("c() calls still return character vectors", {
  sfm <- sdbuildR()

  sfm1 <- custom_func(sfm, c(a, b), eqn = c("1", "2"))
  df <- as.data.frame(sfm1, type = "func", properties = c("name", "eqn"))
  expect_equal(nrow(df), 2)
  expect_equal(df[["eqn"]], c("1", "2"))
})
