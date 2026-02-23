# Test custom_func() function for creating and modifying func-type variables

test_that("custom_func() creates custom functions", {
  sfm <- sdbuildR()

  sfm1 <- custom_func(sfm, "parameter1", eqn = "5")
  df <- as.data.frame(sfm1, type = "func", properties = c("name", "eqn"))
  expect_equal(nrow(df), 1)
  expect_equal(df[["name"]], "parameter1")
  expect_equal(df[["eqn"]], "5")
})

test_that("custom_func() adds multiple funcs", {
  sfm <- sdbuildR()

  sfm1 <- custom_func(sfm, "param1", eqn = "5")
  sfm2 <- custom_func(sfm1, "param2", eqn = "10")
  sfm3 <- custom_func(sfm2, "param3", eqn = "param1 + param2")

  df <- as.data.frame(sfm3, type = "func")
  expect_equal(nrow(df), 3)
  expect_true("param1" %in% df[["name"]])
  expect_true("param2" %in% df[["name"]])
  expect_true("param3" %in% df[["name"]])
})

test_that("custom_func() modifies existing func equations", {
  sfm <- sdbuildR()

  sfm1 <- custom_func(sfm, "param1", eqn = "5")
  sfm2 <- custom_func(sfm1, "param1", eqn = "10")

  df <- as.data.frame(sfm2, type = "func", properties = c("name", "eqn"))
  expect_equal(df[["eqn"]], "10")
})

test_that("custom_func() validates function definitions (defaults at end)", {
  sfm <- sdbuildR()

  # Invalid: default arg not at end
  expect_error(
    custom_func(sfm, "bad_func", eqn = "function(x = 1, y) x + y"),
    "defaults have to be placed at the end"
  )

  # Valid: default arg at end
  sfm1 <- custom_func(sfm, "good_func", eqn = "function(x, y = 1) x + y")
  df <- as.data.frame(sfm1, type = "func")
  expect_equal(nrow(df), 1)
})

test_that("custom_func() sets units", {
  sfm <- sdbuildR()

  sfm1 <- custom_func(sfm, "rate", eqn = "0.05", units = "1/yr")
  df <- as.data.frame(sfm1, type = "func", properties = c("name", "units"))
  expect_equal(df[["units"]], "1/yr")
})

test_that("custom_func() sets documentation", {
  sfm <- sdbuildR()

  sfm1 <- custom_func(sfm, "birth_rate",
    eqn = "0.02",
    doc = "Annual birth rate per individual"
  )
  df <- as.data.frame(sfm1, type = "func", properties = c("name", "doc"))
  expect_equal(df[["doc"]], "Annual birth rate per individual")
})

test_that("discard() removes funcs", {
  sfm <- sdbuildR()

  sfm1 <- custom_func(sfm, "param1", eqn = "5")
  sfm2 <- custom_func(sfm1, "param2", eqn = "10")

  # Remove one func
  sfm3 <- discard(sfm2, "param1")
  df <- as.data.frame(sfm3, type = "func")
  expect_equal(nrow(df), 1)
  expect_equal(df[["name"]], "param2")
})

test_that("discard() removes multiple funcs", {
  sfm <- sdbuildR()

  sfm1 <- custom_func(sfm, "p1", eqn = "1")
  sfm2 <- custom_func(sfm1, "p2", eqn = "2")
  sfm3 <- custom_func(sfm2, "p3", eqn = "3")

  # Remove multiple
  sfm4 <- discard(sfm3, c("p1", "p3"))
  df <- as.data.frame(sfm4, type = "func")
  expect_equal(nrow(df), 1)
  expect_equal(df[["name"]], "p2")
})

test_that("discard() validates func existence", {
  sfm <- sdbuildR()

  sfm1 <- custom_func(sfm, "param1", eqn = "5")

  expect_error(
    discard(sfm1, "nonexistent"),
    "not exist"
  )
})

test_that("change_name() renames funcs", {
  sfm <- sdbuildR()

  sfm1 <- custom_func(sfm, "old_name", eqn = "5")
  sfm2 <- change_name(sfm1, "old_name", new_name = "new_name")

  df <- as.data.frame(sfm2, type = "func", properties = "name")
  expect_equal(df[["name"]], "new_name")
  expect_false("old_name" %in% df[["name"]])
})

test_that("custom_func() preserves variables", {
  sfm <- sdbuildR()

  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- custom_func(sfm1, "param1", eqn = "5")

  # Check both exist
  var_df <- as.data.frame(sfm2, type = "stock")
  func_df <- as.data.frame(sfm2, type = "func")

  expect_equal(nrow(var_df), 1)
  expect_equal(var_df[["name"]], "Stock1")
  expect_equal(nrow(func_df), 1)
  expect_equal(func_df[["name"]], "param1")
})

test_that("custom_func() can be used in variable equations", {
  sfm <- sdbuildR()

  sfm1 <- custom_func(sfm, "rate", eqn = "0.05")
  sfm2 <- build(sfm1, "Stock1", type = "stock")
  sfm3 <- build(sfm2, "Flow1",
    type = "flow",
    eqn = "Stock1 * rate", from = "Stock1"
  )

  df <- as.data.frame(sfm3, type = "flow", properties = c("name", "eqn"))
  expect_match(df[["eqn"]], "rate")
})

test_that("custom_func() with empty model works", {
  sfm <- sdbuildR()

  # Should be able to add func to empty model
  sfm1 <- custom_func(sfm, "f1", eqn = "10")
  df <- as.data.frame(sfm1, type = "func")
  expect_equal(nrow(df), 1)
})

test_that("custom_func() returns the modified model", {
  sfm <- sdbuildR()

  result <- custom_func(sfm, "param1", eqn = "5")
  expect_s3_class(result, "sdbuildR")
  expect_true(is.list(result))
})

test_that("build() with type = 'func' works directly", {
  sfm <- sdbuildR()

  sfm1 <- build(sfm, "f", "func", eqn = "function(x) x * 2")
  df <- as.data.frame(sfm1, type = "func", properties = c("name", "eqn"))
  expect_equal(nrow(df), 1)
  expect_equal(df[["name"]], "f")
  expect_match(df[["eqn"]], "function")
})

