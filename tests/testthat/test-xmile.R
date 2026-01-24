# Tests for basic xmile structure

test_that("xmile() creates an empty model", {
  sfm <- xmile()
  expect_s3_class(sfm, "sdbuildR_xmile")
  expect_type(sfm, "list")
})


test_that("xmile() has required top-level components", {
  sfm <- xmile()
  expect_true("header" %in% names(sfm))
  expect_true("sim_specs" %in% names(sfm))
  expect_true("variables" %in% names(sfm))
  expect_true(P[["macro_name"]] %in% names(sfm))
  expect_true("model_units" %in% names(sfm))
})


test_that("xmile() initializes empty data frames with columns", {
  sfm <- xmile()

  expect_s3_class(sfm$variables, "data.frame")
  expect_equal(nrow(sfm$variables), 0)
  expect_true(all(c("name", "type", "eqn") %in% names(sfm$variables)))

  expect_s3_class(sfm[[P[["macro_name"]]]], "data.frame")
  expect_equal(nrow(sfm[[P[["macro_name"]]]]), 0)
  expect_true(all(c("name", "eqn") %in% names(sfm[[P[["macro_name"]]]])))

  expect_s3_class(sfm$model_units, "data.frame")
  expect_equal(nrow(sfm$model_units), 0)
  expect_true("name" %in% names(sfm$model_units))
})


test_that("xmile() creates default header and sim_specs", {
  sfm <- xmile()

  expect_type(sfm$header, "list")
  expect_true(all(c("name", "author", "created", "version") %in% names(sfm$header)))

  expect_type(sfm$sim_specs, "list")
  expect_true(all(c("start", "stop", "dt", "time_units") %in% names(sfm$sim_specs)))
  expect_true(sfm$sim_specs$start < sfm$sim_specs$stop)
})


test_that("xmile() supports adding variables then remains valid", {
  sfm <- xmile()
  sfm <- build(sfm, "S", type = "stock", eqn = "1")
  sfm <- build(sfm, "FLOW", type = "flow", eqn = "S", to = "S")

  expect_s3_class(sfm, "sdbuildR_xmile")
  expect_equal(nrow(as.data.frame(sfm)), 2)
})


