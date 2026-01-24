# Tests for xmile helpers in xmile.R

test_that("check_xmile rejects non-model objects", {
  expect_error(check_xmile(list()), "Expected object of class")
  expect_error(check_xmile("not a model"), "Expected object of class")
})


test_that("validate_xmile fills missing labels and eqn_julia", {
  sfm <- xmile()
  sfm <- build(sfm, "S", type = "stock", eqn = "2")

  # Remove label and eqn_julia to force defaults
  sfm[["variables"]][["label"]] <- ""
  sfm[["variables"]][["eqn_julia"]] <- ""

  sfm_valid <- validate_xmile(sfm)

  vars <- sfm_valid[["variables"]]
  expect_equal(vars[["label"]], "S")
  expect_equal(vars[["eqn_julia"]], "0.0")
})


test_that("validate_xmile cleans invalid flow connections", {
  sfm <- xmile()
  sfm <- build(sfm, "Stock", type = "stock")
  sfm <- build(sfm, "Aux", type = "aux")
  
  # Invalid flow connections generate warnings during build (which calls validate_xmile internally)
  expect_warning(
    sfm <- build(sfm, "FlowBad", type = "flow", to = "Aux", from = "Aux"),
    "not a stock|flowing to and from"
  )

  flow_row <- sfm[["variables"]][sfm[["variables"]][["name"]] == "FlowBad", ]
  expect_equal(flow_row[["to"]], "")
  expect_equal(flow_row[["from"]], "")
})


test_that("validate_xmile sets macro defaults", {
  sfm <- xmile()
  macro_name <- P[["macro_name"]]
  sfm[[macro_name]] <- data.frame(name = "m1", eqn = "", eqn_julia = "", units = "", doc = "", stringsAsFactors = FALSE)

  sfm_valid <- validate_xmile(sfm)
  macro_row <- sfm_valid[[macro_name]][1, ]
  expect_equal(macro_row$eqn, "0.0")
  expect_equal(macro_row$eqn_julia, "0.0")
})


test_that("validate_xmile sets model_units defaults", {
  sfm <- xmile()
  sfm$model_units <- data.frame(name = "u1", unit = "m", prefix = NA, doc = "", stringsAsFactors = FALSE)

  sfm_valid <- validate_xmile(sfm)
  mu_row <- sfm_valid$model_units[1, ]
  expect_false(is.na(mu_row$prefix))
})


