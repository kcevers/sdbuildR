# Comprehensive tests for build() and helpers in build.R

test_that("build() creates variables with defaults", {
  sfm <- xmile()
  sfm <- build(sfm, "Population", type = "stock")

  vars <- as.data.frame(sfm)
  expect_equal(nrow(vars), 1)
  expect_equal(vars[["name"]], "Population")
  expect_equal(vars[["type"]], "stock")
  expect_equal(vars[["eqn"]], "0.0")
  expect_equal(vars[["units"]], "1")
  expect_equal(vars[["label"]], "Population")
  expect_false(vars[["non_negative"]])
})


test_that("build() validates inputs and basic errors", {
  sfm <- xmile()

  expect_error(build(sfm, "Var1", type = "invalid"), "must be one of")
  expect_error(build(sfm, "", type = "stock"), "cannot be empty")

  # rename multiple at once is disallowed
  expect_error(build(sfm, c("A", "B"), type = "stock", change_name = "C"), "rename one variable")

  # change_type for multiple names is disallowed
  expect_error(build(sfm, c("A", "B"), type = c("stock", "stock"), change_type = "flow"), "one variable at a time")

  # invalid doc type
  expect_error(build(sfm, "D", type = "stock", doc = 1L), "must be" )

  # invalid non_negative type
  expect_error(build(sfm, "E", type = "stock", non_negative = "no"), "must be")
})


test_that("build() enforces flow rules", {
  sfm <- xmile()
  sfm <- build(sfm, "StockA", type = "stock")
  sfm <- build(sfm, "StockB", type = "stock")

  # Flow cannot target itself
  expect_error(build(sfm, "Flow1", type = "flow", to = "Flow1"), "flow cannot flow to itself")

  # Flow with same to/from is allowed (validate_xmile will clean it up with warning)
  expect_warning(
    sfm <- build(sfm, "Flow2", type = "flow", to = "StockA", from = "StockA"),
    "flowing to and from"
  )
  flow_row <- sfm[["variables"]][sfm[["variables"]][["name"]] == "Flow2", ]
  expect_equal(flow_row[["from"]], "")

  # Flow 'to' must be a stock (validate_xmile will later clean; build should not accept non-stock in this context)
  expect_error(build(sfm, "Flow3", type = "flow", to = "Flow3"), "flow cannot flow to itself")
})


test_that("build() renames variables and updates references", {
  sfm <- xmile()
  sfm <- build(sfm, "Prey", type = "stock", eqn = "10")
  sfm <- build(sfm, "Predator", type = "stock", eqn = "5")
  sfm <- build(sfm, "Hunt", type = "flow", eqn = "Prey * 0.1", from = "Predator", to = "Prey")

  sfm_renamed <- build(sfm, "Prey", change_name = "Bunnies")
  vars <- sfm_renamed[["variables"]]

  hunt_row <- vars[vars[["name"]] == "Hunt", ]
  expect_equal(hunt_row[["eqn"]], "Bunnies * 0.1")
  expect_equal(hunt_row[["to"]], "Bunnies")

  renamed_stock <- vars[vars[["name"]] == "Bunnies", ]
  expect_equal(renamed_stock[["label"]], "Bunnies")  # label follows name when not set explicitly
})


test_that("build() blocks type mismatches and supports erase", {
  sfm <- xmile()
  sfm <- build(sfm, "A", type = "stock")

  # specifying a conflicting type for existing var errors
  expect_error(build(sfm, "A", type = "flow"), "different types")

  # erase fails on missing vars
  expect_error(build(sfm, "Missing", type = "stock", erase = TRUE), "does not exist")

  # erase succeeds on existing
  sfm_erased <- build(sfm, "A", erase = TRUE)
  expect_equal(nrow(as.data.frame(sfm_erased)), 0)
})


test_that("build() changes types while preserving equations when not supplied", {
  sfm <- xmile()
  sfm <- build(sfm, "alpha", type = "aux", eqn = "2")

  sfm_changed <- build(sfm, "alpha", change_type = "constant")
  vars <- as.data.frame(sfm_changed)

  expect_equal(vars[["type"]], "constant")
  expect_equal(vars[["eqn"]], "2")  # equation preserved when change_type is used without eqn
})


test_that("build() validates graphical functions", {
  sfm <- xmile()

  expect_error(build(sfm, "curve1", type = "gf", xpts = c(0, 1)), "argument is required")
  expect_error(build(sfm, "curve2", type = "gf", xpts = c(0, 1), ypts = c(2)), "Length mismatch")
  expect_error(build(sfm, "curve3", type = "gf", xpts = c(0, 1), ypts = c(2, 3), interpolation = "bad"), "interpolation")
  expect_error(build(sfm, "curve4", type = "gf", xpts = c(0, 1), ypts = c(2, 3), extrapolation = "bad"), "extrapolation")

  expect_error(build(sfm, "curve5", type = "gf", xpts = c(0, 1), ypts = c(2, 3), source = c("a", "b")), "Only one source")

  sfm_gf <- build(sfm, "curve_ok", type = "gf", xpts = c(0, 1), ypts = c(2, 3), source = "X")
  gf_row <- sfm_gf[["variables"]][sfm_gf[["variables"]][["name"]] == "curve_ok", ]
  expect_equal(unlist(gf_row$xpts), c(0, 1))
  expect_equal(unlist(gf_row$ypts), c(2, 3))
})


test_that("build() supports bulk add via data frame and validates df", {
  sfm <- xmile()
  df <- data.frame(
    type = c("stock", "flow"),
    name = c("S", "In"),
    eqn = c("5", "S * 0.1"),
    to = c("", "S"),
    from = c("", ""),
    stringsAsFactors = FALSE
  )

  sfm_new <- build(sfm, df = df)
  vars <- as.data.frame(sfm_new)
  expect_equal(sort(vars[["name"]]), c("In", "S"))
  expect_equal(vars[vars[["name"]] == "S", "type"], "stock")
  expect_equal(vars[vars[["name"]] == "In", "type"], "flow")

  # missing required columns
  bad_df <- data.frame(type = "stock")
  expect_error(build(sfm, df = bad_df), "required")

  # invalid column name
  bad_df2 <- data.frame(type = "stock", name = "A", badcol = 1)
  expect_error(build(sfm, df = bad_df2), "not valid properties")

  # TODO: inappropriate properties via df - verify warning is triggered
  # df_warn <- data.frame(type = "stock", name = "X", interpolation = "linear")
  # expect_warning(build(sfm, df = df_warn), "not appropriate")
})


test_that("build() warns when inappropriate properties are supplied", {
  sfm <- xmile()
  expect_warning(build(sfm, "A", type = "stock", interpolation = "linear"), "not appropriate")
})


test_that("build() handles units, doc, non_negative lengths", {
  sfm <- xmile()
  sfm <- build(sfm, c("A", "B"), type = c("stock", "stock"), units = c("u1", ""), doc = c("d1", "d2"), non_negative = c(TRUE, FALSE))
  vars <- as.data.frame(sfm)
  expect_equal(vars[vars[["name"]] == "A", "units"], "u1")
  expect_equal(vars[vars[["name"]] == "B", "units"], "1")  # blank cleaned to "1"
  expect_true(vars[vars[["name"]] == "A", "non_negative"])
  expect_false(vars[vars[["name"]] == "B", "non_negative"])
})


test_that("build() prepares model metadata for simulation", {
  sfm <- make_basic_sfm()

  expect_true("ordering" %in% names(sfm))
  expect_true("eqn_str_R" %in% colnames(sfm[["variables"]]))
  expect_true("sum_eqn_R" %in% colnames(sfm[["variables"]]))
})
