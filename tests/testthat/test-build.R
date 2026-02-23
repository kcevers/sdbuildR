# Comprehensive tests for build() and helpers in build.R

test_that("build() creates variables with defaults", {
  sfm <- sdbuildR()
  sfm <- build(sfm, "Population", type = "stock")

  vars <- as.data.frame(sfm)
  expect_equal(nrow(vars), 1)
  expect_equal(vars[["name"]], "Population")
  expect_equal(vars[["type"]], "stock")
  expect_equal(vars[["eqn"]], "0")
  expect_equal(vars[["units"]], "1")
  expect_equal(vars[["label"]], "Population")
  expect_false(vars[["non_negative"]])
})


test_that("build() validates inputs and basic errors", {
  sfm <- sdbuildR()

  expect_error(build(sfm, "Var1", type = "invalid"), "must be one of")
  expect_error(build(sfm, "", type = "stock"), "cannot be empty")

  # invalid doc type
  expect_error(build(sfm, "D", type = "stock", doc = 1L), "must be")

  # invalid non_negative type
  expect_error(build(sfm, "E", type = "stock", non_negative = "no"), "must be")
})


test_that("build() handles long names with spaces without cli parse errors", {
  sfm <- sdbuildR()

  expect_warning(
    sfm2 <- build(sfm, "Very Long Stock Name That Should Wrap", type = "stock"),
    "name.*changed"
  )

  vars <- as.data.frame(sfm2)
  expect_true("Very_Long_Stock_Name_That_Should_Wrap" %in% vars[["name"]])
})


test_that("build() rejects disallowed names", {
  sfm <- sdbuildR()

  # NA name
  expect_error(build(sfm, NA_character_, type = "stock"), "Variable names cannot be NA")

  # whitespace-only name
  expect_error(build(sfm, "   ", type = "stock"), "cannot be empty")
})


test_that("build() enforces flow rules", {
  sfm <- sdbuildR()
  sfm <- build(sfm, "StockA", type = "stock")
  sfm <- build(sfm, "StockB", type = "stock")

  # Flow cannot target itself
  expect_error(build(sfm, "Flow1", type = "flow", to = "Flow1"), "flow cannot flow to itself")

  # Flow with same to/from is not allowed
  expect_error(build(sfm, "Flow2", type = "flow", to = "StockA", from = "StockA"), "same stock as both source and target")

  # Flow 'to' must be a stock (sanitize_sdbuildR will later clean; build should not accept non-stock in this context)
  expect_error(build(sfm, "Flow3", type = "flow", to = "Flow3"), "flow cannot flow to itself")
})


test_that("change_name() renames variables and updates references", {
  sfm <- sdbuildR()
  sfm <- build(sfm, "Prey", type = "stock", eqn = "10")
  sfm <- build(sfm, "Predator", type = "stock", eqn = "5")
  sfm <- build(sfm, "Hunt", type = "flow", eqn = "Prey * 0.1", from = "Predator", to = "Prey")

  sfm_renamed <- change_name(sfm, "Prey", "Bunnies")
  vars <- sfm_renamed[["variables"]]

  hunt_row <- vars[vars[["name"]] == "Hunt", ]
  expect_equal(hunt_row[["eqn"]], "Bunnies * 0.1")
  expect_equal(hunt_row[["to"]], "Bunnies")

  renamed_stock <- vars[vars[["name"]] == "Bunnies", ]
  expect_equal(renamed_stock[["label"]], "Bunnies") # label follows name when not set explicitly
})


test_that("build() blocks type mismatches", {
  sfm <- sdbuildR()
  sfm <- build(sfm, "A", type = "stock")

  # specifying a conflicting type for existing var errors
  expect_error(build(sfm, "A", type = "flow"), "Wrong `type` passed")
})

test_that("discard() removes variables", {
  sfm <- sdbuildR() |> build("A", type = "stock")

  # discard fails on missing vars
  expect_error(discard(sfm, "Missing"), "not exist")

  # discard succeeds on existing
  sfm_erased <- discard(sfm, "A")
  expect_equal(nrow(as.data.frame(sfm_erased)), 0)
})


test_that("change_type() changes types while preserving equations", {
  sfm <- sdbuildR()
  sfm <- build(sfm, "alpha", type = "aux", eqn = "2")

  sfm_changed <- change_type(sfm, "alpha", "constant")
  vars <- as.data.frame(sfm_changed)

  expect_equal(vars[["type"]], "constant")
  expect_equal(vars[["eqn"]], "2") # equation preserved when change_type is used without eqn
})


test_that("build() validates graphical functions", {
  sfm <- sdbuildR()

  expect_error(build(sfm, "curve1", type = "lookup", xpts = c(0, 1)), "required")
  expect_error(build(sfm, "curve2", type = "lookup", xpts = c(0, 1), ypts = c(2)), "Length mismatch")
  expect_error(build(sfm, "curve3", type = "lookup", xpts = c(0, 1), ypts = c(2, 3), interpolation = "bad"), "interpolation")
  expect_error(build(sfm, "curve4", type = "lookup", xpts = c(0, 1), ypts = c(2, 3), extrapolation = "bad"), "extrapolation")

  expect_error(build(sfm, "curve5", type = "lookup", xpts = c(0, 1), ypts = c(2, 3), source = c("a", "b")), "Invalid length")

  sfm_gf <- build(sfm, "curve_ok", type = "lookup", xpts = c(0, 1), ypts = c(2, 3), source = "X")
  gf_row <- sfm_gf[["variables"]][sfm_gf[["variables"]][["name"]] == "curve_ok", ]
  expect_equal(unlist(gf_row$xpts), c(0, 1))
  expect_equal(unlist(gf_row$ypts), c(2, 3))
})


test_that("build() supports bulk add via data frame and validates df", {
  sfm <- sdbuildR()
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
  expect_error(build(sfm, df = bad_df2), "not valid propert")

  # TODO: inappropriate properties via df - verify warning is triggered
  # df_warn <- data.frame(type = "stock", name = "X", interpolation = "linear")
  # expect_warning(build(sfm, df = df_warn), "not appropriate")
})


test_that("build() warns when inappropriate properties are supplied", {
  sfm <- sdbuildR()
  expect_warning(
    build(sfm, "A", type = "stock", interpolation = "linear"),
    "Inappropriate propert"
  )
})


test_that("build() handles units, doc, non_negative lengths", {
  sfm <- sdbuildR()
  sfm <- build(sfm, c("A", "B"), type = c("stock", "stock"), units = c("u1", ""), doc = c("d1", "d2"), non_negative = c(TRUE, FALSE))
  vars <- as.data.frame(sfm)
  expect_equal(vars[vars[["name"]] == "A", "units"], "u1")
  expect_equal(vars[vars[["name"]] == "B", "units"], "1") # blank cleaned to "1"
  expect_true(vars[vars[["name"]] == "A", "non_negative"])
  expect_false(vars[vars[["name"]] == "B", "non_negative"])
})


# --- Vectorized lookup tests ------------------------------------------------

test_that("lookup() supports vectorized creation with lists", {
  sfm <- sdbuildR()
  sfm <- lookup(sfm,
    c("L1", "L2"),
    xpts = list(c(0, 1), c(0, 5, 10)),
    ypts = list(c(10, 20), c(0, 50, 100))
  )
  vars <- sfm[["variables"]]
  l1 <- vars[vars[["name"]] == "L1", ]
  l2 <- vars[vars[["name"]] == "L2", ]
  expect_equal(unlist(l1$xpts), c(0, 1))
  expect_equal(unlist(l1$ypts), c(10, 20))
  expect_equal(unlist(l2$xpts), c(0, 5, 10))
  expect_equal(unlist(l2$ypts), c(0, 50, 100))
  expect_equal(l1$type, "lookup")
  expect_equal(l2$type, "lookup")
})

test_that("lookup() single with list input unlists correctly", {
  sfm <- sdbuildR()
  sfm <- lookup(sfm, "L1",
    xpts = list(0, 1, 2),
    ypts = list(10, 20, 30)
  )
  l1 <- sfm[["variables"]][sfm[["variables"]][["name"]] == "L1", ]
  expect_equal(unlist(l1$xpts), c(0, 1, 2))
  expect_equal(unlist(l1$ypts), c(10, 20, 30))
})

test_that("lookup() single with vector input still works", {
  sfm <- sdbuildR()
  sfm <- lookup(sfm, "L1", xpts = c(0, 1), ypts = c(10, 20))
  l1 <- sfm[["variables"]][sfm[["variables"]][["name"]] == "L1", ]
  expect_equal(unlist(l1$xpts), c(0, 1))
  expect_equal(unlist(l1$ypts), c(10, 20))
})

test_that("lookup() vectorized errors on non-list xpts/ypts", {
  sfm <- sdbuildR()
  expect_error(
    lookup(sfm, c("L1", "L2"), xpts = c(0, 1), ypts = c(10, 20)),
    "must be a list"
  )
})

test_that("lookup() vectorized errors on wrong-length list", {
  sfm <- sdbuildR()
  expect_error(
    lookup(sfm, c("L1", "L2"),
      xpts = list(c(0, 1)),
      ypts = list(c(10, 20), c(0, 100))
    ),
    "Length mismatch"
  )
})

test_that("lookup() vectorized recycles scalar params", {
  sfm <- sdbuildR()
  sfm <- lookup(sfm,
    c("L1", "L2"),
    xpts = list(c(0, 1), c(0, 5)),
    ypts = list(c(10, 20), c(0, 100)),
    interpolation = "constant"
  )
  vars <- sfm[["variables"]]
  expect_equal(
    vars[vars[["name"]] == "L1", "interpolation"],
    "constant"
  )
  expect_equal(
    vars[vars[["name"]] == "L2", "interpolation"],
    "constant"
  )
})

test_that("lookup() vectorized accepts per-element params", {
  sfm <- sdbuildR() |>
    stock("A", eqn = 1) |>
    stock("B", eqn = 2)
  sfm <- lookup(sfm,
    c("L1", "L2"),
    xpts = list(c(0, 1), c(0, 5)),
    ypts = list(c(10, 20), c(0, 100)),
    source = c("A", "B")
  )
  vars <- sfm[["variables"]]
  expect_equal(vars[vars[["name"]] == "L1", "source"], "A")
  expect_equal(vars[vars[["name"]] == "L2", "source"], "B")
})

test_that("build() with type = 'gf' still works (backward compat)", {
  sfm <- sdbuildR()
  sfm <- build(sfm, "L1", type = "gf",
    xpts = c(0, 1), ypts = c(10, 20)
  )
  l1 <- sfm[["variables"]][sfm[["variables"]][["name"]] == "L1", ]
  expect_equal(l1$type, "lookup")
  expect_equal(unlist(l1$xpts), c(0, 1))
})


# --- Wrapper function tests --------------------------------------------------
# These verify that each thin wrapper correctly injects `type` and forwards
# its parameters to build().  Validation / error paths are already covered
# by the build() tests above and are NOT duplicated here.

test_that("stock() creates a stock and forwards all parameters", {
  sfm <- sdbuildR()
  sfm <- stock(sfm, "Pop", eqn = "100", units = "people",
               label = "Population", doc = "total pop", non_negative = TRUE)

  v <- as.data.frame(sfm)
  expect_equal(nrow(v), 1)
  expect_equal(v[["name"]], "Pop")
  expect_equal(v[["type"]], "stock")
  expect_equal(v[["eqn"]], "100")
  expect_equal(v[["units"]], "people")
  expect_equal(v[["label"]], "Population")
  expect_equal(v[["doc"]], "total pop")
  expect_true(v[["non_negative"]])
})

test_that("flow() creates a flow and forwards to/from", {
  sfm <- sdbuildR() |> stock(c("S1", "S2"))
  sfm <- flow(sfm, "F1", eqn = "S1 * 0.5", from = "S1", to = "S2",
              units = "people/yr", non_negative = TRUE)

  v <- sfm[["variables"]]
  f <- v[v[["name"]] == "F1", ]
  expect_equal(f[["type"]], "flow")
  expect_equal(f[["eqn"]], "S1 * 0.5")
  expect_equal(f[["from"]], "S1")
  expect_equal(f[["to"]], "S2")
  expect_equal(f[["units"]], "people/yr")
  expect_true(f[["non_negative"]])
})

test_that("auxiliary() creates an aux and forwards parameters", {
  sfm <- sdbuildR()
  sfm <- auxiliary(sfm, "rate", eqn = "0.05", units = "1/yr",
                   label = "growth rate", doc = "per-capita")

  v <- as.data.frame(sfm)
  expect_equal(nrow(v), 1)
  expect_equal(v[["name"]], "rate")
  expect_equal(v[["type"]], "aux")
  expect_equal(v[["eqn"]], "0.05")
  expect_equal(v[["units"]], "1/yr")
  expect_equal(v[["label"]], "growth rate")
  expect_equal(v[["doc"]], "per-capita")
})

test_that("aux() behaves identically to auxiliary()", {
  sfm1 <- sdbuildR()
  sfm1 <- auxiliary(sfm1, "r", eqn = "0.1", units = "1/yr")

  sfm2 <- sdbuildR()
  sfm2 <- sdbuildR:::aux(sfm2, "r", eqn = "0.1", units = "1/yr")

  v1 <- as.data.frame(sfm1)
  v2 <- as.data.frame(sfm2)
  expect_equal(v1[["type"]], v2[["type"]])
  expect_equal(v1[["eqn"]], v2[["eqn"]])
  expect_equal(v1[["units"]], v2[["units"]])
})

test_that("custom_func() creates a func and forwards parameters", {
  sfm <- sdbuildR()
  sfm <- custom_func(sfm, "square", eqn = "x^2", units = "1", doc = "square fn")

  v <- as.data.frame(sfm)
  expect_equal(nrow(v), 1)
  expect_equal(v[["name"]], "square")
  expect_equal(v[["type"]], "func")
  expect_equal(v[["eqn"]], "x^2")
  expect_equal(v[["doc"]], "square fn")
})

test_that("lookup() injects type = 'lookup'", {
  # Detailed lookup tests already exist above; this only checks type injection.
  sfm <- sdbuildR()
  sfm <- lookup(sfm, "LU", xpts = c(0, 10), ypts = c(1, 2),
                units = "widgets", doc = "a lookup")

  v <- as.data.frame(sfm)
  expect_equal(v[["type"]], "lookup")
  expect_equal(v[["units"]], "widgets")
  expect_equal(v[["doc"]], "a lookup")
})

test_that("wrappers work in a pipe chain", {
  sfm <- sdbuildR() |>
    stock("Prey", eqn = "100") |>
    stock("Predator", eqn = "10") |>
    flow("births", eqn = "Prey * r", to = "Prey") |>
    flow("hunt", eqn = "Prey * Predator * b", from = "Prey", to = "Predator") |>
    auxiliary("r", eqn = "0.5") |>
    custom_func("half", eqn = "x / 2") |>
    lookup("eff", xpts = c(0, 100), ypts = c(0, 1))

  v <- as.data.frame(sfm)
  expect_equal(nrow(v), 7)
  expect_equal(sort(v[["name"]]),
               sort(c("Prey", "Predator", "births", "hunt", "r", "half", "eff")))
  expect_equal(v[v[["name"]] == "Prey",     "type"], "stock")
  expect_equal(v[v[["name"]] == "births",   "type"], "flow")
  expect_equal(v[v[["name"]] == "r",        "type"], "aux")
  expect_equal(v[v[["name"]] == "half",     "type"], "func")
  expect_equal(v[v[["name"]] == "eff",      "type"], "lookup")
})


# ------------------------------------------------------------------------------
# Tests for detecting accidentally doubly-passed model object
# e.g. sfm |> constant(sfm, A) instead of sfm |> constant(A)
# ------------------------------------------------------------------------------

test_that("build() errors when model object passed as name", {
  sfm <- sdbuildR()
  expect_error(build(sfm, sfm, type = "constant"), "passed where a variable name")
})

test_that("wrapper functions error when model object passed as name", {
  sfm <- sdbuildR()
  expect_error(constant(sfm, sfm),   "passed where a variable name")
  expect_error(stock(sfm, sfm),      "passed where a variable name")
  expect_error(flow(sfm, sfm),       "passed where a variable name")
  expect_error(auxiliary(sfm, sfm),  "passed where a variable name")
})

test_that("discard() errors when model object passed as name", {
  sfm <- sdbuildR() |> build("A", type = "constant")
  expect_error(discard(sfm, sfm), "passed where a variable name")
})

test_that("change_type() errors when model object passed as name", {
  sfm <- sdbuildR() |> build("A", type = "constant")
  expect_error(change_type(sfm, sfm, new_type = "aux"), "passed where a variable name")
})

