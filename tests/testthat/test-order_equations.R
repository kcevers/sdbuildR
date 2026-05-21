## Tests for dependencies and dependencies_

test_that("dependencies maps basic cross-type dependencies", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "c1", type = "constant", eqn = "5")
  sfm <- update(sfm, "S", type = "stock", eqn = "c1")
  sfm <- update(sfm, "A", type = "aux", eqn = "S + c1")
  sfm <- update(sfm, "F1", type = "flow", from = "S", eqn = "A")

  deps <- dependencies(sfm)
  expect_equal(sort(deps[["S"]]), c("c1"))
  expect_equal(sort(deps[["A"]]), sort(c("S", "c1")))
  expect_equal(sort(deps[["F1"]]), c("A"))
})

test_that("dependencies includes gf source dependencies", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "S", type = "stock", eqn = "10")
  # Graphical function with source=S
  sfm <- update(sfm, "G", type = "lookup", source = "S", xpts = c(0, 10), ypts = c(0, 100))

  deps <- dependencies(sfm)
  expect_equal(deps[["G"]], "S")
})

test_that("dependencies_ includes func names with only_model_var=TRUE", {
  sfm <- sdbuildR() |>
    custom_func("mfun", "function(x){ x }") |>
    update("x", "constant", eqn = "1") |>
    update("A", "aux", eqn = "mfun(x)")

  deps <- dependencies_(sfm, eqns = c(A = "mfun(x)"), only_model_var = TRUE)
  # Should include both variable and func name (as a dependency in model context)
  expect_true(all(c("x", "mfun") %in% deps[["A"]]))
})

test_that("dependencies_ extracts model variable dependencies", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "a", type = "constant", eqn = "5")
  sfm2 <- update(sfm1, "b", type = "constant", eqn = "a * 2")
  sfm3 <- update(sfm2, "c", type = "constant", eqn = "b + 1")

  deps <- dependencies_(sfm3, eqns = c(b = "a * 2", c = "b + 1"))
  expect_equal(deps[["b"]], c("a"))
  expect_equal(deps[["c"]], c("b"))
})

test_that("dependencies_ ignores non-model functions when only_model_var", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "x", type = "constant", eqn = "10")
  sfm2 <- update(sfm1, "y", type = "constant", eqn = "x")

  deps <- dependencies_(sfm2, eqns = c(y = "sin(x) + 2"), only_model_var = TRUE)
  expect_equal(deps[["y"]], c("x"))
})

test_that("dependencies_ returns all names when only_model_var=FALSE and only_var=FALSE", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "x", type = "constant", eqn = "10")
  sfm2 <- update(sfm1, "y", type = "constant", eqn = "x")

  deps <- dependencies_(sfm2, eqns = c(y = "sin(x) + 2"), only_model_var = FALSE, only_var = FALSE)
  expect_true("sin" %in% deps[["y"]])
  expect_true("x" %in% deps[["y"]])
})

test_that("dependencies_ ignores names inside quotes", {
  sfm <- sdbuildR() |>
    update("Stock1", "stock", eqn = "10") |>
    update("A", "aux", eqn = "paste('Stock1')")

  deps <- dependencies_(sfm, eqns = c(A = "paste('Stock1')"), only_model_var = TRUE)
  expect_equal(length(deps[["A"]]), 0)
})


test_that("dependencies_ flag behavior: only_model_var=TRUE vs FALSE", {
  sfm <- sdbuildR() |>
    update("x", "constant", eqn = "1")

  # Model-only vars should exclude non-model functions
  deps1 <- dependencies_(sfm, eqns = c(y = "sin(x)"), only_model_var = TRUE)
  expect_equal(deps1[["y"]], "x")

  # Include function names when only_model_var=FALSE and only_var=FALSE
  deps2 <- dependencies_(sfm,
    eqns = c(y = "sin(x)"),
    only_model_var = FALSE, only_var = FALSE
  )
  expect_true(all(c("x", "sin") %in% deps2[["y"]]))
})

test_that("dependencies exported API returns full mapping and reverse mapping", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "c1", type = "constant", eqn = "5")
  sfm <- update(sfm, "S", type = "stock", eqn = "c1")
  sfm <- update(sfm, "A", type = "aux", eqn = "S + c1")

  deps <- dependencies(sfm)
  expect_equal(sort(deps[["S"]]), c("c1"))
  expect_equal(sort(deps[["A"]]), sort(c("S", "c1")))

  rev_deps <- dependencies(sfm, reverse = TRUE)
  expect_true(all(c("S", "A") %in% names(rev_deps)))
  expect_true("S" %in% rev_deps[["c1"]]) # c1 is used by S
  expect_true("A" %in% rev_deps[["S"]]) # S is used by A
})

test_that("dependencies_ self-reference behavior matches implementation", {
  # Current implementation does not remove self-references from eqn
  sfm <- sdbuildR() |>
    update("A", "aux", eqn = "A + 1")

  deps <- dependencies_(sfm, only_model_var = TRUE)
  expect_true("A" %in% deps[["A"]])
})

test_that("dependencies_ respects naming in input eqns", {
  sfm <- sdbuildR()
  deps <- dependencies_(sfm, eqns = c(foo = "bar + baz"), only_model_var = FALSE)
  expect_true("foo" %in% names(deps))
})


# order_equations() tests -------------------------------------------------

test_that("order_equations() handles simple linear dependencies", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "a", type = "constant", eqn = "5")
  sfm2 <- update(sfm1, "b", type = "constant", eqn = "a * 2")
  sfm3 <- update(sfm2, "c", type = "constant", eqn = "b + 1")
  sfm4 <- update(sfm3, "Stock1", type = "stock")
  sfm5 <- update(sfm4, "Flow1", type = "flow", from = "Stock1", eqn = "0")

  result <- order_equations(sfm5, print_msg = FALSE)

  expect_false(result[["static"]][["issue"]])
  expect_false(result[["dynamic"]][["issue"]])
})

test_that("order_equations() handles independent variables", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "x", type = "constant", eqn = "10")
  sfm2 <- update(sfm1, "y", type = "constant", eqn = "20")
  sfm3 <- update(sfm2, "z", type = "constant", eqn = "30")
  sfm4 <- update(sfm3, "Stock1", type = "stock")
  sfm5 <- update(sfm4, "Flow1", type = "flow", from = "Stock1", eqn = "0")

  result <- order_equations(sfm5, print_msg = FALSE)

  expect_false(result[["static"]][["issue"]])
  expect_false(result[["dynamic"]][["issue"]])
})

test_that("order_equations() detects circular dependencies", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "a", type = "constant", eqn = "b")
  suppressWarnings(sfm <- update(sfm, "b", type = "constant", eqn = "a") |>
    update("Stock1", type = "stock") |> update("Flow1", type = "flow", from = "Stock1", eqn = "0"))

  result <- order_equations(sfm, print_msg = FALSE)

  expect_true(result[["static"]][["issue"]])
})

test_that("order_equations() handles auxiliaries depending on stocks", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock", eqn = "100")
  sfm2 <- update(sfm1, "aux1", type = "aux", eqn = "Stock1 * 0.5")
  sfm3 <- update(sfm2, "Flow1", type = "flow", from = "Stock1", eqn = "aux1")

  result <- order_equations(sfm3, print_msg = FALSE)

  expect_false(result[["dynamic"]][["issue"]])
})

test_that("order_equations() returns correct structure", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock", eqn = "100")
  sfm2 <- update(sfm1, "Flow1", type = "flow", from = "Stock1", eqn = "1")

  result <- order_equations(sfm2, print_msg = FALSE)

  expect_type(result, "list")
  expect_true("static" %in% names(result))
  expect_true("dynamic" %in% names(result))
  expect_true("issue" %in% names(result[["static"]]))
  expect_true("issue" %in% names(result[["dynamic"]]))
})

test_that("order_equations() handles complex dependency chains", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "const1", type = "constant", eqn = "10")
  sfm2 <- update(sfm1, "const2", type = "constant", eqn = "const1 * 2")
  sfm3 <- update(sfm2, "Stock1", type = "stock", eqn = "const2")
  sfm4 <- update(sfm3, "aux1", type = "aux", eqn = "Stock1 + const1")
  sfm5 <- update(sfm4, "Flow1", type = "flow", from = "Stock1", eqn = "aux1")

  result <- order_equations(sfm5, print_msg = FALSE)

  expect_false(result[["static"]][["issue"]])
  expect_false(result[["dynamic"]][["issue"]])
})

test_that("order_equations() handles empty model gracefully", {
  sfm <- sdbuildR()
  result <- order_equations(sfm, print_msg = TRUE)

  expect_type(result, "list")
  expect_true("static" %in% names(result))
  expect_true("dynamic" %in% names(result))
  expect_true("static_and_dynamic" %in% names(result))
  expect_false(result[["static"]][["issue"]])
  expect_false(result[["dynamic"]][["issue"]])
  expect_false(result[["static_and_dynamic"]][["issue"]])
  expect_length(result[["static"]][["order"]], 0)
  expect_length(result[["dynamic"]][["order"]], 0)
  expect_length(result[["static_and_dynamic"]][["order"]], 0)
})

test_that("order_equations() warns for circular dependencies in static part", {
  sfm <- sdbuildR()
  # Create circular aux dependencies
  sfm <- update(sfm, "constant1", type = "constant", eqn = "constant2")
  suppressWarnings(sfm <- update(sfm, "constant2", type = "constant", eqn = "constant1"))

  expect_warning(
    {
      result <- order_equations(sfm, print_msg = TRUE)
    },
    regexp = "Could not order static equations"
  )

  # No dynamic part, only static
  expect_true(result[["static_and_dynamic"]][["issue"]])
  expect_true(result[["static"]][["issue"]])
  expect_false(result[["dynamic"]][["issue"]])
})

test_that("order_equations() warns for circular dependencies in aux", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "aux1", type = "aux", eqn = "aux2")
  suppressWarnings(sfm2 <- update(sfm1, "aux2", type = "aux", eqn = "aux1"))

  expect_warning(
    {
      result <- order_equations(sfm2, print_msg = TRUE)
    },
    regexp = "Could not order dynamic equations"
  )

  # No static part, only dynamic
  expect_true(result[["static_and_dynamic"]][["issue"]])
  expect_false(result[["static"]][["issue"]])
  expect_true(result[["dynamic"]][["issue"]])
})


# ==============================================================================
# dependencies() NSE support
# ==============================================================================

test_that("dependencies() accepts bare symbol for name", {
  sfm <- sdbuildR("SIR")
  deps <- dependencies(sfm, name = Susceptible)
  expect_true("Susceptible" %in% names(deps))
  expect_equal(length(deps), 1L)
})

test_that("dependencies() accepts c() of bare symbols for name", {
  sfm <- sdbuildR("SIR")
  deps <- dependencies(sfm, name = c(Susceptible, Infected))
  expect_equal(length(deps), 2L)
  expect_true(all(c("Susceptible", "Infected") %in% names(deps)))
})

test_that("dependencies() accepts bare symbol for type", {
  sfm <- sdbuildR("SIR")
  deps <- dependencies(sfm, type = stock)
  stock_names <- as.data.frame(sfm, type = "stock")[["name"]]
  expect_true(all(names(deps) %in% stock_names))
})

test_that("dependencies() backward compat: strings still work for name", {
  sfm <- sdbuildR("SIR")
  deps <- dependencies(sfm, name = "Susceptible")
  expect_equal(length(deps), 1L)
  expect_true("Susceptible" %in% names(deps))
})
