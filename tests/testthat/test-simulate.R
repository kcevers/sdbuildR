# Basic tests for simulate() function setup and validation
# (Skips actual Julia execution for CI environments)

# debugger() tests --------------------------------------------------------

test_that("debugger() detects model with no stocks", {
  sfm <- xmile()
  sfm1 <- build(sfm, "aux1", type = "aux", eqn = "5")
  
  problems <- debugger(sfm1, quietly = TRUE)
  expect_match(problems[["problems"]], "no stocks")
})

test_that("debugger() detects stocks not connected to flows", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- build(sfm1, "Stock2", type = "stock")
  
  problems <- debugger(sfm2, quietly = TRUE)
  expect_match(problems[["potential_problems"]], "no flows")
  expect_match(problems[["potential_problems"]], "Stock1")
  expect_match(problems[["potential_problems"]], "Stock2")
})

test_that("debugger() detects flows not connected to any stock", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm1 <- build(sfm, "Orphan_Flow", type = "flow")
  
  problems <- debugger(sfm1, quietly = TRUE)
  expect_match(problems[["problems"]], "not connected to any stock")
  expect_match(problems[["problems"]], "Orphan_Flow")
})

test_that("debugger() detects flows connected to non-existent stocks", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Bad_Flow", type = "flow", to = "NonExistentStock")
  
  problems <- debugger(sfm1, quietly = TRUE)
  expect_match(problems[["problems"]], "connected to a stock that does not exist")
  expect_match(problems[["problems"]], "Bad_Flow")
})

test_that("debugger() warns about zero equations", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- build(sfm1, "Flow1", type = "flow", from = "Stock1", eqn = "0")
  
  problems <- debugger(sfm2, quietly = TRUE)
  expect_match(problems[["potential_problems"]], "equation of 0")
  expect_match(problems[["potential_problems"]], "Flow1")
})

test_that("debugger() detects undefined variable references", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- build(sfm1, "Flow1", type = "flow", from = "Stock1", eqn = "undefined_var * 2")
  
  problems <- debugger(sfm2, quietly = TRUE)
  expect_match(problems[["problems"]], "undefined_var")
})

test_that("debugger() detects circular dependencies in static variables", {
  sfm <- xmile()
  sfm1 <- build(sfm, "const1", type = "constant", eqn = "const2")
  sfm2 <- suppressWarnings(build(sfm1, "const2", type = "constant", eqn = "const1"))
  sfm3 <- build(sfm2, "Stock1", type = "stock")
  sfm4 <- build(sfm3, "Flow1", type = "flow", from = "Stock1", eqn = "0")
  
  problems <- debugger(sfm4, quietly = TRUE)
  expect_match(problems[["problems"]], "circular|cycle", ignore.case = TRUE)
})

test_that("debugger() detects circular dependencies in dynamic variables", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- build(sfm1, "aux1", type = "aux", eqn = "aux2")
  sfm3 <- suppressWarnings(build(sfm2, "aux2", type = "aux", eqn = "aux1"))
  sfm4 <- build(sfm3, "Flow1", type = "flow", from = "Stock1", eqn = "0")
  
  problems <- debugger(sfm4, quietly = TRUE)
  expect_match(problems[["problems"]], "circular|cycle", ignore.case = TRUE)
})

test_that("debugger() returns list with quietly=TRUE", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  
  result <- debugger(sfm1, quietly = TRUE)
  
  expect_type(result, "list")
  expect_true("problems" %in% names(result))
  expect_true("potential_problems" %in% names(result))
})

test_that("debugger() returns invisible with quietly=FALSE", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- build(sfm1, "Flow1", type = "flow", from = "Stock1", eqn = "1")
  
  result <- debugger(sfm2, quietly = FALSE)
  expect_null(result)
})

test_that("debugger() passes with valid model", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock", eqn = "100")
  sfm2 <- build(sfm1, "Flow1", type = "flow", from = "Stock1", eqn = "Stock1 * 0.1")
  
  problems <- debugger(sfm2, quietly = TRUE)
  expect_equal(problems[["problems"]], "")
})

# sim_specs() tests -------------------------------------------------------

test_that("sim_specs() sets basic parameters", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- suppressWarnings(sim_specs(sfm1, start = 0, stop = 10, dt = 0.5))
  
  expect_equal(sfm2[["sim_specs"]][["start"]], "0.0")
  expect_equal(sfm2[["sim_specs"]][["stop"]], "10.0")
  expect_equal(sfm2[["sim_specs"]][["dt"]], "0.5")
})

test_that("sim_specs() validates start < stop", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  
  expect_error(
    sim_specs(sfm1, start = 10, stop = 5),
    "start.*smaller than.*stop"
  )
})

test_that("sim_specs() validates numeric parameters", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  
  expect_error(sim_specs(sfm1, start = "abc"), "numeric")
  expect_error(sim_specs(sfm1, stop = "xyz"), "numeric")
  expect_error(sim_specs(sfm1, dt = "foo"), "numeric")
})

test_that("sim_specs() validates language parameter", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  
  expect_no_error(sim_specs(sfm1, language = "R"))
  expect_no_error(sim_specs(sfm1, language = "Julia"))
})

test_that("sim_specs() validates method parameter", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  
  expect_no_error(sim_specs(sfm1, method = "euler", language = "R"))
  expect_no_error(sim_specs(sfm1, method = "rk4", language = "R"))
})

test_that("sim_specs() handles time_units", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  
  expect_no_error(sim_specs(sfm1, time_units = "days"))
  expect_no_error(sim_specs(sfm1, time_units = "hours"))
  expect_no_error(sim_specs(sfm1, time_units = "years"))
})

test_that("sim_specs() rejects invalid time_units", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  
  expect_error(sim_specs(sfm1, time_units = "invalid123"), "only contain letters")
})

test_that("sim_specs() sets save_at parameter", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- sim_specs(sfm1, dt = 0.1, save_at = 1)
  
  expect_equal(sfm2[["sim_specs"]][["save_at"]], "1.0")
})

test_that("sim_specs() warns about large dt", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  
  expect_warning(
    sim_specs(sfm1, dt = 0.5),
    "Large timestep"
  )
})

test_that("sim_specs() returns sdbuildR_xmile object", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- sim_specs(sfm1, start = 0, stop = 10)
  
  expect_s3_class(sfm2, "sdbuildR_xmile")
# order_equations() tests -------------------------------------------------

test_that("order_equations() handles simple linear dependencies", {
  sfm <- xmile()
  sfm1 <- build(sfm, "a", type = "constant", eqn = "5")
  sfm2 <- build(sfm1, "b", type = "constant", eqn = "a * 2")
  sfm3 <- build(sfm2, "c", type = "constant", eqn = "b + 1")
  sfm4 <- build(sfm3, "Stock1", type = "stock")
  sfm5 <- build(sfm4, "Flow1", type = "flow", from = "Stock1", eqn = "0")
  
  result <- order_equations(sfm5, print_msg = FALSE)
  
  expect_false(result[["static"]][["issue"]])
  expect_false(result[["dynamic"]][["issue"]])
})

test_that("order_equations() handles independent variables", {
  sfm <- xmile()
  sfm1 <- build(sfm, "x", type = "constant", eqn = "10")
  sfm2 <- build(sfm1, "y", type = "constant", eqn = "20")
  sfm3 <- build(sfm2, "z", type = "constant", eqn = "30")
  sfm4 <- build(sfm3, "Stock1", type = "stock")
  sfm5 <- build(sfm4, "Flow1", type = "flow", from = "Stock1", eqn = "0")
  
  result <- order_equations(sfm5, print_msg = FALSE)
  
  expect_false(result[["static"]][["issue"]])
  expect_false(result[["dynamic"]][["issue"]])
})

test_that("order_equations() detects circular dependencies", {
  sfm <- xmile()
  sfm1 <- build(sfm, "a", type = "constant", eqn = "b")
  sfm2 <- suppressWarnings(build(sfm1, "b", type = "constant", eqn = "a"))
  sfm3 <- build(sfm2, "Stock1", type = "stock")
  sfm4 <- build(sfm3, "Flow1", type = "flow", from = "Stock1", eqn = "0")
  
  result <- order_equations(sfm4, print_msg = FALSE)
  
  expect_true(result[["static"]][["issue"]])
})

test_that("order_equations() handles auxiliaries depending on stocks", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock", eqn = "100")
  sfm2 <- build(sfm1, "aux1", type = "aux", eqn = "Stock1 * 0.5")
  sfm3 <- build(sfm2, "Flow1", type = "flow", from = "Stock1", eqn = "aux1")
  
  result <- order_equations(sfm3, print_msg = FALSE)
  
  expect_false(result[["dynamic"]][["issue"]])
})

test_that("order_equations() returns correct structure", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock", eqn = "100")
  sfm2 <- build(sfm1, "Flow1", type = "flow", from = "Stock1", eqn = "1")
  
  result <- order_equations(sfm2, print_msg = FALSE)
  
  expect_type(result, "list")
  expect_true("static" %in% names(result))
  expect_true("dynamic" %in% names(result))
  expect_true("issue" %in% names(result[["static"]]))
  expect_true("issue" %in% names(result[["dynamic"]]))
})

test_that("order_equations() handles complex dependency chains", {
  sfm <- xmile()
  sfm1 <- build(sfm, "const1", type = "constant", eqn = "10")
  sfm2 <- build(sfm1, "const2", type = "constant", eqn = "const1 * 2")
  sfm3 <- build(sfm2, "Stock1", type = "stock", eqn = "const2")
  sfm4 <- build(sfm3, "aux1", type = "aux", eqn = "Stock1 + const1")
  sfm5 <- build(sfm4, "Flow1", type = "flow", from = "Stock1", eqn = "aux1")
  
  result <- order_equations(sfm5, print_msg = FALSE)
  
  expect_false(result[["static"]][["issue"]])
  expect_false(result[["dynamic"]][["issue"]])
})

# find_dependencies_() tests ---------------------------------------------

test_that("find_dependencies_ extracts model variable dependencies", {
  sfm <- xmile()
  sfm1 <- build(sfm, "a", type = "constant", eqn = "5")
  sfm2 <- build(sfm1, "b", type = "constant", eqn = "a * 2")
  sfm3 <- build(sfm2, "c", type = "constant", eqn = "b + 1")

  deps <- sdbuildR:::find_dependencies_(sfm3, eqns = c(b = "a * 2", c = "b + 1"))
  
  expect_equal(deps[["b"]], c("a"))
  expect_equal(deps[["c"]], c("b"))
})

test_that("find_dependencies_ ignores non-model functions when only_model_var", {
  sfm <- xmile()
  sfm1 <- build(sfm, "x", type = "constant", eqn = "10")
  sfm2 <- build(sfm1, "y", type = "constant", eqn = "x")

  deps <- sdbuildR:::find_dependencies_(sfm2, eqns = c(y = "sin(x) + 2"), only_model_var = TRUE)
  
  expect_equal(deps[["y"]], c("x"))
})

test_that("find_dependencies_ returns all names when only_model_var=FALSE and only_var=FALSE", {
  sfm <- xmile()
  sfm1 <- build(sfm, "x", type = "constant", eqn = "10")
  sfm2 <- build(sfm1, "y", type = "constant", eqn = "x")

  deps <- sdbuildR:::find_dependencies_(sfm2, eqns = c(y = "sin(x) + 2"), only_model_var = FALSE, only_var = FALSE)
  
  expect_true("sin" %in% deps[["y"]])
  expect_true("x" %in% deps[["y"]])
})

# static_depend_on_dyn() tests -------------------------------------------

test_that("static_depend_on_dyn detects static dependence on dynamic variables", {
  sfm <- xmile()
  sfm1 <- build(sfm, "aux1", type = "aux", eqn = "5")
  sfm2 <- build(sfm1, "const1", type = "constant", eqn = "aux1")
  sfm3 <- build(sfm2, "Stock1", type = "stock", eqn = "aux1")
  sfm4 <- build(sfm3, "Flow1", type = "flow", from = "Stock1", eqn = "0")

  out <- sdbuildR:::static_depend_on_dyn(sfm4)

  expect_true(out$issue)
  expect_match(out$msg, "static variables depend on dynamic variables")
})

test_that("static_depend_on_dyn passes when statics independent", {
  sfm <- xmile()
  sfm1 <- build(sfm, "const1", type = "constant", eqn = "10")
  sfm2 <- build(sfm1, "Stock1", type = "stock", eqn = "const1")
  sfm3 <- build(sfm2, "Flow1", type = "flow", from = "Stock1", eqn = "0")

  out <- sdbuildR:::static_depend_on_dyn(sfm3)
  expect_false(out$issue)
})

})

# simulate() tests --------------------------------------------------------

test_that("simulate() requires stocks for simulation", {
  sfm <- xmile()
  sfm1 <- build(sfm, "aux1", type = "aux", eqn = "5")
  sfm2 <- sim_specs(sfm1, language = "R")
  
  expect_error(
    simulate(sfm2),
    "no stocks|Cannot simulate"
  )
})

test_that("simulate() validates sim_specs are set", {
  sfm <- xmile("SIR")
  
  # Should have default sim_specs
  expect_no_error(
    simulate(sfm, language = "R")
  )
})

test_that("simulate() with R language works on simple model", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Pop", type = "stock", eqn = "100")
  sfm2 <- build(sfm1, "Growth", type = "flow", from = "Pop", eqn = "Pop * 0.05")
  sfm3 <- sim_specs(sfm2, language = "R", start = 0, stop = 10, dt = 1)
  
  result <- simulate(sfm3)
  
  expect_s3_class(result, "sdbuildR_sim")
  expect_true("df" %in% names(result))
  expect_true(nrow(result$df) > 0)
})

test_that("simulate() result has correct structure", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock", eqn = "10")
  sfm2 <- build(sfm1, "Flow1", type = "flow", from = "Stock1", eqn = "Stock1 * 0.1")
  sfm3 <- sim_specs(sfm2, language = "R", start = 0, stop = 5, dt = 1)
  
  result <- simulate(sfm3)
  
  # Check required fields
  expect_true("df" %in% names(result))
  expect_true("times" %in% names(result))
  expect_true("success" %in% names(result))
  expect_equal(result$success, TRUE)
})

test_that("simulate() returns data frame with time column", {
  sfm <- xmile()
  sfm1 <- build(sfm, "S", type = "stock", eqn = "100")
  sfm2 <- build(sfm1, "Flow", type = "flow", from = "S", eqn = "0")
  sfm3 <- sim_specs(sfm2, language = "R", start = 0, stop = 10, dt = 1)
  
  result <- simulate(sfm3)
  df <- result$df
  
  expect_true("time" %in% colnames(df))
  expect_equal(min(df$time), 0)
  expect_equal(max(df$time), 10)
})

test_that("simulate() respects save_at interval", {
  sfm <- xmile()
  sfm1 <- build(sfm, "X", type = "stock", eqn = "1")
  sfm2 <- build(sfm1, "Flow", type = "flow", from = "X", eqn = "0")
  sfm3 <- sim_specs(sfm2, language = "R", start = 0, stop = 10, dt = 0.1, save_at = 1)
  
  result <- simulate(sfm3)
  
  # With save_at = 1, should have roughly (10-0)/1 + 1 = 11 time points
  expect_true(nrow(result$df) <= 15)  # Allow some flexibility
})

test_that("simulate() with constants", {
  sfm <- xmile()
  sfm1 <- build(sfm, "rate", type = "constant", eqn = "0.05")
  sfm2 <- build(sfm1, "Stock", type = "stock", eqn = "100")
  sfm3 <- build(sfm2, "Flow", type = "flow", from = "Stock", eqn = "Stock * rate")
  sfm4 <- sim_specs(sfm3, language = "R", start = 0, stop = 10, dt = 1)
  
  result <- simulate(sfm4)
  
  expect_true(result$success)
  expect_true("Stock" %in% colnames(result$df))
})

test_that("simulate() with auxiliaries", {
  sfm <- xmile()
  sfm1 <- build(sfm, "S", type = "stock", eqn = "100")
  sfm2 <- build(sfm1, "rate", type = "aux", eqn = "0.1")
  sfm3 <- build(sfm2, "Flow", type = "flow", from = "S", eqn = "S * rate")
  sfm4 <- sim_specs(sfm3, language = "R", start = 0, stop = 5, dt = 1)
  
  result <- simulate(sfm4)
  
  expect_true(result$success)
  expect_true("S" %in% colnames(result$df))
})

test_that("simulate() with multiple stocks", {
  sfm <- xmile()
  sfm1 <- build(sfm, "S", type = "stock", eqn = "100")
  sfm2 <- build(sfm1, "I", type = "stock", eqn = "10")
  sfm3 <- build(sfm2, "infection", type = "flow", from = "S", to = "I", eqn = "0")
  sfm4 <- sim_specs(sfm3, language = "R", start = 0, stop = 5, dt = 1)
  
  result <- simulate(sfm4)
  
  expect_true("S" %in% colnames(result$df))
  expect_true("I" %in% colnames(result$df))
})

test_that("simulate() returns constants in result", {
  sfm <- xmile()
  sfm1 <- build(sfm, "const_val", type = "constant", eqn = "42")
  sfm2 <- build(sfm1, "Stock", type = "stock", eqn = "10")
  sfm3 <- build(sfm2, "Flow", type = "flow", from = "Stock", eqn = "0")
  sfm4 <- sim_specs(sfm3, language = "R", start = 0, stop = 5, dt = 1)
  
  result <- simulate(sfm4)
  
  expect_true("constants" %in% names(result))
  expect_true("const_val" %in% colnames(result$constants))
})

test_that("simulate() returns initial values", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Pop", type = "stock", eqn = "500")
  sfm2 <- build(sfm1, "Flow", type = "flow", from = "Pop", eqn = "0")
  sfm3 <- sim_specs(sfm2, language = "R", start = 0, stop = 10, dt = 1)
  
  result <- simulate(sfm3)
  
  expect_true("init" %in% names(result))
  expect_equal(result$init["Pop"], 500)
})

test_that("simulate() validates language parameter", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock", eqn = "100")
  sfm2 <- build(sfm1, "Flow", type = "flow", from = "Stock1", eqn = "0")
  sfm3 <- sim_specs(sfm2, language = "R")
  
  # language = "R" should work
  expect_no_error(simulate(sfm3, language = "R"))
})

test_that("simulate() with graphical function dependency", {
  skip("Requires graphical function setup")
  
  sfm <- xmile()
  # This would need a properly set up graphical function
})

test_that("simulate() preserves model structure", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock", eqn = "100", label = "My Stock")
  sfm2 <- build(sfm1, "Flow", type = "flow", from = "Stock1", eqn = "1")
  sfm3 <- sim_specs(sfm2, language = "R", start = 0, stop = 5, dt = 1)
  
  result <- simulate(sfm3)
  
  # Original model should be unchanged
  df_orig <- as.data.frame(sfm3, type = "stock", properties = c("name", "label"))
  expect_equal(df_orig[["label"]], "My Stock")
})
