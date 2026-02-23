# Tests for incremental assembly cache invalidation

# --- invalidate_assemble() categories -----------------------------------------

test_that("invalidate_assemble 'all' wipes entire cache", {
  sfm <- sdbuildR("SIR")

  sfm <- invalidate_assemble(sfm, "all")

  expect_null(sfm[["assemble"]][["language"]])
  expect_null(sfm[["assemble"]][["ordering"]])
  expect_equal(sfm[["assemble"]][["ode"]], "")
  expect_equal(sfm[["assemble"]][["static"]][["script"]], "")
  expect_equal(sfm[["assemble"]][["times"]], "")
  expect_equal(sfm[["assemble"]][["funcs"]], "")
})


test_that("invalidate_assemble 'variables' preserves times/funcs/units", {
  sfm <- sdbuildR("SIR")

  old_times <- sfm[["assemble"]][["times"]]
  old_funcs <- sfm[["assemble"]][["funcs"]]
  old_language <- sfm[["assemble"]][["language"]]

  sfm <- invalidate_assemble(sfm, "variables")

  # Variable-dependent components cleared

  expect_null(sfm[["assemble"]][["ordering"]])
  expect_equal(sfm[["assemble"]][["ode"]], "")
  expect_equal(sfm[["assemble"]][["static"]][["script"]], "")

  # Independent components preserved
  expect_equal(sfm[["assemble"]][["times"]], old_times)
  expect_equal(sfm[["assemble"]][["funcs"]], old_funcs)
  expect_equal(sfm[["assemble"]][["language"]], old_language)
})


test_that("invalidate_assemble 'static' only clears static component", {
  sfm <- sdbuildR("SIR")

  old_ode <- sfm[["assemble"]][["ode"]]
  old_ordering <- sfm[["assemble"]][["ordering"]]
  old_times <- sfm[["assemble"]][["times"]]

  sfm <- invalidate_assemble(sfm, "static")

  # Static cleared
  expect_equal(sfm[["assemble"]][["static"]][["script"]], "")

  # Everything else preserved
  expect_equal(sfm[["assemble"]][["ode"]], old_ode)
  expect_equal(sfm[["assemble"]][["ordering"]], old_ordering)
  expect_equal(sfm[["assemble"]][["times"]], old_times)
})


test_that("invalidate_assemble 'dynamic' only clears ode/callback/intermediaries", {
  sfm <- sdbuildR("SIR")

  old_static <- sfm[["assemble"]][["static"]]
  old_ordering <- sfm[["assemble"]][["ordering"]]

  sfm <- invalidate_assemble(sfm, "dynamic")

  # Dynamic cleared
  expect_equal(sfm[["assemble"]][["ode"]], "")
  expect_equal(sfm[["assemble"]][["callback"]], "")

  # Static and ordering preserved
  expect_equal(sfm[["assemble"]][["static"]], old_static)
  expect_equal(sfm[["assemble"]][["ordering"]], old_ordering)
})


test_that("invalidate_assemble 'times' only clears times", {
  sfm <- sdbuildR("SIR")

  old_ode <- sfm[["assemble"]][["ode"]]
  old_static <- sfm[["assemble"]][["static"]]

  sfm <- invalidate_assemble(sfm, "times")

  expect_equal(sfm[["assemble"]][["times"]], "")
  expect_equal(sfm[["assemble"]][["ode"]], old_ode)
  expect_equal(sfm[["assemble"]][["static"]], old_static)
})


# --- build() dependency-aware invalidation ------------------------------------

test_that("changing constant value preserves ode and ordering", {
  sfm <- sdbuildR("SIR")

  old_ode <- sfm[["assemble"]][["ode"]]
  old_ordering <- sfm[["assemble"]][["ordering"]]

  # Change constant value (same dependencies, i.e., none)
  sfm <- build(sfm, "Total_Population", eqn = "100")

  # ODE and ordering should be preserved
  expect_equal(sfm[["assemble"]][["ode"]], old_ode)
  expect_equal(sfm[["assemble"]][["ordering"]], old_ordering)

})

test_that("changing equation dependencies forces full variable invalidation", {
  sfm <- sdbuildR()
  sfm <- build(sfm, "A", type = "stock", eqn = "10")
  sfm <- build(sfm, "B", type = "constant", eqn = "5")
  sfm <- build(sfm, "C", type = "constant", eqn = "3")
  sfm <- build(sfm, "F1", type = "flow", eqn = "B", to = "A")

  old_dep <- sfm[["assemble"]][["ordering"]][["deps_by_name"]][["F1"]]
  expect_equal(old_dep, "B")

  # Change flow equation to depend on C instead of B (dependency change)
  sfm <- build(sfm, "F1", eqn = "C")

  # Ordering should be cleared since dependencies changed
  new_dep <- sfm[["assemble"]][["ordering"]][["deps_by_name"]][["F1"]]

  expect_equal(new_dep, "C")
})


test_that("changing aux equation preserves ordering and static", {
  sfm <- sdbuildR()
  sfm <- build(sfm, "A", type = "stock", eqn = "10")
  sfm <- build(sfm, "k", type = "constant", eqn = "5")
  sfm <- build(sfm, "rate", type = "aux", eqn = "k * 2")
  sfm <- build(sfm, "F1", type = "flow", eqn = "rate", to = "A")

  old_ordering <- sfm[["assemble"]][["ordering"]]
  old_static <- sfm[["assemble"]][["static"]]

  # Change aux equation with same dependencies
  sfm <- build(sfm, "rate", eqn = "k * 3")

  # Ordering and static should be preserved
  expect_equal(sfm[["assemble"]][["ordering"]], old_ordering)
  expect_equal(sfm[["assemble"]][["static"]], old_static)
})


# --- sim_specs selective invalidation ------------------------------------------

test_that("sim_specs time change preserves variable components", {
  sfm <- sdbuildR("SIR") |> sim_specs(stop = 10)

  old_times <- sfm[["assemble"]][["times"]]
  old_ode <- sfm[["assemble"]][["ode"]]
  old_static <- sfm[["assemble"]][["static"]]
  old_ordering <- sfm[["assemble"]][["ordering"]]

  sfm <- sim_specs(sfm, stop = 200)

  # Variable-related components should be preserved
  expect_equal(sfm[["assemble"]][["ode"]], old_ode)
  expect_equal(sfm[["assemble"]][["static"]], old_static)
  expect_equal(sfm[["assemble"]][["ordering"]], old_ordering)

  # Times should be cleared (sim_specs changed)
  expect_true(old_times != sfm[["assemble"]][["times"]])
})



# --- Correctness: targeted matches full invalidation --------------------------

test_that("targeted invalidation produces same simulation as full invalidation", {
  sfm <- sdbuildR("SIR")
  sfm <- sim_specs(sfm, stop = 50)

  # Simulate to populate cache
  sim1 <- simulate(sfm)
  sfm_cached <- sim1$sfm

  # Path 1: Targeted invalidation (default behavior)
  sfm2 <- build(sfm_cached, "Delay", eqn = "2")
  sim2 <- simulate(sfm2)

  # Path 2: Force full invalidation
  sfm3 <- build(sfm_cached, "Delay", eqn = "2")
  sfm3 <- invalidate_assemble(sfm3, "all")
  sim3 <- simulate(sfm3)

  # Results must match
  expect_equal(sim2$df$value, sim3$df$value)
  expect_equal(sim2$df$variable, sim3$df$variable)
  expect_equal(sim2$df$time, sim3$df$time)
})



# --- diagnose and unit_strings cache fields -----------------------------------

test_that("pre_assemble_components populates diagnose cache", {
  sfm <- sdbuildR("SIR")
  expect_false(is.null(sfm[["assemble"]][["diagnose"]]))
  expect_true(is.list(sfm[["assemble"]][["diagnose"]]))
  expect_equal(sfm[["assemble"]][["diagnose"]][["zero_equations"]][["problem"]], "warning")
})


test_that("pre_assemble_components populates unit_strings cache", {
  sfm <- sdbuildR("SIR")
  expect_false(is.null(sfm[["assemble"]][["unit_strings"]]))
})


test_that("invalidate_assemble 'all' clears diagnose and unit_strings", {
  sfm <- sdbuildR("SIR")
  sfm <- invalidate_assemble(sfm, "all")
  expect_null(sfm[["assemble"]][["diagnose"]])
  expect_null(sfm[["assemble"]][["unit_strings"]])
})


test_that("invalidate_assemble 'variables' clears diagnose and unit_strings", {
  sfm <- sdbuildR("SIR")
  sfm <- invalidate_assemble(sfm, "variables")
  expect_null(sfm[["assemble"]][["diagnose"]])
  expect_null(sfm[["assemble"]][["unit_strings"]])
})


test_that("invalidate_assemble 'units' clears diagnose but preserves unit_strings", {
  sfm <- sdbuildR("SIR")
  old_unit_strings <- sfm[["assemble"]][["unit_strings"]]
  sfm <- invalidate_assemble(sfm, "units")
  expect_null(sfm[["assemble"]][["diagnose"]])
  expect_equal(sfm[["assemble"]][["unit_strings"]], old_unit_strings)
})

