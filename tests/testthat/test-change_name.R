test_that("change_name updates dependencies correctly", {
  sfm <- stockflow() |>
    update("a", "constant", eqn = 1) |>
    update("b", "aux", eqn = "a * 2") |>
    sim_settings()

  # Check initial dependencies

  deps <- sfm[["assemble"]][["ordering"]][["deps_by_name"]]
  expect_equal(deps[["b"]], "a")

  # Rename 'a' to 'alpha'
  sfm <- change_name(sfm, "a", "alpha")
  sfm <- sim_settings(sfm) # Re-trigger pre-assembly

  # Dependencies should now reference 'alpha' not 'a'
  deps <- sfm[["assemble"]][["ordering"]][["deps_by_name"]]
  expect_null(deps[["a"]]) # Old name gone
  expect_equal(deps[["alpha"]], character(0)) # alpha has no deps
  expect_equal(deps[["b"]], "alpha") # b now depends on alpha

  # Equation should also be updated
  expect_equal(
    sfm[["variables"]][sfm[["variables"]][["name"]] == "b", "eqn"],
    "alpha * 2"
  )
})

test_that("change_name clears and rebuilds cache", {
  sfm <- stockflow() |>
    update("x", "stock", eqn = 10) |>
    update("flow_in", "flow", eqn = "rate", to = "x") |>
    update("rate", "constant", eqn = 0.5) |>
    sim_settings()

  # Cache should be populated
  expect_false(is.null(sfm[["assemble"]][["ordering"]]))

  # Rename variable
  sfm <- change_name(sfm, "rate", "growth_rate")

  # Cache should be cleared after update
  expect_null(sfm[["assemble"]][["ordering"]])

  # After sim_settings, cache should be repopulated with new name
  sfm <- sim_settings(sfm)
  deps <- sfm[["assemble"]][["ordering"]][["deps_by_name"]]
  expect_true("growth_rate" %in% names(deps))
  expect_false("rate" %in% names(deps))
})

test_that("change_name updates flow to/from references in dependencies", {
  sfm <- stockflow() |>
    update("population", "stock", eqn = 100) |>
    update("births", "flow", eqn = "birth_rate * population", to = "population") |>
    update("birth_rate", "constant", eqn = 0.1) |>
    sim_settings()

  # Rename the stock

  sfm <- change_name(sfm, "population", "pop")
  sfm <- sim_settings(sfm)

  # Flow should now reference 'pop' in its equation
  flow_row <- sfm[["variables"]][sfm[["variables"]][["name"]] == "births", ]
  expect_equal(flow_row[["to"]], "pop")
  expect_true(grepl("pop", flow_row[["eqn"]]))

  # Dependencies should reflect the rename
  deps <- sfm[["assemble"]][["ordering"]][["deps_by_name"]]
  expect_true("pop" %in% names(deps))
  expect_false("population" %in% names(deps))
  expect_true("pop" %in% deps[["births"]])
})

test_that("change_name updates graphical function source", {
  sfm <- stockflow() |>
    update("input_var", "aux", eqn = "Time") |>
    update("lookup1", "lookup",
      xpts = c(0, 1, 2),
      ypts = c(0, 0.5, 1),
      source = "input_var"
    ) |>
    sim_settings()

  # Rename the source variable
  sfm <- change_name(sfm, "input_var", "x_input")
  sfm <- sim_settings(sfm)

  # Graphical function source should be updated
  gf_row <- sfm[["variables"]][sfm[["variables"]][["name"]] == "lookup1", ]
  expect_equal(gf_row[["source"]], "x_input")

  # Dependencies should reflect this
  deps <- sfm[["assemble"]][["ordering"]][["deps_by_name"]]
  expect_false("input_var" %in% names(deps))
  expect_true("x_input" %in% names(deps))
})

test_that("change_name() errors when model object passed as name", {
  sfm <- stockflow() |> update("A", type = "constant")
  expect_error(change_name(sfm, sfm, new_name = "B"), "passed where a variable name")
})

test_that("change_name() errors on missing source variable (symbol and string)", {
  sfm <- stockflow()

  # Bare symbol missing
  expect_error(
    change_name(sfm, recovery_rate, new_name = t),
    regexp = "not found in model"
  )

  # String name missing
  expect_error(
    change_name(sfm, "no_such_var", new_name = "t"),
    regexp = "not found in model"
  )
})

test_that("change_name() errors when one of multiple names is missing and leaves model unchanged", {
  sfm <- stockflow() |> update("S", type = "stock")

  expect_error(
    sfm <- change_name(sfm, c("S", "missing_var"), new_name = c("Stock", "M")),
    regexp = "not found in model"
  )

  # Original model should be unchanged
  vars <- as.data.frame(sfm)
  expect_true("S" %in% vars[["name"]])
})
