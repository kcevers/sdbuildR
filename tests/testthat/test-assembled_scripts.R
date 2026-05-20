test_that("R script components are pre-cached in sfm after sim_specs", {
  sfm <- sdbuildR("SIR") |>
    sim_specs(language = "R", start = 0, stop = 10, dt = 0.1)

  # Check that all standard fields exist
  expect_equal(length(sfm$assemble), length(empty_assemble()))
  expect_equal(sort(names(sfm$assemble)), sort(names(empty_assemble())))

  # Check that components are already cached in sfm BEFORE simulate
  expect_true(!is.null(sfm$assemble$language))
  expect_equal(sfm$assemble$language, "R")

  req_names <- names(empty_assemble())
  for (name in req_names) {
    expect_true(name %in% names(sfm$assemble))
    expect_false(is.null(sfm$assemble[[name]]))
  }
})

test_that("R script components persist through simulation", {
  sfm <- sdbuildR("SIR") |>
    sim_specs(language = "R", start = 0, stop = 10, dt = 0.1)

  # Capture field names before simulate
  fields_before <- names(sfm$assemble)

  # Run simulation
  sim <- simulate(sfm, only_stocks = TRUE)

  # Check that NO NEW fields were added during simulate
  fields_after <- names(sim$object$assemble)
  expect_equal(sort(fields_before), sort(fields_after))
  expect_equal(length(fields_after), length(empty_assemble()))

  # Check that components still exist in sim$object
  req_names <- names(empty_assemble())
  for (name in req_names) {
    expect_true(name %in% names(sim$object$assemble))
    expect_false(is.null(sim$object$assemble[[name]]))
  }

  expect_equal(sim$success, TRUE)
})


test_that("R funcs are pre-cached in assemble", {
  sfm <- sdbuildR() |>
    update("S", "stock", eqn = 100) |>
    update("I", "stock", eqn = 1) |>
    update("contact_rate", "constant", eqn = 0.5) |>
    custom_func(name = "infection_rate", eqn = "contact_rate * S * I") |>
    sim_specs(language = "R", start = 0, stop = 10, dt = 0.1)

  # Check that funcs are cached BEFORE simulate
  expect_true(nzchar(sfm$assemble$funcs))
  expect_true(grepl("infection_rate", sfm$assemble$funcs, fixed = TRUE))

  # Check structure is preserved (test succeeds without simulating)
  expect_equal(length(sfm$assemble), length(empty_assemble()))
})


# Julia Script Assembly Tests ---------------------------------------------------

test_that("Julia script components are pre-cached after sim_specs", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("SIR") |>
    sim_specs(language = "Julia", start = 0, stop = 10, dt = 0.1)

  expect_true(!is.null(sfm$assemble$language))
  expect_equal(sfm$assemble$language, "Julia")

  # Check that all standard fields exist (same as R)
  expect_equal(length(sfm$assemble), length(empty_assemble()))
  expect_equal(sort(names(sfm$assemble)), sort(names(empty_assemble())))
})

test_that("Julia script components persist through simulation", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("SIR") |>
    sim_specs(language = "Julia", start = 0, stop = 10, dt = 0.1)

  # Capture field names before simulate
  fields_before <- names(sfm$assemble)

  # Trigger compilation
  sim <- simulate(sfm, only_stocks = TRUE)

  # Check that NO NEW fields were added during simulate
  fields_after <- names(sim$object$assemble)
  expect_equal(sort(fields_before), sort(fields_after))
  expect_equal(length(fields_after), length(empty_assemble()))

  # Check that components are still cached after simulate
  req_names <- names(empty_assemble())
  for (name in req_names) {
    expect_true(name %in% names(sim$object$assemble))
    expect_false(is.null(sim$object$assemble[[name]]))
  }

  expect_equal(sim$success, TRUE)
})

test_that("Julia and R have identical assemble structure fields", {
  skip_if_julia_not_ready()

  sfm_r <- sdbuildR("SIR") |>
    sim_specs(language = "R", start = 0, stop = 10, dt = 0.1)

  sfm_julia <- sdbuildR("SIR") |>
    sim_specs(language = "Julia", start = 0, stop = 10, dt = 0.1)

  # Both should have identical field names
  expect_equal(sort(names(sfm_r$assemble)), sort(names(sfm_julia$assemble)))
  expect_equal(length(sfm_r$assemble), length(empty_assemble()))
  expect_equal(length(sfm_julia$assemble), length(empty_assemble()))
})


test_that("Julia equations differ from R equations in cached components", {
  skip_if_julia_not_ready()

  sfm_r <- sdbuildR("SIR") |>
    sim_specs(language = "R", start = 0, stop = 10, dt = 0.1)

  sfm_julia <- sdbuildR("SIR") |>
    sim_specs(language = "Julia", start = 0, stop = 10, dt = 0.1)

  # Simulate both
  sim_r <- simulate(sfm_r, only_stocks = TRUE)
  sim_julia <- simulate(sfm_julia, only_stocks = TRUE)

  # Check that variables have different eqn_str for R vs Julia
  susceptible_r <- sfm_r$variables[sfm_r$variables$name == "Susceptible", "eqn_str"]
  susceptible_julia <- sfm_julia$variables[sfm_julia$variables$name == "Susceptible", "eqn_str"]

  # R and Julia should produce different equation strings
  expect_false(identical(susceptible_r, susceptible_julia))
})


test_that("cache invalidates when sim_specs parameters change", {
  sfm <- sdbuildR() |>
    update(name = "S", type = "stock", eqn = "100") |>
    update(name = "births", type = "flow", eqn = "0.1 * S", to = "S")

  # Initial sim_specs
  sfm1 <- sim_specs(sfm, language = "R", start = 0, stop = 10, dt = 0.1)
  cache1 <- sfm1$assemble

  # Change dt
  sfm2 <- sim_specs(sfm1, dt = 0.01)
  cache2 <- sfm2$assemble

  # Cache should be regenerated (dt affects time sequence)
  expect_true(!is.null(cache2$times))
  # Times script should be different due to different dt
  expect_false(identical(cache1$times, cache2$times))
})
