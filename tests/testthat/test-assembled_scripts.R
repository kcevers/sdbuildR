withr::local_options(list(sdbuildR.defer_codegen = FALSE))

test_that("assemble cache keeps canonical structure after invalidation", {
  sfm <- sdbuildR()
  sfm$assemble <- sfm$assemble[setdiff(
    names(sfm$assemble),
    c("eqn_cache", "summary", "run")
  )]

  sfm <- invalidate_assemble(sfm, "variables")

  expect_equal(length(sfm$assemble), length(empty_assemble()))
  expect_setequal(names(sfm$assemble), names(empty_assemble()))
})


test_that("R script components are pre-cached in sfm after sim_settings", {
  sfm <- sdbuildR("SIR") |>
    sim_settings(language = "R", start = 0, stop = 10, dt = 0.1)

  expect_equal(sfm$assemble$language, "R")
  expect_true(nzchar(sfm$assemble$times))
  expect_true(nzchar(sfm$assemble$static$script))
})

test_that("R script components persist through simulation", {
  sfm <- sdbuildR("SIR") |>
    sim_settings(language = "R", start = 0, stop = 10, dt = 0.1)

  # Capture field names before simulate
  fields_before <- names(sfm$assemble)

  # Run simulation
  sim <- simulate(sfm, only_stocks = TRUE)

  expect_setequal(fields_before, names(sim$object$assemble))
  expect_true(sim$success)
})


test_that("R funcs are pre-cached in assemble", {
  sfm <- sdbuildR() |>
    update("S", "stock", eqn = 100) |>
    update("I", "stock", eqn = 1) |>
    update("contact_rate", "constant", eqn = 0.5) |>
    custom_func(name = "infection_rate", eqn = "contact_rate * S * I") |>
    sim_settings(language = "R", start = 0, stop = 10, dt = 0.1)

  # Check that funcs are cached BEFORE simulate
  expect_true(nzchar(sfm$assemble$funcs))
  expect_true(grepl("infection_rate", sfm$assemble$funcs, fixed = TRUE))
})


# Julia Script Assembly Tests ---------------------------------------------------

test_that("Julia script components are pre-cached after sim_settings", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("SIR") |>
    sim_settings(language = "Julia", start = 0, stop = 10, dt = 0.1)

  expect_equal(sfm$assemble$language, "Julia")
  expect_true(nzchar(sfm$assemble$times))
  expect_true(nzchar(sfm$assemble$static$script))
})

test_that("Julia script components persist through simulation", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("SIR") |>
    sim_settings(language = "Julia", start = 0, stop = 10, dt = 0.1)

  # Capture field names before simulate
  fields_before <- names(sfm$assemble)

  # Trigger compilation
  sim <- simulate(sfm, only_stocks = TRUE)

  expect_equal(sort(fields_before), sort(names(sim$object$assemble)))
  expect_equal(sim$success, TRUE)
})

test_that("Julia and R have identical assemble structure fields", {
  skip_if_julia_not_ready()

  sfm_r <- sdbuildR("SIR") |>
    sim_settings(language = "R", start = 0, stop = 10, dt = 0.1)

  sfm_julia <- sdbuildR("SIR") |>
    sim_settings(language = "Julia", start = 0, stop = 10, dt = 0.1)

  # Both should have identical field names
  expect_equal(sort(names(sfm_r$assemble)), sort(names(sfm_julia$assemble)))
})


test_that("Julia equations differ from R equations in cached components", {
  skip_if_julia_not_ready()

  sfm_r <- sdbuildR("SIR") |>
    sim_settings(language = "R", start = 0, stop = 10, dt = 0.1)

  sfm_julia <- sdbuildR("SIR") |>
    sim_settings(language = "Julia", start = 0, stop = 10, dt = 0.1)

  # Check that variables have different eqn_str for R vs Julia
  susceptible_r <- sfm_r$variables[sfm_r$variables$name == "susceptible", "eqn_str"]
  susceptible_julia <- sfm_julia$variables[sfm_julia$variables$name == "susceptible", "eqn_str"]

  # R and Julia should produce different equation strings
  expect_false(identical(susceptible_r, susceptible_julia))
})


test_that("cache invalidates when sim_settings parameters change", {
  sfm <- sdbuildR() |>
    update(name = "S", type = "stock", eqn = "100") |>
    update(name = "births", type = "flow", eqn = "0.1 * S", to = "S")

  # Initial sim_settings
  sfm1 <- sim_settings(sfm, language = "R", start = 0, stop = 10, dt = 0.1)
  cache1 <- sfm1$assemble

  # Change dt
  sfm2 <- sim_settings(sfm1, dt = 0.01)
  cache2 <- sfm2$assemble

  # Cache should be regenerated (dt affects time sequence)
  expect_true(!is.null(cache2$times))
  # Times script should be different due to different dt
  expect_false(identical(cache1$times, cache2$times))
})
