test_that("simulate R for templates", {
  for (s in c("SIR", "predator_prey", "logistic_model", "Crielaard2022", "Duffing", "Chua")) {
    sfm <- sdbuildR(s) |> sim_specs(save_at = 1)

    if (s == "Crielaard2022") {
      sfm <- sfm |>
        # Update initial condition to be non-stochastic
        update(c("Food_intake", "Hunger", "Compensatory_behaviour"), eqn = round(runif(3), 8))
    }

    sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = TRUE)
    expect_true(sim1$success)
    expect_equal(nrow(sim1$df) > 0, TRUE)

    sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = FALSE)
    expect_true(sim1$success)
    expect_equal(nrow(sim1$df) > 0, TRUE)

    if (s == "logistic_model") {
      # Check whether the population converges to the carrying capacity
      expect_equal(last(sim1$df[sim1$df$variable == "X", "value"]),
        sim1$constants[["K"]],
        tolerance = .01
      )
    }
  }
})

test_that("simulate with different components works", {
  # Without stocks throws error
  sfm <- sdbuildR() |> sim_specs(language = "R")
  expect_warning(sim <- simulate(sfm), "Your model has no stocks.")
  expect_false(sim$success)

  sfm <- sdbuildR() |>
    sim_specs(language = "R") |>
    update("a", "stock") |>
    update("b", "flow")
  expect_warning(sim <- simulate(sfm), "These flows are not connected to any stock:\\n- b")
  expect_false(sim$success)

  # With one stock and no flows and no parameters
  sfm <- sdbuildR() |>
    sim_specs(language = "R", start = 0, stop = 10, dt = 0.1) |>
    update("A", "stock", eqn = "100")
  sim <- expect_no_error(simulate(sfm))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  # Basic plot covered in consolidated test-plot-simulate_sdbuildR.R

  # One stock with flows, other stock without flows
  sfm <- sdbuildR() |>
    sim_specs(language = "R", start = 0, stop = 10, dt = 0.1) |>
    update(c("A", "B"), "stock", eqn = "100") |>
    update("C", "flow", eqn = "1", to = "A")
  sim <- expect_no_error(simulate(sfm, only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A", "B", "C"))

  # With one intermediary -> error in constructing Dataframe before in Julia
  sfm <- sdbuildR() |>
    sim_specs(language = "R", start = 0, stop = 10, dt = 0.1) |>
    update("A", "stock", eqn = "100") |>
    update("B", "flow", eqn = "1", to = "A") |>
    update("C", "aux", eqn = "B + 1")
  sim <- expect_no_message(simulate(sfm, only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A", "B", "C"))

  # Stocks without flows
  sfm <- sdbuildR() |>
    sim_specs(language = "R", start = 0, stop = 10, dt = 0.1) |>
    update("A", "stock", eqn = "100") |>
    update("B", "stock", eqn = "1") |>
    update("C", "aux", eqn = "B + 1")
  sim <- expect_no_message(simulate(sfm, only_stocks = FALSE))
  expect_equal(sort(names(sim$df)), c("time", "value", "variable"))
  expect_equal(unique(sim$df$variable), c("A", "B", "C"))

  # # With macros
  # sfm = sdbuildR(start = 0, stop = 10, dt = 0.1) |>
  #   update("A", "stock", eqn = "100") |>
  #   update("B", "flow", eqn = "1 + C(t)", to = "A") |>
  #   macro("C", eqn = "function(x) x + 1")
  # expect_no_message(simulate(sfm))
  # **solve macros& functions and how to define, maybe don't use (;x) as all arguments have to be named then, but with (x) they have to be in the right order
  # **variables cannot be functions because of the name issue with translating functions to Julia
  # **sigmoid() errorsfm = sdbuildR() |> meta(name = "Maya's Burnout") |>
  # eqn = "sigmoid((workday - normal_workday), midpoint = health)"


  # Only keep stocks
  sfm <- sdbuildR("SIR") |> sim_specs(language = "R", stop = 10, dt = 0.1)
  sim <- simulate(sfm, only_stocks = TRUE)
  expect_equal(
    length(unique(as.data.frame(sim)$variable)),
    length(names(sfm$model$variables$stock))
  )

  # All variables should be kept if only_stocks = FALSE
  sfm <- sdbuildR("SIR") |> sim_specs(language = "R", stop = 10, dt = 0.1)
  sim <- simulate(sfm, only_stocks = FALSE)
  df <- as.data.frame(sfm)
  df <- df[df$type != "constant", ]
  expect_equal(length(unique(as.data.frame(sim)$variable)), length(df$name))
})


test_that("equations that refer to the variable itself throw error", {
  sfm <- sdbuildR() %>%
    update("E", "stock", eqn = "E")
  expect_warning(
    sim <- simulate(sfm),
    "Define these missing variables or correct any spelling mistakes"
  )
  expect_false(sim$success)
})


test_that("output of simulate in R", {
  sfm <- sdbuildR("SIR") |> sim_specs(language = "R", start = 0, stop = 10, dt = .1)
  sim <- expect_no_error(simulate(sfm))
  expect_equal(all(c("df", "init", "constants", "sfm", "script", "duration")
  %in% names(sim)), TRUE)

  # Check that init and constants are not Julia objects
  expect_equal(class(sim$constants), "numeric")
  expect_equal(class(sim$init), "numeric")
  expect_equal(
    sort(names(sim$constants)),
    c("Beta", "Delay", "Effective_Contact_Rate", "Total_Population")
  )
  expect_equal(
    sort(names(sim$init)),
    c("Infected", "Recovered", "Susceptible")
  )
})

test_that("save_at works", {
  # Cannot set save_at to lower than dt
  sfm <- sdbuildR("SIR")

  # Check whether dataframe is returned at save_at times
  sfm <- sfm |>
    sim_specs(save_at = 1, dt = 0.1, start = 10, stop = 20)

  sim <- simulate(sfm |> sim_specs(language = "R"))
  expect_equal(
    diff(sim$df[sim$df$variable == "Infected", "time"])[1],
    as.numeric(sfm$sim_specs$save_at)
  )
})


test_that("negative times are possible", {
  sfm <- sdbuildR("logistic_model") |>
    sim_specs(start = -1, language = "R", stop = 10, dt = 0.1)
  expect_no_error({
    sim <- simulate(sfm)
  })
})


test_that("save_from works", {
  sfm <- sdbuildR("SIR") |> sim_specs(
    start = 0, stop = 20, save_at = .1,
    save_from = 10, language = "R"
  )
  sim <- expect_no_error(simulate(sfm))
  expect_equal(min(sim$df$time), 10)
  expect_equal(max(sim$df$time), 20)
  # Basic plot covered in consolidated test-plot-simulate_sdbuildR.R
  expect_no_error(summary(sfm))
})


test_that("seed works", {
  # Without a seed, simulations shouldn't be the same
  sfm <- sdbuildR("predator_prey") |>
    sim_specs(language = "R", start = 0, stop = 10, dt = 0.1) |>
    sim_specs(seed = NULL) |>
    update(c("predator", "prey"), eqn = "runif(1, 20, 50)")
  sim1 <- simulate(sfm)
  sim2 <- simulate(sfm)
  expect_equal(sim1$df$value[1] == sim2$df$value[1], FALSE)
  expect_equal(last(sim1$df$value) == last(sim2$df$value), FALSE)

  # With a seed, simulations should be the same
  sfm <- sfm |> sim_specs(seed = 1)
  sim1 <- simulate(sfm)
  sim2 <- simulate(sfm)
  expect_equal(last(sim1$df$value), last(sim2$df$value))
})


test_that("function in aux still works", {
  sfm <- sdbuildR() |>
    sim_specs(language = "R", start = 0, stop = 10, dt = .1) |>
    update("A", "stock") |>
    update("input", "aux", eqn = "ramp(times, 5, 10, -1)")
  sim <- expect_no_error(expect_no_message(simulate(sfm, only_stocks = FALSE)))

  # Check that input is not returned as a variable
  expect_equal(sort(unique(sim$df$variable)), c("A"))

  # Check with two intermediary variables
  sfm <- sfm |>
    update("a2", "aux", eqn = " 0.38 + input(t)")
  sim <- expect_no_error(expect_no_message(simulate(sfm, only_stocks = FALSE)))

  # Check that input is not returned as a variable
  expect_equal(sort(unique(sim$df$variable)), c("A", "a2"))
})
