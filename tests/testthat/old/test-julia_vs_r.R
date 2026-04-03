test_that("compare output Julia and R for templates", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("SIR") |> sim_specs(dt = 0.1, save_at = 1, stop = 10)
  sim1 <- simulate(sfm |> sim_specs(language = "R"))
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"))
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = TRUE)
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sfm <- sdbuildR("predator_prey") |> sim_specs(dt = 0.1, save_at = 1, stop = 10)
  sim1 <- simulate(sfm |> sim_specs(language = "R"))
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"))
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = TRUE)
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sfm <- sdbuildR("logistic_model") |>
    sim_specs(dt = 0.1, save_at = 10, stop = 100)
  sim1 <- simulate(sfm |> sim_specs(language = "R"))
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"))
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  # Check whether the population converges to the carrying capacity
  expect_equal(last(sim1$df[sim1$df$variable == "X", "value"]),
    sim1$constants[["K"]],
    tolerance = .01
  )
  expect_equal(last(sim2$df[sim2$df$variable == "X", "value"]),
    sim2$constants[["K"]],
    tolerance = .01
  )

  sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = TRUE)
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sfm <- sdbuildR("Crielaard2022") |>
    sim_specs(dt = 0.1, save_at = 1, stop = 10) |>
    # Update initial condition to be non-stochastic
    update(c("Food_intake", "Hunger", "Compensatory_behaviour"),
      eqn = round(runif(3), 8)
    )

  sim1 <- simulate(sfm |> sim_specs(language = "R"))
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"))
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = TRUE)
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  # Duffing previously had an error with cos()
  sfm <- sdbuildR("Duffing") |> sim_specs(dt = 0.1, save_at = 1, stop = 10)
  sim1 <- simulate(sfm |> sim_specs(language = "R"))
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"))
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = TRUE)
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sfm <- sdbuildR("Chua") |> sim_specs(dt = 0.1, save_at = 1, stop = 10)
  sim1 <- simulate(sfm |> sim_specs(language = "R"))
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"))
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  sim1 <- simulate(sfm |> sim_specs(language = "R"), only_stocks = TRUE)
  sim2 <- simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  comp <- compare_sim(sim1, sim2)
  expect_equal(comp$equal, TRUE)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(sim2$success, TRUE)
  expect_equal(nrow(sim2$df) > 0, TRUE)

  # Check whether coffee cup reaches room temperature
  sfm <- sdbuildR("coffee_cup") |> sim_specs(
    dt = 0.1, save_at = 10, stop = 100,
    language = "Julia"
  )
  sim1 <- simulate(sfm)
  expect_equal(sim1$success, TRUE)
  expect_equal(nrow(sim1$df) > 0, TRUE)
  expect_equal(last(sim1$df[sim1$df$variable == "coffee_temperature", "value"]), sim1$constants[["room_temperature"]], tolerance = .01)

  # Can't be simulated in R, already tested in compile
})


test_that("as.data.frame(sim) works", {
  sfm <- sdbuildR("SIR") |> sim_specs(dt = 0.1, save_at = 1, stop = 10)

  sim <- simulate(sfm |> sim_specs(language = "R"))
  expect_equal(class(as.data.frame(sim)), "data.frame")
  expect_equal(nrow(as.data.frame(sim)) > 0, TRUE)

  df <- expect_no_error(as.data.frame(sim, direction = "wide"))
  expect_equal(sort(colnames(df)), c("Infected", "Recovered", "Susceptible", "time"))

  skip_if_julia_not_ready()
  sim <- simulate(sfm |> sim_specs(language = "Julia"), only_stocks = TRUE)
  expect_equal(class(as.data.frame(sim)), "data.frame")
  expect_equal(nrow(as.data.frame(sim)) > 0, TRUE)
})
