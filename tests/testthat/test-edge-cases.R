# Test Edge Cases and Boundary Conditions
# These tests ensure robustness of the package for unusual model structures

# Test stocks with no flows
test_that("stocks with no flows work correctly", {
  sfm <- stockflow() |>
    update(name = "IsolatedStock", type = "stock", eqn = 50)

  sfm_r <- sim_settings(sfm, language = "R", stop = 5)
  sfm_r <- prep_stock_change(sfm_r)

  stocks <- sfm_r$variables[sfm_r$variables$type == "stock", ]

  # sum_eqn should be "0" for isolated stock
  expect_equal(stocks$sum_eqn, "0")

  # Should simulate without error
  sim <- simulate(sfm_r)

  # Stock should remain constant
  expect_equal(unique(sim$df[sim$df$variable == "IsolatedStock", "value"]), 50)
  expect_true(sim$success)
})


# Test empty list columns
test_that("empty list columns handled correctly", {
  sfm <- stockflow() |>
    update(name = "S", type = "stock", eqn = 100)

  sfm <- prep_stock_change(sfm)

  stocks <- sfm$variables[sfm$variables$type == "stock", ]
  s_inflow <- stocks[stocks$name == "S", "inflow"][[1]]
  s_outflow <- stocks[stocks$name == "S", "outflow"][[1]]

  # Empty flows should be character(0)
  expect_length(s_inflow, 0)
  expect_length(s_outflow, 0)
  expect_type(s_inflow, "character")
  expect_type(s_outflow, "character")
})


# Test models with circular dependencies
test_that("models with circular dependencies work in both languages", {
  sfm <- stockflow() |>
    update(name = "A", type = "stock", eqn = "10") |>
    update(name = "B", type = "stock", eqn = "20") |>
    update(name = "rate_a", type = "aux", eqn = "B / 10") |>
    update(name = "rate_b", type = "aux", eqn = "A / 10") |>
    update(name = "flow_a", type = "flow", eqn = "rate_a", to = "A") |>
    update(name = "flow_b", type = "flow", eqn = "rate_b", to = "B") |>
    sim_settings(stop = 5)

  # R simulation
  sim_r <- simulate(sim_settings(sfm, language = "R"))

  # Julia simulation
  skip_if_julia_not_ready()

  sim_j <- simulate(sim_settings(sfm, language = "Julia"))

  # Both should complete
  expect_s3_class(sim_r, "simulate_stockflow")
  expect_s3_class(sim_j, "simulate_stockflow")
  expect_true(sim_r$success)
  expect_true(sim_j$success)

  # Results should be similar
  a_r <- sim_r$df[sim_r$df$variable == "A", "value"]
  a_j <- sim_j$df[sim_j$df$variable == "A", "value"]
  expect_equal(a_r, a_j, tolerance = 1e-5)
})


# Test stock with only inflows (no outflows)
test_that("stock with only inflows works correctly", {
  sfm <- stockflow() |>
    update(name = "Accumulator", type = "stock", eqn = "0") |>
    update(name = "constant_in", type = "flow", eqn = "5", to = "Accumulator") |>
    sim_settings(stop = 10, dt = 0.1)

  sfm <- prep_stock_change(sfm)

  stocks <- sfm$variables[sfm$variables$type == "stock", ]
  acc_inflow <- stocks[stocks$name == "Accumulator", "inflow"][[1]]
  acc_outflow <- stocks[stocks$name == "Accumulator", "outflow"][[1]]

  # Should have inflow, empty outflow
  expect_equal(acc_inflow, "constant_in")
  expect_length(acc_outflow, 0)

  # sum_eqn should only have inflow
  expect_true(grepl("constant_in", stocks$sum_eqn))
  expect_false(grepl("-", stocks$sum_eqn))

  # Simulate
  sim <- simulate(sfm)

  # Should accumulate linearly
  acc_vals <- sim$df[sim$df$variable == "Accumulator", "value"]
  expect_gt(max(acc_vals), 0) # Should increase
})


# Test stock with only outflows (no inflows)
test_that("stock with only outflows works correctly", {
  sfm <- stockflow() |>
    update(name = "Draining", type = "stock", eqn = "100") |>
    update(name = "constant_out", type = "flow", eqn = "2", from = "Draining") |>
    sim_settings(stop = 10, dt = 0.1)

  sfm <- prep_stock_change(sfm)

  stocks <- sfm$variables[sfm$variables$type == "stock", ]
  drain_inflow <- stocks[stocks$name == "Draining", "inflow"][[1]]
  drain_outflow <- stocks[stocks$name == "Draining", "outflow"][[1]]

  # Should have outflow, empty inflow
  expect_length(drain_inflow, 0)
  expect_equal(drain_outflow, "constant_out")

  # sum_eqn should only have outflow (negative)
  expect_true(grepl("constant_out", stocks$sum_eqn))
  expect_true(grepl("-", stocks$sum_eqn))

  # Simulate
  sim <- simulate(sfm)

  # Should decrease
  drain_vals <- sim$df[sim$df$variable == "Draining", "value"]
  expect_lt(min(drain_vals), 100) # Should decrease
})


# Test model with many flows to single stock
test_that("stock with many flows handles list columns correctly", {
  sfm <- stockflow() |>
    update(name = "Hub", type = "stock", eqn = "100") |>
    update(name = "in1", type = "flow", eqn = "1", to = "Hub") |>
    update(name = "in2", type = "flow", eqn = "2", to = "Hub") |>
    update(name = "in3", type = "flow", eqn = "3", to = "Hub") |>
    update(name = "out1", type = "flow", eqn = "0.5", from = "Hub") |>
    update(name = "out2", type = "flow", eqn = "1.5", from = "Hub")

  sfm <- prep_stock_change(sfm)

  stocks <- sfm$variables[sfm$variables$type == "stock", ]
  hub_inflow <- stocks[stocks$name == "Hub", "inflow"][[1]]
  hub_outflow <- stocks[stocks$name == "Hub", "outflow"][[1]]

  # Should have all inflows and outflows
  expect_setequal(hub_inflow, c("in1", "in2", "in3"))
  expect_setequal(hub_outflow, c("out1", "out2"))

  # sum_eqn should contain all flows
  sum_eqn <- stocks$sum_eqn
  expect_true(all(sapply(c("in1", "in2", "in3"), function(f) grepl(f, sum_eqn))))
  expect_true(all(sapply(c("out1", "out2"), function(f) grepl(f, sum_eqn))))
})


# Test flow between two stocks (both from and to defined)
test_that("flow between two stocks works correctly", {
  sfm <- stockflow() |>
    update(name = "Source", type = "stock", eqn = "100") |>
    update(name = "Sink", type = "stock", eqn = "0") |>
    update(name = "transfer", type = "flow", eqn = "5", from = "Source", to = "Sink") |>
    sim_settings(stop = 10)

  sfm <- prep_stock_change(sfm)

  stocks <- sfm$variables[sfm$variables$type == "stock", ]

  # Source should have transfer as outflow
  source_outflow <- stocks[stocks$name == "Source", "outflow"][[1]]
  expect_equal(source_outflow, "transfer")

  # Sink should have transfer as inflow
  sink_inflow <- stocks[stocks$name == "Sink", "inflow"][[1]]
  expect_equal(sink_inflow, "transfer")

  # Simulate
  sim <- simulate(sfm)

  # Source should decrease, Sink should increase
  source_vals <- sim$df[sim$df$variable == "Source", "value"]
  sink_vals <- sim$df[sim$df$variable == "Sink", "value"]

  expect_lt(min(source_vals), 100)
  expect_gt(max(sink_vals), 0)
})


# Test model with zero initial stock value
test_that("stock with zero initial value works correctly", {
  sfm <- stockflow() |>
    update(name = "ZeroStart", type = "stock", eqn = "0") |>
    update(name = "inflow", type = "flow", eqn = "1", to = "ZeroStart") |>
    sim_settings(stop = 5)

  sim <- simulate(sfm)

  # Should start at 0 and increase
  zero_vals <- sim$df[sim$df$variable == "ZeroStart", "value"]
  expect_equal(min(zero_vals), 0)
  expect_gt(max(zero_vals), 0)
})


# Test model with very large initial stock value
test_that("stock with large initial value works correctly", {
  sfm <- stockflow() |>
    update(name = "LargeStock", type = "stock", eqn = "1e10") |>
    update(name = "tiny_drain", type = "flow", eqn = "1", from = "LargeStock") |>
    sim_settings(stop = 5)

  sim <- expect_no_error(simulate(sfm))
  expect_true(sim$success)

  # Stock should remain very large
  large_vals <- sim$df[sim$df$variable == "LargeStock", "value"]
  expect_gt(min(large_vals), 1e9)
})


# Test stock with negative initial value
test_that("stock with negative initial value works correctly", {
  sfm <- stockflow() |>
    update(name = "Debt", type = "stock", eqn = "-50") |>
    update(name = "payment", type = "flow", eqn = "5", to = "Debt") |>
    sim_settings(stop = 5)

  sim <- simulate(sfm)

  # Should start negative and increase (become less negative)
  debt_vals <- sim$df[sim$df$variable == "Debt", "value"]
  expect_lt(min(debt_vals), 0) # Starts negative
  expect_gt(max(debt_vals), -50) # Increases (payments reduce debt)
})


# Test model with auxiliary depending on stock
test_that("auxiliary depending on stock works with prep functions", {
  sfm <- stockflow() |>
    update(name = "S", type = "stock", eqn = "100") |>
    update(name = "double_s", type = "aux", eqn = "2 * S") |>
    update(name = "drain", type = "flow", eqn = "double_s * 0.01", from = "S") |>
    sim_settings(stop = 5)

  sfm_prep <- prep_stock_change(sfm)

  # Should have valid sum_eqn
  stocks <- sfm_prep$variables[sfm_prep$variables$type == "stock", ]
  expect_true(grepl("drain", stocks$sum_eqn))

  # Simulate
  sim <- simulate(sfm)
  expect_true(sim$success)
})


# Test complex SIR-like model
test_that("complex model with multiple stocks and flows works", {
  sfm <- stockflow() |>
    update(name = "S", type = "stock", eqn = "900") |>
    update(name = "I", type = "stock", eqn = "100") |>
    update(name = "R", type = "stock", eqn = "0") |>
    update(name = "beta", type = "aux", eqn = "0.5") |>
    update(name = "gamma", type = "aux", eqn = "0.1") |>
    update(name = "N", type = "aux", eqn = "S + I + R") |>
    update(name = "infection", type = "flow", eqn = "beta * S * I / N", from = "S", to = "I") |>
    update(name = "recovery", type = "flow", eqn = "gamma * I", from = "I", to = "R") |>
    sim_settings(stop = 50)

  # Prep all stocks
  sfm_prep <- prep_stock_change(sfm)

  stocks <- sfm_prep$variables[sfm_prep$variables$type == "stock", ]

  # Check S has infection as outflow
  s_outflow <- stocks[stocks$name == "S", "outflow"][[1]]
  expect_equal(s_outflow, "infection")

  # Check I has infection as inflow and recovery as outflow
  i_inflow <- stocks[stocks$name == "I", "inflow"][[1]]
  i_outflow <- stocks[stocks$name == "I", "outflow"][[1]]
  expect_equal(i_inflow, "infection")
  expect_equal(i_outflow, "recovery")

  # Check R has recovery as inflow
  r_inflow <- stocks[stocks$name == "R", "inflow"][[1]]
  expect_equal(r_inflow, "recovery")

  # Simulate
  sim <- simulate(sfm)
  expect_true(sim$success)

  # Check conservation: S + I + R should be constant (1000)
  final_s <- sim$df[sim$df$variable == "S" & sim$df$time == max(sim$df$time), "value"]
  final_i <- sim$df[sim$df$variable == "I" & sim$df$time == max(sim$df$time), "value"]
  final_r <- sim$df[sim$df$variable == "R" & sim$df$time == max(sim$df$time), "value"]

  expect_equal(final_s + final_i + final_r, 1000, tolerance = 1)
})
