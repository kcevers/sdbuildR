# Test prep_stock_change() Functions and List Column Standardization
# These tests ensure that:
# 1. prep_stock_change() creates and populates list columns correctly
# 2. prep_stock_change() creates and populates list columns correctly
# 3. List columns work consistently across R and Julia
# 4. Multiple flows populate correctly in list columns

# Test prep_stock_change() creates list columns
test_that("prep_stock_change() creates list columns", {
  sfm <- sdbuildR() |>
    build(name = "S", type = "stock", eqn = "100") |>
    build(name = "inflow1", type = "flow", eqn = "5", to = "S") |>
    build(name = "outflow1", type = "flow", eqn = "3", from = "S")

  # Call prep function
  sfm <- prep_stock_change(sfm)

  # Check inflow/outflow are list columns
  stocks <- sfm$variables[sfm$variables$type == "stock", ]
  expect_true(is.list(stocks$inflow))
  expect_true(is.list(stocks$outflow))

  # Check contents
  s_inflow <- stocks[stocks$name == "S", "inflow"][[1]]
  s_outflow <- stocks[stocks$name == "S", "outflow"][[1]]

  expect_equal(s_inflow, "inflow1")
  expect_equal(s_outflow, "outflow1")
})

# Test prep_stock_change() creates list columns
test_that("prep_stock_change() creates list columns", {
  sfm <- sdbuildR() |>
    build(name = "S", type = "stock", eqn = "100", units = "people") |>
    build(name = "inflow1", type = "flow", eqn = "5", to = "S", units = "people/day") |>
    build(name = "outflow1", type = "flow", eqn = "3", from = "S", units = "people/day") |>
    sim_specs(language = "Julia")

  # Call Julia prep function
  sfm <- prep_stock_change(sfm)

  # Check inflow/outflow are list columns
  stocks <- sfm$variables[sfm$variables$type == "stock", ]
  expect_true(is.list(stocks$inflow))
  expect_true(is.list(stocks$outflow))

  # Check contents
  s_inflow <- stocks[stocks$name == "S", "inflow"][[1]]
  s_outflow <- stocks[stocks$name == "S", "outflow"][[1]]

  expect_equal(s_inflow, "inflow1")
  expect_equal(s_outflow, "outflow1")
})

# Test multiple flows populate correctly
test_that("multiple flows populate correctly in list columns", {
  sfm <- sdbuildR() |>
    build(name = "S", type = "stock", eqn = "100") |>
    build(name = "in1", type = "flow", eqn = "2", to = "S") |>
    build(name = "in2", type = "flow", eqn = "3", to = "S") |>
    build(name = "out1", type = "flow", eqn = "1", from = "S") |>
    build(name = "out2", type = "flow", eqn = "1.5", from = "S")

  sfm <- prep_stock_change(sfm)

  stocks <- sfm$variables[sfm$variables$type == "stock", ]
  s_inflow <- stocks[stocks$name == "S", "inflow"][[1]]
  s_outflow <- stocks[stocks$name == "S", "outflow"][[1]]

  # Should contain all flows
  expect_setequal(s_inflow, c("in1", "in2"))
  expect_setequal(s_outflow, c("out1", "out2"))

  # sum_eqn should concatenate correctly
  expect_true(grepl("in1 \\+ in2", stocks$sum_eqn) | grepl("in2 \\+ in1", stocks$sum_eqn))
  expect_true(grepl("out1.*out2", stocks$sum_eqn) | grepl("out2.*out1", stocks$sum_eqn))
})

# Test list columns work across language switching
test_that("list columns work across language switching", {
  sfm <- sdbuildR() |>
    build(name = "S", type = "stock", eqn = "100") |>
    build(name = "births", type = "flow", eqn = "0.1 * S", to = "S")

  # Start with R
  sfm <- sim_specs(sfm, language = "R")
  sfm_after_r <- prep_stock_change(sfm)

  expect_true(is.list(sfm_after_r$variables$inflow))
  expect_true(is.list(sfm_after_r$variables$outflow))

  # Switch to Julia
  sfm <- sim_specs(sfm, language = "Julia")
  sfm_after_j <- prep_stock_change(sfm)

  expect_true(is.list(sfm_after_j$variables$inflow))
  expect_true(is.list(sfm_after_j$variables$outflow))

  # Switch back to R
  sfm <- sim_specs(sfm, language = "R")
  sfm_after_r2 <- prep_stock_change(sfm)

  expect_true(is.list(sfm_after_r2$variables$inflow))
  expect_true(is.list(sfm_after_r2$variables$outflow))
})

# Test stocks with no flows have empty list columns
test_that("stocks with no flows have empty list columns", {
  sfm <- sdbuildR() |>
    build(name = "IsolatedStock", type = "stock", eqn = "50")

  sfm <- prep_stock_change(sfm)

  stocks <- sfm$variables[sfm$variables$type == "stock", ]
  s_inflow <- stocks[stocks$name == "IsolatedStock", "inflow"][[1]]
  s_outflow <- stocks[stocks$name == "IsolatedStock", "outflow"][[1]]

  # Empty flows should be character(0)
  expect_length(s_inflow, 0)
  expect_length(s_outflow, 0)
  expect_type(s_inflow, "character")
  expect_type(s_outflow, "character")

  # sum_eqn should be "0"
  expect_equal(stocks$sum_eqn, "0")
})

# Test list extraction in sum_eqn generation
test_that("sum_eqn correctly generated from list columns", {
  sfm <- sdbuildR() |>
    build(name = "A", type = "stock", eqn = "100") |>
    build(name = "B", type = "stock", eqn = "50") |>
    build(name = "f1", type = "flow", eqn = "10", to = "A") |>
    build(name = "f2", type = "flow", eqn = "5", to = "A") |>
    build(name = "f3", type = "flow", eqn = "8", from = "A") |>
    build(name = "f4", type = "flow", eqn = "3", to = "B")

  sfm <- prep_stock_change(sfm)

  stocks <- sfm$variables[sfm$variables$type == "stock", ]

  # Stock A should have inflows f1, f2 and outflow f3
  a_stocks <- stocks[stocks$name == "A", ]
  expect_true(grepl("f1", a_stocks$sum_eqn))
  expect_true(grepl("f2", a_stocks$sum_eqn))
  expect_true(grepl("f3", a_stocks$sum_eqn))

  # Stock B should have inflow f4 only
  b_stocks <- stocks[stocks$name == "B", ]
  expect_true(grepl("f4", b_stocks$sum_eqn))
  expect_false(grepl("f1", b_stocks$sum_eqn))
})

# Test consistency between R and Julia prep
test_that("R and Julia prep produce equivalent list structures", {
  # Create model
  sfm_base <- sdbuildR() |>
    build(name = "S", type = "stock", eqn = "100") |>
    build(name = "in1", type = "flow", eqn = "5", to = "S") |>
    build(name = "out1", type = "flow", eqn = "3", from = "S")

  # R prep
  sfm_r <- sfm_base
  sfm_r <- sim_specs(sfm_r, language = "R")
  sfm_r <- prep_stock_change(sfm_r)

  # Julia prep
  sfm_j <- sfm_base
  sfm_j <- sim_specs(sfm_j, language = "Julia")
  sfm_j <- prep_stock_change(sfm_j)

  # Both should have list columns
  expect_true(is.list(sfm_r$variables$inflow))
  expect_true(is.list(sfm_j$variables$inflow))

  # Contents should be identical
  r_inflow <- sfm_r$variables[sfm_r$variables$name == "S", "inflow"][[1]]
  j_inflow <- sfm_j$variables[sfm_j$variables$name == "S", "inflow"][[1]]

  expect_equal(r_inflow, j_inflow)
})

# Test prep functions handle existing list columns
test_that("prep functions handle pre-existing list columns", {
  # Manually create a model with list columns already set
  sfm <- sdbuildR() |>
    build(name = "S", type = "stock", eqn = "100") |>
    build(name = "births", type = "flow", eqn = "0.1 * S", to = "S")

  # First prep call
  sfm <- prep_stock_change(sfm)
  first_inflow <- sfm$variables[sfm$variables$name == "S", "inflow"][[1]]

  # Second prep call (columns already exist)
  sfm <- prep_stock_change(sfm)
  second_inflow <- sfm$variables[sfm$variables$name == "S", "inflow"][[1]]

  # Should be identical
  expect_equal(first_inflow, second_inflow)
})

# Test bidirectional flow (from and to same stock)
test_that("bidirectional flows handled correctly", {
  sfm <- sdbuildR() |>
    build(name = "Reservoir", type = "stock", eqn = "1000") |>
    build(name = "pump_in", type = "flow", eqn = "10", to = "Reservoir") |>
    build(name = "drain_out", type = "flow", eqn = "5", from = "Reservoir")

  sfm <- prep_stock_change(sfm)

  stocks <- sfm$variables[sfm$variables$type == "stock", ]
  res_inflow <- stocks[stocks$name == "Reservoir", "inflow"][[1]]
  res_outflow <- stocks[stocks$name == "Reservoir", "outflow"][[1]]

  expect_equal(res_inflow, "pump_in")
  expect_equal(res_outflow, "drain_out")

  # sum_eqn should have both
  expect_true(grepl("pump_in", stocks$sum_eqn))
  expect_true(grepl("drain_out", stocks$sum_eqn))
})

# Test Julia prep with units
test_that("Julia prep handles units correctly with list columns", {
  sfm <- sdbuildR() |>
    build(name = "Water", type = "stock", eqn = "100", units = "liters") |>
    build(name = "rain", type = "flow", eqn = "2", to = "Water", units = "liters/hour") |>
    build(name = "evap", type = "flow", eqn = "1", from = "Water", units = "liters/hour") |>
    sim_specs(language = "Julia")

  sfm <- prep_stock_change(sfm)

  stocks <- sfm$variables[sfm$variables$type == "stock", ]

  # Should have list columns
  expect_true(is.list(stocks$inflow))
  expect_true(is.list(stocks$outflow))

  # Should have proper inflow/outflow
  w_inflow <- stocks[stocks$name == "Water", "inflow"][[1]]
  w_outflow <- stocks[stocks$name == "Water", "outflow"][[1]]

  expect_equal(w_inflow, "rain")
  expect_equal(w_outflow, "evap")
})
