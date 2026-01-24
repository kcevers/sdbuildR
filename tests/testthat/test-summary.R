# Test summary() function for sdbuildR_xmile objects

test_that("summary() returns summary.sdbuildR_xmile object", {
  sfm <- xmile()
  
  result <- summary(sfm)
  expect_s3_class(result, "summary.sdbuildR_xmile")
})

test_that("summary() counts empty model correctly", {
  sfm <- xmile()
  
  summ <- summary(sfm)
  expect_equal(length(summ$stocks), 0)
  expect_equal(length(summ$flows), 0)
  expect_equal(length(summ$constants), 0)
  expect_equal(length(summ$aux), 0)
  expect_equal(length(summ$gf), 0)
})

test_that("summary() counts variables correctly", {
  sfm <- xmile()
  
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- build(sfm1, "Flow1", type = "flow")
  sfm3 <- build(sfm2, "Const1", type = "constant")
  sfm4 <- build(sfm3, "Aux1", type = "aux")
  
  summ <- summary(sfm4)
  expect_equal(length(summ$stocks), 1)
  expect_equal(length(summ$flows), 1)
  expect_equal(length(summ$constants), 1)
  expect_equal(length(summ$aux), 1)
})

test_that("summary() counts template variables", {
  sfm <- xmile("SIR")
  
  summ <- summary(sfm)
  expect_gt(length(summ$stocks), 0)
  expect_gt(length(summ$flows), 0)
  # SIR model should have stocks and flows
  expect_true("Susceptible" %in% summ$stocks)
  expect_true("Infected" %in% summ$stocks)
  expect_true("Recovered" %in% summ$stocks)
})

test_that("summary() reports macros", {
  sfm <- xmile()
  
  sfm1 <- macro(sfm, "param1", eqn = "5")
  sfm2 <- macro(sfm1, "param2", eqn = "10")
  
  summ <- summary(sfm2)
  expect_equal(length(summ$macros), 2)
  expect_true("param1" %in% summ$macros)
  expect_true("param2" %in% summ$macros)
})

test_that("summary() reports custom units", {
  sfm <- xmile()
  
  sfm1 <- model_units(sfm, "unit1", "eqn1")
  sfm2 <- model_units(sfm1, "unit2", "eqn2")
  
  summ <- summary(sfm2)
  expect_equal(length(summ$model_units), 2)
  expect_true("unit1" %in% summ$model_units)
  expect_true("unit2" %in% summ$model_units)
})

test_that("summary() displays simulation specs", {
  sfm <- xmile()
  
  sfm1 <- sim_specs(sfm, start = 0, stop = 100, dt = 0.1)
  
  summ <- summary(sfm1)
  expect_true("sim_specs" %in% names(summ))
  expect_equal(as.numeric(summ$sim_specs$start), 0)
  expect_equal(as.numeric(summ$sim_specs$stop), 100)
  expect_equal(as.numeric(summ$sim_specs$dt), 0.1)
})

test_that("summary() reports language setting", {
  sfm <- xmile()
  
  sfm1 <- sim_specs(sfm, language = "R")
  
  summ <- summary(sfm1)
  expect_equal(summ$sim_specs$language, "R")
})

test_that("summary() detects delay functions", {
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- build(sfm1, "DelayedVar", type = "aux", eqn = "delay(Stock1, 5)")
  
  summ <- summary(sfm2)
  expect_true(summ$has_delays)
})

test_that("summary() lists all components", {
  sfm <- xmile("SIR")
  
  summ <- summary(sfm)
  
  # Should have these fields
  expected_fields <- c("stocks", "flows", "constants", "aux", "gf", "macros", "model_units", "sim_specs")
  for (field in expected_fields) {
    expect_true(field %in% names(summ))
  }
})

test_that("summary() print method produces output", {
  sfm <- xmile("SIR")
  
  summ <- summary(sfm)
  
  # Should be able to print without error
  expect_no_error(print(summ))
})

test_that("summary() with graphical functions", {
  sfm <- xmile()
  
  sfm1 <- build(sfm, "GF1", type = "gf", 
                xpts = c(0, 1, 2), ypts = c(0, 1, 0))
  
  summ <- summary(sfm1)
  expect_equal(length(summ$gf), 1)
  expect_true("GF1" %in% summ$gf)
})

test_that("summary() preserves variable names correctly", {
  sfm <- xmile()
  
  names_to_add <- c("MyStock", "MyFlow", "MyAux", "MyConstant")
  sfm1 <- build(sfm, names_to_add[1], type = "stock")
  sfm2 <- build(sfm1, names_to_add[2], type = "flow")
  sfm3 <- build(sfm2, names_to_add[3], type = "aux")
  sfm4 <- build(sfm3, names_to_add[4], type = "constant")
  
  summ <- summary(sfm4)
  
  expect_equal(summ$stocks, names_to_add[1])
  expect_equal(summ$flows, names_to_add[2])
  expect_equal(summ$aux, names_to_add[3])
  expect_equal(summ$constants, names_to_add[4])
})

test_that("summary() with multiple items of same type", {
  sfm <- xmile()
  
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- build(sfm1, "Stock2", type = "stock")
  sfm3 <- build(sfm2, "Stock3", type = "stock")
  
  summ <- summary(sfm3)
  expect_equal(length(summ$stocks), 3)
  expect_equal(sort(summ$stocks), c("Stock1", "Stock2", "Stock3"))
})
