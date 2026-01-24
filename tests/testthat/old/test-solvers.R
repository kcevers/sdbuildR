test_that("translating solvers works", {
  # Translate solvers
  expect_equal(solvers("euler", from = "R", to = "Julia", show_info = FALSE), "Euler()")
  expect_equal(solvers("rk4", from = "R", to = "Julia", show_info = FALSE), "RK4()")
  expect_equal(solvers("Tsit5()", from = "Julia", to = "R", show_info = FALSE), "rk45dp6")

  # Check whether solver exists
  expect_equal(solvers("euler", from = "R"), "euler")
  expect_error(solvers("Tsit5()", from = "R"), "Method Tsit5\\(\\) is not found in deSolve methods")
  expect_equal(solvers("Tsit5()", from = "Julia"), "Tsit5()")
  expect_equal(solvers("euler", from = "Julia"), "Euler()")

  # Name all solvers
  expect_error(solvers(), "Either method or from must be specified")
  expect_no_error(solvers(from = "R"))
  expect_no_error(solvers(from = "Julia"))

  # Validate input arguments
  expect_error(solvers(), "Either method or from must be specified")
  expect_error(solvers(method = NULL), "Either method or from must be specified")
  expect_error(solvers(from = NULL), "Either method or from must be specified")
  expect_error(solvers(to = NULL), "Either method or from must be specified")
  expect_error(solvers(method = NA), "Either method or from must be specified")
  expect_error(solvers(method = c("rk1", "rk4")), "method must be a single string")
  expect_error(solvers(from = c("rk1", "rk4")), "from must be a single string")
  expect_error(solvers(method = "rk1", to = c("rk1", "rk4")), "to must be a single string")
})
