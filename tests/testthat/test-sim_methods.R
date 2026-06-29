test_that("translating sim_methods works", {
  # Translate sim_methods
  expect_equal(sim_methods("euler", from = "R", to = "Julia"), "Euler()")
  expect_equal(sim_methods("rk4", from = "R", to = "Julia"), "RK4()")
  expect_equal(sim_methods("Tsit5()", from = "Julia", to = "R"), "rk45dp6")

  # Check whether solver exists
  expect_equal(sim_methods("euler", from = "R"), "euler")
  expect_error(sim_methods("Tsit5()", from = "R"), "Unknown")
  expect_equal(sim_methods("Tsit5()", from = "Julia"), "Tsit5()")
  expect_equal(sim_methods("euler", from = "Julia"), "Euler()")

  # clean_language normalisation applies to from and to
  expect_equal(sim_methods("euler", from = "r", to = "julia"), "Euler()")
  expect_equal(sim_methods("euler", from = "r"), "euler")

  # No-arg call lists sim_methods for both languages
  all_solvers <- sim_methods()
  expect_type(all_solvers, "list")
  expect_named(all_solvers, c("R", "Julia"))
  expect_no_error(sim_methods(from = "R"))
  expect_no_error(sim_methods(from = "Julia"))

  # from is required when method is provided
  expect_error(sim_methods("rk4"), "from")

  # Invalid method names
  expect_error(sim_methods("not_a_method", from = "R"), "Unknown")
  expect_error(sim_methods("NotASolver", from = "Julia"), "Unknown")

  # Invalid language values
  expect_error(sim_methods(from = "fortran"), "language")
})
