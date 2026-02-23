test_that("use_julia() works", {

  skip_if_julia_not_ready()

  # Stop Julia
  expect_no_error(expect_no_warning(JuliaConnectoR::stopJulia()))

  # This starts Julia
  expect_true(is_julia_ok())

  # Check if environment was initialized; now this should be false
  expect_false(is_julia_init())

  # Set up environment 
  expect_no_error(expect_no_warning(expect_message(use_julia())))

  # But the environment should be installed after calling use_julia()
  expect_true(is_julia_env_installed())
  expect_true(is_julia_init())

  # Close Julia again
  expect_no_error(expect_no_warning(use_julia(stop = TRUE)))

  # Start Julia again
  expect_no_error(expect_no_warning(expect_message(use_julia())))
  expect_true(is_julia_ok())
  expect_true(is_julia_env_installed())
  expect_true(is_julia_init())

})


test_that("use_julia() with threads works", {
  skip_if_julia_not_ready()

  # Set to 2 threads
  expect_no_error(use_julia(nthreads = 2))
  expect_equal(Sys.getenv("JULIA_NUM_THREADS"), "2")
  actual_threads <- as.integer(JuliaConnectoR::juliaEval("string(Threads.nthreads())"))
  expect_equal(actual_threads, 2)

  # If Julia was already running, it should restart and change the number of threads
  expect_no_error(use_julia(nthreads = 4))
  expect_equal(Sys.getenv("JULIA_NUM_THREADS"), "4")
  actual_threads <- as.integer(JuliaConnectoR::juliaEval("string(Threads.nthreads())"))
  expect_equal(actual_threads, 4)

})




test_that("install_julia_env() works", {
  skip_if_julia_not_ready()

  # Test installation
  expect_no_error(install_julia_env())
  expect_no_error(install_julia_env(remove = TRUE))
  expect_no_error(install_julia_env())
})

