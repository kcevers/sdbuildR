test_that("use_julia() works", {
  skip_if_julia_not_ready()

  # Stop Julia
  expect_no_error(expect_no_warning(JuliaConnectoR::stopJulia()))

  # This starts Julia
  expect_true(is_julia_working())
  expect_true(is_julia_version_ok())

  # Check if environment was initialized; now this should be false
  expect_false(is_julia_init())

  # Set up environment
  expect_no_error(expect_no_warning(expect_message(use_julia())))

  # But the environment should be installed after calling use_julia()
  expect_true(is_julia_env_setup())
  expect_true(is_julia_init())

  # Close Julia again
  expect_no_error(expect_no_warning(use_julia(stop = TRUE)))

  # Start Julia again
  expect_no_error(expect_no_warning(expect_message(use_julia())))
  expect_true(is_julia_working())
  expect_true(is_julia_env_setup())
  expect_true(is_julia_init())
})

test_that("julia_eval() works", {
  skip_if_julia_not_ready()

  # Stop Julia
  expect_no_error(expect_no_warning(expect_message(use_julia(stop = TRUE))))

  # Calling julia_eval should start new Julia session quietly
  expect_no_message(expect_no_warning(expect_no_error(julia_eval("1 + 1"))))

  # Clean up
  JuliaConnectoR::stopJulia()
})


test_that("use_julia() with threads works", {
  skip_if_julia_not_ready()
  old_threads <- Sys.getenv("JULIA_NUM_THREADS")
  get_threads <- function() as.integer(julia_eval("string(Threads.nthreads())"))

  # Set to 2 threads
  nthreads <- 2
  expect_no_error(use_julia(nthreads = nthreads))

  # Should not change global environment variable
  expect_equal(Sys.getenv("JULIA_NUM_THREADS"), old_threads)
  actual_threads <- get_threads()
  expect_equal(actual_threads, nthreads)
  expect_true(.sdbuildR_env[["jl"]][["use_threads"]])

  # If Julia was already running, it should restart and change the number of threads
  nthreads <- 4
  expect_no_error(use_julia(nthreads = nthreads))
  expect_equal(Sys.getenv("JULIA_NUM_THREADS"), old_threads)
  actual_threads <- get_threads()
  expect_equal(actual_threads, nthreads)
  expect_true(.sdbuildR_env[["jl"]][["use_threads"]])

  # Stopping Julia should reset the number of threads
  expect_no_error(use_julia(stop = TRUE))
  expect_equal(Sys.getenv("JULIA_NUM_THREADS"), old_threads)
  expect_false(.sdbuildR_env[["jl"]][["use_threads"]])

  # This will start Julia again, but should not use threads
  actual_threads <- get_threads()
  if (is.na(old_threads) || old_threads == "") {
    expect_equal(actual_threads, 1)
  } else {
    expect_equal(actual_threads, as.numeric(old_threads))
  }

})


test_that("install_julia_env() works", {
  skip_if_julia_not_ready()

  # Test installation
  expect_no_error(install_julia_env())
  expect_no_error(install_julia_env(remove = TRUE))
  expect_false(julia_env_ready())
  expect_false(is_julia_env_setup(error = FALSE))
  expect_false(is_julia_env_setup(error = FALSE, force = TRUE))

  # Removing again should not cause an error
  expect_message(expect_no_error(install_julia_env(remove = TRUE)), "no need to remove")

  # Install again and check that environment is ready
  expect_no_error(install_julia_env())
  expect_true(julia_env_ready())
  expect_true(is_julia_env_setup())
  expect_true(is_julia_env_setup(force = TRUE))
})
