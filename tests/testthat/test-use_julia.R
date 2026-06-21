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

  # Restart Julia
  expect_no_error(expect_no_warning(expect_message(use_julia(restart = TRUE))))
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


test_that("find_manifest_julia_version() parses the julia_version field", {
  # v2.0 Manifest.toml records the Julia version that resolved the environment
  manifest <- tempfile()
  on.exit(unlink(manifest))
  writeLines(c('julia_version = "1.10.4"', 'manifest_format = "2.0"'), manifest)
  expect_equal(find_manifest_julia_version(manifest), "1.10.4")

  # Older manifests without the field should return NULL (check is skipped)
  no_field <- tempfile()
  on.exit(unlink(no_field), add = TRUE)
  writeLines('manifest_format = "1.0"', no_field)
  expect_null(find_manifest_julia_version(no_field))

  # Missing file should return NULL
  expect_null(find_manifest_julia_version(tempfile()))
})


test_that("julia_version_compatible() compares major and minor versions", {
  # Same major and minor is compatible, regardless of patch
  expect_true(julia_version_compatible("1.10.4", "1.10.9"))
  expect_true(julia_version_compatible("1.10.0", "1.10.0"))

  # Differing minor or major is incompatible
  expect_false(julia_version_compatible("1.10.4", "1.11.0"))
  expect_false(julia_version_compatible("1.10.4", "2.10.4"))

  # Unparseable versions should not block the user
  expect_true(julia_version_compatible("not-a-version", "1.10.4"))
})


test_that("install_julia_env() works", {
  skip_if_julia_not_ready()
  skip_if(interactive())
  skip_if_no_internet()

  manifest_exists <- function() {
    file.exists(system.file("Manifest.toml", package = "sdbuildR"))
  }

  # Test installation
  expect_no_error(install_julia_env())
  expect_no_error(install_julia_env(remove = TRUE))
  expect_false(manifest_exists())
  expect_false(is_julia_env_setup(error = FALSE))
  expect_false(is_julia_env_setup(error = FALSE, force = TRUE))

  # Removing again should not cause an error
  expect_message(expect_no_error(install_julia_env(remove = TRUE)), "no need to remove")

  # Install again and check that environment is ready
  expect_no_error(install_julia_env())
  expect_true(manifest_exists())
  expect_true(is_julia_env_setup())
  expect_true(is_julia_env_setup(force = TRUE))
})
