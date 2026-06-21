# Neutralize any globally-set sdbuildR plotting options so snapshot tests run
# against package defaults, regardless of the user's R session (e.g. a stray
# options(sdbuildR.webgl = FALSE)). Setting the option to NULL removes it, so
# getOption("sdbuildR.webgl", default = TRUE) falls back to the package default.
# Restored after the full test run via teardown_env(). Tests that deliberately
# exercise a specific webgl setting override this locally with
# withr::local_options() inside their own test_that().
withr::local_options(
  list(sdbuildR.webgl = NULL),
  .local_envir = testthat::teardown_env()
)
