.onLoad <- function(libname, pkgname) {
  # Auto-setup Julia environment ONLY during automated testing;  never runs for regular users

  should_auto_setup <- (
    # Condition 1: NOT_CRAN is "true" (testing environment)
    identical(Sys.getenv("NOT_CRAN"), "true") &&

      # Condition 2: Custom environmental variable is "true"
      identical(Sys.getenv("AUTO_INSTALL_JULIA_ENV"), "true")
  )

  if (should_auto_setup) {
    tryCatch(
      {
        # status <- is_julia_env_setup()
        # if (!status) {
        #   install_julia_env()
        # }

        # For a clean installation, remove the Manifest.toml file
        manifest_file <- system.file("Manifest.toml", package = "sdbuildR")
        if (!file.exists(manifest_file)) {
          install_julia_env()

          # Close Julia session
          use_julia(stop = TRUE)
        }
      },
      error = function(e) {
        invisible()
      }
    )
  }
}
