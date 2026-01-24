# Global variables for the sdbuildR package

# Check first if .sdbuildR_env was already initialized. This is for the rare case where someone has run use_julia() already, and reloads sdbuildR, which will overwrite the initialization of use_julia()
if (!exists(".sdbuildR_env")) {
  .sdbuildR_env <- new.env(parent = emptyenv())

  .sdbuildR_env[["jl"]] <- list(
    init = FALSE,
    required_version = "1.10",
    pkg_version = "0.2.5" # required version
  )

}
