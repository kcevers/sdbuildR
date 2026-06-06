#' Install, update, or remove Julia environment
#'
#' Instantiate the Julia environment for sdbuildR to run stock-and-flow models using Julia. For more guidance, see [this vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html).
#'
#' `install_julia_env()` will:
#' * Start a Julia session
#' * Activate a Julia environment using sdbuildR's Project.toml
#' * Install StockFlowRSupport.jl from GitHub (https://github.com/KCEvers/StockFlowRSupport.jl)
#' * Install all other required Julia packages
#' * Create Manifest.toml
#' * Precompile packages for faster subsequent loading
#' * Stop the Julia session
#'
#' Note that this may take 10-25 minutes the first time as Julia downloads and compiles packages.
#'
#' @param remove If `TRUE`, remove Julia environment for sdbuildR. This will delete the Manifest.toml file, as well as the StockFlowRSupport.jl package. All other Julia packages remain untouched.
#'
#' @returns Invisibly returns `NULL` after instantiating the Julia environment.
#' @export
#' @seealso [use_julia()]
#' @concept julia
#'
#' @examplesIf Sys.getenv("NOT_CRAN") == "true"
#' \dontrun{
#' install_julia_env()
#'
#' # Remove Julia environment
#' install_julia_env(remove = TRUE)
#' }
install_julia_env <- function(remove = FALSE) {
  # Just in case a Julia session is already running:
  on.exit(
    {
      .sdbuildR_env[["jl"]][["use_threads"]] <- FALSE

      # Stop Julia
      JuliaConnectoR::stopJulia()
    },
    add = TRUE
  )

  # Julia should be able to be started; JuliaConnectoR will handle errors if this is not the case
  is_julia_working()

  # Julia version needs to be correct
  is_julia_version_ok()

  # Invalidate cached env check
  .sdbuildR_env[["jl"]][["env_checked"]] <- FALSE

  if (remove) {
    # Find set-up location for sdbuildR in Julia
    env_path <- dirname(system.file("Project.toml", package = "sdbuildR"))

    # Activate the Julia environment for sdbuildR; juliaEval() automatically starts Julia
    julia_cmd <- sprintf("using Pkg; Pkg.activate(\"%s\"; io=devnull)", jl_path(env_path))
    julia_eval(julia_cmd)

    # Delete StockFlowRSupport.jl, but only if it is installed, to avoid unnecessary warnings
    status <- is_julia_env_setup(force = TRUE, error = FALSE)
    if (!isTRUE(status)) {
      cli::cli_inform(c("i" = paste0(P[["jl_pkg_name"]], ".jl not found in Julia environment; no need to remove.")))
      return(invisible())
    }

    julia_eval(sprintf(
      'Pkg.rm("%s")',
      P[["jl_pkg_name"]]
    ))

    manifest_file <- system.file("Manifest.toml", package = "sdbuildR")
    remove_files(manifest_file)
    julia_eval("Pkg.gc()")
    status <- is_julia_env_setup(force = TRUE, error = FALSE)

    if (isTRUE(status)) {
      cli::cli_inform(c("x" = "Failed to remove Julia environment."))
    } else {
      cli::cli_inform(c("v" = "Julia environment removed."))
    }
  } else {
    # First stop Julia for a clean installation
    JuliaConnectoR::stopJulia()

    # For a clean installation, remove the Manifest.toml file
    manifest_file <- system.file("Manifest.toml", package = "sdbuildR")
    remove_files(manifest_file)

    # Remove SystemDynamicsBuildR.jl (earlier sdbuildR package) if it is installed, to ensure clean installation of the required version.
    x <- sprintf(
      'using Pkg; if haskey(Pkg.project().dependencies, "%s") Pkg.rm("%s"); Pkg.gc() end',
      "SystemDynamicsBuildR", "SystemDynamicsBuildR"
    )
    tryCatch(
      julia_eval(x),
      error = function(e) {
        cli::cli_inform(c("!" = paste0("Could not remove SystemDynamicsBuildR.jl (older version of StockFlowRSupport.jl): ", e$message)))
      }
    )

    # Run the setup script
    setup_script <- system.file("setup.jl", package = "sdbuildR")
    julia_eval(paste0('include("', jl_path(setup_script), '")'))
    status <- is_julia_env_setup(force = TRUE, error = TRUE)

    if (isTRUE(status)) {
      cli::cli_inform(c("v" = "Julia environment installed."))
    } else {
      cli::cli_inform(c("x" = "Failed to install Julia environment."))
    }
  }

  invisible()
}


#' Start Julia and activate environment
#'
#' Start Julia session and activate Julia environment to simulate stock-and-flow models. To do so, Julia needs to be installed (see [https://julialang.org/install/](https://julialang.org/install/)) and findable from within R. See [this vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html) for guidance. In addition, the Julia environment specifically for sdbuildR needs to have been instantiated. This can be set up with [install_julia_env()].
#'
#' In every R session, [use_julia()] needs to be run once (which is done automatically in [`simulate()`][simulate.sdbuildR]), which can take around 30-60 seconds.
#'
#' @param stop If `TRUE`, stop active Julia session. Defaults to `FALSE`.
#' @param restart If `TRUE`, force Julia session to restart.
#' @param nthreads If not `NULL`, set the number of threads for Julia to use. This will temporarily set the environment variable `JULIA_NUM_THREADS` and restart Julia if it is already running to apply the new thread setting. See [this page](https://docs.julialang.org/en/v1/manual/parallel-computing/#man-parallel-computing) for more details on threading in Julia.
#'
#' @returns Returns `NULL` invisibly, used for side effects
#' @export
#' @seealso [install_julia_env()]
#' @concept julia
#'
#' @examplesIf Sys.getenv("NOT_CRAN") == "true"
#' # Start a Julia session and activate the Julia environment for sdbuildR
#' use_julia()
#'
#' # Start Julia with 4 threads (only works if threading is supported)
#' use_julia(nthreads = 4)
#'
#' # Restart Julia session (in case of issues)
#' use_julia(restart = TRUE)
#'
#' # Stop Julia session
#' use_julia(stop = TRUE)
#'
use_julia <- function(
  stop = FALSE,
  restart = FALSE,
  nthreads = NULL
) {
  if (stop || restart) {
    .sdbuildR_env[["jl"]][["use_threads"]] <- FALSE
    JuliaConnectoR::stopJulia()

    cli::cli_inform(c("v" = "Closed Julia session."))

    if (stop) {
      return(invisible())
    }
  }

  # If use_julia() was already run, no need to do anything, unless nthreads is specified
  if (!is.null(nthreads)) {
    if (!is.numeric(nthreads) || length(nthreads) != 1) {
      cli::cli_abort(c("x" = "nthreads must be a single positive integer."))
    }
    if (nthreads <= 0) {
      cli::cli_abort(c("x" = "nthreads must be a positive integer."))
    }
    nthreads <- as.integer(nthreads)

    if (nthreads == 1) {
      nthreads <- NULL
      .sdbuildR_env[["jl"]][["use_threads"]] <- FALSE
      # Sys.unsetenv("JULIA_NUM_THREADS")
    } else {
      # Set to FALSE in case of an error in stopping Julia
      .sdbuildR_env[["jl"]][["use_threads"]] <- FALSE

      # If nthreads was set, need to restart Julia to apply new thread setting (regardless of whether environment was already initialized, since thread setting applies to Julia session, not environment)
      JuliaConnectoR::stopJulia()

      # Find current thread setting to restore it after Julia session is started (this won't affect the new Julia session)
      .sdbuildR_env[["jl"]][["use_threads"]] <- TRUE
      withr::local_envvar(JULIA_NUM_THREADS = nthreads)
    }
  }


  # First check if Julia environment was already initialized. If so, we know:
  # - Julia is working
  # - Julia version is ok
  # - Julia environment is set up and up to date
  status <- is_julia_init()
  if (is.null(nthreads) && status) {
    return(invisible())
  }

  # If not, check whether install_julia_env() has been run
  env_checked <- is_julia_env_setup(error = TRUE)

  # If Julia environment is set up, it just has not been initialized
  if (!status && env_checked) {
    run_init_julia_env()
    status <- is_julia_init()
  }

  # Check threads were set correctly if nthreads was specified
  if (!is.null(nthreads) && status) {
    actual_threads <- as.integer(julia_eval("string(Threads.nthreads())"))
    if (actual_threads != nthreads) {
      cli::cli_warn(c(
        "!" = "Failed to set JULIA_NUM_THREADS to {nthreads}.",
        "i" = "Julia is running with {actual_threads} threads.",
        ">" = "Check that your Julia installation supports threading and that JULIA_NUM_THREADS is set correctly in your environment variables."
      ))
    } else {
      cli::cli_inform(c("v" = "Julia environment ready with {nthreads} threads."))
    }
  } else if (status) {
    cli::cli_inform(c("v" = "Julia environment ready."))
  } else {
    cli::cli_abort(c("x" = "Julia environment setup failed."))
  }

  invisible(TRUE)
}


#' Evaluate a Julia expression
#'
#' Evaluate a Julia expression using julia_eval(), with an option to suppress messages (such as the startup message when starting a Julia session).
#'
#' @param string Julia code to evaluate as a string
#' @param suppressMessages If `TRUE`, suppress messages from julia_eval(). Defaults to `TRUE`.
#'
#' @noRd
julia_eval <- function(string, suppressMessages = TRUE) {
  if (suppressMessages) {
    suppressMessages(JuliaConnectoR::juliaEval(string))
  } else {
    JuliaConnectoR::juliaEval(string)
  }
}


#' Check Julia environment was initialized
#'
#' This should only be run if a Julia session was already initialized with JuliaConnectoR.
#'
#' @returns Logical value
#' @noRd
is_julia_init <- function() {
  tryCatch(
    {
      vals <- julia_eval(sprintf("
      [
        string(isdefined(Main, :%s)),
        string(isdefined(Main, :%s))
      ]
    ", P[["jl_pkg_name"]], P[["init_sdbuildR"]]))
      isTRUE(vals[1] == "true") && isTRUE(vals[2] == "true")
    },
    error = function(e) {
      FALSE
    }
  )
}

#' Check if Julia can be started and is the correct version
#'
#' @noRd
#'
is_julia_working <- function() {
  x <- julia_eval("0")

  invisible(TRUE)
}


#' Check Julia version is sufficient for sdbuildR
#'
#' Checks if the version of Julia that can be started from R meets the minimum required version for sdbuildR. This should only be run if a Julia session was already initialized with JuliaConnectoR.
#'
#' @noRd
is_julia_version_ok <- function() {
  v <- julia_eval("string(VERSION)")

  # Required Julia version for sdbuildR
  required_jl_version <- P[["jl_required_version"]]

  # Check if version is sufficient
  if (package_version(v) < package_version(required_jl_version)) {
    cli::cli_abort(c(
      "x" = "Julia version {.val {v}} is too old.",
      "i" = "Requires version {.val {required_jl_version}} or higher.",
      ">" = "Update at {.url https://julialang.org/install/}."
    ))
  }

  invisible(TRUE)
}


#' Check if Julia environment for sdbuildR is set up and up to date
#'
#' Checks if the Julia environment for sdbuildR has been instantiated by verifying that the required package is installed and up to date. This should only be run if a Julia session was already initialized with JuliaConnectoR.
#' @param force If `TRUE`, force the check to run again even if it was already run in this session, which can be useful after installing or removing the Julia environment without restarting R. Defaults to `FALSE`.
#' @param error If `TRUE`, throw an error if the environment is not set up or not up to date. If `FALSE`, return `FALSE` invisibly instead of throwing an error. Defaults to `TRUE`.
#'
#' @returns Logical value. `TRUE` if the Julia environment for sdbuildR is set up and up to date, `FALSE` otherwise (if `error = FALSE`).
#' @noRd
is_julia_env_setup <- function(force = FALSE, error = TRUE) {
  # Return cached result if available (within same session)
  if (!force && isTRUE(.sdbuildR_env[["jl"]][["env_checked"]])) {
    return(invisible(TRUE))
  }

  .sdbuildR_env[["jl"]][["env_checked"]] <- FALSE

  # Julia version needs to be correct
  is_julia_version_ok()

  # Manifest.toml should exist in sdbuildR package to indicate Julia environment was instantiated
  # Check sdbuildR environment files
  project_file <- system.file("Project.toml", package = "sdbuildR")
  manifest_file <- system.file("Manifest.toml", package = "sdbuildR")
  # manifest_file <- file.path(dirname(project_file), "Manifest.toml")

  env_exists <- nzchar(project_file)
  env_instantiated <- file.exists(manifest_file)

  if (!env_exists) {
    if (error) {
      cli::cli_abort(c(
        "x" = "sdbuildR {.file Project.toml} not found.",
        ">" = "Try reinstalling {.pkg sdbuildR}."
      ))
    } else {
      return(invisible(FALSE))
    }
  }

  if (!env_instantiated) {
    if (error) {
      cli::cli_abort(c(
        "x" = "Julia environment for sdbuildR has not been set up.",
        ">" = "Run {.fn install_julia_env}."
      ))
    } else {
      return(invisible(FALSE))
    }
  }

  # The {jl_pkg_name} needs to be installed and up to date
  required_pkg_version <- P[["jl_pkg_version_github_release"]]
  installed_pkg_version <- find_jl_pkg_version(P[["jl_pkg_name"]])

  if (package_version(installed_pkg_version) < package_version(required_pkg_version)) {
    if (error) {
      .sdbuildR_env[["jl"]][["use_threads"]] <- FALSE
      JuliaConnectoR::stopJulia()
      cli::cli_abort(c(
        "x" = "Julia packages need updating.",
        ">" = "Run {.fn install_julia_env}."
      ))
    } else {
      return(invisible(FALSE))
    }
  }

  # Cache successful check
  .sdbuildR_env[["jl"]][["env_checked"]] <- TRUE

  invisible(TRUE)
}


#' Check Manifest.toml for a package
#'
#' Parses Manifest.toml to check if a package is installed and get its version.
#'
#' @param manifest_file Path to Manifest.toml file
#' @inheritParams find_jl_pkg_version
#'
#' @return List with components:
#'   \item{installed}{Logical. TRUE if package found in Manifest.}
#'   \item{version}{Character. Package version, or NULL if not found.}
#'
#' @noRd
#' @keywords internal
check_manifest_for_pkg <- function(manifest_file, pkg_name) {
  result <- list(
    installed = FALSE,
    version = "0.0.0"
  )

  if (!file.exists(manifest_file)) {
    return(result)
  }

  tryCatch(
    {
      # Read Manifest.toml
      manifest_content <- readLines(manifest_file, warn = FALSE)

      # Find the package section
      # Manifest.toml format (v2.0):
      # [[deps.PackageName]]
      # uuid = "..."
      # version = "1.2.3"

      # Look for the package
      pkg_pattern <- sprintf("\\[\\[deps\\.%s\\]\\]", pkg_name)
      pkg_idx <- grep(pkg_pattern, manifest_content)

      if (length(pkg_idx) == 0) {
        # Try alternative format (older manifests)
        pkg_pattern <- sprintf("\\[\"%s\"\\]", pkg_name)
        pkg_idx <- grep(pkg_pattern, manifest_content)
      }

      if (length(pkg_idx) > 0) {
        result$installed <- TRUE

        # Look for version in the next few lines
        # Read up to 20 lines after the package declaration
        search_lines <- manifest_content[pkg_idx[1]:(min(pkg_idx[1] + 20, length(manifest_content)))]

        version_line <- grep("^version\\s*=", search_lines, value = TRUE)

        if (length(version_line) > 0) {
          # Extract version string
          version_match <- regmatches(
            version_line[1],
            regexec("version\\s*=\\s*\"([^\"]+)\"", version_line[1])
          )

          if (length(version_match[[1]]) > 1) {
            result$version <- version_match[[1]][2]
          }
        }
      }
    },
    error = function(e) {
      cli::cli_inform(c("!" = paste0("Error reading Manifest.toml: ", conditionMessage(e))))
    }
  )

  result
}


#' Check Julia environment for a package
#'
#' Starts a Julia session, activates the Julia environment for sdbuildR, and checks if a package is installed and get its version.
#'
#' @inheritParams find_jl_pkg_version
#'
#' @returns List with components:
#'   \item{installed}{Logical. TRUE if package found in Manifest.}
#'   \item{version}{Character. Package version, or NULL if not found.}
#'
#' @noRd
check_julia_env_for_pkg <- function(pkg_name) {
  result <- list(
    installed = FALSE,
    version = "0.0.0"
  )

  result <- tryCatch(
    {
      # Check if package is installed
      is_installed <- tryCatch(
        {
          julia_eval(paste0(
            'using Pkg; !isnothing(findfirst(p -> p.name == "', pkg_name,
            '", Pkg.dependencies()))'
          ))
        },
        error = function(e) {
          FALSE
        }
      )

      # Check whether version is up to date
      if (is_installed) {
        installed_version <- tryCatch(
          {
            julia_eval(paste0(
              'string(Pkg.dependencies()[findfirst(p -> p.name == "',
              pkg_name,
              '", Pkg.dependencies())].version)'
            ))
          },
          error = function(e) {
            "0.0.0" # If can't determine version, assume it needs update
          }
        )
      } else {
        installed_version <- FALSE
      }

      list(
        installed = is_installed,
        version = installed_version
      )
    },
    error = function(e) {
      result
    }
  )

  result
}


#' Find installed version of Julia package
#'
#' Find the current version of a package by inspecting the Manifest.toml. If this does not work, verify its version in the Julia session. This function should only be run if Julia session was started and Julia environment was activated.
#'
#' @param pkg_name Name of the package to look for
#'
#' @returns Logical value indicating whether the package needs to be updated
#' @noRd
#'
find_jl_pkg_version <- function(pkg_name) {
  # Check if {jl_pkg_name}.jl is installed by examining Manifest.toml
  manifest_file <- system.file("Manifest.toml", package = "sdbuildR")
  pkg_check <- check_manifest_for_pkg(
    manifest_file,
    pkg_name
  )

  if (pkg_check$installed && pkg_check$version == "0.0.0") {
    # Try alternative way of finding package version if this did not work
    pkg_check <- check_julia_env_for_pkg(pkg_name)
  }

  pkg_check$version
}


#' Normalize a file path for use in Julia strings
#'
#' Ensures forward slashes so Julia doesn't interpret backslashes as escapes.
#'
#' @param path File path
#' @returns Normalized path with forward slashes
#' @noRd
jl_path <- function(path) {
  gsub("\\\\", "/", normalizePath(path, winslash = "/", mustWork = FALSE))
}


#' Set up Julia environment for sdbuildR with init.jl
#'
#' @returns NULL
#' @noRd
run_init_julia_env <- function() {
  # Find set-up location for sdbuildR in Julia
  env_path <- dirname(system.file("Project.toml", package = "sdbuildR"))

  cli::cli_inform(c("i" = "Activating Julia environment for {.pkg sdbuildR} at {.file {env_path}}..."))

  # Activate the Julia environment for sdbuildR
  julia_cmd <- sprintf("using Pkg; Pkg.activate(\"%s\"; io=devnull)", jl_path(env_path))
  julia_eval(julia_cmd)

  # # Install all dependencies from Project.toml
  # julia_eval("Pkg.instantiate()")
  # julia_eval("Pkg.resolve()")

  # Source the init.jl script
  init_file <- system.file("init.jl", package = "sdbuildR")
  julia_cmd <- paste0('include("', jl_path(init_file), '")')
  julia_eval(julia_cmd)

  invisible(NULL)
}
