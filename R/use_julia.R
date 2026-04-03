#' Install, update, or remove Julia environment
#'
#' Instantiate the Julia environment for sdbuildR to run stock-and-flow models using Julia. For more guidance, see [this vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html).
#'
#' `install_julia_env()` will:
#' * Start a Julia session
#' * Activate a Julia environment using sdbuildR's Project.toml
#' * Install SystemDynamicsBuildR.jl from GitHub (https://github.com/KCEvers/SystemDynamicsBuildR.jl)
#' * Install all other required Julia packages
#' * Create Manifest.toml
#' * Precompile packages for faster subsequent loading
#' * Stop the Julia session
#'
#' Note that this may take 10-25 minutes the first time as Julia downloads and compiles packages.
#'
#' @param remove If TRUE, remove Julia environment for sdbuildR. This will delete the Manifest.toml file, as well as the SystemDynamicsBuildR.jl package. All other Julia packages remain untouched.
#'
#' @returns Invisibly returns NULL after instantiating the Julia environment.
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
  # Julia should be able to be started; JuliaConnectoR will handle errors if this is not the case
  is_julia_ok()

  if (remove) {
    # Find set-up location for sdbuildR in Julia
    env_path <- system.file(package = "sdbuildR")

    # Activate the Julia environment for sdbuildR; juliaEval() automatically starts Julia
    julia_cmd <- sprintf("using Pkg; Pkg.activate(\"%s\")", jl_path(env_path))
    JuliaConnectoR::juliaEval(julia_cmd)

    # Delete SystemDynamicsBuildR.jl, but only if it is installed, to avoid unnecessary warnings
    status <- is_julia_env_installed(force = TRUE, error = FALSE)
    if (!isTRUE(status)) {
      cli::cli_inform("SystemDynamicsBuildR.jl not found in Julia environment; no need to remove.")
      return(invisible())
    }

    JuliaConnectoR::juliaEval(sprintf(
      'Pkg.rm("%s")',
      P[["jl_pkg_name"]]
    ))

    manifest_file <- system.file("Manifest.toml", package = "sdbuildR")
    if (file.exists(manifest_file)) {
      file.remove(manifest_file)
    }
    JuliaConnectoR::juliaEval("Pkg.gc()")
    status <- is_julia_env_installed(force = TRUE, error = FALSE)

    if (isTRUE(status)) {
      cli::cli_warn("Failed to remove Julia environment.")
    } else {
      cli::cli_inform("Julia environment removed.")
    }
  } else {

    # First stop Julia for a clean installation
    JuliaConnectoR::stopJulia()

    # Run the setup script
    setup_script <- system.file("setup.jl", package = "sdbuildR")
    JuliaConnectoR::juliaEval(sprintf(
      'jl_pkg_version = \"v%s\";',
      P[["jl_pkg_version"]]
    ))
    JuliaConnectoR::juliaEval(sprintf('include("%s")', jl_path(setup_script)))
    status <- is_julia_env_installed(force = TRUE)

    if (isTRUE(status)) {
      cli::cli_inform("Julia environment installed.")
    } else {
      cli::cli_warn("Failed to install Julia environment.")
    }
  }

  # Invalidate cached env check
  .sdbuildR_env[["jl"]][["env_checked"]] <- FALSE

  # Stop Julia
  JuliaConnectoR::stopJulia()

  invisible()
}


#' Start Julia and activate environment
#'
#' Start Julia session and activate Julia environment to simulate stock-and-flow models. To do so, Julia needs to be installed (see [https://julialang.org/install/](https://julialang.org/install/)) and findable from within R. See [this vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html) for guidance. In addition, the Julia environment specifically for sdbuildR needs to have been instantiated. This can be set up with [install_julia_env()].
#'
#' Julia supports running stock-and-flow models with units as well as [ensemble simulations][ensemble()].
#'
#' In every R session, [use_julia()] needs to be run once (which is done automatically in [`simulate()`][simulate.sdbuildR]), which can take around 30-60 seconds.
#'
#' @param stop If `TRUE`, stop active Julia session. Defaults to `FALSE`.
#' @param force If `TRUE`, force Julia setup to execute again.
#' @param nthreads If not `NULL`, set the number of threads for Julia to use. This will set the environment variable `JULIA_NUM_THREADS` and restart Julia if it is already running to apply the new thread setting. See [this page](https://docs.julialang.org/en/v1/manual/parallel-computing/#man-parallel-computing) for more details on threading in Julia.
#'
#' @returns Returns `NULL` invisibly, used for side effects
#' @export
#' @seealso [install_julia_env()]
#' @concept julia
#'
#' @examplesIf is_julia_ready()
#' # Start a Julia session and activate the Julia environment for sdbuildR
#' use_julia()
#'
#' # Start Julia with 4 threads (if your Julia installation supports threading)
#' use_julia(nthreads = 4)
#'
#' # Stop Julia session
#' use_julia(stop = TRUE)
#'
use_julia <- function(
  stop = FALSE,
  force = FALSE,
  nthreads = NULL
) {
  if (stop) {
    JuliaConnectoR::stopJulia()

    cli::cli_inform("Closed Julia session.")
    return(invisible())
  }

  # If use_julia() was already run, no need to do anything, unless force or nthreads is specified
  if (!is.null(nthreads)) {
    if (!is.numeric(nthreads) || length(nthreads) != 1 || nthreads < 1) {
      cli::cli_abort("nthreads must be a single positive integer.")
    }
    nthreads <- as.integer(nthreads)

    if (nthreads == 1) {
      nthreads <- NULL
      .sdbuildR_env[["jl"]][["use_threads"]] <- FALSE
      Sys.unsetenv("JULIA_NUM_THREADS")
    } else {
      # If nthreads was set, need to restart Julia to apply new thread setting (regardless of whether environment was already initialized, since thread setting applies to Julia session, not environment)
      JuliaConnectoR::stopJulia()
      .sdbuildR_env[["jl"]][["use_threads"]] <- TRUE
      Sys.setenv(JULIA_NUM_THREADS = nthreads)
    }
  }

  status <- is_julia_init()
  if (!force && is.null(nthreads) && status) {
    return(invisible())
  }

  if (!status) {
    run_init_julia_env()
    status <- is_julia_init()
  }

  # Check threads were set correctly if nthreads was specified
  if (!is.null(nthreads) && status) {
    actual_threads <- as.integer(JuliaConnectoR::juliaEval("string(Threads.nthreads())"))
    if (actual_threads != nthreads) {
      cli::cli_warn(c(
        "Failed to set JULIA_NUM_THREADS to {nthreads}.",
        "i" = "Julia is running with {actual_threads} threads.",
        ">" = "Check that your Julia installation supports threading and that JULIA_NUM_THREADS is set correctly in your environment variables."
      ))
    } else {
      cli::cli_inform("Julia environment ready with {nthreads} threads.")
    }
  } else if (status) {
    cli::cli_inform("Julia environment ready.")
  } else {
    cli::cli_abort("Julia environment setup failed.")
  }

  invisible()
}


#' Check if Julia is ready to be used with sdbuildR
#'
#' Check if Julia can be started and if the Julia environment for sdbuildR has been instantiated.
#'
#' @return Logical value. `TRUE` if Julia is ready to be used with sdbuildR, `FALSE` otherwise.
#' @export
#' @seealso [install_julia_env()], [use_julia()]
#' @examplesIf Sys.getenv("NOT_CRAN") == "true"
#' # Check if Julia is ready; this automatically opens a Julia session if possible
#' is_julia_ready()
#' 
#' # Close Julia session
#' use_julia(stop = TRUE)
#' 
is_julia_ready <- function() {
  if (Sys.getenv("NOT_CRAN") != "true") {
    return(FALSE)
  }

  tryCatch(
    {
      suppressWarnings({
        is_julia_ok() && is_julia_env_installed()
      })
    },
    error = function(e) {
      return(FALSE)
    }
  )
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
      vals <- JuliaConnectoR::juliaEval("
      [
        string(isdefined(Main, :SystemDynamicsBuildR)),
        string(isdefined(Main, :init_sdbuildR))
      ]
    ")
      isTRUE(vals[1] == "true") && isTRUE(vals[2] == "true")
    },
    error = function(e) {
      FALSE
    }
  )
}


is_julia_ok <- function() {
  x <- JuliaConnectoR::juliaEval("0")

  # Julia version needs to be correct
  is_julia_version_ok()

  invisible(TRUE)
}

is_julia_version_ok <- function() {
  v <- JuliaConnectoR::juliaEval("string(VERSION)")

  # Required Julia version for sdbuildR
  required_jl_version <- P[["jl_required_version"]]

  # Check if version is sufficient
  if (package_version(v) < package_version(required_jl_version)) {
    cli::cli_abort(c(
      "Julia version {.val {v}} is too old.",
      "i" = "Requires version {.val {required_jl_version}} or higher.",
      ">" = "Update at {.url https://julialang.org/install/}."
    ))
  }

  invisible(TRUE)
}


is_julia_env_installed <- function(force = FALSE, error = TRUE) {
  # Return cached result if available (within same session)
  if (!force && isTRUE(.sdbuildR_env[["jl"]][["env_checked"]])) {
    return(invisible(TRUE))
  }

  # Manifest.toml should exist in sdbuildR package to indicate Julia environment was instantiated
  # Check sdbuildR environment files
  env_path <- system.file(package = "sdbuildR")
  project_file <- file.path(env_path, "Project.toml")
  manifest_file <- file.path(env_path, "Manifest.toml")

  env_exists <- file.exists(project_file)
  env_instantiated <- file.exists(manifest_file)

  if (!env_exists) {
    if (error) {
      cli::cli_abort("sdbuildR {.file Project.toml} not found. Try reinstalling {.pkg sdbuildR}.")
    } else {
      return(invisible(FALSE))
    }
  }

  if (!env_instantiated) {
    if (error) {
      cli::cli_abort("Julia environment not instantiated. Run {.fn install_julia_env}.")
    } else {
      return(invisible(FALSE))
    }
  }

  # SystemDynamicsBuildR.jl needs to be installed and up to date
  required_pkg_version <- P[["jl_pkg_version"]]
  installed_pkg_version <- find_jl_pkg_version(P[["jl_pkg_name"]])

  if (package_version(installed_pkg_version) < package_version(required_pkg_version)) {
    if (error) {
      JuliaConnectoR::stopJulia()
      cli::cli_abort(c(
        "Julia packages need updating.",
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
      cli::cli_warn("Error reading Manifest.toml: ", conditionMessage(e))
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
          JuliaConnectoR::juliaEval(paste0(
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
            JuliaConnectoR::juliaEval(paste0(
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
  # Check if SystemDynamicsBuildR.jl is installed by examining Manifest.toml
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
  cli::cli_inform("Setting up Julia environment for {.pkg sdbuildR}...")

  # Julia should be able to be started; JuliaConnectoR will handle errors if this is not the case
  is_julia_ok()

  # Julia environment needs to be have been instantiated with required packages
  is_julia_env_installed()

  # Find set-up location for sdbuildR in Julia
  env_path <- system.file(package = "sdbuildR")

  # Activate the Julia environment for sdbuildR
  julia_cmd <- sprintf("using Pkg; Pkg.activate(\"%s\")", jl_path(env_path))
  JuliaConnectoR::juliaEval(julia_cmd)

  # # Install all dependencies from Project.toml
  # JuliaConnectoR::juliaEval("Pkg.instantiate()")
  # JuliaConnectoR::juliaEval("Pkg.resolve()")

  # Source the init.jl script
  init_file <- system.file("init.jl", package = "sdbuildR")
  julia_cmd <- sprintf("include(\"%s\")", jl_path(init_file))
  JuliaConnectoR::juliaEval(julia_cmd)

  invisible(NULL)
}


#' Internal function to create initialization file for Julia
#'
#' @returns NULL
#' @noRd
#'
create_julia_init_env <- function() {
  # Note for extending comparison operators:
  # eltype(5u"m") <: Unitful.Quantity # true
  # eltype(5u"m") <: Number # true
  # eltype(5u"m") <: Float64 # false
  # eltype(5.0) <: Real # true
  # eltype(5) <: Unitful.Quantity # false
  # so we cannot use x::Number, but have to use x::Real

  script <- paste0(
    "# Load packages\n",
    # "using DifferentialEquations#: ODEProblem, solve, Euler, RK4, Tsit5\n",
    # "using SciMLBase.EnsembleAnalysis\n",
    "using CSV\n",
    "using DataFrames\n", # : DataFrame, select, innerjoin, rename!\n",
    "using DataInterpolations\n",
    "using DiffEqCallbacks\n", # : SavingCallback, SavedValues\n",
    "using Distributions\n",
    "using OrdinaryDiffEq\n",
    "using Random\n",
    "using Statistics\n",
    "using StatsBase\n",
    "using Unitful\n",
    "using ", P[["jl_pkg_name"]], "\n",
    "using ", P[["jl_pkg_name"]], ".",
    P[["sdbuildR_units"]], "\n",
    # "Unitful.register(", P[["jl_pkg_name"]], ".", P[["sdbuildR_units"]], ")\n",

    # # Required when extending a module’s function
    # #import Base: <, >, <=, >=, ==, != #, +, - #, *, /, ^
    #
    # Extend base methods (multiple dispatch) to allow for comparison between a unit and a non-unit; if one of the arguments is a Unitful.Quantity, convert the other to the same unit.
    "Base.:<(x::Unitful.Quantity, y::Float64) = <(x, y * Unitful.unit(x))
Base.:<(x::Float64, y::Unitful.Quantity) = <(x * Unitful.unit(y), y)

Base.:>(x::Unitful.Quantity, y::Float64) = >(x, y * Unitful.unit(x))
Base.:>(x::Float64, y::Unitful.Quantity) = >(x * Unitful.unit(y), y)

Base.:(<=)(x::Unitful.Quantity, y::Float64) = <=(x, y * Unitful.unit(x))
Base.:(<=)(x::Float64, y::Unitful.Quantity) = <=(x * Unitful.unit(y), y)

Base.:(>=)(x::Unitful.Quantity, y::Float64) = >=(x, y * Unitful.unit(x))
Base.:(>=)(x::Float64, y::Unitful.Quantity) = >=(x * Unitful.unit(y), y)

Base.:(==)(x::Unitful.Quantity, y::Float64) = ==(x, y * Unitful.unit(x))
Base.:(==)(x::Float64, y::Unitful.Quantity) = ==(x * Unitful.unit(y), y)

Base.:(!=)(x::Unitful.Quantity, y::Float64) = !=(x, y * Unitful.unit(x))
Base.:(!=)(x::Float64, y::Unitful.Quantity) = !=(x * Unitful.unit(y), y)

Base.:%(x::Unitful.Quantity, y::Float64) = %(x, y * Unitful.unit(x))
Base.:%(x::Float64, y::Unitful.Quantity) = %(x * Unitful.unit(y), y)

Base.mod(x::Unitful.Quantity, y::Float64) = mod(x, y * Unitful.unit(x))
Base.mod(x::Float64, y::Unitful.Quantity) = mod(x * Unitful.unit(y), y)

Base.rem(x::Unitful.Quantity, y::Float64) = rem(x, y * Unitful.unit(x))
Base.rem(x::Float64, y::Unitful.Quantity) = rem(x * Unitful.unit(y), y)

Base.min(x::Unitful.Quantity, y::Float64) = min(x, y * Unitful.unit(x))
Base.min(x::Float64, y::Unitful.Quantity) = min(x * Unitful.unit(y), y)

Base.max(x::Unitful.Quantity, y::Float64) = max(x, y * Unitful.unit(x))
Base.max(x::Float64, y::Unitful.Quantity) = max(x * Unitful.unit(y), y)

# Extend min/max: when applied to a single vector, use minimum, like in R
Base.min(v::AbstractVector) = minimum(v)
Base.max(v::AbstractVector) = maximum(v)

Base.floor(x::Unitful.Quantity) = floor(Unitful.ustrip.(x)) * Unitful.unit(x)
Base.ceil(x::Unitful.Quantity) = ceil(Unitful.ustrip.(x)) * Unitful.unit(x)
Base.trunc(x::Unitful.Quantity) = trunc(Unitful.ustrip.(x)) * Unitful.unit(x)\n",

    # Add initialization of sdbuildR
    paste0("\n", P[["init_sdbuildR"]], " = true"),
    collapse = "\n"
  )

  # Write script
  env_path <- system.file(package = "sdbuildR")
  filepath <- file.path(env_path, "init.jl")
  write_script(script, filepath)

  invisible()
}
