# #' Check Julia setup
# #'
# #' @returns Logical value
# #' @noRd
# julia_setup_ok <- function() {
#   JuliaConnectoR::juliaSetupOk() &&
#     isTRUE(.sdbuildR_env[["jl"]][["init"]]) # &&
#   # !is.null(.sdbuildR_env[["JULIA_BINDIR"]])
# }

is_julia_ready <- function(){
  if (Sys.getenv("NOT_CRAN") != "true"){
    return(FALSE)
  }

  tryCatch({
    suppressWarnings({
      is_julia_ok() && is_julia_env_installed()
    })
  }, error = function(e) {
    return(FALSE)
  })
}


#' Check Julia environment was initialized
#'
#' This should only be run if a Julia session was already initialized with JuliaConnectoR.
#'
#' @returns Logical value
#' @noRd
is_julia_init <- function() {
  tryCatch({
    vals <- JuliaConnectoR::juliaEval('
      [
        string(isdefined(Main, :SystemDynamicsBuildR)),
        string(isdefined(Main, :init_sdbuildR))
      ]
    ')
    isTRUE(vals[1] == "true") && isTRUE(vals[2] == "true")    
  }, error = function(e) {
    FALSE
  })
}



#' Check Julia environment was initialized
#'
#' This should only be run if a Julia session was already initialized with JuliaConnectoR.
#'
#' @returns Logical value
#' @noRd
# is_julia_init <- function() {
#   # Check if Unitful was loaded (as an example package that needs to be loaded)
#   julia_cmd <- "isdefined(Main, :Unitful)"
#   unitful_loaded <- JuliaConnectoR::juliaEval(julia_cmd)

#   # Get project name
#   julia_cmd <- "using Pkg; Pkg.Types.read_project(Pkg.project().path).name"
#   project_name <- JuliaConnectoR::juliaEval(julia_cmd)

#   # Check init variable exists
#   julia_cmd <- paste0(
#     "isdefined(Main, :",
#     P[["init_sdbuildR"]], ")"
#   )
#   init_sdbuildR <- JuliaConnectoR::juliaEval(julia_cmd)

#   isTRUE(unitful_loaded) && 
#   project_name == "sdbuildR" && 
#   isTRUE(init_sdbuildR)
# }


is_julia_ok <- function(){
  x <- JuliaConnectoR::juliaEval("0")
  
  # Julia version needs to be correct
  is_julia_version_ok()

  invisible(TRUE)
}

is_julia_version_ok <- function(){
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


is_julia_env_installed <- function(){

  # Manifest.toml should exist in sdbuildR package to indicate Julia environment was instantiated
  # Check sdbuildR environment files
  env_path <- system.file(package = "sdbuildR")
  project_file <- file.path(env_path, "Project.toml")
  manifest_file <- file.path(env_path, "Manifest.toml")

  env_exists <- file.exists(project_file)
  env_instantiated <- file.exists(manifest_file)

  if (!env_exists) {
    cli::cli_abort("sdbuildR {.file Project.toml} not found. Try reinstalling {.pkg sdbuildR}.")
  }

  if (!env_instantiated) {
    cli::cli_abort("Julia environment not instantiated. Run {.fn install_julia_env}.")
  }

  # SystemDynamicsBuildR.jl needs to be installed and up to date
  required_pkg_version <- P[["jl_pkg_version"]]
  installed_pkg_version <- find_jl_pkg_version(P[["jl_pkg_name"]])

  if (package_version(installed_pkg_version) < package_version(required_pkg_version)) {
    JuliaConnectoR::stopJulia()
    cli::cli_abort(c(
      "Julia packages need updating.",
      ">" = "Run {.fn install_julia_env}."
    ))
  }

  invisible(TRUE)
}


#' Check status of Julia installation and environment
#'
#' Check if Julia can be found and if the Julia environment for sdbuildR has been instantiated. Note that this does not mean a Julia session has been started, merely whether it *could* be. For more guidance, see [this vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html).
#'
#'
#' @param verbose If `TRUE`, print detailed status information. Defaults to `TRUE`.
#'
#' @returns A list with components:
#'   \item{julia_found}{Logical. TRUE if Julia installation found.}
#'   \item{julia_version}{Character. Julia version string, or "" if not found.}
#'   \item{env_exists}{Logical. TRUE if Project.toml exists in sdbuildR package, which specifies the Julia packages and versions needed to instantiate the Julia environment for sdbuildR.}
#'   \item{env_instantiated}{Logical. TRUE if Manifest.toml exists (i.e., Julia environment was instantiated).}
#'   \item{status}{Character. Overall status: "julia_not_installed", "julia_needs_update", "sdbuildR_needs_reinstall", "install_julia_env", "ready", or "unknown".}
#'
#' @section What to Do Next:
#' Based on the 'status' value:
#' \describe{
#'   \item{"julia_not_installed"}{Install Julia from [https://julialang.org/install/](https://julialang.org/install/)}
#'   \item{"julia_needs_update"}{Update Julia to >= version 1.10}
#'   \item{"install_julia_env"}{Run \code{install_julia_env()}}
#'   \item{"ready"}{Run \code{use_julia()} to start a session}
#' }
#'
#' @export
#' @concept julia
#' @examples
#' status <- julia_status()
#' print(status)
#'
# julia_status <- function(verbose = TRUE) {
#   result <- list(
#     julia_found = FALSE,
#     julia_version = "",
#     env_exists = FALSE,
#     env_instantiated = FALSE,
#     status = "unknown"
#   )

#   # Find Julia installation
#   julia_version <- tryCatch(
#     {
#       getJuliaVersionViaCmd(getJuliaExecutablePath())
#     },
#     error = function(e) {
#       return(list(version = NULL))
#     }
#   )
#   result[["julia_found"]] <- !is.null(julia_version) && nzchar(julia_version)

#   if (!result[["julia_found"]]) {
#     result$status <- "julia_not_installed"
#     if (verbose) {
#       cli::cli_inform("Julia not found. Install from {.url https://julialang.org/install/}.")
#     }
#     return(result)
#   }

#   result[["julia_version"]] <- julia_version

#   # Required Julia version for sdbuildR
#   required_jl_version <- P[["jl_required_version"]]

#   # Check if version is sufficient
#   if (package_version(result[["julia_version"]]) < package_version(required_jl_version)) {
#     result$status <- "julia_needs_update"
#     if (verbose) {
#       cli::cli_inform(c(
#         "Julia version {.val {result[[\"julia_version\"]]}} is too old.",
#         "i" = "Requires version {.val {required_jl_version}} or higher.",
#         ">" = "Update at {.url https://julialang.org/install/}."
#       ))
#     }
#     return(result)
#   }

#   # Check sdbuildR environment files
#   env_path <- system.file(package = "sdbuildR")
#   project_file <- file.path(env_path, "Project.toml")
#   manifest_file <- file.path(env_path, "Manifest.toml")

#   result$env_exists <- file.exists(project_file)
#   result$env_instantiated <- file.exists(manifest_file)

#   if (!result$env_exists) {
#     result$status <- "sdbuildR_needs_reinstall"
#     if (verbose) {
#       cli::cli_inform("sdbuildR {.file Project.toml} not found. Try reinstalling {.pkg sdbuildR}.")
#     }
#     return(result)
#   }

#   if (!result$env_instantiated) {
#     result$status <- "install_julia_env"
#     if (verbose) {
#       cli::cli_inform("Julia environment not instantiated. Run {.fn install_julia_env}.")
#     }
#     return(result)
#   }

#   # Everything looks good
#   result$status <- "ready"

#   if (verbose) {
#     cli::cli_inform("Julia environment ready.")
#   }

#   result
# }


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
      return(result)
    }
  )

  result
}


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

  # # Start Julia
  # suppressWarnings({ # There is already a connection to Julia established
  #   JuliaConnectoR::startJuliaServer()
  # })

  if (remove) {
    # Find set-up location for sdbuildR in Julia
    env_path <- system.file(package = "sdbuildR")

    # Activate the Julia environment for sdbuildR; juliaEval() automatically starts Julia
    julia_cmd <- sprintf("using Pkg; Pkg.activate(\"%s\")", env_path)
    JuliaConnectoR::juliaEval(julia_cmd)

    # Delete SystemDynamicsBuildR.jl
    JuliaConnectoR::juliaEval(sprintf(
      'Pkg.rm("%s")',
      P[["jl_pkg_name"]]
    ))

    manifest_file <- system.file("Manifest.toml", package = "sdbuildR")
    if (file.exists(manifest_file)) {
      file.remove(manifest_file)
    }
    JuliaConnectoR::juliaEval("Pkg.gc()")
    status <- is_julia_env_installed()

    if (isTRUE(status)) {
      cli::cli_warn("Failed to remove Julia environment.")
    } else {
      cli::cli_inform("Julia environment removed.")
    }
  } else {
    # Run the setup script
    setup_script <- system.file("setup.jl", package = "sdbuildR")
    JuliaConnectoR::juliaEval(sprintf(
      'jl_pkg_version = \"v%s\";',
      P[["jl_pkg_version"]]
    ))
    JuliaConnectoR::juliaEval(sprintf('include("%s")', setup_script))
    status <- is_julia_env_installed()

    if (isTRUE(status)) {
      cli::cli_inform("Julia environment installed.")
    } else {
      cli::cli_warn("Failed to install Julia environment.")
    }
  }

  # Stop Julia
  JuliaConnectoR::stopJulia()

  invisible()
}


#' Start Julia and activate environment
#'
#' Start Julia session and activate Julia environment to simulate stock-and-flow models. To do so, Julia needs to be installed and findable from within R. See [this vignette](https://kcevers.github.io/sdbuildR/articles/julia-setup.html) for guidance. In addition, the Julia environment specifically for sdbuildR needs to have been instantiated. This can be set up with `install_julia_env()`.
#'
#' Julia supports running stock-and-flow models with units as well as ensemble simulations (see `ensemble()`).
#'
#' In every R session, `use_julia()` needs to be run once (which is done automatically in `simulate()`), which can take around 30-60 seconds.
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
#' # Stop Julia session
#' use_julia(stop = TRUE)
#'
use_julia <- function(
  stop = FALSE,
  force = FALSE, 
  nthreads = NULL
) {
  if (stop) {
    # # Check whether a session is active
    # if (!julia_setup_ok()) {
    #   cli::cli_inform("No active Julia session")
    # } else {
    #   .sdbuildR_env[["jl"]][["init"]] <- FALSE
      JuliaConnectoR::stopJulia()

      cli::cli_inform("Julia session closed")
    # }
    return(invisible())
  }

  # If use_julia() was already run, no need to do anything, unless force or nthreads is specified
  
  if (!is.null(nthreads)) {
    if (!is.numeric(nthreads) || length(nthreads) != 1 || nthreads < 1) {
      cli::cli_abort("nthreads must be a single positive integer.")
    }
    nthreads <- as.integer(nthreads)
    Sys.setenv(JULIA_NUM_THREADS = nthreads)
  }

  status <- is_julia_init()
  if (!force && is.null(nthreads) && status) {
    return(invisible())
  }

  # If nthreads was set and Julia was already initialized, need to restart Julia to apply new thread setting
  if (!is.null(nthreads) && status) {
    JuliaConnectoR::stopJulia()
  }

 
  if (!status){
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

  # status <- julia_status(verbose = FALSE)

  # if (status[["status"]] != "ready") {
  #   status <- julia_status()
  #   cli::cli_abort()
  # }

  # if (!JuliaConnectoR::juliaSetupOk()) {
  #   cli::cli_abort("JuliaConnectoR setup failed")
  # }

  # tryCatch(
  #   {
  #     JuliaConnectoR::startJuliaServer()

  #     # Run initialization
  #     init_julia_env()
  #   },
  #   warning = function(w) {
  #     if (grepl(
  #       "There is already a connection to Julia established",
  #       conditionMessage(w)
  #     )) {
  #       # Ensure Julia environment was correctly initialized for sdbuildR
  #       if (!is_julia_init()) {
  #         use_julia(stop = TRUE)
  #         JuliaConnectoR::startJuliaServer()

  #         # Run initialization
  #         run_init_julia_env()
  #       }
  #     }
  #   }
  # )

  # # Try one more time
  # if (!is_julia_init()) {
  #   use_julia(stop = TRUE)
  #   JuliaConnectoR::startJuliaServer()

  #   # Run initialization
  #   run_init_julia_env()
  # }

  # if (!is_julia_init()) {
  #   cli::cli_abort("Julia environment setup failed.")
  # }

  # # Set global option of initialization
  # .sdbuildR_env[["jl"]][["init"]] <- TRUE

  # invisible()
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
  julia_cmd <- sprintf("using Pkg; Pkg.activate(\"%s\")", env_path)
  JuliaConnectoR::juliaEval(julia_cmd)

  # Install all dependencies from Project.toml
  JuliaConnectoR::juliaEval("Pkg.instantiate()")
  JuliaConnectoR::juliaEval("Pkg.resolve()")

  # Source the init.jl script
  init_file <- system.file("init.jl", package = "sdbuildR")
  julia_cmd <- sprintf("include(\"%s\")", init_file)
  JuliaConnectoR::juliaEval(julia_cmd)

  invisible(NULL)
}


# getJuliaExecutablePath <- function() {
#   juliaBindir <- Sys.getenv("JULIA_BINDIR")
#   if (juliaBindir == "") {
#     if (Sys.which("julia") == "") {
#       juliaCmd <- fallbackOnDefaultJuliaupPath()
#     } else { # Julia is on the PATH, simply use the command "julia"
#       juliaCmd <- "julia"
#     }
#   } else { # use the JULIA_BINDIR variable, as it is specified
#     juliaExe <- list.files(path = juliaBindir, pattern = "^julia.*")
#     if (length(juliaExe) == 0) {
#       cli::cli_abort(paste0(
#         "No Julia executable file found in supposed bin directory \"",
#         juliaBindir, "\""
#       ))
#     }
#     juliaCmd <- file.path(juliaBindir, "julia")
#   }
#   return(juliaCmd)
# }

# fallbackOnDefaultJuliaupPath <- function() {
#   # If Julia is not found on the PATH, check the default Juliaup installation location
#   # on Linux and Mac and use the Julia command there if it exists.
#   # (On Mac, Julia might not be on the PATH in the R session even though
#   # Julia has been installed in the default way via Juliaup.)
#   juliaCmd <- file.path(Sys.getenv("HOME"), ".juliaup", "bin", "julia")
#   if (!file.exists(juliaCmd) || Sys.info()["sysname"] == "Windows") {
#     cli::cli_abort(c(
#       "Julia could not be found.",
#       "x" = "Julia needs to be installed and findable by {.pkg JuliaConnectoR}.",
#       "i" = "After installing, add the Julia executable to the {.envvar PATH} environment variable.",
#       ">" = "See {.help Julia-Setup} for more information."
#     ), call = NULL)
#   } else {
#     return(juliaCmd)
#   }
# }


# getJuliaVersionViaCmd <- function(juliaCmd = getJuliaExecutablePath()) {
#   juliaVersion <- NULL
#   try({
#     juliaVersion <- system2(juliaCmd, "--version",
#       stdout = TRUE
#       # env = getJuliaEnv()
#     )
#     juliaVersion <- regmatches(
#       juliaVersion,
#       regexpr(
#         "[0-9]+\\.[0-9]+\\.[0-9]+",
#         juliaVersion
#       )
#     )
#   })
#   juliaVersion
# }


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
