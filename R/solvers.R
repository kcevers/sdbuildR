#' Check or translate between deSolve and Julia DifferentialEquations solvers
#'
#' @param method Solver name to validate or translate.
#' @param from Source solver family, either `"R"` or `"Julia"`.
#' @param to Target solver family when translating, either `"R"` or `"Julia"`.
#' @param show_info If `TRUE`, return the full solver metadata instead of only the name.
#'
#' @return A character scalar, a character vector of solver names, or a solver metadata list when `show_info = TRUE`.
#' @export
#' @concept simulate
solvers <- function(method, from = NULL, to = NULL, show_info = FALSE) {
  valid_source <- c("R", "Julia")

  valid_scalar <- function(x) is.character(x) && length(x) == 1
  missing_input <- function(x) is.null(x) || (length(x) == 1 && is.na(x))

  if (nargs() == 0) {
    stop("Missing required arguments")
  }

  if (missing(method) || missing_input(method)) {
    if (missing(method) || is.null(method) || (length(method) == 1 && is.na(method))) {
      if (missing(from) && missing(to)) {
        stop("Missing required arguments")
      }
    }
  } else if (!valid_scalar(method)) {
    stop("Invalid `method` argument")
  }

  if (missing(from) || missing_input(from)) {
    if (!missing(from) && is.null(from)) {
      stop("Missing required arguments")
    }
  } else if (!valid_scalar(from)) {
    stop("Invalid `from` argument")
  } else if (!from %in% valid_source) {
    stop("Invalid `from` argument")
  }

  if (!missing(to)) {
    if (missing_input(to)) {
      stop("Missing required arguments")
    }
    if (!valid_scalar(to)) {
      stop("Invalid `to` argument")
    }
    if (!to %in% valid_source) {
      stop("Invalid `to` argument")
    }
  }

  translate <- !missing(to) && !is.null(to)

  solver_dict <- list(
    r_to_julia = list(
      euler = list(translation = "Euler()", alternatives = NULL, approximate = FALSE),
      rk2 = list(translation = "Midpoint()", alternatives = c("Heun()"), approximate = FALSE),
      rk4 = list(translation = "RK4()", alternatives = NULL, approximate = FALSE),
      rk23bs = list(translation = "BS3()", alternatives = c("ode23"), approximate = FALSE),
      ode23 = list(translation = "BS3()", alternatives = c("rk23bs"), approximate = FALSE),
      rk45dp6 = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      rk45dp7 = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      rk45e = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      rk45f = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      rk45ck = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      rk78dp = list(translation = "Vern8()", alternatives = c("Vern7()", "Vern9()"), approximate = TRUE),
      rk78f = list(translation = "Vern8()", alternatives = c("Vern7()", "Vern9()"), approximate = TRUE),
      ode45 = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      irk3r = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      irk5r = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      irk4hh = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      irk4l = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      irk6kb = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      irk6l = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      lsoda = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      lsodar = list(translation = "Tsit5()", alternatives = NULL, approximate = TRUE),
      lsode = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      lsodes = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      bdf = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      bdf_d = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      vode = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      daspk = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      adams = list(translation = "Vern6()", alternatives = c("Vern7()", "Vern8()"), approximate = TRUE),
      impAdams = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      impAdams_d = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE),
      radau = list(translation = "Rosenbrock23()", alternatives = NULL, approximate = TRUE)
    ),
    julia_to_r = list(
      "Euler()" = list(translation = "euler", alternatives = NULL),
      "ForwardEuler()" = list(translation = "euler", alternatives = NULL),
      "Midpoint()" = list(translation = "rk2", alternatives = NULL),
      "Heun()" = list(translation = "rk2", alternatives = NULL),
      "RK4()" = list(translation = "rk4", alternatives = NULL),
      "BS3()" = list(translation = "rk23bs", alternatives = c("ode23")),
      "Tsit5()" = list(translation = "rk45dp6", alternatives = c("rk45dp6", "rk45dp7", "rk45e", "rk45f", "rk45ck", "ode45")),
      "Vern6()" = list(translation = NULL, alternatives = c("adams")),
      "Vern7()" = list(translation = NULL, alternatives = c("rk78dp", "rk78f", "adams")),
      "Vern8()" = list(translation = NULL, alternatives = c("rk78dp", "rk78f")),
      "Vern9()" = list(translation = NULL, alternatives = c("rk78dp", "rk78f")),
      "Rosenbrock23()" = list(translation = NULL, alternatives = c("irk3r", "irk5r", "irk4hh", "irk4l", "irk6kb", "irk6l", "lsoda", "lsodar", "lsode", "lsodes", "bdf", "bdf_d", "vode", "daspk", "impAdams", "impAdams_d", "radau"))
    )
  )

  deSolve_methods <- c(
    names(solver_dict$r_to_julia),
    deSolve::rkMethod(),
    "lsoda", "lsode", "lsodes", "lsodar", "vode", "daspk",
    "euler", "rk4", "ode23", "ode45", "radau", "bdf", "bdf_d",
    "adams", "impAdams", "impAdams_d", "iteration"
  )

  normalize_julia_name <- function(x) {
    if (x %in% names(solver_dict$julia_to_r)) {
      return(x)
    }
    variants <- c(
      paste0(x, "()"),
      paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))),
      paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), "()"),
      tolower(x)
    )
    for (variant in variants) {
      if (variant %in% names(solver_dict$julia_to_r)) {
        return(variant)
      }
    }
    NULL
  }

  if (missing(method)) {
    if (missing(from) || is.null(from)) {
      stop("Missing required arguments")
    }
    if (from == "R") {
      return(names(solver_dict$r_to_julia))
    }
    if (from == "Julia") {
      return(names(solver_dict$julia_to_r))
    }
    stop("Invalid `from` argument")
  }

  if (!translate) {
    if (from == "R") {
      if (!method %in% names(solver_dict$r_to_julia)) {
        if (method %in% deSolve_methods) {
          stop("Unsupported solver method")
        }
        stop("Unknown solver method")
      }
      solver_info <- solver_dict$r_to_julia[[method]]
      solver_info$name <- method
      if (show_info) {
        return(solver_info)
      }
      return(solver_info$name)
    }

    if (from == "Julia") {
      method_clean <- normalize_julia_name(method)
      if (is.null(method_clean)) {
        stop("Unknown Julia solver method.")
      }
      solver_info <- solver_dict$julia_to_r[[method_clean]]
      solver_info$name <- method_clean
      if (show_info) {
        return(solver_info)
      }
      return(solver_info$name)
    }

    stop("Invalid `from` argument")
  }

  if (from == "R" && to == "Julia") {
    if (!method %in% names(solver_dict$r_to_julia)) {
      if (method %in% deSolve_methods) {
        stop("Unsupported solver method")
      }
      stop("Unknown solver method")
    }
    solver_info <- solver_dict$r_to_julia[[method]]
    if (isTRUE(solver_info$approximate)) {
      cli::cli_warn(c(
        "!" = "No exact Julia equivalent for {.val {method}}.",
        "i" = "Using {.val {solver_info$translation}} as the closest supported Julia solver."
      ))
    }
    if (show_info) {
      return(solver_info)
    }
    return(solver_info$translation)
  }

  if (from == "Julia" && to == "R") {
    method_clean <- normalize_julia_name(method)
    if (is.null(method_clean)) {
      stop("Solver not found.")
    }
    solver_info <- solver_dict$julia_to_r[[method_clean]]
    if (show_info) {
      return(solver_info)
    }
    if (!is.null(solver_info$translation)) {
      return(solver_info$translation)
    }
    return(method_clean)
  }

  stop("Unsupported translation request")
}
