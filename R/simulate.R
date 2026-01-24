#' Simulate stock-and-flow model
#'
#' Simulate a stock-and-flow model with simulation specifications defined by [sim_specs()]. If `sim_specs(language = "julia")`, the Julia environment will first be set up with [use_julia()]. If any problems are detected by [debugger()], the model cannot be simulated.
#'
#' @inheritParams insightmaker_to_sfm
#' @inheritParams build
#' @param verbose If TRUE, print duration of simulation. Defaults to FALSE.
#' @param only_stocks If TRUE, only return stocks in output, discarding flows and auxiliaries. If FALSE, flows and auxiliaries are saved, which slows down the simulation. Defaults to FALSE.
#' @param ... Optional arguments
#'
#' @returns Object of class [`sdbuildR_sim`][simulate], a list containing:
#' \describe{
#'   \item{sfm}{Stock-and-flow model object of class [`sdbuildR_xmile`][xmile]}
#'   \item{df}{Data frame: simulation results (time, variable, value)}
#'   \item{init}{Named vector: initial stock values}
#'   \item{constants}{Named vector: constant parameters}
#'   \item{script}{Character: generated simulation code (R or Julia)}
#'   \item{duration}{Numeric: simulation time in seconds}
#'   \item{success}{Logical: TRUE if completed without errors}
#'   \item{error_message}{NULL if completed without errors}
#' }
#'
#' Use [as.data.frame()] to extract results, [plot()] to visualize.
#'
#'
#' @export
#' @concept simulate
#' @seealso [build()], [xmile()], [debugger()], [sim_specs()], [use_julia()]
#'
#' @examples
#' sfm <- xmile("SIR")
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # Obtain all model variables
#' sim <- simulate(sfm, only_stocks = FALSE)
#' plot(sim, add_constants = TRUE)
#'
#' @examplesIf julia_status()$status == "ready"
#' # Use Julia for models with units
#' sfm <- sim_specs(xmile("coffee_cup"), language = "Julia")
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # Close Julia session
#' use_julia(stop = TRUE)
#'
simulate <- function(sfm,
                     only_stocks = TRUE,
                     verbose = FALSE,
                     ...) {
  check_xmile(sfm)

  # First assess whether the model is valid
  problems <- debugger(sfm, quietly = TRUE)
  if (nzchar(problems[["problems"]])) {
    txt <- problems[["problems"]]
    cli::cli_abort(txt)
  }

  # Check model for delayN() and smoothN() functions
  delayN_smoothN <- get_delay(sfm, type = "delayN_smoothN")

  # Check model for delay() and past() functions
  delay_past <- get_delay(sfm, type = "past")

  if (length(delayN_smoothN) > 0) {
    txt <- "The model contains either delayN() or smoothN(), which are not supported."
    cli::cli_warn(c(
      "The model contains unsupported delay functions.",
      "!" = "Functions {.fn delayN()} and {.fn smoothN()} are not supported for R simulations.",
      "i" = "Use {.fn sim_specs}(sfm, language = {.code 'Julia'}) for full support.",
      ">" = "Or modify the equations of these variables: {paste0(names(delayN_smoothN), collapse = ', ')}"
    ))
    return(new_sdbuildR_sim(
      success = FALSE,
      error_message = txt,
      sfm = sfm
    ))
  }

  if (length(delay_past) > 0) {
    txt <- "The model contains either delay() or past(), which are not supported."
    cli::cli_warn(c(
      "The model contains unsupported history functions.",
      "!" = "Functions {.fn delay()} and {.fn past()} are not supported for R simulations.",
      "i" = "Use {.fn sim_specs}(sfm, language = {.code 'Julia'}) for full support.",
      ">" = "Or modify the equations of these variables: {paste0(names(delay_past), collapse = ', ')}"
    ))
    return(new_sdbuildR_sim(
      success = FALSE,
      error_message = txt,
      sfm = sfm
    ))
  }

  if (tolower(sfm[["sim_specs"]][["language"]]) == "julia") {
    return(simulate_julia(sfm,
      only_stocks = only_stocks,
      verbose = verbose
    ))
  } else if (tolower(sfm[["sim_specs"]][["language"]]) == "r") {
    # Check model for unit strings
    eqn_units <- find_unit_strings(sfm)

    # Stop if equations contain unit strings
    if (length(eqn_units) > 0) {
      txt <- paste0(
        "The model contains unit strings u(''), which are not supported for simulations in R.\nSet sim_specs(sfm, language = 'Julia') or modify the equations of these variables:\n\n",
        paste0(names(eqn_units), collapse = ", ")
      )
      cli::cli_warn(c(
        "The model contains unsupported unit strings.",
        "!" = "Function {.fn u()} with unit strings is not supported for R simulations.",
        "i" = "Use {.fn sim_specs}(sfm, language = {.code 'Julia'}) for unit support.",
        ">" = "Or modify the equations of these variables: {paste0(names(eqn_units), collapse = ', ')}"
      ))
      return(new_sdbuildR_sim(
        success = FALSE,
        error_message = txt,
        sfm = sfm
      ))
    }

    return(simulate_R(sfm,
      only_stocks = only_stocks,
      verbose = verbose
    ))
  } else {
    txt <- "Simulation language not supported.\nPlease run either sim_specs(sfm, language = 'Julia') (recommended) or sim_specs(sfm, language = 'R') (no unit or ensemble support)."
    cli::cli_warn(c(
      "Unsupported simulation language.",
      "!" = "Simulation language must be either {.code 'Julia'} or {.code 'R'}.",
      "i" = "Recommended: {.fn sim_specs}(sfm, language = {.code 'Julia'})",
      ">" = "Alternative: {.fn sim_specs}(sfm, language = {.code 'R'}) (limited features)"
    ))
    return(new_sdbuildR_sim(
      success = FALSE,
      error_message = txt,
      sfm = sfm
    ))
  }
}


#' Create new object of class [`sdbuildR_sim`][simulate]
#'
#' @returns A simulation of a stock-and-flow model of class [`sdbuildR_sim`][simulate]
#' @noRd
#'
new_sdbuildR_sim <- function(success = FALSE,
                             error_message = NULL,
                             sfm = NULL,
                             df = NULL,
                             init = NULL,
                             constants = NULL,
                             script = NULL,
                             duration = NULL,
                             ...) {
  obj <- list(
    success = success,
    error_message = error_message,
    sfm = sfm,
    df = df,
    init = init,
    constants = constants,
    script = script,
    duration = duration,
    ...
  )

  structure(obj, class = "sdbuildR_sim")
}


#' Validate class [`sdbuildR_sim`][simulate]
#'
#' @param x A simulation of a stock-and-flow model of class [`sdbuildR_sim`][simulate]
#'
#' @returns A simulation of a stock-and-flow model of class [`sdbuildR_sim`][simulate]
#' @noRd
#'
validate_sdbuildR_sim <- function(x) {
  if (!inherits(x, "sdbuildR_sim")) {
    cli::cli_abort(c(
      "Invalid object type.",
      "x" = "Expected object of class {.cls sdbuildR_sim}.",
      "i" = "Use {.fn simulate()} to create a valid simulation object."
    ))
  }

  if (!is.logical(x$success) || length(x$success) != 1) {
    cli::cli_abort(c(
      "Invalid {.arg success} field.",
      "x" = "The {.arg success} field must be a single {.cls logical} value.",
      "i" = "Expected {.val TRUE} or {.val FALSE}."
    ))
  }

  if (x$success) {
    # Successful simulation must have these components
    if (is.null(x$df) || !is.data.frame(x$df)) {
      cli::cli_abort(c(
        "Missing or invalid simulation data.",
        "x" = "Successful simulation must have a {.cls data.frame} in {.arg df}.",
        "i" = "This field is populated by {.fn simulate()} with results."
      ))
    }
    if (is.null(x$init)) {
      cli::cli_abort(c(
        "Missing initial stock values.",
        "x" = "Successful simulation must preserve initial values in {.arg init}.",
        "i" = "Initial values of all stocks should be recorded."
      ))
    }
    # if (is.null(x$constants) || !is.numeric(x$constants)) {
    #   cli::cli_abort("Successful simulation must have numeric `constants`")
    # }
    if (is.null(x$duration)) {
      cli::cli_abort(c(
        "Missing simulation duration.",
        "x" = "Successful simulation must have {.arg duration}."
      ))
    }
  } else {
    # Failed simulation should have error message
    if (is.null(x$error_message)) {
      cli::cli_warn(c(
        "Failed simulation missing error information.",
        "!" = "Field {.arg error_message} should be populated for failed simulations."
      ))
    }
  }

  x
}

#' Detect undefined variables in equations
#'
#' @inheritParams build
#'
#' @returns List with issue and message
#' @noRd
#'
detect_undefined_var <- function(sfm) {
  # Get names
  var_names <- get_model_var(sfm)

  # Macros and graphical functions can be functions
  gf_names <- sfm[["variables"]][sfm[["variables"]][["type"]] == "gf", "name"]
  possible_func_in_model <- c(
    names(sfm[[P[["macro_name"]]]]),
    gf_names
  )

  possible_func <- c(
    possible_func_in_model,
    syntax_julia[["syntax_df"]][["R_first_iter"]],
    unlist(P),
    # Remove base R names
    "pi", "letters", "LETTERS",
    "month.abb", "month.name"
  )

  # Find references to variables which are not in names_df[["name"]]
  missing_ref <- split(sfm[["variables"]], seq_len(nrow(sfm[["variables"]]))) |>
    lapply(function(x) {
      y <- x[names(x) %in% c("eqn", "to", "from", "source")]
      y <- y[vapply(y, is_defined, logical(1))]

      A <- lapply(y, function(z) {
        dependencies <- find_dependencies_(sfm, z, only_var = TRUE, only_model_var = FALSE)

        # Find all undefined variables and functions
        setdiff(
          unlist(dependencies),
          # Cannot depend on itself
          c(possible_func, setdiff(var_names, x[["name"]]))
        )
      })
      A <- A[lengths(A) > 0]
      if (length(A) == 0) {
        return(NULL)
      } else {
        return(stats::setNames(list(A), x[["name"]]))
      }
    })

  missing_ref <- unlist(missing_ref, recursive = FALSE)

  if (length(missing_ref) > 0) {
    missing_ref_format <- lapply(seq_along(missing_ref), function(i) {
      x <- missing_ref[[i]]
      name <- names(missing_ref)[i]
      lapply(seq_along(x), function(j) {
        y <- x[[j]]
        prop <- names(x)[j]
        # paste0("- ", name, "$", prop, ": ", paste0(unname(y), collapse = ", "))
        paste0(
          "- The variable", ifelse(length(y) > 1, "s ", " "),
          paste0(paste0("'", unname(y), "'"), collapse = ", "),
          ifelse(length(y) > 1, " are ", " is "), "referenced in ",
          name, "$", prop, " but hasn't been defined.\n"
        )
      })
    }) |>
      unlist() |>
      unname()

    return(list(
      issue = TRUE,
      msg = paste0(c(
        # "The properties below contain references to undefined variables.\nPlease define the missing variables or correct any spelling mistakes.",
        "Please define these missing variables or correct any spelling mistakes:",
        paste0(missing_ref_format, collapse = "\n")
      ), collapse = "\n")
    ))
  } else {
    return(list(issue = FALSE))
  }

  return(NULL)
}


#' Topologically sort equations according to their dependencies
#'
#' @param dependencies_dict Named list with dependencies for each equation; names are equation names and entries are dependencies
#'
#' @returns Equation names ordered according to their dependencies
#' @noRd
#'
topological_sort <- function(dependencies_dict) {
  if (length(dependencies_dict) == 0) {
    return(list(issue = FALSE, msg = "", order = c()))
  }

  # Get equation names and dependencies
  eq_names <- names(dependencies_dict)
  dependencies <- unname(dependencies_dict)

  # Ensure all dependencies are in eq_names, otherwise these result in NAs
  dependencies <- lapply(dependencies, function(x) {
    new_dependencies <- intersect(x, eq_names)
    if (length(new_dependencies) == 0) {
      return("")
    } else {
      return(new_dependencies)
    }
  })

  # Order parameters according to dependencies
  edges <- lapply(seq_along(dependencies), function(i) {
    # If no dependencies, repeat own name
    if (all(dependencies[[i]] == "")) {
      edge <- rep(eq_names[i], 2)
    } else {
      edge <- cbind(dependencies[[i]], rep(eq_names[i], length(dependencies[[i]])))
    }
    return(edge)
  }) |>
    do.call(rbind, args = _) |>
    set_rownames(NULL) |>
    # Turn into vector by row
    as.data.frame()
  edges <- edges[!duplicated(edges), ] # Remove duplicates
  edges <- c(t(as.matrix(edges)))

  # Create a directed graph from the edges
  g <- igraph::make_graph(edges, directed = TRUE)

  # Get correct order using topological sort
  out <- tryCatch(
    {
      list(order = igraph::topo_sort(g, mode = "out") |> names(), issue = FALSE, msg = "")
    },
    error = function(msg) {
      # cli::cli_inform("Something went wrong when attempting to order the equations in your ODE, which may be because of circular definition (e.g. x = y, y = x). The correct order is important as e.g. for x = 1/a, a needs to be defined before x. Please check the order manually.")
      out <- circularity(g)

      list(order = eq_names, issue = out[["issue"]], msg = out[["msg"]])
    }
  )

  return(out)
}


#' Detect circular dependencies in equations
#'
#' @param g Graph object
#'
#' @returns List with issue and message
#' @noRd
#'
circularity <- function(g) {
  # Check for cycles by finding strongly connected components
  scc <- igraph::components(g, mode = "strong")
  if (any(scc[["csize"]] > 1)) {
    # Identify vertices in cycles (strongly connected components with more than one node)
    cycle_nodes <- names(scc[["membership"]])[scc[["membership"]] %in% which(scc[["csize"]] > 1)]
    cycle_message <- paste(
      "Circular dependencies detected involving variables:",
      paste(cycle_nodes, collapse = ", ")
    )

    # Find the specific edges in the cycles
    sub_g <- igraph::induced_subgraph(g, cycle_nodes)
    cycle_edges <- igraph::as_edgelist(sub_g)
    edge_message <- paste0(paste0(
      "- ", cycle_edges[, 1], " depends on ",
      cycle_edges[, 2]
    ), collapse = "\n")

    msg <- paste0(c(cycle_message, edge_message), collapse = "\n")
    return(list(issue = TRUE, msg = msg))
  } else {
    return(list(issue = FALSE, msg = ""))
  }
}


#' Find newly defined variables in equation
#'
#' @param eqn Equation
#'
#' @returns Vector of newly defined variables
#' @noRd
find_newly_defined_var <- function(eqn) {
  # For each =, find preceding \n and next =
  newlines <- unique(c(
    1, stringr::str_locate_all(eqn, "\\n")[[1]][, "start"],
    nchar(eqn)
  ))
  assignment <- stringr::str_locate_all(eqn, "=")[[1]]

  # Exclude <- & \n in comments and strings
  seq_quot <- get_seq_exclude(eqn, var_names = NULL, type = "quot")

  assignment <- assignment[!(assignment[, "start"] %in% seq_quot), , drop = FALSE]
  newlines <- newlines[!(newlines %in% seq_quot)]

  new_var <- c()
  if (nrow(assignment) > 0 && length(newlines) > 0) {
    # Find preceding newline before assignment
    start_idxs <- vapply(assignment[, "start"], function(idx) {
      idxs_newline <- which(newlines <= idx)
      newlines[idxs_newline[length(idxs_newline)]] # select last newline before assignment
    }, numeric(1))

    # Isolate defined variables
    new_var <- lapply(seq_len(nrow(assignment)), function(i) {
      # Extract equation indices
      trimws(stringr::str_sub(eqn, start_idxs[i], assignment[i, "start"] - 1))
    })
    new_var <- unlist(new_var)
  }

  return(new_var)
}


#' Find dependencies
#'
#' Find which other variables each variable is dependent on.
#'
#' @inheritParams build
#' @param reverse If FALSE, list for each variable X which variables Y it depends on for its equation definition. If TRUE, don't show dependencies but dependents. This reverses the dependencies, such that for each variable X, it lists what other variables Y depend on X.
#'
#' @returns List, with for each model variable what other variables it depends on, or if \code{reverse = TRUE}, which variables depend on it
#' @concept build
#' @export
#'
#' @examples
#' sfm <- xmile("SIR")
#' find_dependencies(sfm)
#'
find_dependencies <- function(sfm, reverse = FALSE) {
  dep <- find_dependencies_(sfm, eqns = NULL, only_var = TRUE, only_model_var = TRUE)

  if (reverse) {
    dep <- reverse_dep(dep)
  }

  return(dep)
}


#' Reverse dependencies
#'
#' @param dep List of dependencies
#'
#' @returns List with reversed dependencies, showing dependents instead.
#' @noRd
reverse_dep <- function(dep) {
  reverse_dep <- list()

  # Initialize empty lists for all variables that appear as dependencies
  all_dependencies <- unique(unlist(dep))
  for (var in all_dependencies) {
    reverse_dep[[var]] <- character(0)
  }

  # Also initialize for variables that have dependencies (they might not be dependencies themselves)
  for (var in names(dep)) {
    if (!var %in% names(reverse_dep)) {
      reverse_dep[[var]] <- character(0)
    }
  }

  # Build reverse mapping
  for (target_var in names(dep)) {
    source_vars <- dep[[target_var]]
    if (length(source_vars) > 0) {
      for (source_var in source_vars) {
        reverse_dep[[source_var]] <- c(reverse_dep[[source_var]], target_var)
      }
    }
  }

  # Remove duplicates (shouldn't happen but just in case)
  reverse_dep <- lapply(reverse_dep, unique)

  return(reverse_dep)
}


#' Find dependencies in equation (only for internal use)
#'
#' @param eqns String with equation to find dependencies in; defaults to NULL to find dependencies of all variables.
#' @inheritParams build
#' @param only_var If TRUE, only look for variable names, not functions.
#' @param only_model_var If TRUE, only look for dependencies on other model variables.
#'
#' @returns Vector of dependencies (variable names in equation)
#' @noRd
#'
find_dependencies_ <- function(sfm, eqns = NULL, only_var = TRUE, only_model_var = TRUE) {
  var_names <- unique(get_model_var(sfm))

  # Macros and graphical functions can be functions
  gf_names <- sfm[["variables"]][sfm[["variables"]][["type"]] == "gf", "name"]
  possible_func_in_model <- c(
    sfm[[P[["macro_name"]]]][["name"]],
    gf_names,
    var_names
  ) # Some aux are also functions, such as pulse/step/ramp/seasonal

  # If no equations are provided, use all equations in the model
  if (is.null(eqns)) {
    # Get equations from variables data frame
    eqn_idx <- sfm[["variables"]][["type"]] %in% c("stock", "flow", "aux", "constant")
    eqns <- sfm[["variables"]][eqn_idx, "eqn"]
    names(eqns) <- sfm[["variables"]][eqn_idx, "name"]

    # Add graphical function dependencies on source
    gf_idx <- sfm[["variables"]][["type"]] == "gf"
    if (any(gf_idx)) {
      gf_source <- sfm[["variables"]][gf_idx, "source"]
      gf_source <- gf_source[nzchar(gf_source)]
      gf_names <- sfm[["variables"]][gf_idx, "name"]
      gf_source <- stats::setNames(gf_source, gf_names[nzchar(gf_source)])
      eqns <- c(eqns, gf_source)
    }
  }

  # Find dependencies in each equation
  dependencies <- lapply(eqns, function(eqn) {
    d <- NA

    # Parse the line as an expression
    expr <- tryCatch(parse(text = eqn), error = function(e) NULL)

    # If parsing was successful, extract variable names from equations
    if (!is.null(expr)) {
      # Omit variables that are defined in the expression itself
      new_var <- find_newly_defined_var(eqn)

      # Get all dependencies
      all_d <- setdiff(all.names(expr, functions = TRUE, unique = TRUE), new_var)
      d <- setdiff(all.names(expr, functions = FALSE, unique = TRUE), new_var)
      d_func <- setdiff(all_d, d)

      if (only_model_var) {
        d <- c(d[d %in% var_names], d_func[d_func %in% possible_func_in_model])
      } else if (!only_var) {
        d <- all_d
      }
    }

    # Normalize: drop NA and duplicates
    if (length(d) == 1 && all(is.na(d))) {
      d <- character(0)
    } else {
      d <- unique(d)
    }

    return(d)
  })

  # Preserve names (if eqns were named); helps plotting dependencies
  if (!is.null(names(eqns))) {
    names(dependencies) <- names(eqns)
  }

  return(dependencies)
}


#' Order equations of static and dynamic part of stock-and-flow model
#'
#' @inheritParams build
#' @param print_msg If TRUE, print message if the ordering fails; defaults to TRUE.
#'
#' @returns List with order of static and dynamic variables
#' @noRd
#'
order_equations <- function(sfm, print_msg = TRUE) {
  # Add .outflow to detect delayed variables
  var_names <- unique(get_model_var(sfm))
  idx_delay <- grepl(paste0(
    P[["delayN_suffix"]], "[0-9]+$|",
    P[["smoothN_suffix"]], "[0-9]+$"
  ), var_names)
  delay_var <- var_names[idx_delay]
  delay_pattern <- paste0(
    var_names[idx_delay],
    stringr::str_escape(P[["outflow_suffix"]])
  )

  # Separate auxiliary variables into static parameters and dynamically updated auxiliaries
  dependencies <- split(sfm[["variables"]], seq_len(nrow(sfm[["variables"]]))) |>
    lapply(function(x) {
      if (is_defined(x[["eqn"]])) {
        d <- unlist(find_dependencies_(sfm, x[["eqn"]],
          only_var = TRUE, only_model_var = TRUE
        ))

        # For delay family variables, find .outflow in eqn_julia
        if (length(delay_var) > 0) {
          idx <- stringr::str_detect(x[["eqn_julia"]], delay_pattern)
          d <- c(d, delay_var[idx])
        }
      } else {
        d <- c()
      }

      return(list(
        name = x[["name"]],
        type = x[["type"]],
        deps = d
      ))
    })
  
  # Organize dependencies by type
  deps_by_name <- stats::setNames(lapply(dependencies, function(x) x$deps), sapply(dependencies, function(x) x$name))
  deps_by_type <- split(deps_by_name, sapply(dependencies, function(x) x$type))
  
  # Create type-specific dependency lists
  dependencies <- list(
    gf = if ("gf" %in% names(deps_by_type)) deps_by_type[["gf"]] else list(),
    constant = if ("constant" %in% names(deps_by_type)) deps_by_type[["constant"]] else list(),
    stock = if ("stock" %in% names(deps_by_type)) deps_by_type[["stock"]] else list(),
    aux = if ("aux" %in% names(deps_by_type)) deps_by_type[["aux"]] else list(),
    flow = if ("flow" %in% names(deps_by_type)) deps_by_type[["flow"]] else list()
  )

  # Try to sort static and dynamic equations together
  # in case a static variable depends on a dynamic variable
  dependencies_dict <- unlist(unname(c(
    dependencies[["gf"]],
    dependencies[["constant"]],
    dependencies[["stock"]],
    dependencies[["aux"]],
    dependencies[["flow"]]
  )), recursive = FALSE)
  static_and_dynamic <- topological_sort(dependencies_dict)

  # Topological sort of static equations
  static_dependencies_dict <- c(
    dependencies[["gf"]],
    dependencies[["constant"]],
    dependencies[["stock"]]
  ) |>
    flatten()
  # purrr::list_flatten()

  if (static_and_dynamic[["issue"]]) {
    if (any(unname(static_dependencies_dict) %in% c(names(dependencies[["aux"]]), names(dependencies[["flow"]])))) {
      cli::cli_warn(c(
        "Could not resolve equation ordering.",
        "!" = "Topological sorting of all equations failed.",
        "i" = static_and_dynamic[["msg"]],
        ">" = "Check for circular dependencies in your model equations."
      ))
    }
  }

  static <- topological_sort(static_dependencies_dict)
  if (print_msg && static[["issue"]]) {
    cli::cli_warn(c(
      "Could not order static equations.",
      "!" = "Topological sorting of static equations failed.",
      "i" = static[["msg"]],
      ">" = "Check constant and stock definitions for circular dependencies."
    ))
  }


  # Topological ordering
  dependencies_dict <- c(
    dependencies[["aux"]],
    dependencies[["flow"]]
  ) |>
    flatten()
  # purrr::list_flatten()
  dynamic <- topological_sort(dependencies_dict)

  if (print_msg && dynamic[["issue"]]) {
    cli::cli_warn(c(
      "Could not order dynamic equations.",
      "!" = "Topological sorting of auxiliary and flow equations failed.",
      "i" = dynamic[["msg"]],
      ">" = "Check for circular dependencies in your auxiliary and flow equations."
    ))
  }

  return(list(
    static = static, dynamic = dynamic,
    static_and_dynamic = static_and_dynamic
  ))
}


#' Compare two simulations
#'
#' @param sim1 Simulation 1
#' @param sim2 Simulation 2
#' @param tolerance Numeric; tolerance for comparing values. Defaults to 0.00001.
#'
#' @returns List with comparison results
#' @noRd
#'
compare_sim <- function(sim1, sim2, tolerance = .00001) {
  if (sim1[["success"]] && !sim2[["success"]]) {
    return(c(
      equal = FALSE,
      msg = "Simulation 1 was successful, but simulation 2 failed."
    ))
  }

  if (!sim1[["success"]] && sim2[["success"]]) {
    return(c(
      equal = FALSE,
      msg = "Simulation 2 was successful, but simulation 1 failed."
    ))
  }

  get_prop <- function(sim) {
    list(
      colnames = colnames(sim[[P[["sim_df_name"]]]]),
      var_names = unique(sim[[P[["sim_df_name"]]]][["variable"]]),
      nrow = nrow(sim[[P[["sim_df_name"]]]]),
      ncol = ncol(sim[[P[["sim_df_name"]]]]),
      n_pars = length(sim[[P[["parameter_name"]]]]),
      language = sim[["sfm"]][["sim_specs"]][["language"]],
      method = sim[["sfm"]][["sim_specs"]][["method"]]
    )
  }

  prop1 <- get_prop(sim1)
  prop2 <- get_prop(sim2)

  overlapping_var_names <- intersect(prop1[["var_names"]], prop2[["var_names"]])
  nonoverlapping_var_names <- setdiff(
    union(
      prop1[["var_names"]],
      prop2[["var_names"]]
    ),
    overlapping_var_names
  )

  check_diff <- function(col1, col2) {
    col1 <- as.numeric(col1)
    col2 <- as.numeric(col2)

    if (length(col1) != length(col2)) {
      return(c(
        equal = FALSE,
        msg = paste0(
          "Column lengths are not equal: ",
          length(col1), " (sim1) vs ", length(col2), " (sim2)"
        )
      ))
    }

    # Calculate Euclidean distance, ignoring NAs
    return(c(
      equal = all(abs(col1 - col2) < tolerance, na.rm = TRUE),
      first_diff = which(abs(col1 - col2) > tolerance)[1],
      nr_diff = sum(abs(col1 - col2) > tolerance, na.rm = TRUE),
      max_diff = max(abs(col1 - col2), na.rm = TRUE),
      sqrt_sum_diff = sqrt(sum((col1 - col2)^2, na.rm = TRUE))
    ))
  }

  df <- bind_rows_(
    lapply(
      overlapping_var_names,
      function(name) {
        c(
          name = name,
          check_diff(
            sim1[["df"]][sim1[["df"]][["variable"]] == name, "value"],
            sim2[["df"]][sim2[["df"]][["variable"]] == name, "value"]
          )
        )
      }
    )
  ) #|>
  # do.call(dplyr::bind_rows, args = _) |>
  # as.data.frame()

  return(list(
    equal = all(as.logical(as.numeric(df[["equal"]]))),
    overlapping_var_names = overlapping_var_names,
    nonoverlapping_var_names = nonoverlapping_var_names,
    msg = paste0(
      "The following columns are not equal:\n",
      paste0(df[["name"]], ": ", df[["first_diff"]], " (",
        df[["nr_diff"]], " differences, max diff: ", df[["max_diff"]],
        ")\n",
        collapse = ""
      ),
      "\n"
    ),
    prop1 = prop1,
    prop2 = prop2,
    df = df
  ))
}


#' Run ensemble simulations
#'
#' Run an ensemble simulation of a stock-and-flow model, varying initial conditions and/or parameters in the range specified in `range`. The ensemble can be run in parallel using multiple threads by first setting [use_threads()]. The results are returned as a data.frame with summary statistics and optionally individual simulations.
#'
#' To run large simulations, it is recommended to limit the output size by saving fewer values. To create a reproducible ensemble simulation, set a seed using [sim_specs()].
#'
#' If you do not see any variation within a condition of the ensemble (i.e. the confidence bands are virtually non-existent), there are likely no random elements in your model. Without these, there can be no variability in the model. Try specifying a random initial condition or adding randomness to other model elements.
#'
#' @inheritParams build
#' @inheritParams simulate
#' @param n Number of simulations to run in the ensemble. When range is specified, n defines the number of simulations to run per condition. If each condition only needs to be run once, set n = 1. Defaults to 10.
#' @param return_sims If TRUE, return the individual simulations in the ensemble. Set to FALSE to save memory. Defaults to FALSE.
#' @param range  A named list specifying parameter ranges for ensemble conditions. Names must correspond to existing stock or constant variable names in the model. Each list element should be a numeric vector of values to test.
#'
#' If cross = TRUE (default), all combinations of values are generated. For example, list(param1 = c(1, 2), param2 = c(10, 20)) creates 4 conditions: (1,10), (1,20), (2,10), (2,20).
#'
#' If cross = FALSE, values are paired element-wise, requiring all vectors to have equal length. For example, list(param1 = c(1, 2, 3), param2 = c(10, 20, 30)) creates 3 conditions: (1,10), (2,20), (3,30).
#' Defaults to NULL (no parameter variation).
#'
#' @param cross If TRUE, cross the parameters in the range list to generate all possible combinations of parameters. Defaults to TRUE.
#' @param quantiles Quantiles to calculate in the summary, e.g. c(0.025, 0.975).
#' @param verbose If TRUE, print details and duration of simulation. Defaults to TRUE.
#'
#' @returns Object of class [`sdbuildR_ensemble`][ensemble], which is a list containing:
#' \describe{
#'  \item{success}{If TRUE, simulation was successful. If FALSE, simulation failed.}
#'  \item{error_message}{If success is FALSE, contains the error message.}
#'  \item{df}{data.frame with simulation results in long format, if return_sims is TRUE. The iteration number is indicated by column "i". If range was specified, the condition is indicated by column "j".}
#'  \item{summary}{data.frame with summary statistics of the ensemble, including quantiles specified in quantiles. If range was specified, summary statistics are calculated for each condition (j) in the ensemble.}
#'  \item{n}{Number of simulations run in the ensemble (per condition j if range is specified).}
#'  \item{n_total}{Total number of simulations run in the ensemble (across all conditions if range is specified).}
#'  \item{n_conditions}{Total number of conditions.}
#'  \item{conditions}{data.frame with the conditions used in the ensemble, if range is specified.}
#'  \item{init}{List with df (if return_sims = TRUE) and summary, containing data.frame with the initial values of the stocks used in the ensemble.}
#'  \item{constants}{List with df (if return_sims = TRUE) and summary, containing data.frame with the constant parameters used in the ensemble.}
#'  \item{script}{Julia script used for the ensemble simulation.}
#'  \item{duration}{Duration of the simulation in seconds.}
#'  \item{...}{Other parameters passed to ensemble}
#'  }
#' @export
#' @concept simulate
#' @seealso [use_threads()], [build()], [xmile()], [sim_specs()], [use_julia()]
#'
#' @examplesIf julia_status()$status == "ready"
#' # Load example and set simulation language to Julia
#' sfm <- xmile("predator_prey") |> sim_specs(language = "Julia")
#'
#' # Set random initial conditions
#' sfm <- build(sfm, c("predator", "prey"), eqn = "runif(1, min = 20, max = 80)")
#'
#' # For ensemble simulations, it is highly recommended to reduce the
#' # returned output. For example, to save only every 1 time units and discard
#' # the first 100 time units, use:
#' sfm <- sim_specs(sfm, save_at = 1, save_from = 100)
#'
#' # Run ensemble simulation with 100 simulations
#' sims <- ensemble(sfm, n = 100)
#' plot(sims)
#'
#' # Plot individual trajectories
#' sims <- ensemble(sfm, n = 10, return_sims = TRUE)
#' plot(sims, type = "sims")
#'
#' # Specify which trajectories to plot
#' plot(sims, type = "sims", i = 1)
#'
#' # Plot the median with lighter individual trajectories
#' plot(sims, central_tendency = "median", type = "sims", alpha = 0.1)
#'
#' # Ensembles can also be run with exact values for the initial conditions
#' # and parameters. Below, we vary the initial values of the predator and the
#' # birth rate of the predators (delta). We generate a hunderd samples per
#' # condition. By default, the parameters are crossed, meaning that all
#' # combinations of the parameters are run.
#' sims <- ensemble(sfm,
#'   n = 50,
#'   range = list("predator" = c(10, 50), "delta" = c(.025, .05))
#' )
#'
#' plot(sims)
#'
#' # By default, a maximum of nine conditions is plotted.
#' # Plot specific conditions:
#' plot(sims, j = c(1, 3), nrows = 1)
#'
#' # Generate a non-crossed design, where the length of each range needs to be
#' # equal:
#' sims <- ensemble(sfm,
#'   n = 10, cross = FALSE,
#'   range = list(
#'     "predator" = c(10, 20, 30),
#'     "delta" = c(.020, .025, .03)
#'   )
#' )
#' plot(sims, nrows = 3)
#'
#' # Run simulation in parallel
#' use_threads(4)
#' sims <- ensemble(sfm, n = 10)
#'
#' # Stop using threads
#' use_threads(stop = TRUE)
#'
#' # Close Julia
#' use_julia(stop = TRUE)
#'
ensemble <- function(sfm,
                     n = 10,
                     return_sims = FALSE,
                     range = NULL,
                     cross = TRUE,
                     quantiles = c(0.025, 0.975),
                     only_stocks = TRUE,
                     verbose = TRUE) {
  check_xmile(sfm)

  # Collect arguments
  argg <- c(as.list(environment()))
  # Remove NULL arguments
  argg <- argg[!lengths(argg) == 0]

  if (tolower(sfm[["sim_specs"]][["language"]]) != "julia") {
    cli::cli_abort(c(
      "Ensemble simulations require Julia.",
      "x" = "Ensemble simulations are only supported for {.code Julia} models.",
      "i" = "R backend does not support ensemble functionality.",
      ">" = "Set language with: {.fn sim_specs}(sfm, language = {.code 'Julia'})"
    ))
  }

  if (!is.numeric(n)) {
    cli::cli_abort(c(
      "Invalid {.arg n} argument.",
      "x" = "The {.arg n} argument must be {.cls numeric}.",
      "i" = "Received: {.cls {typeof(n)}}",
      ">" = "Specify the number of simulations as a number, e.g., {.code n = 100}."
    ))
  }

  if (n <= 0) {
    cli::cli_abort(c(
      "Invalid {.arg n} value.",
      "x" = "The {.arg n} argument must be greater than {.val 0}.",
      "i" = "Received: {.val {n}}",
      ">" = "Use a positive integer, e.g., {.code n = 10}."
    ))
  }

  if (!is.numeric(quantiles)) {
    cli::cli_abort(c(
      "Invalid {.arg quantiles} argument.",
      "x" = "The {.arg quantiles} argument must be {.cls numeric}.",
      "i" = "Received: {.cls {typeof(quantiles)}}",
      ">" = "Use a numeric vector, e.g., {.code quantiles = c(0.025, 0.975)}."
    ))
  }

  if (length(unique(quantiles)) < 2) {
    cli::cli_abort(c(
      "Insufficient quantiles specified.",
      "x" = "The {.arg quantiles} argument must have at least {.val 2} unique values.",
      "i" = "Received {.val {length(unique(quantiles))}} unique value(s).",
      ">" = "Provide at least 2 quantiles, e.g., {.code quantiles = c(0.025, 0.975)}."
    ))
  }

  if (any(quantiles < 0 | quantiles > 1)) {
    cli::cli_abort(c(
      "Invalid quantile values.",
      "x" = "All values in {.arg quantiles} must be between {.val 0} and {.val 1}.",
      "i" = "Quantiles represent probabilities and must be proportions.",
      ">" = "Use values like {.code c(0.025, 0.5, 0.975)} for 2.5%, 50%, 97.5% quantiles."
    ))
  }

  if (!is.logical(cross)) {
    cli::cli_abort(c(
      "Invalid {.arg cross} argument.",
      "x" = "The {.arg cross} argument must be {.code TRUE} or {.code FALSE}.",
      "i" = "Received: {.val {cross}}",
      ">" = "Use {.code cross = TRUE} for all combinations or {.code cross = FALSE} for paired values."
    ))
  }

  if (!is.logical(return_sims)) {
    cli::cli_abort(c(
      "Invalid {.arg return_sims} argument.",
      "x" = "The {.arg return_sims} argument must be {.code TRUE} or {.code FALSE}.",
      "i" = "Received: {.val {return_sims}}",
      ">" = "Use {.code return_sims = TRUE} to include individual simulations in output."
    ))
  }

  if (!is.logical(only_stocks)) {
    cli::cli_abort(c(
      "Invalid {.arg only_stocks} argument.",
      "x" = "The {.arg only_stocks} argument must be {.code TRUE} or {.code FALSE}.",
      "i" = "Received: {.val {only_stocks}}",
      ">" = "Use {.code only_stocks = TRUE} to exclude flows and auxiliaries from output."
    ))
  }

  if (!is.null(range)) {
    if (!is.list(range)) {
      cli::cli_abort(c(
        "Invalid {.arg range} argument structure.",
        "x" = "The {.arg range} argument must be a {.cls list}.",
        "i" = "Received: {.cls {typeof(range)}}",
        ">" = "Use format: {.code range = list(param1 = c(1, 2), param2 = c(10, 20))}."
      ))
    }

    if (length(range) == 0) {
      cli::cli_abort(c(
        "Empty {.arg range} specification.",
        "x" = "The {.arg range} argument must have at least one parameter.",
        "i" = "At least one parameter must vary across ensemble conditions.",
        ">" = "Specify like: {.code range = list(param = c(1, 2, 3))}."
      ))
    }

    if (is.null(names(range))) {
      cli::cli_abort(c(
        "Unnamed {.arg range} elements.",
        "x" = "The {.arg range} list elements must be named.",
        "i" = "Names correspond to parameter/stock names in your model.",
        ">" = "Use: {.code range = list(paramname = values, ...)}."
      ))
    }

    # All must be numerical values
    if (!all(vapply(range, is.numeric, logical(1)))) {
      cli::cli_abort(c(
        "Non-numeric values in {.arg range}.",
        "x" = "All {.arg range} elements must be {.cls numeric} vectors.",
        "i" = "Each parameter must map to a vector of numbers.",
        ">" = "Example: {.code range = list(param1 = c(1, 2, 3))}."
      ))
    }

    # Test that names are unique
    if (length(unique(names(range))) != length(range)) {
      cli::cli_abort(c(
        "Duplicate names in {.arg range}.",
        "x" = "All {.arg range} names must be unique.",
        "i" = "Each parameter should be specified once.",
        ">" = "Use: {.code range = list(param1 = ..., param2 = ...)}."
      ))
    }

    # All varied elements must exist in the model
    names_df <- get_names(sfm)
    names_range <- names(range)
    idx <- names_range %in% names_df[["name"]]
    if (any(!idx)) {
      missing_names <- names_range[!idx]
      cli::cli_abort(c(
        "Unknown parameters in {.arg range}.",
        "x" = "The following parameters do not exist in the model: {paste0('{.code ', missing_names, '}', collapse = ', ')}.",
        "i" = "Check spelling and ensure parameters are defined in the model.",
        ">" = "Available variables: {paste0(names_df[['name']][1:min(5, length(names_df[['name']])], collapse = ', ')}"
      ))
    }

    # All varied elements must be a stock or constant
    idx <- names_range %in% c(names_df[names_df[["type"]] %in% c("stock", "constant"), "name"])
    if (any(!idx)) {
      invalid_names <- names_range[!idx]
      cli::cli_abort(c(
        "Cannot vary flows or auxiliaries.",
        "x" = "Only {.cls constant} and {.cls stock} initial values can be varied.",
        "!" = "Cannot vary: {paste0('{.code ', invalid_names, '}', collapse = ', ')}.",
        ">" = "Choose parameters from constants or stock initial values in your model."
      ))
    }

    # All ranges must be of the same length if not a crossed design
    range_lengths <- vapply(range, length, numeric(1))
    if (!cross) {
      if (length(unique(range_lengths)) != 1) {
        cli::cli_abort(c(
          "Mismatched range lengths with {.arg cross = FALSE}.",
          "x" = "When {.arg cross = FALSE}, all ranges must have equal length.",
          "i" = "Found lengths: {paste0(unique(range_lengths), collapse = ', ')} for parameters {paste0(names(range), collapse = ', ')}.",
          ">" = "Either use {.code cross = TRUE} or equalize all range vectors."
        ))
      }

      n_conditions <- unique(range_lengths)
    } else {
      # Compute the total number of conditions
      n_conditions <- prod(range_lengths)
    }

    # Alphabetically sort the ensemble parameters
    range <- range[sort(names(range))]
  } else {
    n_conditions <- 1
  }

  if (verbose) {
    total_sims <- n * n_conditions
    sim_word <- ifelse(total_sims == 1, "simulation", "simulations")
    if (is.null(range)) {
      msg <- c(
        "Starting ensemble simulation.",
        "i" = "Running {.val {total_sims}} {sim_word}."
      )
    } else {
      cond_word <- ifelse(n_conditions == 1, "condition", "conditions")
      sim_per_word <- ifelse(n == 1, "simulation", "simulations")
      msg <- c(
        "Starting ensemble simulation.",
        "i" = "Running {.val {total_sims}} {sim_word} total.",
        "i" = "{.val {n_conditions}} {cond_word} x {.val {n}} {sim_per_word} per condition."
      )
    }
    cli::cli_inform(msg)
  }

  # Create ensemble parameters
  ensemble_pars <- list(
    n = n,
    quantiles = quantiles,
    return_sims = return_sims,
    range = range, cross = cross
  )

  old_threads <- .sdbuildR_env[["prev_JULIA_NUM_THREADS"]]

  if (!is.null(.sdbuildR_env[["JULIA_NUM_THREADS"]]) && !is.null(old_threads)) {
    ensemble_pars[["threaded"]] <- TRUE
    Sys.setenv("JULIA_NUM_THREADS" = .sdbuildR_env[["JULIA_NUM_THREADS"]])

    on.exit(
      {
        if (is.na(old_threads)) {
          Sys.unsetenv("JULIA_NUM_THREADS")
        } else {
          Sys.setenv("JULIA_NUM_THREADS" = old_threads)
        }
      },
      add = TRUE
    )
  } else {
    ensemble_pars[["threaded"]] <- FALSE
  }


  # Get output filepaths
  ensemble_pars[["filepath_df"]] <- c(
    "df" = get_tempfile(fileext = ".csv"),
    "constants" = get_tempfile(fileext = ".csv"),
    "init" = get_tempfile(fileext = ".csv")
  )
  ensemble_pars[["filepath_summary"]] <- c(
    "df" = get_tempfile(fileext = ".csv"),
    "constants" = get_tempfile(fileext = ".csv"),
    "init" = get_tempfile(fileext = ".csv")
  )
  filepath <- get_tempfile(fileext = ".jl")

  # Compile script
  script <- compile_julia(sfm,
    filepath_sim = "",
    ensemble_pars = ensemble_pars,
    only_stocks = only_stocks
  )
  write_script(script, filepath)
  script <- paste0(readLines(filepath), collapse = "\n")


  # Evaluate script
  sim <- tryCatch(
    {
      use_julia()

      # Evaluate script
      start_t <- Sys.time()

      # Wrap in invisible and capture.output to not show message of units module being overwritten
      invisible(utils::capture.output(
        JuliaConnectoR::juliaEval(paste0('include("', filepath, '")'))
      ))

      end_t <- Sys.time()

      if (verbose) {
        elapsed <- round(end_t - start_t, 4)
        cli::cli_inform(c(
          "Ensemble simulation completed.",
          "i" = "Elapsed time: {.val {elapsed}} seconds."
        ))
      }

      # Delete file
      file.remove(filepath)

      # Read the total number of simulations
      n <- JuliaConnectoR::juliaEval(P[["ensemble_n"]])
      n_total <- JuliaConnectoR::juliaEval(P[["ensemble_total_n"]])

      # Read the ensemble conditions
      if (!is.null(ensemble_pars[["range"]])) {
        conditions <- JuliaConnectoR::juliaEval(paste0("Matrix(hcat(", P[["ensemble_pars"]], "...)')"))
        colnames(conditions) <- names(ensemble_pars[["range"]])
        conditions <- cbind(j = seq_len(nrow(conditions)), conditions)
      } else {
        conditions <- NULL
      }

      constants <- list()
      init <- list()

      # Read the simulation results
      if (return_sims) {
        df <- as.data.frame(data.table::fread(ensemble_pars[["filepath_df"]][["df"]],
          na.strings = c("", "NA")
        ))
        constants[["df"]] <- as.data.frame(data.table::fread(ensemble_pars[["filepath_df"]][["constants"]],
          na.strings = c("", "NA")
        ))
        init[["df"]] <- as.data.frame(data.table::fread(ensemble_pars[["filepath_df"]][["init"]],
          na.strings = c("", "NA")
        ))

        # Delete files
        file.remove(ensemble_pars[["filepath_df"]][["df"]])
        file.remove(ensemble_pars[["filepath_df"]][["constants"]])
        file.remove(ensemble_pars[["filepath_df"]][["init"]])
      } else {
        df <- NULL
      }

      # Read the summary file
      summary <- as.data.frame(data.table::fread(ensemble_pars[["filepath_summary"]][["df"]], na.strings = c("", "NA")))
      constants[["summary"]] <- as.data.frame(data.table::fread(ensemble_pars[["filepath_summary"]][["constants"]], na.strings = c("", "NA")))
      init[["summary"]] <- as.data.frame(data.table::fread(ensemble_pars[["filepath_summary"]][["init"]], na.strings = c("", "NA")))

      # Delete files
      file.remove(ensemble_pars[["filepath_summary"]][["df"]])
      file.remove(ensemble_pars[["filepath_summary"]][["constants"]])
      file.remove(ensemble_pars[["filepath_summary"]][["init"]])

      list(
        success = TRUE,
        # sfm = sfm,
        df = df,
        summary = summary,
        n = n,
        n_total = n_total,
        n_conditions = n_conditions,
        conditions = conditions,
        init = init,
        constants = constants,
        script = script,
        duration = end_t - start_t
      ) |>
        utils::modifyList(argg) |>
        structure(class = "sdbuildR_ensemble")
    },
    error = function(e) {
      cli::cli_warn(c(
        "Julia execution failed.",
        "!" = "An error occurred while running the Julia script.",
        "i" = "Error: {e[['message']]}"
      ))
      list(
        success = FALSE,
        error_message = e[["message"]],
        df = NULL,
        summary = NULL,
        n = n,
        n_total = n_total,
        n_conditions = n_conditions,
        conditions = NULL,
        init = NULL,
        constants = NULL,
        script = script,
        duration = end_t - start_t
        # sfm = sfm
      ) |>
        utils::modifyList(argg) |>
        structure(class = "sdbuildR_ensemble")
    }
  )

  return(sim)
}


#' Set up threaded ensemble simulations
#'
#' Specify the number of threads for ensemble simulations in Julia. This will not overwrite your current global setting for JULIA_NUM_THREADS. Note that this does not affect regular simulations with [simulate()].
#'
#' @param n Number of Julia threads to use. Defaults to parallel::detectCores() - 1. If set to a value higher than the number of available cores minus 1, it will be set to the number of available cores minus 1.
#' @param stop Stop using threaded ensemble simulations. Defaults to FALSE.
#'
#' @returns No return value, called for side effects
#' @concept julia
#' @seealso [ensemble()], [use_julia()]
#' @export
#'
#' @examplesIf julia_status()$status == "ready"
#' # Use Julia with 4 threads
#' use_julia()
#' use_threads(n = 4)
#'
#' # Stop using threads
#' use_threads(stop = TRUE)
#'
#' # Stop using Julia
#' use_julia(stop = TRUE)
#'
use_threads <- function(n = parallel::detectCores() - 1, stop = FALSE) {
  if (!is.numeric(n)) {
    cli::cli_abort(c(
      "Invalid {.arg n} argument.",
      "x" = "The {.arg n} argument must be {.cls numeric}.",
      "i" = "Received: {.cls {typeof(n)}}",
      ">" = "Specify number of threads, e.g., {.code n = 4}."
    ))
  }

  if (n < 1) {
    cli::cli_abort(c(
      "Invalid thread count.",
      "x" = "The {.arg n} argument must be at least {.val 1}.",
      "i" = "Received: {.val {n}}",
      ">" = "Use a positive integer, e.g., {.code n = 4}."
    ))
  }

  if (!is.logical(stop)) {
    cli::cli_abort(c(
      "Invalid {.arg stop} argument.",
      "x" = "The {.arg stop} argument must be {.code TRUE} or {.code FALSE}.",
      "i" = "Received: {.val {stop}}",
      ">" = "Use {.code stop = TRUE} to disable threading."
    ))
  }

  if (stop) {
    .sdbuildR_env[["JULIA_NUM_THREADS"]] <- NULL
  } else {
    # Set number of Julia threads to use
    if (n > (parallel::detectCores() - 1)) {
      max_threads <- parallel::detectCores() - 1
      cli::cli_warn(c(
        "Thread count exceeds available cores.",
        "!" = "Requested {.val {n}} threads but only {.val {max_threads}} cores available.",
        "i" = "Using maximum safe value: {.val {max_threads}} threads.",
        "i" = "Note: It's recommended to leave at least 1 core free for the system."
      ))
      n <- max_threads
    }

    # Save user's old setting
    .sdbuildR_env[["prev_JULIA_NUM_THREADS"]] <- Sys.getenv("JULIA_NUM_THREADS",
      unset = NA
    )

    .sdbuildR_env[["JULIA_NUM_THREADS"]] <- n
  }

  return(invisible())
}


#' Generate code to build stock-and-flow model
#'
#' Create R code to rebuild an existing stock-and-flow model. This may help to understand how a model is built, or to modify an existing one.
#'
#' @inheritParams build
#'
#' @returns String with code to build stock-and-flow model from scratch.
#' @concept build
#' @export
#'
#' @examples
#' sfm <- xmile("SIR")
#' get_build_code(sfm)
#'
get_build_code <- function(sfm) {
  check_xmile(sfm)

  # Simulation specifications - careful here. If a default is 100.0, this will be turned into 100. Need to have character defaults to preserve digits.
  sim_specs_list <- sfm[["sim_specs"]]
  sim_specs_list <- lapply(sim_specs_list, function(z) if (is.character(z)) paste0("\"", z, "\"") else z)
  sim_specs_str <- paste0(names(sim_specs_list), " = ", unname(sim_specs_list), collapse = ", ")
  sim_specs_str <- paste0(" |>\n\t\tsim_specs(", sim_specs_str, ")")

  # Model units
  if (nrow(sfm[["model_units"]]) > 0) {
    model_units_str <- apply(sfm[["model_units"]], 1, function(row) {
      row_list <- as.list(row)
      row_list <- lapply(row_list, function(z) if (is.character(z)) paste0("\"", z, "\"") else z)


      sprintf("model_units(%s)", paste0(names(row_list), " = ", unname(row_list), collapse = ", "))
    }) |>
      paste0(collapse = "|>\n\t\t")
    model_units_str <- paste0(" |>\n\t\t", model_units_str)
  } else {
    model_units_str <- ""
  }

  # Macros
  macro_df <- sfm[[P[["macro_name"]]]]
  if (nrow(macro_df) > 0) {
    # Drop columns ending with _julia for readability
    macro_df <- macro_df[, !grepl("_julia$", names(macro_df)), drop = FALSE]

    macro_str <- vapply(seq_len(nrow(macro_df)), function(i) {
      row <- as.list(macro_df[i, , drop = FALSE])
      row <- lapply(row, function(z) if (is.character(z)) paste0("\"", z, "\"") else z)
      sprintf("macro(%s)", paste0(names(row), " = ", unname(row), collapse = ", "))
    }, character(1)) |>
      paste0(collapse = "|>\n\t\t")

    macro_str <- paste0(" |>\n\t\t", macro_str)
  } else {
    macro_str <- ""
  }

  # Header string
  h <- sfm[["header"]]
  defaults_header <- formals(header)
  defaults_header <- defaults_header[!names(defaults_header) %in%
    c("sfm", "created", "...")]

  # Find which elements in h are identical to those in defaults_header
  h <- h[vapply(names(h), function(name) {
    !name %in% names(defaults_header) || !identical(
      h[[name]],
      defaults_header[[name]]
    )
  }, logical(1))]

  h <- lapply(h, function(z) {
    if (is.character(z) |
      inherits(z, "POSIXt")) {
      paste0("\"", z, "\"")
    } else {
      z
    }
  })

  header_str <- paste0(
    " |>\n\t\theader(",
    paste0(names(h), " = ", unname(h), collapse = ", "), ")"
  )

  # Variables
  if (nrow(sfm[["variables"]]) > 0) {
    defaults <- formals(build)
    defaults <- defaults[!names(defaults) %in% c("sfm", "name", "type", "label", "...")]

    # Get properties per building block
    keep_prop <- get_building_block_prop()

    var_str <- split(sfm[["variables"]], seq_len(nrow(sfm[["variables"]]))) |>
      lapply(function(y) {
        z <- as.list(y)

        # Remove properties containing "_julia"
        z[grepl("_julia", names(z))] <- NULL

        # Find which elements in h are identical to those in defaults_header
        z <- z[vapply(names(z), function(name) {
          !name %in% names(defaults) || !identical(z[[name]], defaults[[name]])
        }, logical(1))]

        # Order z according to default
        order_names <- intersect(keep_prop[[z[["type"]]]], names(z))
        z <- z[order_names]

        z <- lapply(z, function(a) {
          ifelse(is.character(a), paste0("\"", a, "\""), a)
        })

        paste0(
          "build(",
          paste0(names(z), " = ", unname(z),
            collapse = ", "
          ), ")"
        )
      })
    var_str <- paste0(" |>\n\t\t", paste0(unlist(var_str), collapse = " |>\n\t\t"))
  } else {
    var_str <- ""
  }

  script <- sprintf(
    "sfm = xmile()%s%s%s%s%s", sim_specs_str,
    header_str, var_str, macro_str, model_units_str
  )

  # Format code
  if (requireNamespace("styler", quietly = TRUE)) {
    # Temporarily set option
    old_option <- getOption("styler.colored_print.vertical")
    options(styler.colored_print.vertical = FALSE)

    script <- tryCatch(
      {
        suppressWarnings(suppressMessages(
          styler::style_text(script)
        ))
      },
      error = function(e) {
        return(script)
      }
    )

    on.exit(
      {
        if (is.null(old_option)) {
          options(styler.colored_print.vertical = NULL)
        } else {
          options(styler.colored_print.vertical = old_option)
        }
      },
      add = TRUE
    )
  } else {
    cli::cli_inform(c(
      "Code formatting skipped.",
      "i" = "Package {.pkg styler} is not installed.",
      ">" = "Install with: {.code install.packages('styler')}.",
      "i" = "For now, wrap the script in {.fn cat()} to view the formatted code."
    ))
  }

  return(script)
}
