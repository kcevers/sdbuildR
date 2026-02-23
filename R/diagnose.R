#' Debug stock-and-flow model
#'
#' Check for common formulation problems in a stock-and-flow model.
#'
#' The following problems are detected:
#' - An absence of stocks
#' - Flows without a source (`from`) or target (`to`)
#' - Flows connected to a stock that does not exist
#' - Undefined variable references in equations
#' - Circularity in equations
#' - Connected stocks and flows without both having units or no units
#' - Missing unit definitions
#'
#' The following potential problems are detected:
#' - Absence of flows
#' - Stocks without inflows or outflows
#' - Equations with a value of 0
#'
#' @inheritParams build
#'
#' @returns Object of class `diagnose_sdbuildR`. A flat named list with one
#'   entry per check. Each entry contains a `problem` field (`"none"`,
#'   `"warning"`, or `"error"`) and type-specific data fields.
#' @concept build
#' @export
#'
#' @examples
#' # No issues
#' sfm <- sdbuildR("SIR")
#' diagnose(sfm)
#'
#' # Detect absence of stocks or flows
#' sfm <- sdbuildR()
#' diagnose(sfm)
#'
#' # Detect stocks without inflows or outflows
#' sfm <- sdbuildR() |> build("Prey", "stock")
#' diagnose(sfm)
#'
#' # Detect circularity in equation definitions
#' sfm <- sdbuildR() |>
#'   build("Prey", "stock", eqn = "Predator") |>
#'   build("Predator", "stock", eqn = "Prey")
#' diagnose(sfm)
#'
diagnose <- function(sfm) {
  check_sdbuildR(sfm)

  # Initialize all checks with "none" (all data fields empty)
  checks <- list(
    no_stocks            = list(problem = "none"),
    no_flows             = list(problem = "none"),
    disconnected_stocks  = list(problem = "none", stocks = character()),
    disconnected_flows   = list(problem = "none", flows = character()),
    bad_flow_connections = list(problem = "none", flows = character()),
    zero_equations       = list(problem = "none", variables = character()),
    undefined_vars       = list(problem = "none", refs = list()),
    circular_static      = list(problem = "none", cycle_nodes = character(), edge_list = NULL),
    circular_dynamic     = list(problem = "none", cycle_nodes = character(), edge_list = NULL),
    undefined_units      = list(problem = "none", units = character())
  )

  # Get stock and flow names
  stock_names <- sfm[["variables"]][sfm[["variables"]][["type"]] == "stock", "name"]
  flow_df <- get_flow_df(sfm)
  flow_names <- flow_df[["name"]]

  ### Check whether all stocks have inflows and/or outflows
  if (length(stock_names) > 0) {

    if (nrow(flow_df) > 0) {
      idx <- stock_names %in% flow_df[["to"]] | stock_names %in% flow_df[["from"]]
    } else {
      # If there are no flows, all stocks are disconnected
      idx <- rep(FALSE, length(stock_names))
    }

    if (any(!idx)) {
      checks[["disconnected_stocks"]] <- list(
        problem = "warning",
        stocks = stock_names[!idx]
      )
    }

  } else {
    checks[["no_stocks"]] <- list(problem = "error")
  }

  ### Check whether all flows either have a from or to property
  if (length(flow_names) > 0) {
    idx <- !nzchar(flow_df[["from"]]) & !nzchar(flow_df[["to"]])

    if (any(idx)) {
      checks[["disconnected_flows"]] <- list(
        problem = "error",
        flows = flow_names[idx]
      )
    }

    ### Find whether the from and to stocks exist
    idx_to   <- (!flow_df[["to"]]   %in% stock_names) & nzchar(flow_df[["to"]])
    idx_from <- (!flow_df[["from"]] %in% stock_names) & nzchar(flow_df[["from"]])

    if (any(idx_to) || any(idx_from)) {
      checks[["bad_flow_connections"]] <- list(
        problem = "error",
        flows = c(flow_names[idx_to], flow_names[idx_from])
      )
    }
  } else {
    checks[["no_flows"]] <- list(problem = "warning")
  }

  ### Check equations with zero
  zero_idx <- sfm[["variables"]][["eqn"]] %in% c("0", "0.0")
  if (any(zero_idx)) {
    checks[["zero_equations"]] <- list(
      problem = "warning",
      variables = sfm[["variables"]][zero_idx, "name"]
    )
  }

  ### Detect undefined variable references in equations
  out <- detect_undefined_var(sfm)
  if (out[["issue"]]) {
    checks[["undefined_vars"]] <- list(
      problem = "error",
      refs = out[["data"]][["refs"]]
    )
  }

  ### Detect circularity in equations
  out <- order_equations(sfm, print_msg = FALSE)
  if (out[["static"]][["issue"]]) {
    checks[["circular_static"]] <- list(
      problem = "error",
      cycle_nodes = out[["static"]][["cycle_nodes"]],
      edge_list   = out[["static"]][["edge_list"]]
    )
  }
  if (out[["dynamic"]][["issue"]]) {
    checks[["circular_dynamic"]] <- list(
      problem = "error",
      cycle_nodes = out[["dynamic"]][["cycle_nodes"]],
      edge_list   = out[["dynamic"]][["edge_list"]]
    )
  }

  ### Find missing unit definitions
  regex_units <- get_regex_units()

  add_custom_unit <- detect_undefined_units(
    sfm,
    new_eqns  = sfm[["variables"]][["eqn"]],
    new_units = sfm[["variables"]][["units"]],
    regex_units = regex_units,
    R_or_Julia  = "R"
  )
  if (NROW(add_custom_unit) > 0) {
    checks[["undefined_units"]] <- list(
      problem = "error",
      units = add_custom_unit[["name"]]
    )
  }

  result <- new_diagnose_sdbuildR(checks)
  result
}


#' Constructor for diagnose_sdbuildR class
#'
#' @param checks Flat named list of check results, one entry per check.
#'   Each entry has a `problem` field (`"none"`, `"warning"`, or `"error"`)
#'   and type-specific data fields.
#'
#' @returns Object of class `diagnose_sdbuildR`
#' @noRd
new_diagnose_sdbuildR <- function(checks) {
  structure(checks, class = "diagnose_sdbuildR")
}


#' Print method for diagnose_sdbuildR
#'
#' @param x Object of class `diagnose_sdbuildR`
#' @param ... Ignored
#'
#' @returns `x` invisibly
#' @export
print.diagnose_sdbuildR <- function(x, ...) {
  errors   <- Filter(function(y) y$problem == "error",   x)
  warnings <- Filter(function(y) y$problem == "warning", x)

  if (length(errors) == 0 && length(warnings) == 0) {
    cli::cli_inform("No problems detected!")
    return(invisible(x))
  }

  n_err  <- length(errors)
  n_warn <- length(warnings)

  if (n_err > 0) {
    cli::cli_h2("Problem{?s} ({n_err})")
    lapply(names(errors), function(nm) .print_one_check(nm, errors[[nm]]))
  }

  if (n_warn > 0) {
    cli::cli_h2("Potential problem{?s} ({n_warn})")
    lapply(names(warnings), function(nm) .print_one_check(nm, warnings[[nm]]))
  }

  invisible(x)
}


#' Format and print a single check entry
#'
#' @param nm Component name (key in the diagnose_sdbuildR list)
#' @param check Named list with `problem` field and type-specific data fields
#'
#' @noRd
.print_one_check <- function(nm, check) {
  switch(nm,
    no_stocks = cli::cli_inform(c(
      "!" = "Model has no stocks.",
      ">" = "Add at least one stock with {.fn stock} or {.fn build}."
    )),
    no_flows = cli::cli_inform(c(
      "i" = "Model has no flows.",
      ">" = "Add flows with {.fn flow} or {.fn build}."
    )),
    disconnected_stocks = cli::cli_inform(c(
      "i" = "{cli::qty(length(check$stocks))}Stock{?s} not connected to any flow: {.code {check$stocks}}."
    )),
    disconnected_flows = cli::cli_inform(c(
      "!" = "{cli::qty(length(check$flows))}Flow{?s} not connected to any stock: {.code {check$flows}}.",
      ">" = "Connect with {.arg to} and/or {.arg from} in {.fn flow} or {.fn build}."
    )),
    bad_flow_connections = cli::cli_inform(c(
      "!" = "{cli::qty(length(check$flows))}Flow{?s} connected to a non-existent stock: {.code {check$flows}}."
    )),
    zero_equations = cli::cli_inform(c(
      "i" = "{cli::qty(length(check$variables))}{.code {check$variables}} {?has/have} an equation of 0."
    )),
    undefined_vars = {
      n <- length(check$refs)
      cli::cli_inform(c("!" = "{cli::qty(n)}Reference{?s} to undefined variable{?s} found."))
      lapply(check$refs, function(r) {
        cli::cli_inform(c(
          " " = "{.code {r$var}}${r$prop}: {cli::qty(length(r$undefined))}{.code {r$undefined}} {?is/are} undefined."
        ))
      })
      cli::cli_inform(c(">" = "Define these variables or check for spelling mistakes."))
    },
    circular_static = {
      cli::cli_inform(c("!" = "Circular dependency in static equations."))
      if (length(check$cycle_nodes) > 0) {
        cli::cli_inform(c(" " = "Variables involved: {.code {check$cycle_nodes}}."))
      }
      if (!is.null(check$edge_list) && nrow(check$edge_list) > 0) {
        for (i in seq_len(nrow(check$edge_list))) {
          cli::cli_inform(c(
            " " = "{.code {check$edge_list[i, 1]}} depends on {.code {check$edge_list[i, 2]}}."
          ))
        }
      }
    },
    circular_dynamic = {
      cli::cli_inform(c("!" = "Circular dependency in dynamic equations."))
      if (length(check$cycle_nodes) > 0) {
        cli::cli_inform(c(" " = "Variables involved: {.code {check$cycle_nodes}}."))
      }
      if (!is.null(check$edge_list) && nrow(check$edge_list) > 0) {
        for (i in seq_len(nrow(check$edge_list))) {
          cli::cli_inform(c(
            " " = "{.code {check$edge_list[i, 1]}} depends on {.code {check$edge_list[i, 2]}}."
          ))
        }
      }
    },
    undefined_units = cli::cli_inform(c(
      "!" = "{cli::qty(length(check$units))}Unit{?s} not defined: {.code {check$units}}.",
      ">" = "Add custom units with {.fn custom_unit}."
    )),
    cli::cli_inform(c("!" = "Unknown check: {nm}."))
  )
}


#' Short label for a check entry (used in simulate() abort message)
#'
#' @param nm Component name string
#'
#' @returns Single character string
#' @noRd
.problem_label <- function(nm) {
  switch(nm,
    no_stocks            = "No stocks.",
    disconnected_flows   = "Flow(s) not connected to a stock.",
    bad_flow_connections = "Flow(s) connected to non-existent stock.",
    undefined_vars       = "Undefined variable reference(s).",
    circular_static      = "Circular dependency in static equations.",
    circular_dynamic     = "Circular dependency in dynamic equations.",
    undefined_units      = "Undefined unit(s).",
    nm
  )
}


#' Detect undefined variables in equations
#'
#' @inheritParams build
#'
#' @returns List with `issue` (logical) and, if TRUE, `data` (structured problem list)
#' @noRd
#'
detect_undefined_var <- function(sfm) {
  # Get names
  var_names <- get_model_var(sfm)

  # Funcs and graphical functions can be functions
  gf_names <- sfm[["variables"]][sfm[["variables"]][["type"]] == "lookup", "name"]
  func_names <- get_funcs(sfm)[["name"]]
  possible_func_in_model <- c(
    func_names,
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

      # Get function argument names if this is a func-type variable
      func_arg_names <- character(0)
      if (x[["type"]] == "func") {
        parsed <- tryCatch(parse(text = x[["eqn"]])[[1]], error = function(e) NULL)
        if (!is.null(parsed) && is.call(parsed) &&
            identical(parsed[[1]], as.name("function"))) {
          func_arg_names <- names(parsed[[2]])
        }
      }

      A <- lapply(y, function(z) {
        deps <- dependencies_(sfm, z, only_var = TRUE, only_model_var = FALSE)

        # Find all undefined variables and functions
        setdiff(
          unlist(deps),
          # Cannot depend on itself
          c(possible_func, setdiff(var_names, x[["name"]]), func_arg_names)
        )
      })
      A <- A[lengths(A) > 0]
      if (length(A) == 0) {
        return(NULL)
      } else {
        return(stats::setNames(list(A), x[["name"]]))
      }
    })

  missing_ref <- unlist(unname(missing_ref), recursive = FALSE)

  if (length(missing_ref) > 0) {
    refs <- lapply(seq_along(missing_ref), function(i) {
      x <- missing_ref[[i]]
      nm <- names(missing_ref)[i]
      lapply(seq_along(x), function(j) {
        list(var = nm, prop = names(x)[j], undefined = unname(x[[j]]))
      })
    }) |> unlist(recursive = FALSE)

    list(
      issue = TRUE,
      data = list(type = "undefined_vars", refs = refs)
    )
  } else {
    list(issue = FALSE)
  }
}
