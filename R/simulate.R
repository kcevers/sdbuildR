#' Simulate stock-and-flow model
#'
#' Simulate a stock-and-flow model with simulation specifications defined by [sim_specs()]. If `sim_specs(language = "julia")`, the Julia environment will first be set up with [use_julia()]. If any problems are detected by [diagnose()], the model cannot be simulated.
#'
#' @inheritParams insightmaker_to_sfm
#' @inheritParams update.sdbuildR
#' @inheritParams sim_specs
#' @param nsim Number of simulations to run (currently unused; see [ensemble()] for running multiple simulations).
#' @param verbose If `TRUE`, print duration of simulation. Defaults to `FALSE`.
#' @param ... Optional arguments passed to [sim_specs()]; these can be used to override the simulation specifications set in the model object.
#'
#' @returns Object of class [`simulate_sdbuildR`][simulate.sdbuildR()], a list containing:
#' \describe{
#'   \item{object}{Stock-and-flow model object of class [`sdbuildR`][sdbuildR]}
#'   \item{df}{Data frame: simulation results (time, variable, value)}
#'   \item{init}{Named vector: initial stock values}
#'   \item{constants}{Named vector: constant parameters}
#'   \item{script}{Character: generated simulation code (R or Julia)}
#'   \item{duration}{Numeric: simulation time in seconds}
#'   \item{success}{Logical: TRUE if completed without errors}
#'   \item{error_message}{NULL if completed without errors}
#' }
#'
#' Use [as.data.frame()][as.data.frame.simulate_sdbuildR()] to extract results, [plot()][plot.simulate_sdbuildR()] to visualize.
#'
#'
#' @export
#' @importFrom stats simulate
#' @method simulate sdbuildR
#' @concept simulate
#' @seealso [update()], [sdbuildR()], [diagnose()], [sim_specs()], [use_julia()]
#'
#' @examples
#' sfm <- sdbuildR("SIR")
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # Obtain all model variables
#' sim <- simulate(sim_specs(sfm, only_stocks = FALSE))
#' plot(sim, add_constants = TRUE)
#'
#' @examplesIf Sys.getenv("NOT_CRAN") == "true" && is_julia_ready()
#' # Use Julia for models with units
#' sfm <- sim_specs(sdbuildR("coffee_cup"), language = "Julia")
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # Close Julia session
#' use_julia(stop = TRUE)
#'
simulate.sdbuildR <- function(
  object,
  nsim = 1, seed = NULL,
  verbose = FALSE,
  ...
) {
  check_sdbuildR(object)

  # First assess whether the model is valid
  if (is.null(object[["assemble"]][["diagnose"]])) {
    object[["assemble"]][["diagnose"]] <- diagnose(object)
  }
  debug_result <- object[["assemble"]][["diagnose"]]
  errors <- Filter(function(c) c$problem == "error", debug_result)
  if (length(errors) > 0) {
    n <- length(errors)
    labels <- vapply(names(errors), .problem_label, character(1))
    names(labels) <- rep("x", n)
    cli::cli_abort(c(
      "{cli::qty(n)}Model has {n} problem{?s}. Run {.fn diagnose} for details.",
      labels
    ))
  }

  # Override sim_specs with any arguments passed via ...
  varargs <- list(...)
  # Add seed if seed was passed
  if (!missing(seed)) {
    varargs <- c(list(seed = seed), varargs)
  }
  if (length(varargs) > 0) {
    object <- do.call(sim_specs, c(list(object), varargs))
  }

  only_stocks <- object[["sim_specs"]][["only_stocks"]]
  vars <- object[["sim_specs"]][["vars"]]

  if (!is.null(vars)) {
    vars <- validate_sim_vars(object, vars)
    stock_names <- get_variables_by_type(object, "stock")[["name"]]
    only_stocks <- all(vars %in% stock_names)
  }

  if (tolower(object[["sim_specs"]][["language"]]) == "julia") {
    return(simulate_julia(object,
      only_stocks = only_stocks,
      vars = vars,
      verbose = verbose
    ))
  } else if (tolower(object[["sim_specs"]][["language"]]) == "r") {
    # Check model for unit strings
    if (is.null(object[["assemble"]][["unit_strings"]])) {
      object[["assemble"]][["unit_strings"]] <- find_unit_strings(object)
    }
    eqn_units <- object[["assemble"]][["unit_strings"]]

    # Stop if equations contain unit strings
    if (length(eqn_units) > 0) {
      txt <- paste0(
        "The model contains unit strings u(''), which are not supported for simulations in R.\nSet sim_specs(sfm, language = 'Julia') or modify the equations of these variables:\n\n",
        paste0(names(eqn_units), collapse = ", ")
      )
      cli::cli_warn(c(
        "!" = "Function {.fn u} with unit strings is not supported for R simulations.",
        "i" = "Use {.fn sim_specs}(sfm, language = {.code 'Julia'}) for unit support.",
        ">" = "Or modify the equations of these variables: {paste0(names(eqn_units), collapse = ', ')}"
      ))
      return(new_simulate_sdbuildR(
        success = FALSE,
        error_message = txt,
        object = object
      ))
    }

    return(simulate_r(object,
      only_stocks = only_stocks,
      vars = vars,
      verbose = verbose
    ))
  }
  # else {
  #   txt <- "Simulation language not supported.\nPlease run either sim_specs(sfm, language = 'Julia') (recommended) or sim_specs(sfm, language = 'R') (no unit or ensemble support)."
  #   cli::cli_warn(c(
  #     "!" = "Simulation language must be either {.code 'Julia'} or {.code 'R'}.",
  #     ">" = "Set: {.fn sim_specs}(sfm, language = {.code 'R'}) or {.fn sim_specs}(sfm, language = {.code 'Julia'})."
  #   ))
  #   return(new_simulate_sdbuildR(
  #     success = FALSE,
  #     error_message = txt,
  #     object = object
  #   ))
  # }
}


#' Create new object of class [`simulate_sdbuildR`][simulate.sdbuildR()]
#' @noRd
new_simulate_sdbuildR <- function(success = FALSE,
                                  error_message = NULL,
                                  object = NULL,
                                  df = NULL,
                                  init = NULL,
                                  constants = NULL,
                                  script = NULL,
                                  duration = NULL,
                                  ...) {
  obj <- list(
    success = success,
    error_message = error_message,
    object = object,
    df = df,
    init = init,
    constants = constants,
    script = script,
    duration = duration,
    ...
  )

  structure(obj, class = "simulate_sdbuildR")
}


#' Check class [`simulate_sdbuildR`][simulate.sdbuildR()]
#'
#' @param x A simulation of a stock-and-flow model of class [`simulate_sdbuildR`][simulate.sdbuildR()]
#'
#' @returns Invisible x if valid, otherwise an error is thrown
#' @noRd
#'
check_simulate_sdbuildR <- function(x) {
  if (!inherits(x, "simulate_sdbuildR")) {
    cli::cli_abort(c(
      "Invalid object type.",
      "x" = "Expected object of class {.cls simulate_sdbuildR}.",
      "i" = "Use {.fn simulate} to create a valid simulation object."
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
        "i" = "This field is populated by {.fn simulate} with results."
      ))
    }
    if (is.null(x$init)) {
      cli::cli_abort(c(
        "Missing initial stock values.",
        "x" = "Successful simulation must preserve initial values in {.arg init}.",
        "i" = "Initial values of all stocks should be recorded."
      ))
    }

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

  invisible(x)
}


# Internal helper: compute model properties including nonlinearity score
model_properties <- function(object) {
  vars <- object[["variables"]]

  by_type <- function(t) vars[vars[["type"]] == t, , drop = FALSE]
  stocks <- by_type("stock")
  flows <- by_type("flow")
  auxs <- by_type("aux")
  constants <- by_type("constant")
  lookups <- by_type("lookup")

  dynamic_names <- c(stocks[["name"]], flows[["name"]], auxs[["name"]])
  lookup_names <- lookups[["name"]]

  # Variables whose equations we scan (everything except lookups and funcs)
  eqn_vars <- vars[!vars[["type"]] %in% c("lookup", "func"), , drop = FALSE]

  nonlinear_fn_pattern <- paste0(
    "\\b(exp|log|log10|log2|sqrt|sin|cos|tan|asin|acos|atan|abs|ceiling|floor)\\s*\\("
  )

  by_variable <- character(0)

  tag_var <- function(vec, nm, tag) {
    if (nm %in% names(vec)) {
      vec[nm] <- paste0(vec[nm], ", ", tag)
    } else {
      vec[nm] <- tag
    }
    vec
  }

  n_lookup_refs <- 0L
  n_nonlinear_fns <- 0L
  n_multiplicative_dynamic <- 0L

  if (nrow(eqn_vars) > 0 && length(lookup_names) > 0) {
    for (i in seq_len(nrow(eqn_vars))) {
      eqn <- eqn_vars[i, "eqn"]
      nm <- eqn_vars[i, "name"]
      if (is.na(eqn) || eqn == "") next
      if (any(sapply(lookup_names, function(lk) grepl(lk, eqn, fixed = TRUE)))) {
        n_lookup_refs <- n_lookup_refs + 1L
        by_variable <- tag_var(by_variable, nm, "lookup_ref")
      }
    }
  }

  if (nrow(eqn_vars) > 0) {
    for (i in seq_len(nrow(eqn_vars))) {
      eqn <- eqn_vars[i, "eqn"]
      nm <- eqn_vars[i, "name"]
      if (is.na(eqn) || eqn == "") next

      if (grepl(nonlinear_fn_pattern, eqn, perl = TRUE)) {
        n_nonlinear_fns <- n_nonlinear_fns + 1L
        by_variable <- tag_var(by_variable, nm, "nonlinear_fn")
      }

      if (length(dynamic_names) >= 2 && grepl("*", eqn, fixed = TRUE)) {
        refs <- sum(sapply(dynamic_names, function(v) grepl(v, eqn, fixed = TRUE)))
        if (refs >= 2L) {
          n_multiplicative_dynamic <- n_multiplicative_dynamic + 1L
          by_variable <- tag_var(by_variable, nm, "multiplicative")
        }
      }
    }
  }

  list(
    n_stocks = nrow(stocks),
    n_flows = nrow(flows),
    n_aux = nrow(auxs),
    n_constants = nrow(constants),
    n_lookups = nrow(lookups),
    nonlinearity = list(
      score                    = n_lookup_refs + n_nonlinear_fns + n_multiplicative_dynamic,
      n_lookup_refs            = n_lookup_refs,
      n_nonlinear_fns          = n_nonlinear_fns,
      n_multiplicative_dynamic = n_multiplicative_dynamic,
      by_variable              = by_variable
    )
  )
}


#' Compare two stock-and-flow models
#'
#' Compares the structure, equations, units, and simulation settings of two
#' `sdbuildR` models, and computes a nonlinearity score for each.
#'
#' @param sfm1 A stock-and-flow model of class [`sdbuildR`][sdbuildR()].
#' @param sfm2 A stock-and-flow model of class [`sdbuildR`][sdbuildR()].
#'
#' @returns An object of class `compare_sdbuildR` (a list) containing:
#'   \describe{
#'     \item{`labels`}{Names of the two model objects (captured expressions).}
#'     \item{`added`}{Variables present in `sfm2` but not `sfm1`.}
#'     \item{`removed`}{Variables present in `sfm1` but not `sfm2`.}
#'     \item{`type_changed`}{Variables with different types.}
#'     \item{`eqn_changed`}{Variables with different equations.}
#'     \item{`units_changed`}{Variables with different units.}
#'     \item{`sim_specs_diff`}{Simulation settings that differ.}
#'     \item{`properties`}{Per-model counts and nonlinearity scores.}
#'   }
#' @seealso [`simulate()`][simulate.sdbuildR()], [`summary()`][summary.sdbuildR()]
#' @concept build
#' @export
#' @examples
#' sfm1 <- sdbuildR("SIR")
#' sfm2 <- stock(sfm1, "Susceptible", eqn = 0.5)
#' compare_models(sfm1, sfm2)
#'
compare_models <- function(sfm1, sfm2) {
  label1 <- deparse(substitute(sfm1))
  label2 <- deparse(substitute(sfm2))

  check_sdbuildR(sfm1)
  check_sdbuildR(sfm2)

  v1 <- sfm1[["variables"]]
  v2 <- sfm2[["variables"]]

  names1 <- v1[["name"]]
  names2 <- v2[["name"]]

  only_in_2 <- setdiff(names2, names1)
  only_in_1 <- setdiff(names1, names2)
  in_both <- intersect(names1, names2)

  # Variables added / removed
  make_var_df <- function(vars, nms) {
    rows <- vars[vars[["name"]] %in% nms, c("name", "type", "eqn", "units"), drop = FALSE]
    rownames(rows) <- NULL
    rows
  }
  added <- make_var_df(v2, only_in_2)
  removed <- make_var_df(v1, only_in_1)

  # Changes among shared variables
  v1s <- v1[v1[["name"]] %in% in_both, , drop = FALSE]
  v2s <- v2[v2[["name"]] %in% in_both, , drop = FALSE]
  # Align row order
  v1s <- v1s[order(match(v1s[["name"]], in_both)), , drop = FALSE]
  v2s <- v2s[order(match(v2s[["name"]], in_both)), , drop = FALSE]

  type_changed <- data.frame(
    name = in_both[v1s[["type"]] != v2s[["type"]]],
    type_1 = v1s[["type"]][v1s[["type"]] != v2s[["type"]]],
    type_2 = v2s[["type"]][v1s[["type"]] != v2s[["type"]]],
    stringsAsFactors = FALSE
  )

  eqn1 <- ifelse(is.na(v1s[["eqn"]]), "", v1s[["eqn"]])
  eqn2 <- ifelse(is.na(v2s[["eqn"]]), "", v2s[["eqn"]])
  eqn_diff_idx <- eqn1 != eqn2
  eqn_changed <- data.frame(
    name = in_both[eqn_diff_idx],
    eqn_1 = eqn1[eqn_diff_idx],
    eqn_2 = eqn2[eqn_diff_idx],
    stringsAsFactors = FALSE
  )

  u1 <- ifelse(is.na(v1s[["units"]]), "", v1s[["units"]])
  u2 <- ifelse(is.na(v2s[["units"]]), "", v2s[["units"]])
  u_diff_idx <- u1 != u2
  units_changed <- data.frame(
    name = in_both[u_diff_idx],
    units_1 = u1[u_diff_idx],
    units_2 = u2[u_diff_idx],
    stringsAsFactors = FALSE
  )

  # Sim specs diff
  spec_fields <- c(
    "start", "stop", "dt", "save_at", "save_type", "save_n",
    "time_units", "method", "seed", "language", "only_stocks"
  )
  s1 <- sfm1[["sim_specs"]]
  s2 <- sfm2[["sim_specs"]]
  sim_specs_diff <- Filter(
    Negate(is.null),
    stats::setNames(
      lapply(spec_fields, function(f) {
        val1 <- s1[[f]]
        val2 <- s2[[f]]
        # Treat NULL and NA as equal to themselves
        both_null <- is.null(val1) && is.null(val2)
        if (both_null) {
          return(NULL)
        }
        vals_equal <- isTRUE(all.equal(val1, val2))
        if (vals_equal) {
          return(NULL)
        }
        list(sfm1 = val1, sfm2 = val2)
      }),
      spec_fields
    )
  )

  result <- list(
    labels = c(label1, label2),
    added = added,
    removed = removed,
    type_changed = type_changed,
    eqn_changed = eqn_changed,
    units_changed = units_changed,
    sim_specs_diff = sim_specs_diff,
    properties = list(
      sfm1 = model_properties(sfm1),
      sfm2 = model_properties(sfm2)
    )
  )

  class(result) <- "compare_sdbuildR"
  result
}


#' Print comparison of two stock-and-flow models
#'
#' @param x An object of class [`compare_sdbuildR`][compare_models()]
#' @param ... Additional arguments (unused)
#'
#' @returns Invisibly returns `x`.
#' @export
#' @concept build
#' @method print compare_sdbuildR
#'
print.compare_sdbuildR <- function(x, ...) {
  l1 <- x[["labels"]][1]
  l2 <- x[["labels"]][2]

  cli::cli_h1("Model Comparison: {l1} vs {l2}")

  # ── Structural Differences ──────────────────────────────────────
  cli::cli_h2("Structural Differences")

  n_added <- nrow(x[["added"]])
  n_removed <- nrow(x[["removed"]])
  n_type <- nrow(x[["type_changed"]])
  n_eqn <- nrow(x[["eqn_changed"]])
  n_units <- nrow(x[["units_changed"]])
  total_struct <- n_added + n_removed + n_type + n_eqn + n_units

  if (total_struct == 0L) {
    cli::cli_alert_success("No structural differences")
  } else {
    if (n_added > 0) {
      entries <- paste0(x[["added"]][["name"]], " [", x[["added"]][["type"]], "]")
      cli::cli_alert_success("Added ({n_added}): {.code {entries}}")
    }
    if (n_removed > 0) {
      entries <- paste0(x[["removed"]][["name"]], " [", x[["removed"]][["type"]], "]")
      cli::cli_alert_danger(
        "Removed ({n_removed}): {.code {entries}}"
      )
    }
    if (n_type > 0) {
      tc <- x[["type_changed"]]
      entries <- paste0(tc[["name"]], ": ", tc[["type_1"]], " \u2192 ", tc[["type_2"]])
      cli::cli_alert_warning("Type changed ({n_type}): {.code {entries}}")
    }
    if (n_eqn > 0) {
      ec <- x[["eqn_changed"]]
      for (i in seq_len(nrow(ec))) {
        cli::cli_alert_warning(
          "Equation changed: {.code {ec$name[i]}}: {.code {ec$eqn_1[i]}} \u2192 {.code {ec$eqn_2[i]}}"
        )
      }
    }
    if (n_units > 0) {
      uc <- x[["units_changed"]]
      entries <- paste0(uc[["name"]], ": \"", uc[["units_1"]], "\" \u2192 \"", uc[["units_2"]], "\"")
      cli::cli_alert_warning("Units changed ({n_units}): {entries}")
    }
  }

  # ── Simulation Settings ──────────────────────────────────────
  cli::cli_h2("Simulation Settings")
  if (length(x[["sim_specs_diff"]]) == 0L) {
    cli::cli_alert_success("Identical")
  } else {
    for (f in names(x[["sim_specs_diff"]])) {
      v1 <- x[["sim_specs_diff"]][[f]][["sfm1"]]
      v2 <- x[["sim_specs_diff"]][[f]][["sfm2"]]
      cli::cli_alert_warning("{f}: {.val {v1}} \u2192 {.val {v2}}")
    }
  }

  # ── Model Properties ──────────────────────────────────────────
  cli::cli_h2("Model Properties")

  p1 <- x[["properties"]][["sfm1"]]
  p2 <- x[["properties"]][["sfm2"]]

  fmt <- function(n) formatC(n, width = 8)
  row <- function(label, val1, val2) {
    cli::cli_text(
      paste0("  ", formatC(label, width = 24, flag = "-"), fmt(val1), fmt(val2))
    )
  }

  cli::cli_text(
    paste0(
      "  ", formatC("", width = 24, flag = "-"),
      formatC(l1, width = 8), formatC(l2, width = 8)
    )
  )
  cli::cli_text(paste0("  ", strrep("-", 40)))
  row("Stocks", p1[["n_stocks"]], p2[["n_stocks"]])
  row("Flows", p1[["n_flows"]], p2[["n_flows"]])
  row("Auxiliaries", p1[["n_aux"]], p2[["n_aux"]])
  row("Constants", p1[["n_constants"]], p2[["n_constants"]])
  row("Lookups", p1[["n_lookups"]], p2[["n_lookups"]])
  cli::cli_text(paste0("  ", strrep("-", 40)))
  row(
    "Nonlinearity score", p1[["nonlinearity"]][["score"]],
    p2[["nonlinearity"]][["score"]]
  )
  row(
    "  Lookup refs", p1[["nonlinearity"]][["n_lookup_refs"]],
    p2[["nonlinearity"]][["n_lookup_refs"]]
  )
  row(
    "  Nonlinear fns", p1[["nonlinearity"]][["n_nonlinear_fns"]],
    p2[["nonlinearity"]][["n_nonlinear_fns"]]
  )
  row(
    "  Multiplicative", p1[["nonlinearity"]][["n_multiplicative_dynamic"]],
    p2[["nonlinearity"]][["n_multiplicative_dynamic"]]
  )

  invisible(x)
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
      language = sim[["object"]][["sim_specs"]][["language"]],
      method = sim[["object"]][["sim_specs"]][["method"]]
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
  )

  list(
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
  )
}


#' Create data frame of simulation results
#'
#' Convert simulation results to a data.frame.
#'
#' @inheritParams plot.simulate_sdbuildR
#' @param direction Format of data frame, either "long" (default) or "wide".
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional Ignored parameter.
#'
#' @returns A data.frame with simulation results. For \code{direction = "long"} (default),
#'   the data frame has three columns: \code{time}, \code{variable}, and \code{value}.
#'   For \code{direction = "wide"}, the data frame has columns \code{time} followed by
#'   one column per variable.
#' @export
#' @seealso [`simulate()`][simulate.sdbuildR()], [sdbuildR()]
#' @concept build
#' @method as.data.frame simulate_sdbuildR
#'
#' @examples
#' sfm <- sdbuildR("SIR")
#' sim <- simulate(sfm)
#' df <- as.data.frame(sim)
#' head(df)
#'
#' # Get results in wide format
#' df_wide <- as.data.frame(sim, direction = "wide")
#' head(df_wide)
#'
as.data.frame.simulate_sdbuildR <- function(x,
                                            row.names = NULL, optional = FALSE,
                                            direction = "long", ...) {
  check_simulate_sdbuildR(x)

  direction <- trimws(tolower(direction))
  if (!direction %in% c("long", "wide")) {
    cli::cli_abort(c(
      "Invalid {.arg direction} argument.",
      "x" = "Must be either {.code 'long'} or {.code 'wide'}."
    ))
  }

  if (direction == "long") {
    df <- x[["df"]]
  } else if (direction == "wide") {
    df <- stats::reshape(x[["df"]],
      timevar = "variable",
      idvar = "time",
      direction = "wide"
    )

    # Remove value. prefix
    names(df) <- sub("^value\\.", "", names(df))

    # Remove row names
    rownames(df) <- NULL
  }

  # Handle row.names if provided
  if (!is.null(row.names)) {
    if (length(row.names) != nrow(df)) {
      cli::cli_abort(c(
        "Length mismatch in {.arg row.names}.",
        "x" = "Got {length(row.names)} name{?s} but {nrow(df)} row{?s}."
      ))
    }
    rownames(df) <- row.names
  }

  df
}


#' Print first rows of a simulation
#'
#' Print the first rows of a simulation data frame of a stock-and-flow model. This is a wrapper around [head()] that first converts the simulation results to a data frame using [as.data.frame()][as.data.frame.simulate_sdbuildR()].
#'
#' @inheritParams as.data.frame.simulate_sdbuildR
#' @param n Number of rows to print. Defaults to 6.
#' @param ... Other arguments passed to [as.data.frame.simulate_sdbuildR()].
#'
#' @returns A data.frame with the first rows of the simulation results.
#' @export
#' @importFrom utils head
#' @examples
#' sfm <- sdbuildR("SIR")
#' sim <- simulate(sfm)
#' head(sim)
head.simulate_sdbuildR <- function(x, n = 6L, ...) {
  check_simulate_sdbuildR(x)

  df <- as.data.frame(x, ...)
  head(df, n)
}


#' Print last rows of a simulation
#'
#' Print the last rows of a simulation data frame of a stock-and-flow model. This is a wrapper around [tail()] that first converts the simulation results to a data frame using [as.data.frame()][as.data.frame.simulate_sdbuildR()].
#'
#' @inheritParams as.data.frame.simulate_sdbuildR
#' @param n Number of rows to print. Defaults to 6.
#' @param ... Other arguments passed to [as.data.frame.simulate_sdbuildR()].
#' @return A data.frame with the last rows of the simulation results.
#' @export
#' @importFrom utils tail
#' @examples
#' sfm <- sdbuildR("SIR")
#' sim <- simulate(sfm)
#' tail(sim)
tail.simulate_sdbuildR <- function(x, n = 6L, ...) {
  check_simulate_sdbuildR(x)

  df <- as.data.frame(x, ...)
  tail(df, n)
}


#' Generate code to build stock-and-flow model
#'
#' Create R code to rebuild an existing stock-and-flow model. This may help to understand how a model is built, or to modify an existing one.
#'
#' @inheritParams update.sdbuildR
#'
#' @returns String with code to build stock-and-flow model from scratch.
#' @concept build
#' @export
#'
#' @examples
#' sfm <- sdbuildR("SIR")
#' cat(get_build_code(sfm))
#'
get_build_code <- function(object) {
  check_sdbuildR(object)

  # Simulation specifications — filter out defaults
  sim_specs_list <- object[["sim_specs"]]
  ss_defaults <- formals(sim_specs)
  ss_defaults <- ss_defaults[!names(ss_defaults) %in% c("object", "save_at", "save_n")]

  sim_specs_list <- sim_specs_list[vapply(names(sim_specs_list), function(nm) {
    val <- sim_specs_list[[nm]]
    # Omit save_type = "all" (the default) and NULL save_at/save_n
    if (nm == "save_type") {
      return(!identical(val, "all"))
    }
    if (nm %in% c("save_at", "save_n")) {
      return(!is.null(val))
    }
    !nm %in% names(ss_defaults) || !identical(val, ss_defaults[[nm]])
  }, logical(1))]

  sim_specs_list <- lapply(sim_specs_list, function(z) if (is.character(z)) paste0("\"", z, "\"") else z)
  sim_specs_str <- if (length(sim_specs_list) > 0) {
    paste0(" |>\n\tsim_specs(", paste0(names(sim_specs_list), " = ", unname(sim_specs_list), collapse = ", "), ")")
  } else {
    ""
  }

  # Model units — name and eqn are NSE in custom_unit() (bare expressions, no quotes)
  if (nrow(object[["custom_unit"]]) > 0) {
    cu_defaults <- formals(custom_unit)
    cu_defaults <- cu_defaults[!names(cu_defaults) %in% c("object", "name")]
    cu_default_eqn <- as.character(cu_defaults[["eqn"]]) # "1"
    cu_default_doc <- cu_defaults[["doc"]] # ""

    custom_unit_str <- apply(object[["custom_unit"]], 1, function(row) {
      row <- as.list(row)
      unit_name <- row[["name"]] # bare expression, no quotes

      args <- character(0)
      # eqn: unquoted bare expression; skip if default
      if (!is.null(row[["eqn"]]) && !identical(row[["eqn"]], cu_default_eqn)) {
        args <- c(args, paste0("eqn = ", row[["eqn"]]))
      }
      # doc: quoted string; skip if default ""
      if (!is.null(row[["doc"]]) && !identical(row[["doc"]], cu_default_doc)) {
        args <- c(args, paste0("doc = \"", row[["doc"]], "\""))
      }

      paste0("custom_unit(", paste(c(unit_name, args), collapse = ", "), ")")
    }) |>
      paste0(collapse = " |>\n\t")
    custom_unit_str <- paste0(" |>\n\t", custom_unit_str)
  } else {
    custom_unit_str <- ""
  }

  # Funcs (custom functions) — name is NSE (bare symbol, no quotes)
  func_df <- get_funcs(object)
  if (nrow(func_df) > 0) {
    func_cols <- intersect(c("name", "eqn", "units", "doc"), names(func_df))
    func_df <- func_df[, func_cols, drop = FALSE]

    cf_defaults <- formals(custom_func)
    cf_defaults <- cf_defaults[!names(cf_defaults) %in% c("object", "name", "label")]

    func_str <- vapply(seq_len(nrow(func_df)), function(i) {
      row <- as.list(func_df[i, , drop = FALSE])
      func_name <- row[["name"]] # bare symbol, no quotes
      row[["name"]] <- NULL

      # Filter out defaults
      row <- row[vapply(names(row), function(nm) {
        !nm %in% names(cf_defaults) || !identical(row[[nm]], cf_defaults[[nm]])
      }, logical(1))]

      args_str <- vapply(names(row), function(nm) {
        val <- row[[nm]]
        # eqn is NSE in custom_func() — emit unquoted
        if (nm == "eqn" || !is.character(val)) {
          paste0(nm, " = ", val)
        } else {
          paste0(nm, " = \"", val, "\"")
        }
      }, character(1))

      paste0("custom_func(", paste(c(func_name, args_str), collapse = ", "), ")")
    }, character(1)) |>
      paste0(collapse = " |>\n\t")

    func_str <- paste0(" |>\n\t", func_str)
  } else {
    func_str <- ""
  }

  # Meta-information string
  h <- object[["meta"]]
  defaults_meta <- formals(meta)
  defaults_meta <- defaults_meta[!names(defaults_meta) %in%
    c("object", "created", "...")]

  # Find which elements in h are identical to those in defaults_meta
  h <- h[vapply(names(h), function(name) {
    !name %in% names(defaults_meta) || !identical(
      h[[name]],
      defaults_meta[[name]]
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

  meta_str <- paste0(
    " |>\n\tmeta(",
    paste0(names(h), " = ", unname(h), collapse = ", "), ")"
  )

  # Variables — use type-specific helpers; name/to/from/source are NSE (bare symbols)
  # func-type variables are handled above via custom_func(), so exclude them here
  vars_df <- object[["variables"]]
  vars_df <- vars_df[vars_df[["type"]] != "func", , drop = FALSE]

  if (nrow(vars_df) > 0) {
    type_to_func <- c(
      stock = "stock", flow = "flow", constant = "constant",
      aux = "aux", lookup = "lookup"
    )

    # Args emitted unquoted — NSE in the target functions
    # nse_skip: also skip when empty/NA (variable cross-references)
    nse_skip <- c("to", "from", "source")
    # nse_expr: unquoted but filtered by defaults normally (expressions)
    nse_expr <- c("eqn")
    # Args stored as list columns containing numeric vectors
    vec_args <- c("xpts", "ypts")

    # Pre-compute defaults for each helper (exclude object, name, label, vec args)
    helper_defaults_list <- lapply(type_to_func, function(fn) {
      d <- formals(get(fn))
      d[!names(d) %in% c("object", "name", "label", vec_args, "...")]
    })

    keep_prop <- get_building_block_prop()

    var_str <- split(vars_df, seq_len(nrow(vars_df))) |>
      lapply(function(y) {
        z <- as.list(y)
        var_name <- z[["name"]]
        var_type <- z[["type"]]
        func_name <- type_to_func[[var_type]]
        helper_defaults <- helper_defaults_list[[var_type]]

        # Keep only relevant properties for this type, excluding name/type/_julia cols
        type_props <- setdiff(keep_prop[[var_type]], c("name", "type"))
        type_props <- type_props[!grepl("_julia", type_props)]
        z <- z[intersect(type_props, names(z))]

        # Skip label if it equals the variable name (default is label = name)
        if (!is.null(z[["label"]]) && identical(z[["label"]], var_name)) {
          z[["label"]] <- NULL
        }

        # Filter out defaults; skip empty/NA cross-references (nse_skip)
        z <- z[vapply(names(z), function(nm) {
          val <- z[[nm]]
          if (nm %in% vec_args) {
            if (is.list(val)) val <- val[[1]]
            return(length(val) > 0 && !all(is.na(val)))
          }
          if (nm %in% nse_skip) {
            return(!is.null(val) && !identical(val, "") && !is.na(val))
          }
          !nm %in% names(helper_defaults) || !identical(val, helper_defaults[[nm]])
        }, logical(1))]

        # Format each argument value
        args_str <- vapply(names(z), function(nm) {
          val <- z[[nm]]
          if (nm %in% vec_args) {
            if (is.list(val)) val <- val[[1]]
            formatted <- if (length(val) == 1) {
              as.character(val)
            } else {
              paste0("c(", paste(val, collapse = ", "), ")")
            }
            paste0(nm, " = ", formatted)
          } else if (nm %in% c(nse_skip, nse_expr)) {
            paste0(nm, " = ", val) # bare expression, no quotes
          } else if (is.character(val)) {
            paste0(nm, " = \"", val, "\"")
          } else {
            paste0(nm, " = ", val)
          }
        }, character(1))

        paste0(func_name, "(", paste(c(var_name, args_str), collapse = ", "), ")")
      })
    var_str <- paste0(" |>\n\t", paste0(unlist(var_str), collapse = " |>\n\t"))
  } else {
    var_str <- ""
  }

  script <- sprintf(
    "sfm <-\tsdbuildR()%s%s%s%s%s", sim_specs_str,
    meta_str, var_str, func_str, custom_unit_str
  )

  paste0(script, "\n")
}
