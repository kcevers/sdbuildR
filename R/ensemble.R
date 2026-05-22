#' Run ensemble simulations
#'
#' Run an ensemble simulation of a stock-and-flow model, varying initial
#' conditions and/or parameters in the conditions specified in `conditions`.
#' For Julia models, the ensemble can be run in parallel using multiple threads
#' by first setting `nthreads` in [use_julia()]. For R models, use
#' [future::plan()] to control parallel execution. The results are returned as a
#' data.frame with summary statistics and optionally individual simulations.
#'
#' To run large simulations, it is recommended to limit the output size by
#' saving fewer values with `save_at` or `save_n` in [sim_settings()]. To create a reproducible ensemble simulation, set a seed using [sim_settings()].
#'
#' If you do not see any variation within a condition of the ensemble (i.e. the
#' confidence bands are virtually non-existent), there are likely no random
#' elements in your model. Without these, there can be no variability in the
#' model. Try specifying a random initial condition or adding randomness to
#' other model elements (see examples).
#'
#' @inheritParams update.sdbuildR
#' @inheritParams simulate.sdbuildR
#' @param n Number of simulations to run in the ensemble. When conditions is
#'   specified, n defines the number of simulations to run per condition. If
#'   each condition only needs to be run once, set `n = 1`. Defaults to `n = 10`.
#' @param conditions A named list specifying parameter values for ensemble
#'   conditions. Names must correspond to existing stocks or constants
#'   in the model. Each list element should be a numeric vector of values
#'   to test.
#'
#'   If cross = `TRUE` (default), all combinations of values are generated. For
#'   example, list(param1 = c(1, 2), param2 = c(10, 20)) creates 4 conditions:
#'   (1,10), (1,20), (2,10), (2,20).
#'
#'   If cross = `FALSE`, values are paired element-wise, requiring all vectors to
#'   have equal length. For example, list(param1 = c(1, 2, 3), param2 =
#'   c(10, 20, 30)) creates 3 conditions: (1,10), (2,20), (3,30).
#'   Defaults to `NULL` (no parameter variation).
#'
#' @param cross If `TRUE`, cross the parameters in the conditions list to
#'   generate all possible combinations of parameters. Defaults to `TRUE`.
#' @param quantiles Quantiles to calculate in the summary, e.g. c(0.025,
#'   0.975).
#' @param verbose If `TRUE` (default), print details and duration of simulation.
#' @param ... Optional arguments passed to [sim_settings()]; these can be used to override the simulation specifications set in the model object.
#'
#' @returns Object of class [`ensemble_sdbuildR`][ensemble()], which is a list
#'   containing:
#' \describe{
#'  \item{success}{If `TRUE`, simulation was successful. If `FALSE`, simulation
#'    failed.}
#'
#'  \item{error_message}{If success is `FALSE`, contains the error message.}
#'  \item{df}{data.frame with simulation results in long format, if return_sims
#'    is `TRUE`. The iteration number is indicated by column "i". If conditions
#'    was specified, the condition is indicated by column "j".}
#'  \item{summary}{data.frame with summary statistics of the ensemble,
#'    including quantiles specified in quantiles. If conditions was specified,
#'    summary statistics are calculated for each condition (j) in the ensemble.}
#'  \item{n}{Number of simulations run in the ensemble (per condition j if
#'    conditions is specified).}
#'  \item{n_total}{Total number of simulations run in the ensemble (across all
#'    conditions if conditions is specified).}
#'  \item{n_conditions}{Total number of conditions.}
#'  \item{conditions}{data.frame with the conditions used in the ensemble, if
#'    conditions was specified.}
#'  \item{init}{List with df (if return_sims = TRUE) and summary, containing
#'    data.frame with the initial values of the stocks used in the ensemble.}
#'  \item{constants}{List with df (if return_sims = TRUE) and summary,
#'    containing data.frame with the constant parameters used in the ensemble.}
#'  \item{script}{Script used for the ensemble simulation.}
#'  \item{duration}{Duration of the simulation in seconds.}
#'  \item{...}{Other parameters passed to ensemble}
#'  }
#' @export
#' @concept ensemble
#' @seealso [update()], [sdbuildR()], [sim_settings()], [use_julia()],
#'   [future::plan()]
#'
#' @examples
#' # Ensemble simulation in R (no parallelization)
#' # Load example
#' sfm <- sdbuildR(predator_prey)
#'
#' # Set random initial conditions
#' sfm <- update(sfm, c(predator, prey),
#'   eqn = runif(1, min = 20, max = 80)
#' )
#'
#' # For ensemble simulations, it is highly recommended to reduce the
#' # returned output. For example, to save only 100 values per simulation:
#' sfm <- sim_settings(sfm, save_n = 100)
#'
#' # Run ensemble simulation with 10 simulations
#' sims <- ensemble(sfm, n = 10)
#' plot(sims)
#'
#' # To plot individual trajectories, rerun the ensemble with return_sims = TRUE.
#' # Note that this can consume a lot of memory for large simulations.
#' sims <- ensemble(sfm, n = 10, return_sims = TRUE)
#' plot(sims, type = "sims")
#'
#' # Specify which trajectories to plot
#' plot(sims, type = "sims", i = 1)
#'
#' # Plot the median with lighter individual trajectories
#' plot(sims, central_tendency = "median", type = "sims", alpha = 0.1)
#'
#' @examplesIf Sys.getenv("NOT_CRAN") == "true" && julia_env_ready()
#' # For larger ensembles, we can use parallelization with future
#' if (requireNamespace("future", quietly = TRUE) &&
#'   requireNamespace("future.apply", quietly = TRUE)) {
#'   future::plan(future::multisession, workers = 4)
#' }
#'
#' # Ensembles can also be run with exact values for the initial conditions
#' # and parameters. Below, we vary the initial values of the predator and the
#' # birth rate of the predators (delta). We generate a hundred samples per
#' # condition. By default, the parameters are crossed, meaning that all
#' # combinations of the parameters are run.
#' sims <- ensemble(sfm,
#'   n = 50,
#'   conditions = list(predator = c(10, 50), delta = c(.025, .05))
#' )
#'
#' plot(sims)
#'
#' # By default, a maximum of nine conditions is plotted.
#' # Plot specific conditions:
#' plot(sims, j = c(1, 3), nrows = 1)
#'
#' # Generate a non-crossed design, where the length of each conditions vector
#' # needs to be equal:
#' sims <- ensemble(sfm,
#'   n = 10, cross = FALSE,
#'   conditions = list(
#'     predator = c(10, 20, 30),
#'     delta = c(.020, .025, .03)
#'   )
#' )
#' plot(sims, nrows = 3)
#'
#' # Stop parallelization after use
#' if (requireNamespace("future", quietly = TRUE) &&
#'   requireNamespace("future.apply", quietly = TRUE)) {
#'   future::plan(future::sequential)
#' }
#'
ensemble <- function(object,
                     n = 10,
                     conditions = NULL,
                     cross = TRUE,
                     quantiles = c(0.025, 0.975),
                     verbose = TRUE, ...) {
  check_sdbuildR(object)

  # Override sim_settings with any arguments passed via ...
  varargs <- list(...)
  if (length(varargs) > 0) {
    object <- do.call(sim_settings, c(list(object), varargs))
  }
  language <- tolower(object[["sim_settings"]][["language"]])
  only_stocks <- object[["sim_settings"]][["only_stocks"]]
  vars <- object[["sim_settings"]][["vars"]]

  # Persistent meta-setting: read return_sims from sim_settings AFTER applying varargs
  return_sims <- isTRUE(object[["sim_settings"]][["return_sims"]])

  if (!is.null(vars)) {
    vars <- validate_sim_vars(object, vars)
    stock_names <- get_variables_by_type(object, "stock")[["name"]]
    only_stocks <- all(vars %in% stock_names)
  }

  if (!is.numeric(n)) {
    cli::cli_abort(c(
      "x" = "The {.arg n} argument must be {.cls numeric}.",
      "i" = "Received: {.cls {typeof(n)}}"
    ))
  }

  if (n <= 0) {
    cli::cli_abort(c(
      "x" = "The {.arg n} argument must be greater than {.val {0}}."
    ))
  }

  if (!is.numeric(quantiles)) {
    cli::cli_abort(c(
      "x" = "The {.arg quantiles} argument must be {.cls numeric}.",
      "i" = "Received: {.cls {typeof(quantiles)}}",
      ">" = "Use a numeric vector, e.g., {.code quantiles = c(0.025, 0.975)}."
    ))
  }

  if (length(unique(quantiles)) < 2) {
    cli::cli_abort(c(
      "x" = "The {.arg quantiles} argument must have at least {.val {2}} unique values.",
      "i" = "Received {.val {length(unique(quantiles))}} unique value(s).",
      ">" = "Provide at least 2 quantiles, e.g., {.code quantiles = c(0.025, 0.975)}."
    ))
  }

  if (any(quantiles < 0 | quantiles > 1)) {
    cli::cli_abort(c(
      "x" = "All values in {.arg quantiles} must be between {.val {0}} and {.val {1}}.",
      "i" = "Quantiles represent probabilities and must be proportions.",
      ">" = "Use values like {.code c(0.025, 0.5, 0.975)} for 2.5%, 50%, 97.5% quantiles."
    ))
  }

  if (!is.logical(cross)) {
    cli::cli_abort(c(
      "x" = "The {.arg cross} argument must be {.code TRUE} or {.code FALSE}.",
      "i" = "Received: {.val {cross}}",
      ">" = "Use {.code cross = TRUE} for all combinations or {.code cross = FALSE} for paired values."
    ))
  }


  if (!is.null(conditions)) {
    if (!is.list(conditions)) {
      cli::cli_abort(c(
        "x" = "The {.arg conditions} argument must be a {.cls list}.",
        "i" = "Received: {.cls {typeof(conditions)}}",
        ">" = "Use format: {.code conditions = list(param1 = c(1, 2), param2 = c(10, 20))}."
      ))
    }

    if (length(conditions) == 0) {
      cli::cli_abort(c(
        "x" = "The {.arg conditions} argument must have at least one parameter.",
        ">" = "Specify like: {.code conditions = list(param = c(1, 2, 3))}."
      ))
    }

    if (is.null(names(conditions))) {
      cli::cli_abort(c(
        "x" = "The {.arg conditions} list elements must be named.",
        "i" = "Names correspond to parameter/stock names in your model.",
        ">" = "Use: {.code conditions = list(paramname = values, ...)}."
      ))
    }

    # All must be numerical values
    if (!all(vapply(conditions, is.numeric, logical(1)))) {
      cli::cli_abort(c(
        "x" = "All {.arg conditions} elements must be {.cls numeric} vectors.",
        ">" = "Example: {.code conditions = list(param1 = c(1, 2, 3))}."
      ))
    }

    # Test that names are unique
    if (length(unique(names(conditions))) != length(conditions)) {
      cli::cli_abort(c(
        "x" = "All {.arg conditions} names must be unique."
      ))
    }

    # All varied elements must exist in the model
    names_df <- get_names(object)
    names_conditions <- names(conditions)
    allowed_names <- names_df[names_df[["type"]] %in% c("stock", "constant"), "name"]
    idx <- names_conditions %in% names_df[["name"]]
    if (any(!idx)) {
      missing_names <- names_conditions[!idx]
      cli::cli_abort(c(
        "Unknown parameters in {.arg conditions}.",
        "x" = "The following parameters do not exist in the model: {paste0('{.code ', missing_names, '}', collapse = ', ')}.",
        "i" = "Check spelling and ensure parameters are defined in the model.",
        ">" = "Available variables to vary: {paste0(allowed_names, collapse = ', ')}"
      ))
    }

    # All varied elements must be a stock or constant
    idx <- names_conditions %in% c(names_df[names_df[["type"]] %in% c("stock", "constant"), "name"])
    if (any(!idx)) {
      invalid_names <- names_conditions[!idx]
      cli::cli_abort(c(
        "Cannot vary flows or auxiliaries, only stocks and constants.",
        "!" = "Cannot vary: {paste0('{.code ', invalid_names, '}', collapse = ', ')}.",
        ">" = "Available variables to vary: {paste0(allowed_names, collapse = ', ')}"
      ))
    }

    # All conditions vectors must be of the same length if not a crossed design
    conditions_lengths <- vapply(conditions, length, numeric(1))
    if (!cross) {
      if (length(unique(conditions_lengths)) != 1) {
        cli::cli_abort(c(
          "Mismatched conditions lengths with {.arg cross = FALSE}.",
          "x" = "When {.arg cross = FALSE}, all conditions vectors must have equal length.",
          "i" = "Found lengths: {paste0(unique(conditions_lengths), collapse = ', ')} for parameters {paste0(names(conditions), collapse = ', ')}.",
          ">" = "Either use {.code cross = TRUE} or equalize all conditions vectors."
        ))
      }

      n_conditions <- unique(conditions_lengths)
    } else {
      # Compute the total number of conditions
      n_conditions <- prod(conditions_lengths)
    }

    # Alphabetically sort the ensemble parameters
    conditions <- conditions[sort(names(conditions))]
  } else {
    n_conditions <- 1
  }

  total_sims <- n * n_conditions
  if (verbose) {
    sim_word <- ifelse(total_sims == 1, "simulation", "simulations")
    if (is.null(conditions)) {
      msg <- c(
        "Starting ensemble simulation with {.val {total_sims}} {sim_word}."
      )
    } else {
      cond_word <- ifelse(n_conditions == 1, "condition", "conditions")
      sim_per_word <- ifelse(n == 1, "simulation", "simulations")
      msg <- c(
        "Starting ensemble simulation with {.val {total_sims}} {sim_word} in total.",
        "i" = "{.val {n_conditions}} {cond_word} x {.val {n}} {sim_per_word} per condition."
      )
    }
    cli::cli_inform(msg)
  }

  # Dispatch to language-specific backend
  if (language == "julia") {
    return(ensemble_julia(
      object = object, n = n, return_sims = return_sims,
      conditions = conditions, cross = cross, quantiles = quantiles,
      only_stocks = only_stocks, vars = vars, verbose = verbose,
      n_conditions = n_conditions, total_sims = total_sims
    ))
  } else if (language == "r") {
    return(ensemble_r(
      object = object, n = n, return_sims = return_sims,
      conditions = conditions, cross = cross, quantiles = quantiles,
      only_stocks = only_stocks, vars = vars, verbose = verbose,
      n_conditions = n_conditions, total_sims = total_sims
    ))
  } else {
    cli::cli_abort(c(
      "Unsupported simulation language: {.val {language}}.",
      "i" = "Ensemble simulations support {.code 'R'} and {.code 'Julia'}.",
      ">" = "Set via {.fn sim_settings}(object, language = {.code 'R'}) or {.code 'Julia'}."
    ))
  }
}


#' Check class [`ensemble_sdbuildR`][ensemble()]
#'
#' @param x An ensemble simulation of class [`ensemble_sdbuildR`][ensemble()]
#'
#' @returns Invisible x if valid, otherwise an error is thrown
#' @noRd
#'
check_ensemble_sdbuildR <- function(x) {
  if (!inherits(x, "ensemble_sdbuildR")) {
    cli::cli_abort(c(
      "Invalid object type.",
      "x" = "Expected object of class {.cls ensemble_sdbuildR}.",
      "i" = "Use {.fn ensemble} to create a valid ensemble object."
    ))
  }

  if (!is.logical(x$success) || length(x$success) != 1) {
    cli::cli_abort(c(
      "Invalid {.arg success} field.",
      "x" = "The {.arg success} field must be a single {.cls logical} value.",
      "i" = "Expected {.val {TRUE}} or {.val {FALSE}}."
    ))
  }

  if (x$success) {
    if (is.null(x$summary) || !is.data.frame(x$summary)) {
      cli::cli_abort(c(
        "Missing or invalid summary data.",
        "x" = "Successful ensemble must have a {.cls data.frame} in {.arg summary}.",
        "i" = "This field is populated by {.fn ensemble} with summary statistics."
      ))
    }
    if (is.null(x$duration)) {
      cli::cli_abort(c(
        "Missing simulation duration.",
        "x" = "Successful ensemble must have {.arg duration}."
      ))
    }
  } else {
    if (is.null(x$error_message) || !nzchar(x$error_message)) {
      cli::cli_abort(c(
        "Missing error message.",
        "x" = "Failed ensemble must have a non-empty {.arg error_message}."
      ))
    }
  }

  invisible(x)
}


#' @export
#' @concept ensemble
#' @method print ensemble_sdbuildR
print.ensemble_sdbuildR <- function(x, ...) {
  model_name <- x[["object"]][["meta"]][["name"]]
  default_name <- formals(meta)[["name"]]
  has_name <- !is.null(model_name) && nzchar(model_name) && model_name != default_name

  if (has_name) {
    cli::cli_h1("Stock-and-Flow Ensemble Simulation: {model_name}")
  } else {
    cli::cli_h1("Stock-and-Flow Ensemble Simulation")
  }

  has_counts <- all(!vapply(x[c("n_total", "n_conditions", "n")], is.null, logical(1)))
  if (has_counts) {
    cond_word <- if (x[["n_conditions"]] == 1) "condition" else "conditions"
    cli::cli_inform(c("i" = "{x[['n_total']]} total simulations \u2022 {x[['n_conditions']]} {cond_word} \u2022 {x[['n']]} per condition"))
  }

  if (x[["success"]]) {
    cli::cli_alert_success("Completed in {round(as.numeric(x[['duration']]), 4)} seconds")

    if (!is.null(x[["conditions"]])) {
      changed_pars <- setdiff(colnames(x[["conditions"]]), "j")
      if (length(changed_pars) > 0) {
        cli::cli_inform(c(
          "i" = "Parameters changed across conditions: {paste0(changed_pars, collapse = ', ')}"
        ))
      }
    }

    has_df <- !is.null(x[["df"]])
    sims_saved <- if (has_df) "yes" else "no"
    cli::cli_inform(c(
      "i" = "Individual simulations saved: {sims_saved}"
    ))

    n_time <- NA_integer_
    if (!is.null(x[["summary"]]) && "time" %in% names(x[["summary"]])) {
      n_time <- length(unique(x[["summary"]][["time"]]))
    } else if (has_df && "time" %in% names(x[["df"]])) {
      n_time <- length(unique(x[["df"]][["time"]]))
    }
    if (!is.na(n_time)) {
      cli::cli_inform(c(
        "i" = "Time points saved per simulation: {n_time}"
      ))
    }
  } else {
    cli::cli_alert_danger("Simulation failed")
    cli::cli_inform(c(
      "i" = "Inspect the error message with: {.code x$error_message}"
    ))
  }
  invisible(x)
}


#' @export
#' @concept ensemble
#' @method as.data.frame ensemble_sdbuildR
as.data.frame.ensemble_sdbuildR <- function(x, row.names = NULL, optional = FALSE,
                                            type = c("summary", "sims"),
                                            direction = "long",
                                            i = NULL,
                                            j = NULL,
                                            ...) {
  check_ensemble_sdbuildR(x)

  type <- match.arg(type)

  direction <- trimws(tolower(direction))
  if (!direction %in% c("long", "wide")) {
    cli::cli_abort(c(
      "Invalid {.arg direction} argument.",
      "x" = "Must be either {.code 'long'} or {.code 'wide'}."
    ))
  }

  # Validate j
  if (!is.null(j)) {
    .check_j_index(j, x[["n_conditions"]])
  }

  if (type == "sims") {
    if (is.null(x[["df"]])) {
      cli::cli_abort(c(
        "No individual simulation data available.",
        "!" = "Re-run {.fn ensemble} with {.code return_sims = TRUE}."
      ))
    }
    df <- x[["df"]]

    # Validate and apply i filter
    if (!is.null(i)) {
      .check_i_index(i, x[["n"]])
      df <- df[df[["i"]] %in% i, , drop = FALSE]
    }

    # Apply j filter
    if (!is.null(j)) {
      df <- df[df[["j"]] %in% j, , drop = FALSE]
    }

    if (direction == "wide") {
      df <- stats::reshape(df,
        timevar = "variable", idvar = c("j", "i", "time"),
        direction = "wide"
      )
      names(df) <- sub("^value\\.", "", names(df))
      rownames(df) <- NULL
    }
  } else if (type == "summary") {
    if (!is.null(i)) {
      cli::cli_inform(c(
        "i" = "The {.arg i} argument is ignored for {.code type = 'summary'}.",
        ">" = "Set {.code type = 'sims'} to filter by individual trajectory."
      ))
    }
    df <- x[["summary"]]

    # Apply j filter
    if (!is.null(j)) {
      df <- df[df[["j"]] %in% j, , drop = FALSE]
    }

    if (direction == "wide") {
      df <- stats::reshape(df,
        timevar = "variable", idvar = c("j", "time"),
        direction = "wide"
      )
      rownames(df) <- NULL
    }
  }

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

#' @export
#' @concept ensemble
#' @method head ensemble_sdbuildR
head.ensemble_sdbuildR <- function(x, n = 6L, ...) {
  check_ensemble_sdbuildR(x)
  df <- as.data.frame(x, ...)
  head(df, n)
}


#' @export
#' @concept ensemble
#' @method tail ensemble_sdbuildR
tail.ensemble_sdbuildR <- function(x, n = 6L, ...) {
  check_ensemble_sdbuildR(x)
  df <- as.data.frame(x, ...)
  tail(df, n)
}


#' @export
#' @concept ensemble
#' @method summary ensemble_sdbuildR
summary.ensemble_sdbuildR <- function(object, ...) {
  check_ensemble_sdbuildR(object)
  object[["summary"]]
}


#' Constructor for [`ensemble_sdbuildR`][ensemble()]
#'
#' @noRd
new_ensemble_sdbuildR <- function(success = FALSE,
                                  error_message = NULL,
                                  df = NULL,
                                  summary = NULL,
                                  n = NULL,
                                  n_total = NULL,
                                  n_conditions = NULL,
                                  conditions = NULL,
                                  init = NULL,
                                  constants = NULL,
                                  script = NULL,
                                  duration = NULL,
                                  cross = TRUE,
                                  quantiles = NULL,
                                  object = NULL) {
  obj <- list(
    success = success,
    error_message = error_message,
    df = df,
    summary = summary,
    n = n,
    n_total = n_total,
    n_conditions = n_conditions,
    conditions = conditions,
    init = init,
    constants = constants,
    script = script,
    duration = duration,
    cross = cross,
    quantiles = quantiles,
    object = object
  )
  structure(obj, class = "ensemble_sdbuildR")
}


#' Deep validator for [`ensemble_sdbuildR`][ensemble()]
#'
#' @noRd
validate_ensemble_sdbuildR <- function(x) {
  check_ensemble_sdbuildR(x)

  # Check all required fields are present
  required <- c(
    "success", "error_message", "df", "summary", "n", "n_total",
    "n_conditions", "conditions", "init", "constants", "script",
    "duration", "cross", "quantiles", "object"
  )
  missing_fields <- setdiff(required, names(x))
  if (length(missing_fields) > 0) {
    cli::cli_warn(c(
      "Ensemble object is missing fields.",
      "!" = "Missing: {paste0('{.field ', missing_fields, '}', collapse = ', ')}."
    ))
  }

  if (x[["success"]]) {
    # Validate summary structure
    expected_summary_cols <- c("j", "variable", "time", "mean", "median")
    missing_summary_cols <- setdiff(expected_summary_cols, names(x[["summary"]]))
    if (length(missing_summary_cols) > 0) {
      cli::cli_warn(c(
        "Ensemble {.arg summary} is missing expected columns.",
        "!" = "Missing: {paste0('{.field ', missing_summary_cols, '}', collapse = ', ')}."
      ))
    }

    q_cols <- grep("^q", names(x[["summary"]]), value = TRUE)
    if (length(q_cols) == 0) {
      cli::cli_warn(c(
        "Ensemble {.arg summary} has no quantile columns.",
        "i" = "Expected columns prefixed with {.code q}."
      ))
    }

    # Validate individual sims df structure (if present)
    if (!is.null(x[["df"]])) {
      expected_df_cols <- c("i", "j", "variable", "time", "value")
      missing_df_cols <- setdiff(expected_df_cols, names(x[["df"]]))
      if (length(missing_df_cols) > 0) {
        cli::cli_warn(c(
          "Ensemble {.arg df} is missing expected columns.",
          "!" = "Missing: {paste0('{.field ', missing_df_cols, '}', collapse = ', ')}."
        ))
      }
    }

    # Validate n fields are numeric
    for (field in c("n", "n_total", "n_conditions")) {
      if (!is.null(x[[field]]) && !is.numeric(x[[field]])) {
        cli::cli_warn(c(
          "Field {.field {field}} should be {.cls numeric}.",
          "i" = "Received: {.cls {class(x[[field]])}}"
        ))
      }
    }
  }

  invisible(x)
}
