#' @noRd
.validate_sim_numeric <- function(x, arg_name) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument.",
      "x" = "The {.arg {arg_name}} argument must be {.cls numeric}."
    ))
  }
  x
}


#' Validate and cross-check time-related simulation arguments
#'
#' Receives only the arguments the user explicitly supplied (as a named list),
#' coerces them to numeric, applies cross-argument checks, performs
#' auto-corrections, and strips scientific notation. Falls back to the values
#' currently stored in `sfm` when a counterpart argument was not supplied.
#'
#' @param sfm An `sdbuildR` model object (used for fallback values only).
#' @param args Named list of time args supplied by the caller.  Only keys
#'   present in the list are treated as user-supplied; absent keys are not
#'   validated and their current sfm values are used purely as fallbacks.
#' @return The validated, possibly auto-corrected named list.
#' @noRd
.validate_sim_time_args <- function(sfm, args) {
  time_args <- c("start", "stop", "dt", "save_at", "save_from")

  # Record which args the user explicitly supplied before any auto-corrections.
  # Range checks for auto-set args (e.g. save_at synced from dt) are skipped
  # to allow edge cases like save_from == stop (single time-point output).
  user_supplied <- names(args)

  # Step 1: coerce each provided arg to numeric
  for (nm in intersect(names(args), time_args)) {
    args[[nm]] <- .validate_sim_numeric(args[[nm]], nm)
  }

  # Positive-value checks (must come after coercion)
  if ("dt" %in% names(args) && args$dt <= 0) {
    cli::cli_abort(c(
      "Invalid {.arg dt} argument.",
      "x" = "{.arg dt} argument must be positive."
    ))
  }
  if ("save_at" %in% names(args) && args$save_at <= 0) {
    cli::cli_abort(c(
      "Invalid {.arg save_at} argument.",
      "x" = "{.arg save_at} argument must be positive."
    ))
  }

  # Effective values: user-provided, or fall back to current sfm value
  eff <- function(nm) args[[nm]] %||% as.numeric(sfm[["sim_specs"]][[nm]])

  # Step 2: warn when dt is large (> 0.1, excluding exactly 1)
  if ("dt" %in% names(args) && args$dt != 1 && args$dt > 0.1) {
    cli::cli_warn(c(
      "Large timestep detected ({.arg dt} = {.val {args$dt}}).",
      "i" = "This may lead to simulation inaccuracies.",
      ">" = "Consider using smaller timesteps for better accuracy."
    ))
  }

  # Step 3: start < stop
  if ("start" %in% names(args) || "stop" %in% names(args)) {
    eff_start <- eff("start")
    eff_stop  <- eff("stop")
    if (eff_start >= eff_stop) {
      cli::cli_abort(c(
        "Invalid time interval.",
        "x" = "{.arg start} ({.val {eff_start}}) must be smaller than {.arg stop} ({.val {eff_stop}})."
      ))
    }
  }

  # Step 4: dt must fit within the simulation window
  if ("dt" %in% names(args)) {
    eff_start <- eff("start")
    eff_stop  <- eff("stop")
    if (args$dt > (eff_stop - eff_start)) {
      cli::cli_abort(c(
        "Invalid {.arg dt} argument.",
        "x" = "{.arg dt} ({.val {args$dt}}) must be smaller than the time interval ({.arg stop} - {.arg start} = {.val {eff_stop - eff_start}})."
      ))
    }
  }

  # Step 5: dt vs save_at auto-correction
  if ("dt" %in% names(args)) {
    if ("save_at" %in% names(args)) {
      # Both passed: warn and auto-correct if dt > save_at
      if (args$dt > args$save_at) {
        cli::cli_warn(c(
          "Invalid {.arg dt} and {.arg save_at} relationship.",
          "x" = "{.arg dt} ({.val {args$dt}}) must be <= {.arg save_at} ({.val {args$save_at}}).",
          "i" = "Automatically setting {.arg save_at} equal to {.arg dt}."
        ))
        args$save_at <- args$dt
      }
    } else {
      # Only dt passed: sync save_at to dt when existing save_at < new dt,
      # or when save_at has not been set yet
      eff_save_at <- as.numeric(sfm[["sim_specs"]][["save_at"]])
      if (!is_defined(sfm[["sim_specs"]][["save_at"]]) || args$dt > eff_save_at) {
        args$save_at <- args$dt
      }
      # else: existing save_at is already >= new dt — leave it unchanged
    }
  } else if ("save_at" %in% names(args)) {
    # Only save_at passed: auto-correct if save_at < current sfm dt (bug fix:
    # use the sfm's stored dt, not the function's default parameter value)
    eff_dt <- as.numeric(sfm[["sim_specs"]][["dt"]])
    if (is_defined(sfm[["sim_specs"]][["dt"]]) && args$save_at < eff_dt) {
      cli::cli_warn(c(
        "Invalid {.arg dt} and {.arg save_at} relationship.",
        "x" = "{.arg dt} must be smaller than or equal to {.arg save_at}.",
        "i" = "Automatically setting {.arg save_at} equal to {.arg dt}."
      ))
      args$save_at <- eff_dt
    }
  }

  # Step 6: save_at range validation — only when save_at was explicitly supplied.
  # Auto-set values (synced from dt) are not range-checked here to allow edge
  # cases such as save_from == stop (single-point output at final time).
  if ("save_at" %in% user_supplied) {
    eff_start     <- eff("start")
    eff_stop      <- eff("stop")
    eff_save_from <- args$save_from %||% as.numeric(sfm[["sim_specs"]][["save_from"]])
    if (args$save_at > (eff_stop - eff_start)) {
      cli::cli_abort(c(
        "Invalid {.arg save_at} argument.",
        "x" = "{.arg save_at} ({.val {args$save_at}}) must be smaller than the time interval ({.arg stop} - {.arg start} = {.val {eff_stop - eff_start}})."
      ))
    }
    if (args$save_at > (eff_stop - eff_save_from)) {
      cli::cli_abort(c(
        "Invalid {.arg save_at} argument.",
        "x" = "{.arg save_at} ({.val {args$save_at}}) must be smaller than the interval from {.arg save_from} ({.val {eff_save_from}}) to {.arg stop} ({.val {eff_stop}})."
      ))
    }
  }

  # Step 7: save_from auto-follows start when start changes but save_from does not
  if ("start" %in% names(args) && !"save_from" %in% names(args)) {
    args$save_from <- args$start
  }

  # Step 8: save_from must lie within [start, stop]
  if ("save_from" %in% names(args)) {
    eff_start <- eff("start")
    eff_stop  <- eff("stop")
    if (args$save_from < eff_start || args$save_from > eff_stop) {
      cli::cli_abort(c(
        "Invalid {.arg save_from} argument.",
        "x" = "{.arg save_from} ({.val {args$save_from}}) must be within the simulation time interval.",
        "i" = "Must satisfy: {.val {eff_start}} <= {.arg save_from} <= {.val {eff_stop}}"
      ))
    }
  }

  # Step 9: strip scientific notation from all numeric time args
  for (nm in intersect(names(args), time_args)) {
    args[[nm]] <- replace_digits_with_floats(scientific_notation(args[[nm]]), NULL)
  }

  args
}


#' Modify simulation specifications
#'
#' Simulation specifications are the settings that determine how the model is simulated, such as the integration method (i.e. solver), start and stop time, and timestep. Modify these specifications for an existing stock-and-flow model.
#'
#' @inheritParams build
#' @param method Integration method. Defaults to "euler".
#' @param start Start time of simulation. Defaults to 0.
#' @param stop End time of simulation. Defaults to 100.
#' @param dt Timestep of solver; controls simulation accuracy. Smaller = more
#'   accurate but slower. Defaults to 0.01.
#' @param save_at Timestep at which to save computed values; controls output size.
#'   Must be >= dt. Use larger than dt to reduce memory without sacrificing accuracy.
#'   Example: dt = 0.01, save_at = 1 gives accurate simulation but only saves
#'   every 100th point. Defaults to `dt` (i.e., save everything).
#' @param save_from Time at which to start saving values. Use to discard initial
#'   transient behavior. Must be >= start. Defaults to `start`.
#' @param seed Seed number to ensure reproducibility across runs in case of random elements. Must be an integer. Defaults to NULL (no seed).
#' @param time_units Simulation time unit, e.g. 's' (second). Defaults to "s".
#' @param language Coding language in which to simulate model. Either "R" or "Julia". Julia is necessary for using units or delay functions. Defaults to "R".
#' @param keep_nonnegative_stock If TRUE, keeps original non-negativity setting of stocks. Defaults to FALSE.
#' @param keep_nonnegative_flow If TRUE, keeps original non-negativity setting of flows. Defaults to TRUE.
#' @param keep_unit If TRUE, keeps units of variables. Defaults to TRUE.
#'
#' @returns A stock-and-flow model object of class [`sdbuildR`][sdbuildR]
#' @concept simulate
#' @seealso [solvers()]
#' @export
#'
#' @examples
#' sfm <- sdbuildR("predator_prey") |>
#'   sim_specs(start = 0, stop = 50, dt = 0.1)
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # Change the simulation method to "rk4"
#' sfm <- sim_specs(sfm, method = rk4)
#'
#' # Change the time units to "years", such that one time unit is one year
#' sfm <- sim_specs(sfm, time_units = years)
#'
#' # To save storage but not affect accuracy, use save_at and save_from
#' sfm <- sim_specs(sfm, save_at = 1, save_from = 10)
#' sim <- simulate(sfm)
#' head(as.data.frame(sim))
#'
#' # Add stochastic initial condition but specify seed to obtain same result
#' sfm <- sim_specs(sfm, seed = 1) |>
#'   build(c("predator", "prey"), eqn = "runif(1, 20, 50)")
#'
#' # Change the simulation language to Julia to use units
#' sfm <- sim_specs(sfm, language = Julia)
#'
sim_specs <- function(sfm,
                      method = "euler",
                      start = "0.0",
                      stop = "100.0",
                      dt = "0.01",
                      save_at = dt,
                      save_from = start,
                      # adaptive = FALSE,
                      seed = NULL,
                      time_units = "s",
                      language = "R",
                      keep_nonnegative_stock = FALSE,
                      keep_nonnegative_flow = TRUE,
                      keep_unit = TRUE) {
  # Basic check
  if (missing(sfm)) {
    missing_arg("sfm")
  }
  check_sdbuildR(sfm)

  # NSE: allow bare symbols, e.g. sim_specs(sfm, language = Julia, method = rk4)
  if (!missing(time_units)) time_units <- .expr_to_char(rlang::enexpr(time_units))
  if (!missing(method))     method     <- .expr_to_char(rlang::enexpr(method))
  if (!missing(language))   language   <- .expr_to_char(rlang::enexpr(language))

  # Collect and validate all time-related numeric args
  time_vals <- list()
  if (!missing(start))     time_vals$start     <- start
  if (!missing(stop))      time_vals$stop      <- stop
  if (!missing(dt))        time_vals$dt        <- dt
  if (!missing(save_at))   time_vals$save_at   <- save_at
  if (!missing(save_from)) time_vals$save_from <- save_from
  time_vals <- .validate_sim_time_args(sfm, time_vals)

  # Ensure time_units are formatted correctly
  if (!missing(time_units)) {
    if (length(time_units) != 1) {
      cli::cli_abort(c(
        "Invalid {.arg time_units} argument.",
        "x" = "The {.arg time_units} argument must be a single {.cls character} string."
      ))
    }

    # Time units can only contain letters, spaces, or underscores
    if (any(grepl("[^a-zA-Z _]", time_units))) {
      cli::cli_abort(c(
        "Invalid {.arg time_units} format.",
        "x" = "The {.arg time_units} argument can only contain letters, spaces, or underscores."
      ))
    }
    regex_time_units <- get_regex_time_units()
    time_units <- clean_unit(time_units, regex_time_units)

    if (!any(time_units == unname(regex_time_units))) {
      cli::cli_abort(c(
        "Invalid time unit {.val {time_units}}.",
        "i" = "Available time units are: {paste0(unique(unname(regex_time_units)), collapse = ', ')}"
      ))
    }
  }

  # Validate method
  method_auto_set <- FALSE
  if (!missing(method)) {
    if (is.null(method) || any(is.na(method)) || !inherits(method, "character") || length(method) > 1) {
      cli::cli_abort(c(
        "Invalid {.arg method} argument.",
        "x" = "The {.arg method} argument must be a single {.cls character} string."
      ))
    }
    method <- trimws(method)
  }

  # Check coding language and translate method if needed
  if (!missing(language)) {
    language <- clean_language(language)

    old_language <- sfm[["sim_specs"]][["language"]]
    if (missing(method) && language != old_language) {
      # Method not specified: translate the current method to the new language
      method <- solvers(sfm[["sim_specs"]][["method"]],
        from = old_language, to = language,
        show_info = TRUE
      )
      if (is.null(method[["translation"]])) {
        method <- method[["alternatives"]][1]
      } else {
        method <- method[["translation"]]
      }
      method_auto_set <- TRUE
    } else if (!missing(method)) {
      # Method specified: validate it against the new language
      method <- solvers(method, from = language, show_info = TRUE)
      method <- method[["name"]]
    }
  } else if (!missing(method)) {
    # Language not changing: validate method against the current language
    method <- solvers(method, from = sfm[["sim_specs"]][["language"]], show_info = TRUE)
    method <- method[["name"]]
  }

  # Seed must be NULL or an integer
  if (!missing(seed) && !is.null(seed)) {
    if (nzchar(seed)) {
      seed <- strtoi(seed)
      if (is.na(seed)) {
        cli::cli_abort(c(
          "Invalid {.arg seed} argument.",
          "x" = "The {.arg seed} argument must be an {.cls integer}."
        ))
      }
      seed <- as.character(seed)
    } else {
      seed <- NULL
    }
  }

  # Collect all validated args to write into sfm
  argg <- time_vals
  if (!missing(method) || method_auto_set)     argg$method                 <- method
  if (!missing(time_units))                    argg$time_units             <- time_units
  if (!missing(language))                      argg$language               <- language
  if (!missing(keep_nonnegative_stock))        argg$keep_nonnegative_stock <- keep_nonnegative_stock
  if (!missing(keep_nonnegative_flow))         argg$keep_nonnegative_flow  <- keep_nonnegative_flow
  if (!missing(keep_unit))                     argg$keep_unit              <- keep_unit
  # seed handled separately: c() preserves NULL elements, unlike $ assignment
  if (!missing(seed))                          argg <- c(argg, list(seed = seed))

  # Check if language is changing
  language_changed <- "language" %in% names(argg) &&
    argg[["language"]] != sfm[["sim_specs"]][["language"]]

  # Overwrite simulation specifications
  sfm[["sim_specs"]] <- utils::modifyList(sfm[["sim_specs"]], argg)

  # If language changed, clear entire cache and regenerate equation strings
  if (language_changed) {
    sfm <- invalidate_assemble(sfm, "all")

    if (nrow(sfm[["variables"]])) {
      sfm <- prep_equations_variables(sfm)
      sfm <- prep_stock_change(sfm)
    }
  }

  # Selectively invalidate based on what changed
  time_related <- c("start", "stop", "dt", "save_at", "save_from",
                     "time_units", "method", "seed")
  if (all(names(argg) %in% c("language", time_related))) {
    sfm <- invalidate_assemble(sfm, "times")
  } else {
    # keep_unit, keep_nonnegative_stock/flow affect equation formatting
    sfm <- invalidate_assemble(sfm, "all")
  }

  sfm <- sanitize_sdbuildR(sfm)

  # Pre-assemble script components so they're available for modification before simulate()
  sfm <- pre_assemble_components(sfm)

  sfm
}
