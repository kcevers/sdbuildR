
#' Run ensemble simulations
#'
#' Run an ensemble simulation of a stock-and-flow model, varying initial conditions and/or parameters in the range specified in `range`. The ensemble can be run in parallel using multiple threads by first setting `nthreads` in [use_julia()]. The results are returned as a data.frame with summary statistics and optionally individual simulations.
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
#' @returns Object of class [`ensemble_sdbuildR`][ensemble()], which is a list containing:
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
#' @seealso [build()], [sdbuildR()], [sim_specs()], [use_julia()]
#'
#' @examplesIf is_julia_ready()
#' # Load example and set simulation language to Julia
#' sfm <- sdbuildR("predator_prey") |> sim_specs(language = "Julia")
#'
#' # Set random initial conditions
#' sfm <- build(sfm, c("predator", "prey"), eqn = "runif(1, min = 20, max = 80)")
#'
#' # For ensemble simulations, it is highly recommended to reduce the
#' # returned output. For example, to save output only every 1 time unit:
#' sfm <- sim_specs(sfm, save_at = 1)
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
#' use_julia(nthreads = 4)
#' sims <- ensemble(sfm, n = 10)
#'
#' # Stop using threads
#' use_julia(nthreads = 1)
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
  check_sdbuildR(sfm)

  # Collect arguments
  argg <- c(as.list(environment()))
  # Remove NULL arguments
  argg <- argg[!lengths(argg) == 0]

  if (tolower(sfm[["sim_specs"]][["language"]]) != "julia") {
    cli::cli_abort(c(
      "x" = "Ensemble simulations are only supported for {.code Julia} models."
    ))
  }

  if (!is.numeric(n)) {
    cli::cli_abort(c(
      "x" = "The {.arg n} argument must be {.cls numeric}.",
      "i" = "Received: {.cls {typeof(n)}}"
    ))
  }

  if (n <= 0) {
    cli::cli_abort(c(
      "x" = "The {.arg n} argument must be greater than {.val 0}."
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
      "x" = "The {.arg quantiles} argument must have at least {.val 2} unique values.",
      "i" = "Received {.val {length(unique(quantiles))}} unique value(s).",
      ">" = "Provide at least 2 quantiles, e.g., {.code quantiles = c(0.025, 0.975)}."
    ))
  }

  if (any(quantiles < 0 | quantiles > 1)) {
    cli::cli_abort(c(
      "x" = "All values in {.arg quantiles} must be between {.val 0} and {.val 1}.",
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

  if (!is.logical(return_sims)) {
    cli::cli_abort(c(
      "x" = "The {.arg return_sims} argument must be {.code TRUE} or {.code FALSE}.",
      "i" = "Received: {.val {return_sims}}"
    ))
  }

  if (!is.logical(only_stocks)) {
    cli::cli_abort(c(
      "x" = "The {.arg only_stocks} argument must be {.code TRUE} or {.code FALSE}.",
      "i" = "Received: {.val {only_stocks}}",
      ">" = "Use {.code only_stocks = TRUE} to exclude flows and auxiliaries from output."
    ))
  }

  if (!is.null(range)) {
    if (!is.list(range)) {
      cli::cli_abort(c(
        "x" = "The {.arg range} argument must be a {.cls list}.",
        "i" = "Received: {.cls {typeof(range)}}",
        ">" = "Use format: {.code range = list(param1 = c(1, 2), param2 = c(10, 20))}."
      ))
    }

    if (length(range) == 0) {
      cli::cli_abort(c(
        "x" = "The {.arg range} argument must have at least one parameter.",
        ">" = "Specify like: {.code range = list(param = c(1, 2, 3))}."
      ))
    }

    if (is.null(names(range))) {
      cli::cli_abort(c(
        "x" = "The {.arg range} list elements must be named.",
        "i" = "Names correspond to parameter/stock names in your model.",
        ">" = "Use: {.code range = list(paramname = values, ...)}."
      ))
    }

    # All must be numerical values
    if (!all(vapply(range, is.numeric, logical(1)))) {
      cli::cli_abort(c(
        "x" = "All {.arg range} elements must be {.cls numeric} vectors.",
        ">" = "Example: {.code range = list(param1 = c(1, 2, 3))}."
      ))
    }

    # Test that names are unique
    if (length(unique(names(range))) != length(range)) {
      cli::cli_abort(c(
        "x" = "All {.arg range} names must be unique."
      ))
    }

    # All varied elements must exist in the model
    names_df <- get_names(sfm)
    names_range <- names(range)
    allowed_names <- names_df[names_df[["type"]] %in% c("stock", "constant"), "name"]
    idx <- names_range %in% names_df[["name"]]
    if (any(!idx)) {
      missing_names <- names_range[!idx]
      cli::cli_abort(c(
        "Unknown parameters in {.arg range}.",
        "x" = "The following parameters do not exist in the model: {paste0('{.code ', missing_names, '}', collapse = ', ')}.",
        "i" = "Check spelling and ensure parameters are defined in the model.",
        ">" = "Available variables to vary: {paste0(allowed_names, collapse = ', ')}"
      ))
    }

    # All varied elements must be a stock or constant
    idx <- names_range %in% c(names_df[names_df[["type"]] %in% c("stock", "constant"), "name"])
    if (any(!idx)) {
      invalid_names <- names_range[!idx]
      cli::cli_abort(c(
        "Cannot vary flows or auxiliaries, only stocks and constants.",
        "!" = "Cannot vary: {paste0('{.code ', invalid_names, '}', collapse = ', ')}.",
        ">" = "Available variables to vary: {paste0(allowed_names, collapse = ', ')}"
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

  total_sims <- n * n_conditions
  if (verbose) {
    sim_word <- ifelse(total_sims == 1, "simulation", "simulations")
    if (is.null(range)) {
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

  # Create ensemble parameters
  ensemble_pars <- list(
    n = n,
    quantiles = quantiles,
    return_sims = return_sims,
    range = range, cross = cross
  )

  ensemble_pars[["threaded"]] <- .sdbuildR_env[["jl"]][["use_threads"]] 

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

  # Compile ensemble script - returns list with script and modified sfm
  result <- compile_ensemble(sfm,
    ensemble_pars = ensemble_pars,
    only_stocks = only_stocks
  )

  script <- result$script
  sfm <- result$sfm # Get updated sfm with cache populated

  write_script(script, filepath)
  script <- paste0(readLines(filepath), collapse = "\n")

  use_julia()

  # Evaluate script
  sim <- tryCatch(
    {

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

      # Get stock names for filtering when only_stocks = TRUE
      stock_names <- get_variables_by_type(sfm, "stock")[["name"]]

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

        # Filter to stocks only if requested
        if (only_stocks) {
          df <- df[df[["variable"]] %in% stock_names, , drop = FALSE]
        }

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

      # Filter summary to stocks only if requested
      if (only_stocks) {
        summary <- summary[summary[["variable"]] %in% stock_names, , drop = FALSE]
      }

      # Delete files
      file.remove(ensemble_pars[["filepath_summary"]][["df"]])
      file.remove(ensemble_pars[["filepath_summary"]][["constants"]])
      file.remove(ensemble_pars[["filepath_summary"]][["init"]])

      list(
        success = TRUE,
        df = df,
        summary = summary,
        n = n,
        n_total = total_sims,
        n_conditions = n_conditions,
        conditions = conditions,
        init = init,
        constants = constants,
        script = script,
        duration = end_t - start_t
      ) |>
        utils::modifyList(argg) |>
        structure(class = "ensemble_sdbuildR")
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
        n_total = total_sims,
        n_conditions = n_conditions,
        conditions = NULL,
        init = NULL,
        constants = NULL,
        script = script,
        duration = end_t - start_t
      ) |>
        utils::modifyList(argg) |>
        structure(class = "ensemble_sdbuildR")
    }
  )

  sim
}



# new_ensemble_sdbuildR <- function(...) {
#   structure(list(...), class = "ensemble_sdbuildR")
# }

# validate_ensemble_sdbuildR <- function(x) {
#   if (!is.list(x)) {
#     cli::cli_abort(c(
#       "x" = "Object must be a list.",
#       "i" = "Received: {.cls {typeof(x)}}"
#     ))
#   }

#   required_elements <- c("success", "df", "summary", "n", "n_total", "n_conditions", "conditions", "init", "constants", "script", "duration")
#   missing_elements <- setdiff(required_elements, names(x))
#   if (length(missing_elements) > 0) {
#     cli::cli_abort(c(
#       "x" = "Missing required elements in ensemble object.",
#       "i" = "The following elements are missing: {paste0('{.code ', missing_elements, '}', collapse = ', ')}."
#     ))
#   }

#   invisible(NULL)
# }

