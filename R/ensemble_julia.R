
# ==============================================================================
# Julia ensemble backend
# ==============================================================================

#' Run ensemble simulation via Julia
#'
#' Called by [ensemble()] when language is Julia.
#'
#' @inheritParams ensemble
#' @param n_conditions Integer; number of conditions.
#' @param total_sims Integer; total simulations across all conditions.
#'
#' @returns Object of class [`ensemble_sdbuildR`][ensemble()]
#' @noRd
ensemble_julia <- function(object, n, return_sims, conditions, cross,
                           quantiles, only_stocks, vars = NULL, verbose,
                           n_conditions, total_sims) {

  # Create ensemble parameters
  ensemble_pars <- list(
    n = n,
    quantiles = quantiles,
    return_sims = return_sims,
    conditions = conditions, cross = cross
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

  # Compile ensemble script - returns list with script and modified object
  result <- compile_ensemble(object,
    ensemble_pars = ensemble_pars,
    only_stocks = only_stocks
  )

  script <- result$script
  object <- result$object # Get updated object with cache populated

  write_script(script, filepath)
  script <- paste0(readLines(filepath), collapse = "\n")

  use_julia()

  on.exit({
    # Ensure files are deleted even if an error occurs
    paths <- c(filepath, ensemble_pars[["filepath_df"]], ensemble_pars[["filepath_summary"]])
    for (path in paths) {
      if (file.exists(path)) {
        file.remove(path)
      }
    }
  }, add = TRUE)

  # Evaluate script
  sim <- tryCatch(
    {
      # Evaluate script
      start_t <- Sys.time()

      # Wrap in invisible and capture.output to not show message of units module
      # being overwritten
      invisible(utils::capture.output(
        JuliaConnectoR::juliaEval(paste0('include("', jl_path(filepath), '")'))
      ))

      end_t <- Sys.time()

      if (verbose) {
        elapsed <- round(as.numeric(end_t) - as.numeric(start_t), 4)
        cli::cli_inform(c(
          "v" = "Ensemble simulation completed in {.val {elapsed}} seconds."
        ))
      }

      # Read the number of simulations per condition
      n_val <- JuliaConnectoR::juliaEval(P[["ensemble_n"]])

      # Read the ensemble conditions
      if (!is.null(ensemble_pars[["conditions"]])) {
        cond_matrix <- JuliaConnectoR::juliaEval(
          paste0("Matrix(hcat(", P[["ensemble_pars"]], "...)')")
        )
        colnames(cond_matrix) <- names(conditions)
        cond_matrix <- cbind(j = seq_len(nrow(cond_matrix)), cond_matrix)
      } else {
        cond_matrix <- NULL
      }

      constants_out <- list()
      init_out <- list()

      # Get stock names for filtering when only_stocks = TRUE
      stock_names <- get_variables_by_type(object, "stock")[["name"]]

      # Read the simulation results
      if (return_sims) {
 
        # First check whether files exist
        if (!file.exists(ensemble_pars[["filepath_df"]][["df"]]) ||
            !file.exists(ensemble_pars[["filepath_df"]][["constants"]]) ||
            !file.exists(ensemble_pars[["filepath_df"]][["init"]])) {
          cli::cli_abort(c(
            "x" = "Julia simulation failed to store output files.",
            ">" = "Try running a smaller ensemble to see if the issue persists."
          ))
        } else {

          # Read the simulation results
          df <- as.data.frame(data.table::fread(
            ensemble_pars[["filepath_df"]][["df"]],
            na.strings = c("", "NA")
          ))
          constants_out[["df"]] <- as.data.frame(data.table::fread(
            ensemble_pars[["filepath_df"]][["constants"]],
            na.strings = c("", "NA")
          ))
          init_out[["df"]] <- as.data.frame(data.table::fread(
            ensemble_pars[["filepath_df"]][["init"]],
            na.strings = c("", "NA")
          ))        

          # Filter to stocks only if requested
          if (only_stocks) {
            df <- df[df[["variable"]] %in% stock_names, , drop = FALSE]
          }
          df <- filter_sim_df_vars(df, vars)

        }

      } else {
        df <- NULL
      }

      # Read the summary file
      summary_df <- as.data.frame(data.table::fread(
        ensemble_pars[["filepath_summary"]][["df"]],
        na.strings = c("", "NA")
      ))
      constants_out[["summary"]] <- as.data.frame(data.table::fread(
        ensemble_pars[["filepath_summary"]][["constants"]],
        na.strings = c("", "NA")
      ))
      init_out[["summary"]] <- as.data.frame(data.table::fread(
        ensemble_pars[["filepath_summary"]][["init"]],
        na.strings = c("", "NA")
      ))

      # Filter summary to stocks only if requested
      if (only_stocks) {
        summary_df <- summary_df[
          summary_df[["variable"]] %in% stock_names, ,
          drop = FALSE
        ]
      }
      summary_df <- filter_sim_df_vars(summary_df, vars)

      new_ensemble_sdbuildR(
        success = TRUE,
        df = df,
        summary = summary_df,
        n = n_val,
        n_total = total_sims,
        n_conditions = n_conditions,
        conditions = cond_matrix,
        init = init_out,
        constants = constants_out,
        script = script,
        duration = end_t - start_t,
        cross = cross,
        quantiles = quantiles,
        object = object
      )
    },
    error = function(e) {
      cli::cli_warn(c(
        "Julia execution failed.",
        "!" = "An error occurred while running the Julia script.",
        "i" = "Error: {e[['message']]}"
      ))
      new_ensemble_sdbuildR(
        success = FALSE,
        error_message = e[["message"]],
        script = script,
        object = object
      )
    }
  )

  sim
}
