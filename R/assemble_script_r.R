#' Simulate stock-and-flow model in R
#'
#' @inheritParams simulate.stockflow
#'
#' @returns List with variables created in the simulation script
#' @noRd
#'
simulate_r <- function(object,
                       only_stocks,
                       vars = NULL) {
  # Compile script without plot - returns list with script and modified object
  result <- compile(object,
    only_stocks = only_stocks,
    vars = vars
  )
  script <- result$script
  object <- result$object # Get updated object with cache populated
  seed_nr <- object[["sim_settings"]][["seed"]]


  # Evaluate script
  sim <- tryCatch(
    {
      start_t <- Sys.time()

      # Create a new environment to collect variables
      envir <- new.env()

      # Evaluate script
      do_run <- function() {
        eval(parse(text = script), envir = envir)
      }

      if (is.null(seed_nr)) do_run() else withr::with_seed(as.numeric(seed_nr), do_run())

      end_t <- Sys.time()

      # if (verbose) {
      #   elapsed <- round(as.numeric(end_t) - as.numeric(start_t), 4)
      #   cli::cli_inform(c(
      #     "v" = "Simulation completed in {.val {elapsed}} seconds."
      #   ))
      # }

      df <- envir[[P[["sim_df_name"]]]]
      df <- filter_sim_df_vars(df, vars)
      init <- unlist(envir[[P[["initial_value_name"]]]])
      constants <- unlist(Filter(Negate(is.function), envir[[P[["parameter_name"]]]]))

      new_simulate_stockflow(
        success = TRUE,
        object = object, # Return object with cache
        df = df,
        init = init,
        constants = constants,
        script = script,
        duration = end_t - start_t
      )
    },
    error = function(e) {
      warning("\nAn error occurred while running the R script.")

      new_simulate_stockflow(
        success = FALSE,
        error_message = e[["message"]],
        script = script,
        object = object
      )
    }
  )

  sim
}
