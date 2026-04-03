#' Simulate stock-and-flow model in R
#'
#' @inheritParams simulate.sdbuildR
#'
#' @returns List with variables created in the simulation script
#' @noRd
#'
simulate_r <- function(object,
                       verbose,
                       only_stocks,
                       vars = NULL) {
  # Compile script without plot - returns list with script and modified object
  result <- compile(object,
    only_stocks = only_stocks,
    vars = vars
  )
  script <- result$script
  object <- result$object # Get updated object with cache populated

  # Evaluate script
  sim <- tryCatch(
    {
      # Create a new environment to collect variables
      envir <- new.env()
      start_t <- Sys.time()

      # Evaluate script
      eval(parse(text = script), envir = envir)

      end_t <- Sys.time()

      if (verbose) {
        elapsed <- round(as.numeric(end_t) - as.numeric(start_t), 4)
        cli::cli_inform(c(
          "v" = "Simulation completed in {.val {elapsed}} seconds."
        ))
      }

      df <- envir[[P[["sim_df_name"]]]]
      df <- filter_sim_df_vars(df, vars)
      init <- unlist(envir[[P[["initial_value_name"]]]])
      constants <- unlist(Filter(Negate(is.function), envir[[P[["parameter_name"]]]]))

      new_simulate_sdbuildR(
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

      new_simulate_sdbuildR(
        success = FALSE,
        error_message = e[["message"]],
        script = script,
        object = object
      )
    }
  )

  sim
}


#' Compile script for enabling destructuring assignment in R
#'
#' @inheritParams update.sdbuildR
#' @inheritParams compile_ode
#'
#' @returns List with necessary scripts
#' @noRd
#'
compile_destructuring_assign <- function(object, static) {
  # Add package for destructuring assignment in case it was used
  eqns <- c(static, object[["variables"]][["eqn"]])

  if (any(stats::na.omit(stringr::str_detect(eqns, stringr::fixed("%<-%"))))) {
    script <- "\n# Add package for destructuring assignment\nif (!require('zeallot')) install.packages('zeallot'); library(zeallot)\n"
  } else {
    script <- ""
  }

  return(list(script = script))
}
