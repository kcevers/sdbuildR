#' Simulate stock-and-flow model in R
#'
#' @inheritParams simulate
#'
#' @returns List with variables created in the simulation script
#' @noRd
#'
simulate_r <- function(sfm,
                       verbose,
                       only_stocks) {
  # Compile script without plot - returns list with script and modified sfm
  result <- compile(sfm,
    only_stocks = only_stocks
  )
  script <- result$script
  sfm <- result$sfm # Get updated sfm with cache populated

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
        message(paste0("Simulation took ", round(end_t - start_t, 4), " seconds"))
      }

      df <- envir[[P[["sim_df_name"]]]]
      init <- unlist(envir[[P[["initial_value_name"]]]])
      constants <- unlist(envir[[P[["parameter_name"]]]])

      new_simulate_sdbuildR(
        success = TRUE,
        sfm = sfm, # Return sfm with cache
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
        sfm = sfm
      )
    }
  )

  sim
}


#' Compile script for enabling destructuring assignment in R
#'
#' @inheritParams build
#' @inheritParams compile_ode
#'
#' @returns List with necessary scripts
#' @noRd
#'
compile_destructuring_assign <- function(sfm, static) {
  # Add package for destructuring assignment in case it was used
  eqns <- c(static, sfm[["variables"]][["eqn"]])

  if (any(stats::na.omit(stringr::str_detect(eqns, stringr::fixed("%<-%"))))) {
    script <- "\n# Add package for destructuring assignment\nif (!require('zeallot')) install.packages('zeallot'); library(zeallot)\n"
  } else {
    script <- ""
  }

  return(list(script = script))
}
