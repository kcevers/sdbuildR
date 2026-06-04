#' Compute summary stats for a numeric vector
#'
#' @param vals Numeric vector.
#' @param quantiles Numeric vector of quantile probabilities.
#' @param q_names Character vector of quantile column names.
#' @returns Named list of summary statistics.
#' @noRd
ensemble_summary_stats <- function(vals, quantiles, q_names) {
  c(
    list(
      mean   = mean(vals, na.rm = TRUE),
      median = stats::median(vals, na.rm = TRUE),
      sd     = stats::sd(vals, na.rm = TRUE),
      min    = min(vals, na.rm = TRUE),
      max    = max(vals, na.rm = TRUE)
    ),
    stats::setNames(
      lapply(quantiles, function(q) {
        stats::quantile(vals, probs = q, na.rm = TRUE, names = FALSE)
      }),
      q_names
    )
  )
}


#' Summarise a long data frame by groups
#'
#' Splits `df` by the columns in `by`, computes summary stats on the `value`
#' column, and row-binds the results. Uses base R only.
#'
#' @param df Data frame with a `value` column and the grouping columns.
#' @param by Character vector of grouping column names.
#' @param quantiles Numeric vector of quantile probabilities.
#' @param q_names Character vector of quantile column names.
#' @returns Data frame of summary statistics.
#' @noRd
summarise_by <- function(df, by, quantiles, q_names) {
  df <- as.data.frame(df)
  if (nrow(df) == 0L) {
    empty <- data.frame(matrix(
      ncol = length(by) + 5L + length(q_names),
      nrow = 0L
    ))
    names(empty) <- c(by, "mean", "median", "sd", "min", "max", q_names)
    return(empty)
  }
  # Group by the 'by' columns using base R split
  split_df <- split(df, df[by], drop = TRUE)
  # Apply summary stats to each group's value column
  result_list <- lapply(split_df, function(group_df) {
    stats_list <- ensemble_summary_stats(group_df$value, quantiles, q_names)
    # Combine grouping keys with stats into a single data frame
    group_keys_df <- group_df[1L, by, drop = FALSE]
    stats_df <- as.data.frame(t(unlist(stats_list)), stringsAsFactors = FALSE)
    cbind(group_keys_df, stats_df)
  })
  # Combine results
  out <- do.call(rbind, c(result_list, list(make.row.names = FALSE)))
  rownames(out) <- NULL
  # Restore deterministic ordering by grouping keys
  out <- out[do.call(order, as.list(out[by])), , drop = FALSE]
  rownames(out) <- NULL
  out
}


#' Run ensemble simulation in R
#'
#' Called by [ensemble()] when language is R. Runs multiple simulations using
#' `simulate()`, optionally in parallel via `future.apply::future_lapply()` when
#' an active `future::plan()` has multiple workers.
#'
#' @inheritParams ensemble
#' @param n_conditions Integer; number of conditions.
#' @param total_sims Integer; total simulations across all conditions.
#'
#' @returns Object of class [`ensemble_sdbuildR`][ensemble()]
#' @noRd
ensemble_r <- function(object, n, save_sims, conditions, cross,
                       quantiles, only_stocks, vars = NULL, verbose,
                       n_conditions, total_sims) {

  # Specify seed if specified
  has_seed <- !is.null(object[["sim_settings"]][["seed"]])
  seed_nr <- object[["sim_settings"]][["seed"]]
  if (has_seed) {
    withr::local_seed(seed_nr)
    object <- sim_settings(object, seed = NULL)  # clear seed so it doesn't affect individual sims
  }

  start_t <- Sys.time()

  # Build conditions grid
  if (!is.null(conditions)) {
    if (cross) {
      cond_grid <- expand.grid(conditions, KEEP.OUT.ATTRS = FALSE)
    } else {
      cond_grid <- as.data.frame(conditions)
    }
    # Sort columns alphabetically (conditions was already sorted)
    cond_grid <- cond_grid[, sort(names(cond_grid)), drop = FALSE]
    cond_matrix <- cbind(condition = seq_len(nrow(cond_grid)), cond_grid)
  } else {
    cond_grid <- NULL
    cond_matrix <- NULL
  }

  # Use parallel apply only if the user configured an active future plan with
  # more than one worker.
  use_par <- rlang::is_installed("future.apply") && future::nbrOfWorkers() > 1L
  apply_fun <- if (use_par) future.apply::future_lapply else lapply

  # --- Pre-compile scripts (avoids redundant compile() per simulation) --------
  # Compile the base model once to populate the assembly cache
  base_compiled <- compile(object, only_stocks = only_stocks, vars = vars)
  object <- base_compiled$object

  if (is.null(cond_grid)) {
    # No conditions: one script for all runs
    scripts <- list(base_compiled$script)
  } else {
    # With conditions: one script per condition
    # Starting from the cached object so only invalidated parts (static) rebuild
    scripts <- vector("list", n_conditions)
    for (j_idx in seq_len(n_conditions)) {
      obj_mod <- object
      for (nm in names(cond_grid)) {
        val <- cond_grid[j_idx, nm]
        obj_mod <- update(obj_mod, !!nm, eqn = !!val)
      }
      compiled <- compile(obj_mod, only_stocks = only_stocks, vars = vars)
      scripts[[j_idx]] <- compiled$script
    }
  }

  # Parse once per condition to avoid parse() overhead per simulation run
  parsed_scripts <- lapply(scripts, function(script) parse(text = script))

  # Build task list: list of (condition, sim) pairs
  tasks <- vector("list", total_sims)
  idx <- 0L
  for (j_idx in seq_len(n_conditions)) {
    for (i_idx in seq_len(n)) {
      idx <- idx + 1L
      tasks[[idx]] <- list(condition = j_idx, sim = i_idx)
    }
  }


  # Capture internal function so future workers can serialize it as a closure
  # (eval_sim_script_r is not exported, so future.packages alone won't find it)
  .eval_sim <- eval_sim_script_r

  # Run simulations (each task just evals a pre-compiled script)
  sim_results <- tryCatch(
    {
      if (use_par) {
        apply_fun(tasks, function(task) {
          .eval_sim(
            parsed_expr = parsed_scripts[[task[["condition"]]]],
            condition = task[["condition"]], sim = task[["sim"]]
          )
        }, future.seed = TRUE, future.packages = "sdbuildR")
      } else {
        lapply(tasks, function(task) {
          eval_sim_script_r(
            parsed_expr = parsed_scripts[[task[["condition"]]]],
            condition = task[["condition"]], sim = task[["sim"]]
          )
        })
      }
    },
    error = function(e) {
      cli::cli_warn(c(
        "!" = "An error occurred during R ensemble simulation.",
        "i" = "Error: {e[['message']]}"
      ))
      structure(list(), error_message = e[["message"]])
    }
  )

  end_t <- Sys.time()

  # Restore seed if specified
  if (has_seed) {
    object <- sim_settings(object, seed = seed_nr)
  }

  if (length(sim_results) == 0L) {
    err_msg <- attr(sim_results, "error_message") %||%
      "Ensemble simulation failed."
    return(new_ensemble_sdbuildR(
      success = FALSE,
      error_message = err_msg,
      object = object
    ))
  }

  # Check for any failed simulations
  failed <- vapply(sim_results, function(x) !x[["success"]], logical(1))
  if (all(failed)) {
    return(new_ensemble_sdbuildR(
      success = FALSE,
      error_message = sim_results[[1]][["error_message"]],
      object = object
    ))
  }
  if (any(failed)) {
    n_failed <- sum(failed)
    cli::cli_warn(c(
      "!" = "{n_failed}/{total_sims} simulations failed.",
      "i" = "First error: {sim_results[failed][[1]][['error_message']]}"
    ))
  }

  # Filter to only successful results
  good_results <- sim_results[!failed]

  if (verbose) {
    elapsed <- round(as.numeric(end_t) - as.numeric(start_t), 4)
    cli::cli_inform(c(
      "v" = "Ensemble simulation completed in {.val {elapsed}} seconds."
    ))
  }

  # Assemble results
  assemble_ensemble_results_r(
    good_results = good_results,
    n = n, n_conditions = n_conditions, total_sims = total_sims,
    save_sims = save_sims, only_stocks = only_stocks,
    vars = vars,
    quantiles = quantiles, cross = cross,
    cond_matrix = cond_matrix, object = object,
    duration = end_t - start_t
  )
}


#' Evaluate a pre-compiled R simulation script
#'
#' Lightweight alternative to run_single_sim_r() that skips compilation
#' entirely. The script is evaluated in a fresh environment; stochastic
#' equations (e.g., `runif()`) produce different results on each call.
#'
#' @param parsed_expr Parsed R expression for a compiled simulation script.
#' @param condition Integer; condition index.
#' @param sim Integer; simulation index.
#'
#' @returns List with success, df, init, constants, condition, sim
#' @noRd
eval_sim_script_r <- function(parsed_expr, condition, sim) {
  envir <- new.env()
  tryCatch(
    {
      eval(parsed_expr, envir = envir)
      list(
        success = TRUE,
        df = envir[[P[["sim_df_name"]]]],
        init = unlist(envir[[P[["initial_value_name"]]]]),
        constants = unlist(Filter(Negate(is.function), envir[[P[["parameter_name"]]]])),
        condition = condition, sim = sim
      )
    },
    error = function(e) {
      list(
        success = FALSE,
        error_message = e[["message"]],
        condition = condition, sim = sim
      )
    }
  )
}


#' Run a single R simulation for the ensemble (legacy)
#'
#' Kept for reference. Prefer eval_sim_script_r() which avoids redundant
#' compilation by accepting a pre-compiled script.
#'
#' @param object Stock-and-flow model
#' @param task List with condition (condition index) and sim (simulation index)
#' @param conditions Named list of conditions or NULL
#' @param cond_grid Data frame of condition combinations or NULL
#' @param only_stocks Logical; only keep stock variables
#'
#' @returns List with success, df, init, constants, condition, sim
#' @noRd
run_single_sim_r <- function(object, task, conditions, cond_grid,
                             only_stocks) {
  j_idx <- task[["condition"]]
  i_idx <- task[["sim"]]

  # Apply conditions for this condition index
  if (!is.null(cond_grid)) {
    obj_mod <- object
    for (nm in names(cond_grid)) {
      val <- cond_grid[j_idx, nm]
      obj_mod <- update(obj_mod, !!nm, eqn = !!val)
    }
  } else {
    obj_mod <- object
  }

  # Clear assembled cache so compile() re-evaluates with new parameters
  obj_mod[["assemble"]] <- empty_assemble()

  # Use simulate() to run the model
  sim <- simulate(obj_mod, only_stocks = only_stocks, verbose = FALSE)

  if (!sim[["success"]]) {
    return(list(
      success = FALSE,
      error_message = sim[["error_message"]],
      condition = j_idx, sim = i_idx
    ))
  }

  list(
    success = TRUE,
    df = sim[["df"]],
    init = sim[["init"]],
    constants = sim[["constants"]],
    condition = j_idx,
    sim = i_idx
  )
}


#' Assemble R ensemble results into ensemble_sdbuildR object
#'
#' @param good_results List of successful simulation results
#' @param n Simulations per condition
#' @param n_conditions Number of conditions
#' @param total_sims Total simulations
#' @param save_sims Logical; include individual sim data
#' @param only_stocks Logical; filter to stocks only
#' @param quantiles Numeric vector of quantiles
#' @param cross Logical; crossed design
#' @param cond_matrix Data frame of conditions or NULL
#' @param object Stock-and-flow model
#' @param duration Time elapsed
#'
#' @returns Object of class [`ensemble_sdbuildR`][ensemble()]
#' @noRd
assemble_ensemble_results_r <- function(good_results, n, n_conditions,
                                        total_sims, save_sims,
                                        only_stocks, vars = NULL, quantiles, cross,
                                        cond_matrix, object, duration) {
  # Build individual simulation data frames
  all_dfs <- vector("list", length(good_results))
  all_init <- vector("list", length(good_results))
  all_constants <- vector("list", length(good_results))

  for (k in seq_along(good_results)) {
    res <- good_results[[k]]
    df_k <- res[["df"]]
    df_k[["sim"]] <- res[["sim"]]
    df_k[["condition"]] <- res[["condition"]]
    all_dfs[[k]] <- df_k

    # Init values
    init_vals <- res[["init"]]
    all_init[[k]] <- data.frame(
      sim = res[["sim"]], condition = res[["condition"]],
      variable = names(init_vals),
      value = unname(init_vals),
      stringsAsFactors = FALSE
    )

    # Constants
    const_vals <- res[["constants"]]
    if (length(const_vals) == 0L) {
      all_constants[[k]] <- data.frame(
        sim = integer(0),
        condition = integer(0),
        variable = character(0),
        value = numeric(0),
        stringsAsFactors = FALSE
      )
    } else {
      all_constants[[k]] <- data.frame(
        sim = res[["sim"]], condition = res[["condition"]],
        variable = names(const_vals),
        value = unname(const_vals),
        stringsAsFactors = FALSE
      )
    }
  }

  combined_df <- data.table::rbindlist(all_dfs, use.names = TRUE)
  combined_df <- filter_sim_df_vars(combined_df, vars)
  combined_init <- data.table::rbindlist(all_init, use.names = TRUE)
  combined_constants <- data.table::rbindlist(all_constants, use.names = TRUE)

  # Compute summary statistics using base R (avoids data.table [.data.table
  # dispatch issues when the package is not attached)
  q_names <- paste0("q", quantiles)

  summary_df <- summarise_by(
    combined_df,
    by = c("condition", "variable", "time"),
    quantiles = quantiles, q_names = q_names
  )

  # Build init summary
  init_summary <- summarise_by(
    combined_init,
    by = c("condition", "variable"),
    quantiles = quantiles, q_names = q_names
  )

  # Build constants summary
  constants_summary <- summarise_by(
    combined_constants,
    by = c("condition", "variable"),
    quantiles = quantiles, q_names = q_names
  )

  # Prepare init and constants output
  init_out <- list(summary = init_summary)
  constants_out <- list(summary = constants_summary)

  if (save_sims) {
    df_out <- as.data.frame(combined_df)
    init_out[["df"]] <- as.data.frame(combined_init)
    constants_out[["df"]] <- as.data.frame(combined_constants)
  } else {
    df_out <- NULL
  }

  new_ensemble_sdbuildR(
    success = TRUE,
    df = df_out,
    summary = summary_df,
    n = n,
    n_total = total_sims,
    n_conditions = n_conditions,
    conditions = cond_matrix,
    init = init_out,
    constants = constants_out,
    script = NULL,
    duration = duration,
    cross = cross,
    quantiles = quantiles,
    object = object
  )
}
