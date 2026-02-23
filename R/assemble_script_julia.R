#' Simulate stock-and-flow model in Julia
#'
#' @inheritParams simulate
#'
#' @returns List with variables created in the simulation script
#' @noRd
#'
simulate_julia <- function(sfm,
                           only_stocks,
                           verbose) {
  # Get output filepaths
  filepath_sim <- get_tempfile(fileext = ".csv")
  filepath <- get_tempfile(fileext = ".jl")

  # Compile script - returns list with script and modified sfm
  result <- compile(sfm,
    filepath_sim = filepath_sim,
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
      # out <- invisible({
      #   utils::capture.output({
      out <- JuliaConnectoR::juliaEval(paste0('include("', filepath, '")'))
      #   })
      # })

      end_t <- Sys.time()

      if (verbose) {
        cli::cli_inform(paste0("Simulation took ", round(end_t - start_t, 4), " seconds"))
      }

      # Read the constants
      constants <- as.numeric(JuliaConnectoR::juliaEval(P[["parameter_name"]]))
      names(constants) <- JuliaConnectoR::juliaEval(P[["parameter_names"]])

      # Read the initial values of stocks
      init <- as.numeric(JuliaConnectoR::juliaEval(P[["initial_value_name"]]))
      names(init) <- JuliaConnectoR::juliaEval(P[["initial_value_names"]])

      # Read the simulation results dataframe
      df <- as.data.frame(data.table::fread(filepath_sim, na.strings = c("", "NA")))

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
      warning("\nAn error occurred while running the Julia script.")
      new_simulate_sdbuildR(
        success = FALSE,
        error_message = e[["message"]],
        script = script,
        sfm = sfm
      )
    }
  )

  # Clean up temporary file
  file.remove(filepath)

  sim
}


#' Check for keyword arguments in Julia translated equation
#'
#' Check whether any variable names are used as functions with keyword arguments in the Julia translated equation
#'
#' @inheritParams build
#' @param var_names Character vector; variable names in the model.
#'
#' @returns Returns `NULL`, called for side effects.
#' @noRd
check_no_keyword_arg <- function(sfm, var_names) {
  # Check for all variable names if they are used as functions. If so, throw error if they are used with keyword arguments in the Julia translated equation.

  # Batch convert equations to Julia format for checking
  regex_units <- get_regex_units()
  eqns <- character(nrow(sfm[["variables"]]))
  for (i in seq_len(nrow(sfm[["variables"]]))) {
    if (sfm[["variables"]][i, "type"] %in% c("stock", "flow", "constant", "aux")) {
      result <- convert_equations_julia(
        type = sfm[["variables"]][i, "type"],
        name = sfm[["variables"]][i, "name"],
        eqn = sfm[["variables"]][i, "eqn"],
        var_names = var_names,
        regex_units = regex_units
      )
      eqns[i] <- result[["eqn"]]
    }
  }

  # Find if any variables were used as functions
  idx <- stringr::str_detect(eqns, paste0(paste0(var_names, "\\("), collapse = "|"))

  if (any(idx)) {
    # Get the equations that use variables as functions
    eqns <- eqns[idx]

    named_idxs <- vapply(eqns, function(eqn) {
      # Find all round brackets
      paired_idxs <- get_range_all_pairs(eqn, var_names = var_names, type = "round")
      if (nrow(paired_idxs) == 0) {
        return(FALSE)
      }

      # Get start and end indices of variable names
      pair_names <- get_range_names(eqn, var_names = var_names, names_with_brackets = FALSE)
      if (nrow(pair_names) == 0) {
        return(FALSE)
      }

      # Match brackets to variable name
      pair_idxs <- match(pair_names[["end"]] + 1, paired_idxs[["start"]])
      if (all(is.na(pair_idxs))) {
        return(FALSE)
      }
      paired <- cbind(pair_names[pair_idxs, ], paired_idxs[pair_idxs, ])

      # Remove NA
      paired <- paired[!is.na(paired[["match"]]), ]

      named_idxs <- vapply(paired[["match"]], function(x) {
        y <- parse_args(gsub("\\)$", "", gsub("^\\(", "", x)))

        # Check for named arguments
        any(stringr::str_detect(y, "="))
      }, logical(1))
      any(unlist(unname(named_idxs)))
    }, logical(1))

    if (any(named_idxs)) {
      cli::cli_abort(
        paste0(
          "The following variables were used as functions with named arguments in the Julia translated equation: ",
          paste0(names(named_idxs)[unname(named_idxs)], collapse = ", "), ".\n",
          "This is not allowed in Julia. Please use arguments without naming them."
        ),
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}


#' Prepare stock-and-flow model for ensemble range
#'
#' @inheritParams build
#' @inheritParams compile
#'
#' @returns List with updated stock-and-flow model and updated ensemble parameters
#' @noRd
prep_ensemble_range <- function(sfm, ensemble_pars) {
  if (!is.null(ensemble_pars[["range"]])) {
    # Prepare the ranges for Julia
    ensemble_pars[["range"]] <- lapply(
      ensemble_pars[["range"]],
      function(vec) {
        replace_digits_with_floats(
          paste0("[", paste0(vec, collapse = ", "), "]"),
          # No variable names to account for
          NULL
        )
      }
    )

    # Ensemble variables will be removed from ordering in compile_static
    # No need to modify their equations here
  }

  return(list(sfm = sfm, ensemble_pars = ensemble_pars))
}


#' Prepare model for delayN and smoothN
#'
#' @inheritParams build
#' @param delayN_smoothN List with delayN and smoothN functions
#'
#' @returns A stock-and-flow model object of class [`sdbuildR`][sdbuildR]
#' @noRd
prep_delayN_smoothN <- function(sfm, delayN_smoothN) {
  # If delayN() and smoothN() were used, add these to the model
  if (length(delayN_smoothN) > 0) {
    # Order alphabetically
    delayN_smoothN <- delayN_smoothN[sort(names(delayN_smoothN))]

    names_df <- get_names(sfm)
    allowed_delay_var <- names_df[names_df[["type"]] %in% c("stock", "flow", "aux", "lookup"), "name"]
    delayN_smoothN <- unlist(unname(delayN_smoothN), recursive = FALSE)

    # Create new delay stocks and auxiliaries as data frame rows instead of nested lists
    new_stocks_data <- list()
    new_aux_data <- list()

    for (i in seq_along(delayN_smoothN)) {
      x <- delayN_smoothN[[i]]
      name <- names(delayN_smoothN)[i]

      # In rare cases, the delayed variable is a graphical function, and in that case the unit of that variable cannot be found
      bare_var <- sub("\\(.*", "", x[["var"]])

      # Check whether the variable is in the model
      if (!bare_var %in% names_df[["name"]]) {
        cli::cli_abort(paste0(
          "The variable '", bare_var,
          "' used in delayN() or smoothN() is not defined in the model."
        ), call. = FALSE)
      }

      # Check whether variable is either a stock, flow, aux, or gf
      if (!bare_var %in% allowed_delay_var) {
        cli::cli_abort(paste0(name, " attempts to delay a constant ('", bare_var, "') in a delayN() or smoothN() function. Please only use dynamic variables (stock, flow, aux, or gf) in delayN() or smoothN()."), call. = FALSE)
      }

      # Unit is the same as the delayed variable
      unit_val <- names_df[names_df[["name"]] == bare_var, ][["units"]]

      # Create stock row
      # Note: For delayN/smoothN, store Julia code directly in eqn since it's Julia-specific
      new_stocks_data[[i]] <- data.frame(
        name = name,
        type = "delayN_smoothN",
        eqn = x[["setup"]], # Store Julia code directly
        units = unit_val,
        label = name,
        doc = "",
        non_negative = FALSE,
        to = NA,
        from = NA,
        source = NA,
        interpolation = NA,
        extrapolation = NA,
        xpts = NA,
        ypts = NA,
        inflow = list(x[["update"]]),
        outflow = NA,
        delayN = NULL,
        stringsAsFactors = FALSE
      )

      # Create auxiliary row
      # Note: For delayN/smoothN, store Julia code directly in eqn since it's Julia-specific
      new_aux_data[[i]] <- data.frame(
        name = name,
        type = "delayN_smoothN",
        eqn = x[["compute"]], # Store Julia code directly
        units = unit_val,
        label = name,
        doc = "",
        non_negative = FALSE,
        to = NA,
        from = NA,
        source = NA,
        interpolation = NA,
        extrapolation = NA,
        xpts = NA,
        ypts = NA,
        inflow = NA,
        outflow = NA,
        delayN = NULL,
        stringsAsFactors = FALSE
      )
    }

    # Combine and add to data frame
    if (length(new_stocks_data) > 0) {
      new_stocks_df <- do.call(rbind, new_stocks_data)
      sfm[["variables"]] <- rbind(sfm[["variables"]], new_stocks_df)
    }

    if (length(new_aux_data) > 0) {
      new_aux_df <- do.call(rbind, new_aux_data)
      sfm[["variables"]] <- rbind(sfm[["variables"]], new_aux_df)
    }
  }

  sfm
}


#' Prepare intermediary variables
#'
#' @inheritParams build
#' @inheritParams compile_static
#'
#' @returns List of intermediary variables and values
#' @noRd
prep_intermediary_variables <- function(sfm, language) {
  ordering <- sfm[["assemble"]][["ordering"]]

  if (language == "R") {
    cli::cli_abort("prep_intermediary_variables() is not implemented for R.", call. = FALSE)

  } else if (language == "Julia") {
    # Create separate vector for names of intermediate variables and values, because graphical functions need to be in the intermediate function as gf(t), but their name should be gf
    intermediary_var <- intermediary_var_values <- ordering[["dynamic"]][["order"]]

    # Graphical functions (gf) — add gf(source) to intermediary variables
    gf_dict <- build_gf_source_dict(sfm)
    if (!is.null(gf_dict)) {
      intermediary_var <- c(intermediary_var, names(gf_dict))
      intermediary_var_values <- c(intermediary_var_values, unname(gf_dict))
    }

    # # Add fixed delayed and past variables to intermediary_var
    # delay_past <- get_delay(sfm, type = "past")
    # extra_intermediary_var <- list_extract(delay_past, "var")


    # if (length(extra_intermediary_var) > 0) {
    #   # Check whether the intermediary variables are in the model
    #   names_df <- get_names(sfm)
    #   allowed_intermediary_var <- names_df[names_df[["type"]] %in% c("stock", "flow", "aux"), "name"]

    #   idx <- !(extra_intermediary_var %in% names_df[["name"]])
    #   if (any(idx)) {
    #     cli::cli_abort(paste0(
    #       "The following variables used in delay() or past() are not defined in the model: ",
    #       paste0(extra_intermediary_var[idx], collapse = ", ")
    #     ), call. = FALSE)
    #   }

    #   idx <- !(extra_intermediary_var %in% allowed_intermediary_var)
    #   if (any(idx)) {
    #     cli::cli_abort(paste0(
    #       "The following variables used in delay() or past() are not stocks, flows, or auxiliaries: ",
    #       paste0(extra_intermediary_var[idx], collapse = ", ")
    #     ), call. = FALSE)
    #   }

    #   # Get unique intermediary variables to add
    #   new_intermediary_var <- setdiff(unique(unlist(extra_intermediary_var)), intermediary_var)

    #   if (length(new_intermediary_var) > 0) {
    #     intermediary_var <- c(intermediary_var, new_intermediary_var)
    #     intermediary_var_values <- c(intermediary_var_values, new_intermediary_var)
    #   }
    # }

    # # If delayN() and smoothN() were used, state has to be unpacked differently
    # delayN_smoothN <- get_delay(sfm, type = "delayN_smoothN")

    # if (length(delayN_smoothN) > 0) {
    #   delay_names <- names(unlist(unname(delayN_smoothN), recursive = FALSE))

    #   intermediary_var <- setdiff(intermediary_var, delay_names)
    #   intermediary_var_values <- setdiff(intermediary_var_values, delay_names)
    # }

    # Order intermediary variables and values alphabetically
    if (length(intermediary_var) > 0) {
      idx <- order(intermediary_var)
      intermediary_var <- intermediary_var[idx]
      intermediary_var_values <- intermediary_var_values[idx]
    }

    list(
      names = intermediary_var,
      values = intermediary_var_values
    )
  }
}


# ==============================================================================
# Ensemble-specific script generators
# ==============================================================================

#' Compile static script with ensemble modifications
#'
#' Wraps compile_static() with ensemble-specific ordering (range vars excluded)
#' and prepends the ensemble_def block.
#'
#' @param sfm Stock-and-flow model
#' @param ensemble_pars List of ensemble parameters
#'
#' @returns List with `script` and `par_names`
#' @noRd
compile_static_ensemble <- function(sfm, ensemble_pars) {
  ordering <- sfm[["assemble"]][["ordering"]]

  # Remove ensemble range variables from the ordering so they are not
  # included in the static equations (they are defined by the ensemble loop)
  if (length(ensemble_pars[["range"]]) > 0) {
    range_names <- names(ensemble_pars[["range"]])
    ordering[["static"]][["order"]] <- setdiff(ordering[["static"]][["order"]], range_names)
    ordering[["static_and_dynamic"]][["order"]] <- setdiff(ordering[["static_and_dynamic"]][["order"]], range_names)

    range_items <- paste0(
      names(ensemble_pars[["range"]]), " = ",
      unname(ensemble_pars[["range"]]),
      collapse = ",\n"
    )
    ensemble_def <- fmt_script("ensemble_def_range", "Julia",
      n_value = format(ensemble_pars[["n"]], scientific = FALSE),
      range_items = range_items,
      crossed = ifelse(ensemble_pars[["cross"]], "true", "false")
    )
    ensemble_iter <- fmt_script("ensemble_iter", "Julia",
      range_names = paste0(names(ensemble_pars[["range"]]), collapse = ", ")
    )
  } else {
    ensemble_def <- fmt_script("ensemble_def_norange", "Julia",
      n_value = format(ensemble_pars[["n"]], scientific = FALSE)
    )
    ensemble_iter <- ""
  }

  result <- compile_static(sfm, "Julia",
    ordering_override = ordering,
    ensemble_iter_code = ensemble_iter
  )
  result[["script"]] <- paste0(ensemble_def, "\n\n", result[["script"]])
  result
}


#' Compile run_ode script for ensemble simulation
#'
#' Generates the ensemble problem, output function, and solve call for Julia.
#'
#' @param sfm Stock-and-flow model
#' @param ensemble_pars List of ensemble parameters
#' @param static Result of compile_static_ensemble()
#' @param only_stocks Logical; if TRUE, only stock values are saved
#'
#' @returns Character string with the run_ode script
#' @noRd
compile_run_ode_ensemble <- function(sfm, ensemble_pars, static, only_stocks) {
  intermediaries <- sfm[["assemble"]][["intermediaries"]]
  save_intermediaries <- length(intermediaries[["names"]]) > 0

  intermediaries_setup <- ""
  intermediaries_callback <- ""
  intermediaries_remake <- ""

  if (save_intermediaries) {
    intermediaries_setup <- paste0(
      "\n\n# Set up intermediaries for saving in callback\n",
      P[["intermediaries"]],
      " = Vector{SavedValues{eltype(",
      P[["time_name"]], "), Any}}(undef, ",
      P[["ensemble_total_n"]],
      ")\n\n# Populate the vector above with something to avoid undef\n",
      "for ", P[["ensemble_iter"]], " in eachindex(",
      P[["intermediaries"]], ")\n\t",
      P[["intermediaries"]], "[",
      P[["ensemble_iter"]], "] = SavedValues(eltype(",
      P[["time_name"]], "), Any)\nend\n\n"
    )
    intermediaries_callback <- paste0(
      "\n\t", P[["callback_name"]],
      " = SavingCallback(",
      P[["callback_func_name"]], ", ",
      P[["intermediaries"]], "[",
      P[["ensemble_iter"]], "], saveat = ",
      P[["savefrom_name"]], ")\n"
    )
    intermediaries_remake <- paste0(
      ", ", P[["callback_name"]], " = ", P[["callback_name"]]
    )
  }

  fmt_script("ensemble_prob", "Julia",
    intermediaries_setup = intermediaries_setup,
    static_str = static[["script"]],
    intermediaries_callback = intermediaries_callback,
    intermediaries_remake = intermediaries_remake,
    method = sfm[["sim_specs"]][["method"]],
    threaded_str = ifelse(ensemble_pars[["threaded"]], ", EnsembleThreads()", "")
  )
}


#' Compile post-processing script for ensemble simulation
#'
#' Generates the ensemble save, CSV output, and summary statistics scripts.
#'
#' @param sfm Stock-and-flow model
#' @param ensemble_pars List of ensemble parameters
#'
#' @returns Character string with the post script
#' @noRd
compile_post_ensemble <- function(sfm, ensemble_pars) {
  script <- ""

  ensemble_to_df_func <- ifelse(ensemble_pars[["threaded"]],
    " = ensemble_to_df_threaded(", " = ensemble_to_df("
  )
  script <- paste0(script, fmt_script("ensemble_save", "Julia",
    ensemble_to_df_func = ensemble_to_df_func
  ))

  if (ensemble_pars[["return_sims"]]) {
    script <- paste0(script, fmt_script("ensemble_csv", "Julia",
      filepath_df = ensemble_pars[["filepath_df"]][["df"]],
      filepath_constants = ensemble_pars[["filepath_df"]][["constants"]],
      filepath_init = ensemble_pars[["filepath_df"]][["init"]]
    ))
  }

  ensemble_summ_func <- ifelse(ensemble_pars[["threaded"]],
    " = ensemble_summ_threaded(", " = ensemble_summ("
  )
  script <- paste0(script, fmt_script("ensemble_summary", "Julia",
    ensemble_summ_func = ensemble_summ_func,
    quantiles = paste0(ensemble_pars[["quantiles"]], collapse = ", "),
    filepath_summary_df = ensemble_pars[["filepath_summary"]][["df"]],
    filepath_summary_constants = ensemble_pars[["filepath_summary"]][["constants"]],
    filepath_summary_init = ensemble_pars[["filepath_summary"]][["init"]]
  ))

  script
}
