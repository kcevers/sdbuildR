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
  # Access assembly specs from sim_specs
  keep_nonnegative_flow <- sfm[["sim_specs"]][["keep_nonnegative_flow"]]
  keep_nonnegative_stock <- sfm[["sim_specs"]][["keep_nonnegative_stock"]]
  keep_unit <- sfm[["sim_specs"]][["keep_unit"]]

  # Get output filepaths
  filepath_sim <- get_tempfile(fileext = ".csv")
  filepath <- get_tempfile(fileext = ".jl")

  # Compile script
  script <- compile_julia(sfm,
    filepath_sim = filepath_sim,
    ensemble_pars = NULL,
    only_stocks = only_stocks
  )
  write_script(script, filepath)
  script <- paste0(readLines(filepath), collapse = "\n")

  # Evaluate script
  sim <- tryCatch(
    {
      use_julia()

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

      new_sdbuildR_sim(
        success = TRUE,
        sfm = sfm,
        df = df,
        init = init,
        constants = constants,
        script = script,
        duration = end_t - start_t
      )
    },
    error = function(e) {
      warning("\nAn error occurred while running the Julia script.")
      new_sdbuildR_sim(
        success = FALSE,
        error_message = e[["message"]],
        script = script,
        sfm = sfm
      )
    }
  )

  # Clean up temporary file
  file.remove(filepath)

  return(sim)
}


#' Compile Julia script to simulate stock-and-flow model
#'
#' @inheritParams simulate
#' @param ensemble_pars List; parameters for the ensemble simulation. Defaults to NULL to not run an ensemble and simply a regular trajectory.
#'
#' @returns Julia script
#' @noRd
#'
compile_julia <- function(sfm, filepath_sim,
                          ensemble_pars,
                          only_stocks) {
  # Add "inflow" and "outflow" entries to stocks to match flow "to" and "from" entries
  flow_df <- get_flow_df(sfm)

  sfm[["model"]][["variables"]][["stock"]] <- lapply(
    sfm[["model"]][["variables"]][["stock"]],
    function(x) {
      x[["inflow"]] <- flow_df[flow_df[["to"]] == x[["name"]], "name"]
      x[["outflow"]] <- flow_df[flow_df[["from"]] == x[["name"]], "name"]

      if (length(x[["inflow"]]) == 0) {
        x[["inflow"]] <- ""
      }
      if (length(x[["outflow"]]) == 0) {
        x[["outflow"]] <- ""
      }

      return(x)
    }
  )

  # Extract sim_specs for keep_* flags
  keep_nonnegative_flow <- sfm[["sim_specs"]][["keep_nonnegative_flow"]]
  keep_nonnegative_stock <- sfm[["sim_specs"]][["keep_nonnegative_stock"]]
  keep_unit <- sfm[["sim_specs"]][["keep_unit"]]

  # Adjust keep_unit to FALSE if there are no units defined
  names_df <- get_names(sfm)
  var_names <- get_model_var(sfm)

  names_df_no_flow <- names_df
  names_df_no_flow <- names_df_no_flow[names_df_no_flow[["type"]] != "flow", ]

  all_eqns <- c(
    lapply(
      sfm[["model"]][["variables"]],
      function(x) {
        lapply(x, `[[`, "eqn")
      }
    ) |> unlist(),
    unlist(lapply(sfm[["macro"]], `[[`, "eqn"))
  )
  units_used <- unlist(stringr::str_extract_all(all_eqns, "\\bu\\([\"|'](.*?)[\"|']\\)"))

  keep_unit <- ifelse(!any(names_df_no_flow[["units"]] != "1" & nzchar(names_df_no_flow[["units"]])) & length(units_used) == 0, FALSE, keep_unit)

  # Order stocks alphabetically for order in init_names and init
  sfm[["model"]][["variables"]][["stock"]] <- sfm[["model"]][["variables"]][["stock"]][sort(names(sfm[["model"]][["variables"]][["stock"]]))]

  # Check keyword arguments are not used for custom functions in Julia
  check_no_keyword_arg(sfm, var_names)

  # Prepare model for ensemble range if specified
  out <- prep_ensemble_range(sfm, ensemble_pars)
  sfm <- out[["sfm"]]
  ensemble_pars <- out[["ensemble_pars"]]
  rm(out)

  # Prepare model for delayN() and smoothN() functions
  delayN_smoothN <- get_delay(sfm, type = "delayN_smoothN")

  sfm <- prep_delayN_smoothN(sfm, delayN_smoothN)

  # Order equations including delayN() and smoothN() functions, if any
  # Use cached ordering if available, otherwise compute it
  ordering <- if (!is.null(sfm[["ordering"]])) sfm[["ordering"]] else order_equations(sfm)

  # If there are no dynamic variables or delayed variables, set only_stocks to TRUE
  delay_past <- get_delay(sfm, type = "past")

  if (!only_stocks & is.null(ordering[["dynamic"]][["order"]])) {
    only_stocks <- TRUE
  }

  if (length(delay_past) > 0) {
    only_stocks <- FALSE
  }

  # Compile all parts of the Julia script
  times <- compile_times_julia(sfm)

  # Macros
  macros <- compile_macros_julia(sfm)

  # Prepare equations (centralized in R/prep_functions.R)
  sfm <- prep_equations_variables_julia(sfm)

  # Stocks (centralized in R/prep_functions.R)
  sfm <- prep_stock_change_julia(sfm)

  # Prepare intermediary variables
  intermediaries <- prep_intermediary_variables_julia(sfm, ordering = ordering)

  # Static equations
  static_eqn <- compile_static_eqn_julia(sfm,
    ensemble_pars = ensemble_pars,
    ordering = ordering,
    intermediaries = intermediaries
  )

  # Compile unit definitions
  units_def <- compile_units_julia(sfm)

  # Seed string
  seed_str <- ifelse(!is_defined(sfm[["sim_specs"]][["seed"]]), "",
    sprintf("# Ensure reproducibility across runs in case of random elements\nRandom.seed!(%s)\n", as.character(sfm[["sim_specs"]][["seed"]]))
  )

  prep_script <- sprintf(
    "# Script generated on %s by sdbuildR.\n\n%s%s%s%s",
    Sys.time(), seed_str,
    units_def[["script"]],
    times[["script"]],
    macros[["script"]]
  )

  # Compile ODE script
  ode <- compile_ode_julia(sfm,
    ensemble_pars = ensemble_pars,
    ordering = ordering,
    intermediaries = intermediaries,
    prep_script = prep_script, static_eqn = static_eqn,
    only_stocks = only_stocks
  )

  run_ode <- compile_run_ode_julia(sfm,
    ensemble_pars = ensemble_pars,
    static_eqn_script = static_eqn[["script"]],
    filepath_sim = filepath_sim,
    only_stocks = only_stocks,
    stock_names = static_eqn[["stock_names"]]
  )

  script <- paste0(
    prep_script, "\n",
    ode[["script_ode"]],
    ode[["script_callback"]],
    static_eqn[["ensemble_def"]],
    static_eqn[["script"]],
    run_ode[["script"]]
  )

  return(script)
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

  eqns <- sfm[["variables"]][["eqn_julia"]]

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

  return(invisible())
}


#' Prepare stock-and-flow model for ensemble range
#'
#' @inheritParams build
#' @inheritParams compile_julia
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

    # Set the equations of the variables in the model to 0 because these will be replaced by the ensemble parameters
    names_df <- get_names(sfm)
    
    for (i in seq_along(ensemble_pars[["range"]])) {
      name <- names(ensemble_pars[["range"]])[i]
      # Replace the equations of the chosen variables with ensemble_pars.name[i]
      # Find the variable in the data frame and update its eqn_julia
      var_idx <- sfm[["variables"]][["name"]] == name
      if (any(var_idx)) {
        sfm[["variables"]][var_idx, "eqn_julia"] <- "0.0"
      }
    }
  }

  return(list(sfm = sfm, ensemble_pars = ensemble_pars))
}


#' Prepare model for delayN and smoothN
#'
#' @inheritParams build
#' @param delayN_smoothN List with delayN and smoothN functions
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#' @noRd
prep_delayN_smoothN <- function(sfm, delayN_smoothN) {
  # If delayN() and smoothN() were used, add these to the model
  if (length(delayN_smoothN) > 0) {
    # Order alphabetically
    delayN_smoothN <- delayN_smoothN[sort(names(delayN_smoothN))]

    names_df <- get_names(sfm)
    allowed_delay_var <- names_df[names_df[["type"]] %in% c("stock", "flow", "aux", "gf"), "name"]
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
      new_stocks_data[[i]] <- data.frame(
        name = name,
        type = "delayN_smoothN",
        eqn = paste0(x[["initial"]], " / ", x[["length"]], " * ", x[["order"]]),
        eqn_julia = x[["setup"]],
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
      new_aux_data[[i]] <- data.frame(
        name = name,
        type = "delayN_smoothN",
        eqn = paste0(bare_var, " / ", x[["length"]], " * ", x[["order"]]),
        eqn_julia = x[["compute"]],
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

  return(sfm)
}


#' Compile script for defining a units module in Julia
#'
#' @inheritParams compile_julia
#'
#' @returns List with script
#'
#' @noRd
compile_units_julia <- function(sfm) {
  #   script <- sprintf("\n# Clear any existing definitions to avoid conflicts
  # if @isdefined(%s)
  #     # Force garbage collection to clean up old module
  #     %s = nothing
  #     GC.gc()
  # end\n", P[["MyCustomUnits"]], P[["MyCustomUnits"]])
  script <- ""

  if (length(sfm[["model_units"]]) > 0) {
    if (length(sfm[["model_units"]]) > 1) {
      # Topological sort of units
      eq_names <- names(sfm[["model_units"]])
      dependencies <- lapply(
        lapply(sfm[["model_units"]], `[[`, "eqn"),
        function(x) {
          unlist(stringr::str_extract_all(x, eq_names))
        }
      )
      out <- topological_sort(dependencies)

      if (out[["issue"]]) {
        cli::cli_inform(paste0("Ordering custom units failed. ", paste0(out[["msg"]])))
      }

      sfm[["model_units"]] <- sfm[["model_units"]][out[["order"]]]
    }


    unit_str <- lapply(sfm[["model_units"]], function(x) {
      if (is_defined(x[["eqn"]])) {
        unit_def <- x[["eqn"]]
      } else {
        unit_def <- "1"
      }

      paste0(
        "@unit ", x[["name"]], " \"", x[["name"]], "\" ",
        x[["name"]], " u\"", unit_def, "\" ", ifelse(x[["prefix"]], "true", "false")
      )
    }) |> paste0(collapse = sprintf(
      "\n\tUnitful.register(%s)\n\t",
      P[["MyCustomUnits"]]
    ))

    script <- paste0(
      script,
      "\n# Define custom units; register after each unit as some units may be defined by other units\n",
      # Turn off logging warnings to disable warnings about overwriting units

      "old_logger = global_logger(NullLogger())\n",
      "module ", P[["MyCustomUnits"]], "\n\t",

      # Need to load libraries again in module
      "using Unitful\n\tusing ",
      P[["jl_pkg_name"]], ".",
      P[["sdbuildR_units"]], "\n\t",
      unit_str,
      "\n\tUnitful.register(",
      P[["MyCustomUnits"]], ")\nend\n\n",
      "Unitful.register(", P[["MyCustomUnits"]], ")\n",

      # Turn logging warnings back on again
      "global_logger(old_logger)\n"
    )
  }

  return(list(script = script))
}


#' Compile Julia script for global variables
#'
#' @inheritParams compile_R
#'
#' @returns List with macro script
#' @noRd
compile_macros_julia <- function(sfm) {
  script <- ""

  macro_df <- sfm[[P[["macro_name"]]]]
  if (nrow(macro_df) > 0 && "eqn_julia" %in% names(macro_df)) {
    eqns <- macro_df[["eqn_julia"]]
    eqns[is.na(eqns)] <- ""
    has_eqn <- nzchar(eqns)

    if (any(has_eqn)) {
      script_body <- paste0(eqns[has_eqn], collapse = "\n")
      script <- paste0("\n\n# User-specified macros\n", script_body, "\n\n")
    }
  }

  return(list(script = script))
}


#' Compile Julia script for creating time vector
#'
#' @returns List
#' @importFrom rlang .data
#'
#' @inheritParams compile_julia
#'
#' @noRd
compile_times_julia <- function(sfm) {
  keep_unit <- sfm[["sim_specs"]][["keep_unit"]]
  script <- sprintf(
    "\n\n# Simulation time unit (smallest time scale in your model)
%s = u\"%s\"\n# Define time sequence\n%s = (%s, %s)%s\n# Initialize time (only necessary if constants use t)\n%s = %s[1]\n# Time step\n%s = %s%s\n# Save at value\n%s = %s%s\n# Define saving time sequence\n%s = %s%s; %s = %s:%s:%s[2]\n%s = %s[1]:%s:%s[2]\n",
    P[["time_units_name"]], sfm[["sim_specs"]][["time_units"]],
    P[["times_name"]], sfm[["sim_specs"]][["start"]], sfm[["sim_specs"]][["stop"]],
    ifelse(keep_unit, paste0(" .* ", P[["time_units_name"]]), ""),
    P[["time_name"]], P[["times_name"]],
    P[["timestep_name"]], sfm[["sim_specs"]][["dt"]],
    ifelse(keep_unit, paste0(" * ", P[["time_units_name"]]), ""),
    P[["saveat_name"]], sfm[["sim_specs"]][["save_at"]],
    ifelse(keep_unit, paste0(" * ", P[["time_units_name"]]), ""),
    P[["savefrom_name"]], sfm[["sim_specs"]][["save_from"]],
    ifelse(keep_unit, paste0(" .* ", P[["time_units_name"]]), ""),
    P[["savefrom_name"]], P[["savefrom_name"]],
    P[["saveat_name"]], P[["times_name"]],
    P[["tstops_name"]],
    P[["times_name"]],
    P[["timestep_name"]],
    P[["times_name"]]
  )

  return(list(script = script))
}


#' Prepare equations of all model variables
#'
#' @inheritParams build
#' @inheritParams compile
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#' @noRd
prep_equations_variables_julia <- function(sfm) {
  keep_unit <- sfm[["sim_specs"]][["keep_unit"]]
  keep_nonnegative_flow <- sfm[["sim_specs"]][["keep_nonnegative_flow"]]
  names_df <- get_names(sfm)

  # Graphical functions - refactored to use data frame approach
  gf_idx <- sfm[["variables"]][["type"]] == "gf"
  
  if (any(gf_idx)) {
    for (i in which(gf_idx)) {
      x <- sfm[["variables"]][i, ]
      
      if (is_defined(x[["xpts"]]) & is_defined(x[["ypts"]])) {
        # Check whether xpts is defined as numeric or string
        xpts_val <- x[["xpts"]][[1]]  # Extract from list-column
        if (inherits(xpts_val, "numeric")) {
          xpts_str <- paste0("[", paste0(as.character(xpts_val), collapse = ", "), "]")
        } else {
          xpts_str <- stringr::str_replace_all(
            xpts_val,
            "^c\\(", "["
          ) |>
            stringr::str_replace_all("\\)$", "]")
        }

        # Add units of source if defined
        if (keep_unit) {
          if (is_defined(x[["source"]])) {
            if (x[["source"]] == "t") {
              xpts_str <- paste0(xpts_str, " .* ", P[["time_units_name"]])
            } else {
              unit_source <- names_df[names_df[["name"]] == x[["source"]], "units"]
              if (is_defined(unit_source) && unit_source != "1") {
                xpts_str <- paste0(xpts_str, " .* u\"", unit_source, "\"")
              }
            }
          }
        }

        # Check whether ypts is defined as numeric or string
        ypts_val <- x[["ypts"]][[1]]  # Extract from list-column
        if (inherits(ypts_val, "numeric")) {
          ypts_str <- paste0("[", paste0(as.character(ypts_val), collapse = ", "), "]")
        } else {
          ypts_str <- stringr::str_replace_all(ypts_val, "^c\\(", "[") |>
            stringr::str_replace_all("\\)$", "]")
        }

        if (keep_unit & is_defined(x[["units"]]) & x[["units"]] != "1") {
          ypts_str <- paste0(ypts_str, " .* u\"", x[["units"]], "\"")
        }

        sfm[["variables"]][i, "eqn_str"] <- sprintf(
          "%s = itp(%s,\n\t%s, method = \"%s\", extrapolation = \"%s\")",
          x[["name"]], xpts_str, ypts_str,
          x[["interpolation"]], x[["extrapolation"]]
        )
      }
    }
  }

  # Constant equations - refactored to use data frame approach
  const_idx <- sfm[["variables"]][["type"]] == "constant"
  
  if (any(const_idx)) {
    for (i in which(const_idx)) {
      if (keep_unit & is_defined(sfm[["variables"]][i, "units"]) & sfm[["variables"]][i, "units"] != "1") {
        sfm[["variables"]][i, "eqn_str"] <- paste0(
          sfm[["variables"]][i, "name"], " = ",
          P[["convert_u_func"]], "(",
          sfm[["variables"]][i, "eqn_julia"], ", u\"", 
          sfm[["variables"]][i, "units"], "\")"
        )
      } else {
        sfm[["variables"]][i, "eqn_str"] <- paste0(
          sfm[["variables"]][i, "name"], " = ", 
          sfm[["variables"]][i, "eqn_julia"]
        )
      }
    }
  }

  # Initial states of stocks - refactored to use data frame approach
  stock_idx <- sfm[["variables"]][["type"]] == "stock"
  
  if (any(stock_idx)) {
    for (i in which(stock_idx)) {
      if (keep_unit & is_defined(sfm[["variables"]][i, "units"]) & sfm[["variables"]][i, "units"] != "1") {
        sfm[["variables"]][i, "eqn_str"] <- paste0(
          sfm[["variables"]][i, "name"], " = ",
          P[["convert_u_func"]], "(",
          sfm[["variables"]][i, "eqn_julia"], ", u\"",
          sfm[["variables"]][i, "units"], "\")"
        )
      } else {
        sfm[["variables"]][i, "eqn_str"] <- paste0(
          sfm[["variables"]][i, "name"], " = ", 
          sfm[["variables"]][i, "eqn_julia"]
        )
      }
    }
  }


  # Auxiliary equations (dynamic auxiliaries) - refactored to use data frame approach
  aux_idx <- sfm[["variables"]][["type"]] == "aux"
  
  if (any(aux_idx)) {
    for (i in which(aux_idx)) {
      if (keep_unit & is_defined(sfm[["variables"]][i, "units"]) & sfm[["variables"]][i, "units"] != "1") {
        eqn_str <- paste0(
          sfm[["variables"]][i, "name"], " = ", 
          P[["convert_u_func"]],
          "(", sfm[["variables"]][i, "eqn_julia"], ", u\"",
          sfm[["variables"]][i, "units"], "\")"
        )
      } else {
        eqn_str <- paste0(
          sfm[["variables"]][i, "name"], " = ", 
          sfm[["variables"]][i, "eqn_julia"]
        )
      }

      if (!is.null(sfm[["variables"]][i, "preceding_eqn"])) {
        eqn_str <- c(sfm[["variables"]][i, "preceding_eqn"], eqn_str)
      }
      
      sfm[["variables"]][i, "eqn_str"] <- list(eqn_str)
    }
  }

  # Flow equations - refactored to use data frame approach
  flow_idx <- sfm[["variables"]][["type"]] == "flow"
  
  if (any(flow_idx)) {
    for (i in which(flow_idx)) {
      flow_name <- sfm[["variables"]][i, "name"]
      flow_from <- sfm[["variables"]][i, "from"]
      flow_to <- sfm[["variables"]][i, "to"]
      flow_eqn_julia <- sfm[["variables"]][i, "eqn_julia"]
      flow_units <- sfm[["variables"]][i, "units"]
      flow_non_neg <- sfm[["variables"]][i, "non_negative"]
      
      eqn_str <- sprintf(
        "\n\t# Flow%s%s\n\t%s = %s%s%s%s%s",
        # Add comment
        ifelse(is_defined(flow_from), paste0(" from ", flow_from), ""),
        ifelse(is_defined(flow_to), paste0(" to ", flow_to), ""),
        flow_name,
        ifelse(keep_unit & flow_units != "1",
          paste0(P[["convert_u_func"]], "("), ""
        ),
        ifelse(flow_non_neg & keep_nonnegative_flow, "nonnegative(", ""),
        flow_eqn_julia,
        ifelse(flow_non_neg & keep_nonnegative_flow, ")", ""),
        ifelse(keep_unit & flow_units != "1",
          paste0(", u\"", flow_units, "\")"), ""
        )
      )

      if (!is.null(sfm[["variables"]][i, "preceding_eqn"])) {
        eqn_str <- c(sfm[["variables"]][i, "preceding_eqn"], eqn_str)
      }
      
      sfm[["variables"]][i, "eqn_str"] <- list(eqn_str)
    }
  }


  return(sfm)
}


#' Prepare intermediary variables
#'
#' @inheritParams build
#' @inheritParams compile_static_eqn_julia
#'
#' @returns List of intermediary variables and values
#' @noRd
prep_intermediary_variables_julia <- function(sfm, ordering) {
  # Create separate vector for names of intermediate variables and values, because graphical functions need to be in the intermediate funciton as gf(t), but their name should be gf
  intermediary_var <- intermediary_var_values <- ordering[["dynamic"]][["order"]]

  # Graphical functions (gf)
  gf_str <- ""
  gf_idx <- sfm[["variables"]][["type"]] == "gf"
  
  if (any(gf_idx)) {
    # Some gf have other gf as source; recursively replace
    gf_sources <- sfm[["variables"]][gf_idx, "source"]
    gf_names <- sfm[["variables"]][gf_idx, "name"]
    gf_sources_named <- stats::setNames(gf_sources, gf_names)
      compact_() |>
      unlist()

    if (length(gf_sources) > 0) {
      # Graphical functions with source
      gf <- paste0(names(gf_sources), "(", unname(gf_sources), ")") |> stats::setNames(names(gf_sources))

      # Create dictionary to add source to nested graphical functions
      dict2 <- paste0("(", names(gf_sources), "(", unname(gf_sources), "))") |>
        stats::setNames(paste0("\\(", stringr::str_escape(names(gf_sources)), "\\)"))

      gf_str <- stringr::str_replace_all(unname(gf), dict2)

      # Add names of graphical functions to intermediary_var
      intermediary_var <- c(intermediary_var, names(gf))
      intermediary_var_values <- c(intermediary_var_values, gf_str)
    }
  }

  # Add fixed delayed and past variables to intermediary_var
  delay_past <- get_delay(sfm, type = "past")
  extra_intermediary_var <- list_extract(delay_past, "var")


  if (length(extra_intermediary_var) > 0) {
    # Check whether the intermediary variables are in the model
    names_df <- get_names(sfm)
    allowed_intermediary_var <- names_df[names_df[["type"]] %in% c("stock", "flow", "aux"), "name"]

    idx <- !(extra_intermediary_var %in% names_df[["name"]])
    if (any(idx)) {
      cli::cli_abort(paste0(
        "The following variables used in delay() or past() are not defined in the model: ",
        paste0(extra_intermediary_var[idx], collapse = ", ")
      ), call. = FALSE)
    }

    idx <- !(extra_intermediary_var %in% allowed_intermediary_var)
    if (any(idx)) {
      cli::cli_abort(paste0(
        "The following variables used in delay() or past() are not stocks, flows, or auxiliaries: ",
        paste0(extra_intermediary_var[idx], collapse = ", ")
      ), call. = FALSE)
    }

    # Get unique intermediary variables to add
    new_intermediary_var <- setdiff(unique(unlist(extra_intermediary_var)), intermediary_var)

    if (length(new_intermediary_var) > 0) {
      intermediary_var <- c(intermediary_var, new_intermediary_var)
      intermediary_var_values <- c(intermediary_var_values, new_intermediary_var)
    }
  }

  # If delayN() and smoothN() were used, state has to be unpacked differently
  delayN_smoothN <- get_delay(sfm, type = "delayN_smoothN")

  if (length(delayN_smoothN) > 0) {
    delay_names <- names(unlist(unname(delayN_smoothN), recursive = FALSE))

    intermediary_var <- setdiff(intermediary_var, delay_names)
    intermediary_var_values <- setdiff(intermediary_var_values, delay_names)
  }

  # Order intermediary variables and values alphabetically
  if (length(extra_intermediary_var) > 0) {
    idx <- order(intermediary_var)
    intermediary_var <- intermediary_var[idx]
    intermediary_var_values <- intermediary_var_values[idx]
  }

  return(list(
    names = intermediary_var,
    values = intermediary_var_values
  ))
}


#' Compile Julia script for static variables, i.e. initial conditions, functions, and parameters
#'
#' @inheritParams compile_julia
#' @inheritParams order_equations
#' @param ordering List with order of static and dynamic variables, output of order_equations()
#' @param intermediaries List with intermediary variables and values
#'
#' @returns List with necessary scripts
#'
#' @noRd
compile_static_eqn_julia <- function(sfm, ensemble_pars, ordering, intermediaries) {
  # Access keep_unit from sim_specs
  keep_unit <- sfm[["sim_specs"]][["keep_unit"]]

  # names_df <- get_names(sfm)

  # Graphical functions
  gf_eqn <- get_vars_column_as_list(sfm, "gf", "eqn_str")

  # Constant equations
  constant_eqn <- get_vars_column_as_list(sfm, "constant", "eqn_str")

  # Initial states of stocks
  stock_eqn <- get_vars_column_as_list(sfm, "stock", "eqn_str")


  # Prepare ensemble range if specified
  if (length(ensemble_pars[["range"]]) > 0) {
    ensemble_def <- paste0(
      "\n\n# Generate ensemble design\n",
      P[["ensemble_n"]], " = ",
      format(ensemble_pars[["n"]], scientific = FALSE), "\n",
      P[["ensemble_range"]], " = (\n",
      paste0(paste0(
        names(ensemble_pars[["range"]]), " = ",
        unname(ensemble_pars[["range"]])
      ), collapse = ",\n"),
      ",\n)\n",
      P[["ensemble_pars"]], ", ",
      P[["ensemble_total_n"]],
      " = generate_param_combinations(\n",
      P[["ensemble_range"]], "; crossed=",
      ifelse(ensemble_pars[["cross"]], "true", "false"), ", n_replicates = ",
      P[["ensemble_n"]], ")\n",
      # Initialize ensemble range iterator if specified
      P[["ensemble_iter"]], " = 1\n"
    )

    ensemble_iter <- paste0(
      "\n\t# Assign ensemble parameters\n\t",
      paste0(names(ensemble_pars[["range"]]), collapse = ", "), ", = ",
      P[["ensemble_pars"]], "[div(",
      P[["ensemble_iter"]], "-1, ",
      P[["ensemble_n"]], ") + 1]\n\n"
    )

    # Remove ensemble variables from equations
    ordering[["static"]][["order"]] <- ordering[["static"]][["order"]][!ordering[["static"]][["order"]] %in% names(ensemble_pars[["range"]])]
    ordering[["static_and_dynamic"]][["order"]] <- ordering[["static_and_dynamic"]][["order"]][!ordering[["static_and_dynamic"]][["order"]] %in% names(ensemble_pars[["range"]])]
  } else if (!is.null(ensemble_pars)) {
    ensemble_def <- paste0(
      P[["ensemble_n"]], " = ", format(ensemble_pars[["n"]], scientific = FALSE), "\n",
      P[["ensemble_total_n"]], " = ", format(ensemble_pars[["n"]], scientific = FALSE), "\n",
      # Initialize ensemble range iterator if specified
      P[["ensemble_iter"]], " = 1\n"
    )
    ensemble_iter <- ""
  } else {
    ensemble_def <- ensemble_iter <- ""
  }


  # If there was an issue with the ordering of static and dynamic equations, only compile static equations
  if (ordering[["static_and_dynamic"]][["issue"]]) {
    # Compile and order static equations
    static_eqn_str <- unlist(c(gf_eqn, constant_eqn, stock_eqn)[ordering[["static"]][["order"]]]) |>
      paste0(collapse = "\n")
  } else {
    # Auxiliary equations (dynamic auxiliaries)
    aux_eqn <- get_vars_column_as_list(sfm, "aux", "eqn_str")

    # Flow equations
    flow_eqn <- get_vars_column_as_list(sfm, "flow", "eqn_str")

    # Compile and order static and dynamic equations
    static_eqn_str <- c(
      gf_eqn, constant_eqn, stock_eqn,
      aux_eqn, flow_eqn
    )[ordering[["static_and_dynamic"]][["order"]]] |>
      unlist() |>
      paste0(collapse = "\n")
  }

  # Replace any reference to model_setup.intermediary_names with intermediary_names
  static_eqn_str <- stringr::str_replace_all(
    static_eqn_str,
    paste0(
      P[["model_setup_name"]], "\\.",
      P[["intermediary_names"]]
    ),
    P[["intermediary_names"]]
  )


  # Put parameters together in named tuple; include graphical functions as otherwise these are not defined outside of the let block
  if (nrow(get_variables_by_type(sfm, "constant")) > 0 || nrow(get_variables_by_type(sfm, "gf")) > 0) {
    pars_def <- paste0(
      "\n\n# Define parameters in named tuple\n",
      P[["parameter_name"]], " = (",
      paste0(c(names(constant_eqn), names(gf_eqn)), " = ",
        c(names(constant_eqn), names(gf_eqn)),
        collapse = ", "
      ), ",)\n"
    )
  } else {
    pars_def <- paste0("\n\n# Define empty parameters\n", P[["parameter_name"]], " = ()\n")
  }

  # Check for delayN() and smoothN() functions
  delayN_smoothN <- get_delay(sfm, type = "delayN_smoothN")

  if (length(delayN_smoothN) > 0) {
    delay_names <- names(unlist(unname(delayN_smoothN), recursive = FALSE))

    # Preserve order of stocks but wrap delayN and smoothN stocks in values() and keys()
    x <- y <- names(stock_eqn)
    idx <- names(stock_eqn) %in% delay_names
    x[idx] <- paste0("values(", x[idx], ")")
    y[!idx] <- paste0(":", y[!idx])
    y[idx] <- paste0("keys(", y[idx], ")...")

    init_def_stocks <- paste0(x, collapse = ", ")
    init_names <- paste0(y, collapse = ", ")

    # Find indices of names in vector
    init_idx <- paste0(
      "\n", P[["delay_idx_name"]], " = (",
      paste0(paste0(
        delay_names, " = ",
        "findall(n -> occursin(r\"", delay_names, P[["acc_suffix"]], "[0-9]+$\", string(n)), ",
        P[["initial_value_names"]], ")"
      ), collapse = ",\n\t"),
      ",)\n"
    )


    # Make sure that any .outflow references are replaced with first(values(variable))
    dict <- stringr::fixed(stats::setNames(
      paste0("first(values(", delay_names, "))"),
      paste0(delay_names, P[["outflow_suffix"]])
    ))
    static_eqn_str <- stringr::str_replace_all(static_eqn_str, dict)
  } else {
    init_def_stocks <- paste0(names(stock_eqn), collapse = ", ")
    # Symbols are faster than characters
    init_names <- paste0(paste0(":", names(stock_eqn)), collapse = ", ")
    init_idx <- ""
  }

  # Put initial states together in (unnamed) vector
  init_def <- paste0(
    "\n# Define initial condition in vector\n",
    P[["initial_value_name"]],
    " = [Base.Iterators.flatten(",
    ifelse(keep_unit, "Unitful.ustrip.(", ""), "[",
    init_def_stocks,
    # Add extra comma in case there is only one stock
    ",]", ifelse(keep_unit, ")", ""), ")...]\n"
  )

  init_names <- paste0(
    P[["initial_value_names"]], " = [",
    init_names,
    "]\n"
  )


  if (length(intermediaries[["names"]]) > 0) {
    intermediary_names <- paste0(P[["intermediary_names"]], " = [", paste0(
      paste0(
        ":",
        intermediaries[["names"]]
      ),
      collapse = ", "
    ), "]\n")

    # Remove names of intermediaries that are functions; these won't be saved
    intermediary_names_correct <- paste0(
      "\n# Remove names of intermediaries that are functions\n",
      "is_not_function = collect(.!is_function_or_interp.((",
      paste0(intermediaries[["values"]], collapse = ", "),
      ifelse(length(intermediaries[["values"]]) == 1, ",", ""),
      ")))\n",
      P[["intermediary_names"]], " = ", P[["intermediary_names"]], "[is_not_function]\n"
    )
  } else {
    intermediary_names <- paste0(P[["intermediary_names"]], " = Nothing\n")
    intermediary_names_correct <- ""
  }


  return(list(
    stock_names = names(stock_eqn),
    par_names = c(names(constant_eqn), names(gf_eqn)),
    ensemble_def = ensemble_def,
    script = paste0(
      "\n\n# Define parameters, initial conditions, and functions in correct order\n",
      P[["model_setup_name"]],
      " = let\n",
      # Assign ensemble parameters for this iteration
      ensemble_iter,
      intermediary_names,
      static_eqn_str,
      pars_def,
      init_def,
      init_names,
      init_idx,
      intermediary_names_correct,
      "\n\t(", P[["parameter_name"]], " = ",
      P[["parameter_name"]], ", ",
      P[["initial_value_name"]], " = ",
      P[["initial_value_name"]], ", ",
      P[["initial_value_names"]], " = ",
      P[["initial_value_names"]], ", ",
      P[["intermediary_names"]], " = ",
      P[["intermediary_names"]],
      ifelse(nzchar(init_idx),
        paste0(
          ", ", P[["delay_idx_name"]], " = ",
          P[["delay_idx_name"]]
        ), ""
      ),
      ")\n",
      "end\n"
    )
  ))
}


#' Prepare for summing change in stocks in stock-and-flow model in Julia script
#'
#' @inheritParams compile_julia
#'
#' @returns Updated stock-and-flow model
#' @noRd
#'
## prep_stock_change_julia centralized in R/prep_functions.R


#' Compile Julia script for ODE function
#'
#' @inheritParams build
#' @inheritParams compile
#' @inheritParams compile_julia
#' @inheritParams order_equations
#' @inheritParams compile_static_eqn
#' @inheritParams compile_static_eqn_julia
#' @param prep_script Intermediate output of compile_julia()
#' @param static_eqn Output of compile_static_eqn()
#'
#' @returns List
#' @importFrom rlang .data
#' @noRd
#'
compile_ode_julia <- function(sfm, ensemble_pars, ordering, intermediaries,
                              prep_script, static_eqn,
                              only_stocks) {
  keep_nonnegative_stock <- sfm[["sim_specs"]][["keep_nonnegative_stock"]]
  keep_unit <- sfm[["sim_specs"]][["keep_unit"]]
  # Auxiliary equations (dynamic auxiliaries)
  aux_eqn <- get_vars_column_as_list(sfm, "aux", "eqn_str")

  # Flow equations
  flow_eqn <- get_vars_column_as_list(sfm, "flow", "eqn_str")

  # Compile and order all dynamic equations
  dynamic_eqn <- unlist(c(aux_eqn, flow_eqn)[ordering[["dynamic"]][["order"]]])

  # Compile and order all dynamic equations
  dynamic_eqn_str <- paste0(dynamic_eqn, collapse = "\n\t")

  # Sum change in stock equations
  stock_change <- lapply(
    get_variables_by_type(sfm, "stock")[["name"]],
    function(stock_name) {
      stock_row <- sfm[["variables"]][sfm[["variables"]][["name"]] == stock_name, ]
      x <- as.list(stock_row)
      sprintf(
        "%s %s %s", x[["sum_name"]],
        # Broadcast assignment for delayed variables
        ifelse(x[["type"]] == "delayN_smoothN", ".=", "="),
        x[["sum_eqn"]]
      )
    }
  ) |> compact_()

  stock_change_str <- paste0(stock_change, collapse = "\n\t")

  # Add units to stocks
  add_stock_units <- ""
  if (keep_unit) {
    # For each stock that has a unit, add
    stock_vars <- get_variables_by_type(sfm, "stock")
    stock_names <- stock_vars[["name"]]
    stock_units <- stock_vars[["units"]]
    idx <- stock_units != "1"
    if (any(idx)) {
      add_stock_units <- paste0(
        "\n\n\t# Assign units to stocks\n\t",
        paste0(paste0(
          stock_names[idx], " = ",
          stock_names[idx], " .* u\"",
          stock_units[idx], "\""
        ), collapse = "\n\t"), "\n"
      )
    }
  }

  # Non-negative stocks
  stock_vars_df <- get_variables_by_type(sfm, "stock")
  nonneg_stocks <- stock_vars_df[["non_negative"]] |> unlist()
  add_nonneg <- any(nonneg_stocks) & keep_nonnegative_stock

  if (add_nonneg) {
    # Create if-statement to keep selected stocks non-negative
    nonneg_str <- lapply(
      seq_len(nrow(stock_vars_df)),
      function(i) {
        x <- as.list(stock_vars_df[i, ])
        if (x[["non_negative"]]) {
          sprintf(
            "if (%s * %s + %s < 0) %s = %s/%s end",
            x[["sum_name"]], P[["timestep_name"]], x[["name"]],
            x[["sum_name"]], x[["name"]], P[["timestep_name"]]
          )
        } else {
          return(NULL)
        }
      }
    ) |> compact_()

    # Format complete string with explanation
    stock_change_str <- paste0(
      stock_change_str,
      "\n\n\t# Prevent ", paste0(names(nonneg_str), collapse = ", "),
      " from turning negative\n\t",
      paste0(nonneg_str, collapse = "\n\t")
    )
  }

  # # Names of changing stocks, e.g. dR for stock R
  # stock_changes_names <- unname(unlist(
  #   lapply(sfm[["model"]][["variables"]][["stock"]], `[[`, "sum_name")
  # ))

  # If delayN() and smoothN() were used, state has to be unpacked differently
  delayN_smoothN <- get_delay(sfm, type = "delayN_smoothN")

  if (length(delayN_smoothN) > 0) {
    delay_names <- names(unlist(unname(delayN_smoothN), recursive = FALSE))

    # Unpack non delayN stocks
    unpack_nondelayN <- paste0(
      paste0(setdiff(names(stock_change), delay_names), collapse = ", "), ", = ", P[["state_name"]], "[findall(n -> !occursin(r\"", P[["delayN_suffix"]], "[0-9]+", P[["acc_suffix"]], "[0-9]+$|",
      P[["smoothN_suffix"]], "[0-9]+", P[["acc_suffix"]], "[0-9]+$\", string(n)), ", P[["model_setup_name"]], ".", P[["initial_value_names"]], ")]"
    )

    # Unpack each delayN or smoothN stock separately
    unpack_delayN <- lapply(
      seq_len(nrow(stock_vars_df)),
      function(i) {
        x <- as.list(stock_vars_df[i, ])
        if (is_defined(x[["unpack_state"]])) {
          return(paste0(x[["name"]], " = ", x[["unpack_state"]]))
        }
      }
    ) |> compact_()

    unpack_state_str <- paste0(unpack_nondelayN, "\n\t", paste0(unpack_delayN, collapse = "\n\t"))
  } else {
    unpack_state_str <- paste0(
      paste0(names(stock_change), collapse = ", "),
      ", = ", P[["state_name"]]
    )
  }

  # Compile
  script_ode <- paste0(
    sprintf(
      "\n\n# Define ODE
function %s!(%s, %s%s, %s)",
      P[["ode_func_name"]],
      P[["change_state_name"]], P[["state_name"]],
      paste0(", ", P[["parameter_name"]]), P[["time_name"]]
    ),
    # "\n\n\t# Round t to deal with inaccuracies in floating point arithmetic\n\t",
    # P[["time_name"]], " = round_(",
    # P[["time_name"]], ", digits = 12)",
    "\n\n\t# Unpack state variables\n\t",
    unpack_state_str,
    # Assign units to stocks
    add_stock_units,
    ifelse(nrow(get_variables_by_type(sfm, "constant")) > 0,
      paste0(
        "\n\n\t# Unpack parameters\n\t",
        paste0(
          paste0(static_eqn[["par_names"]], collapse = ", "),
          ", = ", P[["parameter_name"]]
        )
      ), ""
    ),
    "\n\n\t# Update auxiliaries\n\t",
    dynamic_eqn_str,
    "\n\n\t# Collect inflows and outflows for each stock\n\t",
    stock_change_str,
    "\n\tnothing\nend\n"
  )

  # Compile callback function
  if (only_stocks) {
    script_callback <- paste0(
      "\n\n# Define empty callback function\n",
      P[["intermediaries"]], " = nothing\n",
      P[["callback_name"]], " = nothing\n\n"
    )
  } else {
    script_callback <- paste0(
      sprintf(
        "\n\n# Define callback function
function %s(%s, %s, integrator)",
        P[["callback_func_name"]],
        P[["state_name"]], P[["time_name"]]
      ),
      # "\n\n\t# Round t to deal with inaccuracies in floating point arithmetic\n\t",
      # P[["time_name"]], " = round_(", P[["time_name"]], ", digits = 12)",
      "\n\n\t# Unpack state variables\n\t",
      unpack_state_str,
      # Assign units to stocks
      add_stock_units,
      ifelse(nrow(get_variables_by_type(sfm, "constant")) > 0,
        paste0(
          "\n\n\t# Get parameters from integrator\n\t",
          paste0(P[["parameter_name"]], " = integrator.p"),
          # Check whether you're overwriting it
          "\n\n\t# Unpack parameters\n\t",
          paste0(
            paste0(static_eqn[["par_names"]], collapse = ", "),
            ", = ", P[["parameter_name"]]
          )
        ), ""
      ),
      "\n\n\t# Update auxiliaries\n\t",
      dynamic_eqn_str,
      "\n\n\t# Return intermediary values and remove funcions\n\t",
      "return filter(x -> !is_function_or_interp(x), (",
      paste0(intermediaries[["values"]], collapse = ", "),
      ifelse(length(intermediaries[["values"]]) == 1, ",", ""),
      "))\n",
      "\n\nend\n\n# Callback setup\n",

      # Only define intermediaries if ensemble_pars is NULL
      ifelse(!is.null(ensemble_pars), "", paste0(
        P[["intermediaries"]], " = SavedValues(",
        # Make time a Unitful.Quantity if keeping units, otherwise a float
        "eltype(", P[["time_name"]], "), Any)\n",
        P[["callback_name"]], " = SavingCallback(",
        P[["callback_func_name"]], ", ", P[["intermediaries"]],
        ", saveat = ", P[["savefrom_name"]],
        ")\n"
      ))
    )
  }

  return(list(
    script_ode = script_ode,
    script_callback = script_callback
  ))
}


#' Compile Julia script for running ODE
#'
#' @param filepath_sim Path to output file
#' @param nonneg_stocks Output of compile_nonneg_stocks()
#' @param stock_names Names of stocks
#' @param static_eqn_script Output of compile_static_eqn_julia(); only used in this function for ensemble trajectories.
#' @inheritParams compile_ode_julia
#' @inheritParams compile_julia
#' @inheritParams compile_R
#' @inheritParams order_equations
#'
#' @returns List
#' @noRd
#'
compile_run_ode_julia <- function(sfm,
                                  ensemble_pars,
                                  static_eqn_script,
                                  filepath_sim,
                                  nonneg_stocks,
                                  stock_names,
                                  only_stocks) {
  keep_unit <- sfm[["sim_specs"]][["keep_unit"]]
  if (is.null(ensemble_pars)) {
    script <- paste0(
      "\n\n# Run ODE\n",
      P[["prob_name"]], " = ODEProblem(",
      P[["ode_func_name"]], "!, ",
      P[["model_setup_name"]], ".",
      P[["initial_value_name"]],
      ", ", P[["times_name"]], ", ",
      P[["model_setup_name"]], ".",
      P[["parameter_name"]],
      ")\n", P[["solution_name"]], " = solve(",
      P[["prob_name"]], ", ",
      sfm[["sim_specs"]][["method"]],
      paste0(
        ", dt = ", P[["timestep_name"]],
        ", saveat = ", P[["savefrom_name"]],
        ", tstops = ", P[["tstops_name"]],
        ", adaptive = false"
      ),
      ifelse(!only_stocks,
        paste0(
          ", ", P[["callback_name"]], " = ",
          P[["callback_name"]]
        ), ""
      ),
      ")\n",
      P[["sim_df_name"]],
      ", ", P[["parameter_name"]],
      ", ", P[["parameter_names"]],
      ", ", P[["initial_value_name"]],
      ", ", P[["initial_value_names"]],
      " = clean_df(",
      P[["prob_name"]], ", ",
      P[["solution_name"]], ", ",
      P[["model_setup_name"]], ".",
      P[["initial_value_names"]], ", ",
      P[["intermediaries"]], ", ",
      P[["model_setup_name"]], ".",
      P[["intermediary_names"]], ")\n"
    )

    # Save to CSV
    script <- paste0(
      script, '\nCSV.write("', filepath_sim, '", ',
      P[["sim_df_name"]],
      ")\n\n# Delete variables\n",
      P[["solution_name"]], " = Nothing\n",
      P[["sim_df_name"]], " = Nothing\n",
      "Nothing"
    )
  } else if (!is.null(ensemble_pars)) {
    script <- paste0(
      "\n\n# Create ODE problem\n",
      P[["prob_name"]], " = ODEProblem(",
      P[["ode_func_name"]], "!, ",
      P[["model_setup_name"]], ".",
      P[["initial_value_name"]],
      ", ", P[["times_name"]], ", ",
      P[["model_setup_name"]], ".",
      P[["parameter_name"]],
      ")\n\n",

      # Callback in ensemble
      ifelse(!only_stocks, paste0(
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
      ), ""),


      # Ensemble problem function
      "# Define ensemble problem\nfunction ",
      P[["ensemble_func_name"]], "(prob, ",
      P[["ensemble_iter"]], ", repeat)\n",
      static_eqn_script,
      ifelse(!only_stocks, paste0(
        "\n\t", P[["callback_name"]],
        " = SavingCallback(",
        P[["callback_func_name"]], ", ",
        P[["intermediaries"]], "[",
        P[["ensemble_iter"]], "], saveat = ",
        P[["savefrom_name"]],
        ")\n"
      ), ""),
      "\n\tremake(prob, u0 = ",
      P[["model_setup_name"]], ".",
      P[["initial_value_name"]],
      ", p = ", P[["model_setup_name"]], ".",
      P[["parameter_name"]],
      ifelse(!only_stocks, paste0(
        ", ", P[["callback_name"]],
        " = ", P[["callback_name"]]
      ), ""),
      ")\nend\n\n",

      # Output function
      "function ", P[["ensemble_output_func"]], "(sol, i)\n",
      "\t# Save both solution and parameters\n",
      "\treturn (t = sol.t, u = sol.u, p = sol.prob.p, u0 = sol.prob.u0), false\n",
      "end\n\n",

      # Ensemble problem definition
      P[["ensemble_prob_name"]], " = EnsembleProblem(",
      P[["prob_name"]], ", prob_func = ",
      P[["ensemble_func_name"]],
      ", output_func = ", P[["ensemble_output_func"]], ")\n",
      # Solve ensemble problem
      P[["solution_name"]], " = solve(",
      P[["ensemble_prob_name"]], ", ",
      sfm[["sim_specs"]][["method"]],
      ifelse(ensemble_pars[["threaded"]], ", EnsembleThreads()", ""),
      ", dt = ", P[["timestep_name"]],
      ", saveat = ", P[["savefrom_name"]],
      ", tstops = ", P[["tstops_name"]],
      ", adaptive = false, trajectories = ",
      P[["ensemble_total_n"]], ");\n"
    )

    # Save timeseries dataframe
    script <- paste0(
      script, "\n# Save timeseries dataframe\n",
      P[["sim_df_name"]], ", ",
      P[["parameter_name"]], ", ",
      P[["initial_value_name"]],
      ifelse(ensemble_pars[["threaded"]], " = ensemble_to_df_threaded(",
        " = ensemble_to_df("
      ),
      P[["solution_name"]], ", ",
      P[["model_setup_name"]], ".",
      P[["initial_value_names"]],
      ", ",
      P[["intermediaries"]],
      ", ",
      P[["model_setup_name"]], ".",
      P[["intermediary_names"]],
      ", ", P[["ensemble_n"]],
      ")\n"
    )

    if (ensemble_pars[["return_sims"]]) {
      script <- paste0(
        script, 'CSV.write("',
        ensemble_pars[["filepath_df"]][["df"]], '", ',
        P[["sim_df_name"]], ")\n"
      )
      script <- paste0(
        script, 'CSV.write("',
        ensemble_pars[["filepath_df"]][["constants"]], '", ',
        P[["parameter_name"]], ")\n"
      )
      script <- paste0(
        script, 'CSV.write("',
        ensemble_pars[["filepath_df"]][["init"]], '", ',
        P[["initial_value_name"]], ")\n"
      )
    }

    # Compute summary statistics
    script <- paste0(
      script, "\n# Compute summary statisics\n",
      P[["summary_df_name"]],
      ifelse(ensemble_pars[["threaded"]], " = ensemble_summ_threaded(",
        " = ensemble_summ("
      ),
      P[["sim_df_name"]], ", ",
      "[", paste0(ensemble_pars[["quantiles"]], collapse = ", "),
      "])\n\n",
      P[["parameter_name"]], "[!, :time] .= 0.0\n",
      P[["summary_df_constants_name"]],
      ifelse(ensemble_pars[["threaded"]], " = ensemble_summ_threaded(",
        " = ensemble_summ("
      ),
      P[["parameter_name"]], ", ",
      "[", paste0(ensemble_pars[["quantiles"]], collapse = ", "),
      "])\n",
      "select!(", P[["summary_df_constants_name"]],
      ", Not(:time))\n\n",
      P[["initial_value_name"]], "[!, :time] .= 0.0\n",
      P[["summary_df_init_name"]],
      ifelse(ensemble_pars[["threaded"]], " = ensemble_summ_threaded(",
        " = ensemble_summ("
      ),
      P[["initial_value_name"]], ", ",
      "[", paste0(ensemble_pars[["quantiles"]], collapse = ", "),
      "])\n",
      "select!(", P[["summary_df_init_name"]],
      ", Not(:time))\n\n",
      "\n# Save to CSV\n",
      "CSV.write(\"", ensemble_pars[["filepath_summary"]][["df"]], "\", ",
      P[["summary_df_name"]], ")\n\n",
      "CSV.write(\"", ensemble_pars[["filepath_summary"]][["constants"]], "\", ",
      P[["summary_df_constants_name"]], ")\n\n",
      "CSV.write(\"", ensemble_pars[["filepath_summary"]][["init"]], "\", ",
      P[["summary_df_init_name"]], ")\n\n# Delete variables\n",
      P[["sim_df_name"]], " = Nothing\n",
      P[["parameter_name"]], " = Nothing\n",
      P[["initial_value_name"]], " = Nothing\n",
      P[["summary_df_name"]], " = Nothing\n",
      P[["summary_df_constants_name"]], " = Nothing\n",
      P[["summary_df_init_name"]], " = Nothing\n",
      P[["solution_name"]], " = Nothing\n",
      P[["intermediaries"]], " = Nothing\n"
    )
  }

  return(list(script = script))
}


#' Write a string to a temporary file
#'
#' @param script String containing the code to write
#' @param fileext String with file extension, either ".R" or ".jl"
#' @returns The path to the created file
#'
#' @noRd
write_script <- function(script,
                         filepath) {
  filepath <- normalizePath(filepath, winslash = "/", mustWork = FALSE)

  # Decode unicode characters when writing to Julia
  if (tools::file_ext(filepath) == "jl") {
    if (grepl("(\\\\u|\\\\\\\\u)[0-9a-fA-F]{4}", script)) {
      script <- decode_unicode(script)
    }
  }

  # Write the script to the file
  writeLines(script, filepath, useBytes = FALSE)

  invisible()
}


#' Get a temporary file path with a specific extension
#'
#' @param fileext String with file extension, either ".R" or ".jl"
#'
#' @returns Filepath to temporary file
#' @noRd
get_tempfile <- function(fileext) {
  filepath <- normalizePath(tempfile(fileext = fileext), winslash = "/", mustWork = FALSE)
  return(filepath)
}


#' Decode unicode characters in a string
#'
#' @param text String containing unicode escape sequences (e.g., "\\uXXXX")
#'
#' @returns String with unicode characters decoded
#' @noRd
#'
decode_unicode <- function(text) {
  stringr::str_replace_all(
    text,
    "(\\\\u|\\\\\\\\u)[0-9a-fA-F]{4}",
    function(matched) {
      # Extract the Unicode escape sequence
      jsonlite::fromJSON(sprintf('"%s"', matched))
    }
  )
}
