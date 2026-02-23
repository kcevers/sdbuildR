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
  filepath
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


#' Compile a complete simulation script
#'
#' Unified entry point for both R and Julia non-ensemble script compilation.
#' Calls pre_assemble_components() to ensure the model structure cache is
#' populated, then generates the runtime-specific run_ode and post sections.
#'
#' @inheritParams simulate
#' @param filepath_sim Path to output CSV file (Julia only).
#' @param only_stocks Logical; if TRUE, only stock values are saved.
#'
#' @returns List with `script` (character) and `sfm` (updated model with cache).
#' @noRd
compile <- function(sfm, only_stocks = FALSE,
                    filepath_sim = NULL) {
  language <- sfm[["sim_specs"]][["language"]]

  # Populate the model structure cache (ordering, times, funcs, static, ode, etc.)
  sfm <- pre_assemble_components(sfm)

  ordering <- sfm[["assemble"]][["ordering"]]

  # Adjust only_stocks based on ordering (no dynamic vars → stocks only)
  if (is.null(ordering[["dynamic"]][["order"]])) {
    only_stocks <- TRUE
  }

  # --- R: compile ODE (depends on only_stocks, not pre-assembled) ------------
  if (language == "R") {
    no_assemble <- empty_assemble()
    ode_undefined <- identical(sfm[["assemble"]][["ode"]], no_assemble[["ode"]])
    if (ode_undefined) {
      sfm[["assemble"]][["ode"]] <- compile_ode(sfm,
        only_stocks = only_stocks,
        language = language,
        is_ensemble = FALSE
      )
    }
  }

  ode    <- sfm[["assemble"]][["ode"]]
  static <- sfm[["assemble"]][["static"]]

  # --- Compile run ODE and post ----------------------------------------------
  run_ode <- compile_run_ode(sfm,
    filepath_sim = filepath_sim,
    only_stocks = only_stocks,
    language = language
  )

  post <- compile_post(sfm,
    filepath_sim = filepath_sim,
    language = language
  )

  # --- Build seed string -----------------------------------------------------
  seed <- sfm[["sim_specs"]][["seed"]]
  seed_str <- if (is_defined(seed)) fmt_script("prep_seed", language, seed = seed) else ""

  # --- Assemble final script -------------------------------------------------
  if (language == "R") {
    script <- paste0(c(
      "# Load packages\nlibrary(sdbuildR)",
      seed_str,
      sfm[["assemble"]][["times"]],
      sfm[["assemble"]][["funcs"]],
      sfm[["assemble"]][["nonneg_stocks"]][["func_def"]],
      ode,
      static[["script"]],
      run_ode,
      post), collapse = "\n"
    )

  } else {
    script <- paste0(c(
      seed_str,
      sfm[["assemble"]][["units"]],
      sfm[["assemble"]][["times"]],
      sfm[["assemble"]][["funcs"]],
      ode,
      static[["script"]],
      run_ode,
      post), collapse = "\n"
    )
  }

  list(script = script, sfm = sfm)
}


#' Compile script for creating time vector
#'
#' @returns List
#' @importFrom rlang .data
#' @inheritParams compile
#' @noRd
#'
compile_times <- function(sfm, language) {
  ss <- sfm[["sim_specs"]]

  if (language == "R") {
    script <- fmt_script("times", "R", ss)
  } else if (language == "Julia") {
    keep_unit <- ss[["keep_unit"]]
    unit_mult <- function(op) if (keep_unit) paste0(" ", op, " ", P[["time_units_name"]]) else ""

    script <- fmt_script("times", "julia", ss,
      times_unit_mult    = unit_mult(".*"),
      dt_unit_mult       = unit_mult("*"),
      saveat_unit_mult   = unit_mult("*"),
      savefrom_unit_mult = unit_mult(".*")
    )
  }

  script
}


#' Compile script for global variables
#'
#' @inheritParams build
#'
#' @returns Func script as character string
#' @noRd
compile_funcs <- function(sfm, language) {
  script <- ""
  func_df <- get_funcs(sfm)

  if (nrow(func_df) == 0 || !any(nzchar(func_df[["eqn"]]))) {
    return(script)
  }

  lang <- lang_adapter(language)

  # Julia needs var_names and regex_units for equation conversion; R ignores them
  var_names <- if (language == "Julia") get_model_var(sfm) else NULL
  regex_units <- if (language == "Julia") get_regex_units() else NULL

  eqns <- character(nrow(func_df))
  for (i in seq_len(nrow(func_df))) {
    if (nzchar(func_df[i, "eqn"])) {
      eqns[i] <- lang$convert_func_eqn(
        name = func_df[i, "name"],
        eqn = func_df[i, "eqn"],
        var_names = var_names,
        regex_units = regex_units
      )
    }
  }

  eqns[is.na(eqns)] <- ""
  has_eqn <- nzchar(eqns)

  if (any(has_eqn)) {
    func_body <- paste0("\n", paste0(eqns[has_eqn], collapse = "\n"))
    script <- fmt_script("funcs", language, func_body = func_body)
  }

  script
}



#' Compile script for static variables, i.e. initial conditions, functions, and parameters
#'
#' @inheritParams compile
#' @inheritParams order_equations
#'
#' @noRd
#'
#' @returns List with necessary scripts
#'
compile_static <- function(sfm, language,
                           ordering_override = NULL,
                           ensemble_iter_code = "") {
  keep_unit <- sfm[["sim_specs"]][["keep_unit"]]
  intermediaries <- sfm[["assemble"]][["intermediaries"]]
  ordering <- if (!is.null(ordering_override)) ordering_override else sfm[["assemble"]][["ordering"]]

  # If ordering for static_and_dynamic is missing, treat as issue and fall back to static only
  if (is.null(ordering[["static_and_dynamic"]][["order"]])) {
    ordering[["static_and_dynamic"]][["issue"]] <- TRUE
  }

  # Extract and order equations (same helper as R branch)
  gathered <- gather_static_equations(sfm, ordering)
  constant_eqn <- gathered$constant_eqn
  stock_eqn <- gathered$stock_eqn
  gf_eqn <- gathered$gf_eqn
  static_str <- gathered$str

  if (language == "R") {
    # Put parameters together
    if (length(constant_eqn) > 0) {
      const_list_items <- paste0(names(constant_eqn), " = ", names(constant_eqn))
      constants_def <- paste0(
        "\n\n# Define parameters in named list\n", P[["parameter_name"]],
        " = list(", paste0(const_list_items, collapse = ", "), ")\n"
      )
    } else {
      constants_def <- paste0("\n\n# Define empty parameters\n", P[["parameter_name"]], " = list()\n")
    }

    # Define init
    init_list_items <- paste0(names(stock_eqn), " = ", names(stock_eqn))
    init_def <- paste0(
      "\n\n# Define initial condition\n", P[["initial_value_name"]], " = c(",
      paste0(init_list_items, collapse = ", "), ")"
    )

    script <- fmt_script("static", "R",
      static_str = static_str,
      constants_def = constants_def,
      init_def = init_def
    )
  } else if (language == "Julia") {
    ensemble_iter <- ensemble_iter_code

    #** removed:
    #   # Re-gather with possibly modified ordering (after ensemble var removal)
    #   gathered <- gather_static_equations(sfm, ordering)
    #   static_str <- gathered$str

    # Replace any reference to model_setup.intermediary_names with intermediary_names
    static_str <- gsub(
      paste0(P[["model_setup_name"]], "\\.", P[["intermediary_names"]]),
      P[["intermediary_names"]], static_str
    )


    # Put parameters together in named tuple; include graphical functions as otherwise these are not defined outside of the let block
    #   if (nrow(get_variables_by_type(sfm, "constant")) > 0 || nrow(get_variables_by_type(sfm, "gf")) > 0) {
    if (length(constant_eqn) > 0 || length(gf_eqn) > 0) {
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

    # # Check for delayN() and smoothN() functions
    # delayN_smoothN <- get_delay(sfm, type = "delayN_smoothN")

    # if (length(delayN_smoothN) > 0) {
    #   delay_names <- names(unlist(unname(delayN_smoothN), recursive = FALSE))

    #   # Preserve order of stocks but wrap delayN and smoothN stocks in values() and keys()
    #   x <- y <- names(stock_eqn)
    #   idx <- names(stock_eqn) %in% delay_names
    #   x[idx] <- paste0("values(", x[idx], ")")
    #   y[!idx] <- paste0(":", y[!idx])
    #   y[idx] <- paste0("keys(", y[idx], ")...")

    #   init_def_stocks <- paste0(x, collapse = ", ")
    #   init_names <- paste0(y, collapse = ", ")

    #   # Find indices of names in vector
    #   init_idx <- paste0(
    #     "\n", P[["delay_idx_name"]], " = (",
    #     paste0(paste0(
    #       delay_names, " = ",
    #       "findall(n -> occursin(r\"", delay_names, P[["acc_suffix"]], "[0-9]+$\", string(n)), ",
    #       P[["initial_value_names"]], ")"
    #     ), collapse = ",\n\t"),
    #     ",)\n"
    #   )


    #   # Make sure that any .outflow references are replaced with first(values(variable))
    #   dict <- stringr::fixed(stats::setNames(
    #     paste0("first(values(", delay_names, "))"),
    #     paste0(delay_names, P[["outflow_suffix"]])
    #   ))
    #   static_str <- stringr::str_replace_all(static_str, dict)
    # } else {
      init_def_stocks <- paste0(names(stock_eqn), collapse = ", ")
      # Symbols are faster than characters
      init_names <- paste0(paste0(":", names(stock_eqn)), collapse = ", ")
      init_idx <- ""
    # }

    # Put initial states together in (unnamed) vector
    init_def <- paste0(
      "\n# Define initial condition in vector\n",
      P[["initial_value_name"]],
      " = [Base.Iterators.flatten(",
      # ifelse(keep_unit, "Unitful.ustrip.(", ""), 
      "[",
      init_def_stocks,
      # Add extra comma in case there is only one stock
      ",]", 
      # ifelse(keep_unit, ")", ""), 
      ")...]\n"
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

      # Keep all intermediary names; skip function filtering to avoid undefined references
      intermediary_names_correct <- paste0(
        "\n# Keep all intermediary names (skip function filtering)\n",
        "is_not_function = trues(length(", P[["intermediary_names"]], "))\n",
        P[["intermediary_names"]], " = ", P[["intermediary_names"]], "[is_not_function]\n"
      )
    } else {
      intermediary_names <- paste0(P[["intermediary_names"]], " = Nothing\n")
      intermediary_names_correct <- ""
    }


    delay_idx_return <- ifelse(nzchar(init_idx),
      paste0(", ", P[["delay_idx_name"]], " = ", P[["delay_idx_name"]]),
      ""
    )

    script <- fmt_script("static", "Julia",
      ensemble_iter_code = ensemble_iter,
      intermediary_names_str = intermediary_names,
      static_str = static_str,
      pars_def = pars_def,
      init_def = init_def,
      init_names_str = init_names,
      init_idx = init_idx,
      intermediary_names_correct = intermediary_names_correct,
      delay_idx_return = delay_idx_return
    )

  }


  list(
    script = script,
    par_names = c(names(constant_eqn), names(gf_eqn))
  )
}


#' Compile script for non-negative stocks
#'
#' @inheritParams build
#' @inheritParams compile
#'
#' @noRd
#' @returns List with necessary scripts for ensuring non-negative stocks
#'
compile_nonneg_stocks <- function(sfm, language) {
  keep_nonnegative_stock <- sfm[["sim_specs"]][["keep_nonnegative_stock"]]
  nonneg_stocks <- empty_nonneg_stocks()
  scripts <- prep_script_template()


  if (language == "R") {
    # Non-negative stocks
    stock_df <- sfm[["variables"]][sfm[["variables"]][["type"]] == "stock", ]
    nonneg_idx <- which(stock_df[["non_negative"]])

    if (keep_nonnegative_stock && length(nonneg_idx) > 0) {
      nonneg_stock_names <- stock_df[nonneg_idx, "name"]
      quoted_names <- paste0("'", nonneg_stock_names, "'", collapse = ", ")

      func_def <- fmt_script("nonneg_stocks", "R",
        nonneg_stock_names_quoted = quoted_names
      )

      root_arg <- scripts[["nonneg_root_arg_r"]][["template"]]

      check_root <- scripts[["nonneg_check_root_r"]][["template"]]

      nonneg_stocks[["func_def"]]   <- func_def
      nonneg_stocks[["root_arg"]]   <- root_arg
      nonneg_stocks[["check_root"]] <- check_root
    }
  }

  #    else if (language == "Julia"){
  #   **check: not supported in julia or inline?
  # }

  nonneg_stocks
}


#' Compile script for running ODE
#'
#' @param nonneg_stocks Output of compile_nonneg_stocks()
#' @inheritParams compile_ode
#'
#' @returns List
#' @inheritParams compile
#' @noRd
#'
compile_run_ode <- function(
  sfm,
  filepath_sim = NULL,
  only_stocks = NULL,
  language
) {
  nonneg_stocks <- sfm[["assemble"]][["nonneg_stocks"]]
  intermediaries <- sfm[["assemble"]][["intermediaries"]]
  save_intermediaries <- length(intermediaries[["names"]]) > 0

  if (language == "R") {
    script <- fmt_script("run_ode", language,
      method = sfm[["sim_specs"]][["method"]],
      root_arg = nonneg_stocks[["root_arg"]],
      check_root = nonneg_stocks[["check_root"]]
    )
  } else if (language == "Julia") {
    callback_arg <- ifelse(save_intermediaries,
      paste0(", ", P[["callback_name"]], " = ", P[["callback_name"]]),
      ""
    )

    script <- fmt_script("run_ode", language,
      method = sfm[["sim_specs"]][["method"]],
      callback_arg = callback_arg
    )
  }

  script
}


compile_post <- function(sfm, filepath_sim = NULL, language) {
  intermediaries <- sfm[["assemble"]][["intermediaries"]]
  save_intermediaries <- length(intermediaries[["names"]]) > 0

  if (language == "R") {
    # If different times need to be saved, linearly interpolate
    saveat_script <- ""
    if (sfm[["sim_specs"]][["dt"]] != sfm[["sim_specs"]][["save_at"]] ||
      sfm[["sim_specs"]][["start"]] != sfm[["sim_specs"]][["save_from"]]) {
      saveat_script <- fmt_script("saveat", "R", sfm[["sim_specs"]])
    }

    # Process ODE output
    script <- fmt_script("post_ode", "R",
      saveat_script = saveat_script
    )
  } else if (language == "Julia") {
    intermediaries_or_nothing <- ifelse(save_intermediaries, P[["intermediaries"]], "nothing")

    script <- fmt_script("post_ode", language,
      intermediaries_or_nothing = intermediaries_or_nothing,
      filepath_sim = filepath_sim
    )
  }

  script
}


#' Compile a complete ensemble simulation script
#'
#' Julia-only entry point for ensemble compilation. Calls
#' pre_assemble_components() to reuse the model structure cache, then builds
#' the ensemble-specific portions (ensemble_def, ensemble_iter, run_ode,
#' post) on top.
#'
#' @inheritParams simulate
#' @inheritParams ensemble
#' @param ensemble_pars List of ensemble parameters constructed by ensemble().
#'
#' @returns List with `script` (character) and `sfm` (updated model).
#' @noRd
compile_ensemble <- function(sfm, ensemble_pars, only_stocks = TRUE) {
  language <- sfm[["sim_specs"]][["language"]]

  # Ensure base cache is populated — reuses build() cache if already done
  sfm <- pre_assemble_components(sfm)

  # Reformat range values as Julia float literals (does not modify sfm)
  out <- prep_ensemble_range(sfm, ensemble_pars)
  sfm <- out[["sfm"]]
  ensemble_pars <- out[["ensemble_pars"]]
  rm(out)

  ordering <- sfm[["assemble"]][["ordering"]]

  # Adjust only_stocks based on ordering (no dynamic vars → stocks only)
  if (is.null(ordering[["dynamic"]][["order"]])) {
    only_stocks <- TRUE
  }

  # Ensemble-specific static: range vars excluded, ensemble_def/iter prepended
  static_ens <- compile_static_ensemble(sfm, ensemble_pars)

  # ODE without callback_setup (recreated per member inside run_ode_ensemble)
  ode <- compile_ode(sfm, only_stocks, language, is_ensemble = TRUE)

  # Ensemble run_ode and post-processing
  run_ode <- compile_run_ode_ensemble(sfm, ensemble_pars, static_ens, only_stocks)
  post    <- compile_post_ensemble(sfm, ensemble_pars)

  # Seed string
  seed <- sfm[["sim_specs"]][["seed"]]
  seed_str <- if (is_defined(seed)) fmt_script("prep_seed", language, seed = seed) else ""

  script <- paste0(c(
    seed_str,
    sfm[["assemble"]][["units"]],
    sfm[["assemble"]][["times"]],
    sfm[["assemble"]][["funcs"]],
    ode,
    static_ens[["script"]],
    run_ode,
    post
  ), collapse = "\n")

  list(script = script, sfm = sfm)
}


#' Compile script for ODE function passed to deSolve::ode
#'
#' @inheritParams build
#' @inheritParams compile
#' @inheritParams order_equations
#' @inheritParams compile_static
#'
#' @returns List
#' @importFrom rlang .data
#' @noRd
#'
compile_ode <- function(sfm,
                        only_stocks,
                        language,
                        is_ensemble = FALSE) {
  keep_nonnegative_stock <- sfm[["sim_specs"]][["keep_nonnegative_stock"]]
  keep_unit <- sfm[["sim_specs"]][["keep_unit"]]

  ordering <- sfm[["assemble"]][["ordering"]]
  static <- sfm[["assemble"]][["static"]]
  intermediaries <- sfm[["assemble"]][["intermediaries"]]
  save_intermediaries <- length(intermediaries[["names"]]) > 0

  lang <- lang_adapter(language)

  # Get and order dynamic equations
  dynamic <- gather_dynamic_equations(sfm, ordering, separator = "\n\t\t")
  dynamic_eqn <- dynamic$eqns
  dynamic_eqn_str <- dynamic$str

  if (language == "R") {
    # Sum change in stock equations
    stock_change <- gather_stock_changes(sfm, assign_op = lang$assign_op, language = language)

    # Compile stock changes in one string
    stock_change_str <- paste0(stock_change, collapse = "\n\t\t")

    # Get names of summed change in stocks from data frame
    stock_df_all <- sfm[["variables"]][sfm[["variables"]][["type"]] == "stock", ]
    stock_changes_names <- stock_df_all[["sum_name"]]

    state_change_str <- paste0(
      P[["change_state_name"]], " = c(",
      paste0(unname(stock_changes_names), collapse = ", "), ")"
    )

    # Graphical functions (gf)
    gf_str <- build_gf_return_str(sfm)

    # Save all variables in return statement
    if (!only_stocks) {
      # Filter out functions in case they are in auxiliaries
      if (length(names(dynamic_eqn)) > 0 || nzchar(gf_str)) {
        # Build variable name assignments
        var_assignments <- ""
        if (length(names(dynamic_eqn)) > 0) {
          var_assignments <- paste0(paste0(names(dynamic_eqn), " = ", names(dynamic_eqn)), collapse = ", ")
        }

        save_var_str <- paste0(
          ", Filter(Negate(is.function), c(",
          var_assignments, ifelse(nzchar(gf_str), ", ", ""),
          gf_str, "))"
        )
      } else {
        # No variables to save (isolated stocks only)
        save_var_str <- ""
      }
    } else {
      save_var_str <- ""
    }

    S_str <- sprintf("%s = as.list(%s)", P[["state_name"]], P[["state_name"]])

    # Compile
    script <- fmt_script("ode", language,
      S_str = S_str,
      dynamic_eqn_str = dynamic_eqn_str,
      stock_change_str = stock_change_str,
      state_change_str = state_change_str,
      save_var_str = save_var_str
    )
  } else if (language == "Julia") {
    # Sum change in stock equations
    stock_change <- gather_stock_changes(sfm, assign_op = lang$assign_op, language = language)
    stock_change_str <- paste0(stock_change, collapse = "\n\t")

    # Add units to stocks
    add_stock_units <- ""
    # if (keep_unit) {
    #   # For each stock that has a unit, add
    #   stock_vars <- get_variables_by_type(sfm, "stock")
    #   stock_names <- stock_vars[["name"]]
    #   stock_units <- stock_vars[["units"]]
    #   idx <- stock_units != "1"
    #   if (any(idx)) {
    #     add_stock_units <- paste0(
    #       "\n\n\t# Assign units to stocks\n\t",
    #       paste0(paste0(
    #         stock_names[idx], " = ",
    #         stock_names[idx], " .* u\"",
    #         stock_units[idx], "\""
    #       ), collapse = "\n\t"), "\n"
    #     )
    #   }
    # }

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

    # # If delayN() and smoothN() were used, state has to be unpacked differently
    # delayN_smoothN <- get_delay(sfm, type = "delayN_smoothN")

    # if (length(delayN_smoothN) > 0) {
    #   delay_names <- names(unlist(unname(delayN_smoothN), recursive = FALSE))

    #   # Unpack non delayN stocks
    #   unpack_nondelayN <- paste0(
    #     paste0(setdiff(names(stock_change), delay_names), collapse = ", "), ", = ", P[["state_name"]], "[findall(n -> !occursin(r\"", P[["delayN_suffix"]], "[0-9]+", P[["acc_suffix"]], "[0-9]+$|",
    #     P[["smoothN_suffix"]], "[0-9]+", P[["acc_suffix"]], "[0-9]+$\", string(n)), ", P[["model_setup_name"]], ".", P[["initial_value_names"]], ")]"
    #   )

    #   # Unpack each delayN or smoothN stock separately
    #   unpack_delayN <- lapply(
    #     seq_len(nrow(stock_vars_df)),
    #     function(i) {
    #       x <- as.list(stock_vars_df[i, ])
    #       if (is_defined(x[["unpack_state"]])) {
    #         return(paste0(x[["name"]], " = ", x[["unpack_state"]]))
    #       }
    #     }
    #   ) |> compact_()

    #   unpack_state_str <- paste0(unpack_nondelayN, "\n\t", paste0(unpack_delayN, collapse = "\n\t"))
    # } else {
      unpack_state_str <- paste0(
        paste0(names(stock_change), collapse = ", "),
        ", = ", P[["state_name"]]
      )
    # }

    # Unpack parameters string
    if (length(static[["par_names"]])) {
      unpack_pars_str <- paste0(
        "\n\n\t# Unpack parameters\n\t",
        paste0(static[["par_names"]], collapse = ", "),
        ", = ", P[["parameter_name"]]
      )
    } else {
      unpack_pars_str <- ""
    }


    # Compile ODE
    script_ode <- fmt_script("ode", "Julia",
      unpack_state_str = unpack_state_str,
      add_stock_units = add_stock_units,
      unpack_pars_str = unpack_pars_str,
      dynamic_eqn_str = dynamic_eqn_str,
      stock_change_str = stock_change_str
      # nonneg_stocks
    )

    # Compile callback function
    if (!save_intermediaries) {
      script_callback <- fmt_script("callback_empty", "Julia")
    } else {
      # Unpack parameters from integrator
      if (length(static[["par_names"]])) {
        unpack_pars_integrator_str <- paste0(
          "\n\n\t# Get parameters from integrator\n\t",
          P[["parameter_name"]], " = integrator.p",
          "\n\n\t# Unpack parameters\n\t",
          paste0(static[["par_names"]], collapse = ", "),
          ", = ", P[["parameter_name"]]
        )
      } else {
        unpack_pars_integrator_str <- ""
      }


      intermediary_values <- paste0(
        paste0(intermediaries[["values"]], collapse = ", "),
        ifelse(length(intermediaries[["values"]]) == 1, ",", "")
      )

      # Callback setup (only define if not ensemble)
      callback_setup <- ifelse(is_ensemble, "", paste0(
        P[["intermediaries"]], " = SavedValues(",
        "eltype(", P[["time_name"]], "), Any)\n",
        P[["callback_name"]], " = SavingCallback(",
        P[["callback_func_name"]], ", ", P[["intermediaries"]],
        ", saveat = ", P[["savefrom_name"]],
        ")\n"
      ))

      script_callback <- fmt_script("callback", "Julia",
        unpack_state_str = unpack_state_str,
        add_stock_units = add_stock_units,
        unpack_pars_integrator_str = unpack_pars_integrator_str,
        dynamic_eqn_str = dynamic_eqn_str,
        intermediary_values = intermediary_values,
        callback_setup = callback_setup
      )
    }

    script <- paste0(script_ode, "\n\n", script_callback)
  }

  script
}


#' Compile script for defining a units module in Julia
#'
#' @inheritParams compile
#'
#' @returns List with script
#'
#' @noRd
compile_units <- function(sfm, language) {
  if (language == "R") {
    stop("compile_units() only works for Julia.")
  } else if (language == "Julia") {
    #   script <- sprintf("\n# Clear any existing definitions to avoid conflicts
    # if @isdefined(%s)
    #     # Force garbage collection to clean up old module
    #     %s = nothing
    #     GC.gc()
    # end\n", P[["MyCustomUnits"]], P[["MyCustomUnits"]])
    script <- ""

    if (nrow(sfm[["custom_unit"]]) > 0) {
      if (nrow(sfm[["custom_unit"]]) > 1) {
        # Topological sort of units
        eq_names <- sfm[["custom_unit"]][["name"]]
        deps <- lapply(
          sfm[["custom_unit"]][["eqn"]],
          function(x) {
            unlist(stringr::str_extract_all(x, eq_names))
          }
        )
        names(deps) <- eq_names
        out <- topological_sort(deps)

        if (out[["issue"]]) {
          cli::cli_inform(paste0("Ordering custom units failed. ", paste0(out[["msg"]])))
        }

        sfm[["custom_unit"]] <- sfm[["custom_unit"]][out[["order"]], ]
      }


      unit_str <- lapply(seq_len(nrow(sfm[["custom_unit"]])), function(i) {
        row <- sfm[["custom_unit"]][i, ]
        if (is_defined(row[["eqn"]])) {
          unit_def <- row[["eqn"]]
        } else {
          unit_def <- "1"
        }

        paste0(
          "@unit ", row[["name"]], " \"", row[["name"]], "\" ",
          row[["name"]], " u\"", unit_def, "\" ", ifelse(row[["prefix"]], "true", "false")
        )
      }) |> paste0(collapse = sprintf(
        "\n\tUnitful.register(%s)\n\t",
        P[["MyCustomUnits"]]
      ))

      script <- fmt_script("units", "Julia", unit_str = unit_str)
    }

    script
  }
}


