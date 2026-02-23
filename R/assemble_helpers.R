# ==============================================================================
# Shared helper functions for R and Julia script assembly
# ==============================================================================
# These functions extract data from the unified data frame structure and
# are used by both R and Julia assembly paths via the language adapter pattern.


#' Selectively invalidate assembly cache components
#'
#' Instead of wiping the entire cache, only clear components affected by a change.
#' This allows compile() to skip regenerating unaffected components.
#'
#' @param sfm Stock-and-flow model
#' @param what Character vector of categories to invalidate. Options:
#'   \describe{
#'     \item{"all"}{Wipe entire cache (equivalent to empty_assemble())}
#'     \item{"variables"}{Clear all variable-dependent components: ordering, static, ode, callback, intermediaries, nonneg_stocks, ensemble}
#'     \item{"static"}{Clear only static equations (constants, stock initial values, gf definitions)}
#'     \item{"dynamic"}{Clear only dynamic equations (ode, callback, intermediaries)}
#'     \item{"times"}{Clear time sequence}
#'     \item{"funcs"}{Clear func definitions}
#'     \item{"units"}{Clear unit definitions}
#'     \item{"nonneg"}{Clear non-negative stock handling}
#'   }
#'
#' @returns A stock-and-flow model with selectively cleared assembly cache
#' @noRd
invalidate_assemble <- function(sfm, what = "all") {
  no_assemble <- empty_assemble()

  if ("all" %in% what) {
    sfm[["assemble"]] <- no_assemble
    return(sfm)
  }

  a <- sfm[["assemble"]]

  if ("variables" %in% what) {
    a[["ordering"]]       <- no_assemble[["ordering"]]
    a[["static"]]         <- no_assemble[["static"]]
    a[["ode"]]            <- no_assemble[["ode"]]
    a[["callback"]]       <- no_assemble[["callback"]]
    a[["intermediaries"]] <- no_assemble[["intermediaries"]]
    a[["nonneg_stocks"]]  <- no_assemble[["nonneg_stocks"]]
    a[["ensemble"]]       <- no_assemble[["ensemble"]]
    a[["diagnose"]]       <- no_assemble[["diagnose"]]
    a[["unit_strings"]]   <- no_assemble[["unit_strings"]]
  }
  if ("static" %in% what && !"variables" %in% what) {
    a[["static"]] <- no_assemble[["static"]]
  }
  if ("dynamic" %in% what && !"variables" %in% what) {
    a[["ode"]]            <- no_assemble[["ode"]]
    a[["callback"]]       <- no_assemble[["callback"]]
    a[["intermediaries"]] <- no_assemble[["intermediaries"]]
  }
  if ("times" %in% what) {
    a[["times"]] <- no_assemble[["times"]]
  }
  if ("funcs" %in% what) {
    a[["funcs"]] <- no_assemble[["funcs"]]
  }
  if ("units" %in% what) {
    a[["units"]]    <- no_assemble[["units"]]
    a[["diagnose"]] <- no_assemble[["diagnose"]]
  }
  if ("nonneg" %in% what) {
    a[["nonneg_stocks"]] <- no_assemble[["nonneg_stocks"]]
  }

  sfm[["assemble"]] <- a
  sfm
}


#' Extract equations by variable type from data frame
#'
#' @param sfm Stock-and-flow model
#' @param type Variable type: "stock", "flow", "constant", "aux", "lookup"
#' @param column Column to extract: "eqn_str", "eqn", "sum_eqn", "sum_name"
#' @returns Named list where names are variable names and values are the extracted column
#' @noRd
get_equations_by_type <- function(sfm, type, column = "eqn_str") {
  df <- sfm[["variables"]][sfm[["variables"]][["type"]] == type, ]

  if (nrow(df) == 0) {
    return(stats::setNames(list(), character(0)))
  }

  stats::setNames(as.list(df[[column]]), df[["name"]])
}

#' Get all equation types for static equations
#'
#' @param sfm Stock-and-flow model
#' @returns List with gf_eqn, constant_eqn, stock_eqn
#' @noRd
get_static_equations <- function(sfm) {
  list(
    gf_eqn = get_equations_by_type(sfm, "lookup", "eqn_str"),
    constant_eqn = get_equations_by_type(sfm, "constant", "eqn_str"),
    stock_eqn = get_equations_by_type(sfm, "stock", "eqn_str")
  )
}

#' Get all equation types for dynamic equations
#'
#' @param sfm Stock-and-flow model
#' @returns List with aux_eqn, flow_eqn
#' @noRd
get_dynamic_equations <- function(sfm) {
  list(
    aux_eqn = get_equations_by_type(sfm, "aux", "eqn_str"),
    flow_eqn = get_equations_by_type(sfm, "flow", "eqn_str")
  )
}



#' Prepare equations and variables for simulation
#'
#' Unified function for both R and Julia. Uses lang_adapter() to dispatch
#' language-specific formatting.
#'
#' @inheritParams build
#' @param modified_names Character vector of variable names that were modified.
#'   If NULL (default), all variables are processed. If provided, only the
#'   specified variables are updated for incremental performance.
#'
#' @returns A stock-and-flow model object of class [`sdbuildR`][sdbuildR]
#' @noRd
#'
prep_equations_variables <- function(sfm, modified_names = NULL) {
  language <- sfm[["sim_specs"]][["language"]]
  lang <- lang_adapter(language)
  keep_unit <- sfm[["sim_specs"]][["keep_unit"]] %||% FALSE
  keep_nonnegative_flow <- sfm[["sim_specs"]][["keep_nonnegative_flow"]]
  names_df <- get_names(sfm)

  # Determine which rows to process
  if (is.null(modified_names)) {
    process_indices <- seq_len(nrow(sfm[["variables"]]))
  } else {
    process_indices <- which(sfm[["variables"]][["name"]] %in% modified_names)
  }

  # Pre-convert equations for types that need it (Julia converts R->Julia syntax)
  eqn_converted <- character(nrow(sfm[["variables"]]))
  regex_units <- get_regex_units()
  var_names <- get_model_var(sfm)
  

  for (i in process_indices) {
    type_i <- sfm[["variables"]][i, "type"]
    if (type_i == "func") next
    if (type_i %in% c("stock", "flow", "constant", "aux")) {
      eqn <- sfm[["variables"]][i, "eqn"]


      if (any(grepl("^[ ]*function[ ]*\\(", eqn))) {
        cli::cli_abort(c(
          "Invalid {.eqn} argument.",
          "x" = "Model variables cannot be defined as functions.",
          ">" = "To add a custom function, use {.fn custom_func} instead."
        ))
      }

      eqn <- clean_unit_in_u(eqn, regex_units)


        eqn_converted[i] <- lang$convert_eqn(
          type = type_i,
          name = sfm[["variables"]][i, "name"],
          eqn = eqn,
          var_names = var_names,
          regex_units = regex_units
        )
    }
  }

  # Replace bare gf name references with gf(source) in converted equations
  gf_dict <- build_gf_source_dict(sfm)
  if (!is.null(gf_dict)) {
    # Match whole-word gf_name NOT already followed by (
    regex_dict <- stats::setNames(
      unname(gf_dict),
      paste0("\\b", stringr::str_escape(names(gf_dict)), "\\b(?!\\s*\\()")
    )
    for (i in process_indices) {
      if (nzchar(eqn_converted[i])) {
        eqn_converted[i] <- stringr::str_replace_all(eqn_converted[i], regex_dict)
      }
    }
  }

  # Process graphical functions
  gf_idx <- sfm[["variables"]][["type"]] == "lookup"
  for (i in intersect(which(gf_idx), process_indices)) {
    row <- sfm[["variables"]][i, ]
    result <- lang$format_lookup(row, keep_unit, names_df)
    if (!is.null(result)) {
      sfm[["variables"]][i, "eqn_str"] <- result
    }
  }

  # Constants and stock initial values (same formatting pattern)
  for (type in c("constant", "stock")) {
    type_idx <- sfm[["variables"]][["type"]] == type
    for (i in intersect(which(type_idx), process_indices)) {
      row <- sfm[["variables"]][i, ]
      sfm[["variables"]][i, "eqn_str"] <- lang$format_static(
        name = row[["name"]],
        eqn_converted = eqn_converted[i],
        row = row,
        keep_unit = keep_unit
      )
    }
  }

  # Auxiliary equations
  aux_idx <- sfm[["variables"]][["type"]] == "aux"
  for (i in intersect(which(aux_idx), process_indices)) {
    row <- sfm[["variables"]][i, ]
    eqn_str <- lang$format_aux(
      name = row[["name"]],
      eqn_converted = eqn_converted[i],
      row = row,
      keep_unit = keep_unit
    )

    if (lang$eqn_str_as_list) {
      if (!is.null(row[["preceding_eqn"]])) {
        eqn_str <- c(row[["preceding_eqn"]], eqn_str)
      }
      sfm[["variables"]][i, "eqn_str"] <- list(eqn_str)
    } else {
      sfm[["variables"]][i, "eqn_str"] <- eqn_str
    }
  }

  # Flow equations
  flow_idx <- sfm[["variables"]][["type"]] == "flow"
  for (i in intersect(which(flow_idx), process_indices)) {
    row <- sfm[["variables"]][i, ]
    eqn_str <- lang$format_flow(
      name = row[["name"]],
      eqn_converted = eqn_converted[i],
      row = row,
      keep_nonnegative_flow = keep_nonnegative_flow
    )

    if (lang$eqn_str_as_list) {
      if (!is.null(row[["preceding_eqn"]])) {
        eqn_str <- c(row[["preceding_eqn"]], eqn_str)
      }
      sfm[["variables"]][i, "eqn_str"] <- list(eqn_str)
    } else {
      sfm[["variables"]][i, "eqn_str"] <- eqn_str
    }
  }

  sfm
}


#' Prepare for summing change in stocks
#'
#' Unified function for both R and Julia. Uses lang_adapter() to dispatch
#' language-specific formatting of stock change names and equations.
#'
#' @inheritParams build
#' @param modified_names Character vector of variable names that were modified.
#'   If NULL (default), all stocks are processed. If provided, only the
#'   specified stocks are updated for incremental performance.
#'
#' @returns A stock-and-flow model object of class [`sdbuildR`][sdbuildR]
#' @noRd
#'
prep_stock_change <- function(sfm, modified_names = NULL) {
  language <- sfm[["sim_specs"]][["language"]]
  lang <- lang_adapter(language)
  keep_unit <- sfm[["sim_specs"]][["keep_unit"]] %||% FALSE

  stock_idx <- sfm[["variables"]][["type"]] == "stock"

  # Determine which stock rows to process
  if (is.null(modified_names)) {
    process_stock_indices <- which(stock_idx)
  } else {
    modified_stock_indices <- which(stock_idx & sfm[["variables"]][["name"]] %in% modified_names)

    # Find stocks affected by modified flows
    flow_idx <- sfm[["variables"]][["type"]] == "flow"
    modified_flows <- sfm[["variables"]][["name"]] %in% modified_names & flow_idx
    if (any(modified_flows)) {
      affected_to <- sfm[["variables"]][modified_flows, "to"]
      affected_from <- sfm[["variables"]][modified_flows, "from"]
      affected_stocks <- unique(c(affected_to[nzchar(affected_to)], 
      affected_from[nzchar(affected_from)]))
      affected_stock_indices <- which(stock_idx & sfm[["variables"]][["name"]] %in% affected_stocks)
      process_stock_indices <- unique(c(modified_stock_indices, affected_stock_indices))
    } else {
      process_stock_indices <- modified_stock_indices
    }
  }

  # Populate inflows and outflows for each stock
  if (length(process_stock_indices) > 0) {
    flow_df <- get_flow_df(sfm)
    for (i in process_stock_indices) {
      stock_name <- sfm[["variables"]][i, "name"]
      inflows <- flow_df[flow_df[["to"]] == stock_name, "name"]
      outflows <- flow_df[flow_df[["from"]] == stock_name, "name"]

      sfm[["variables"]]$inflow[[i]] <- inflows
      sfm[["variables"]]$outflow[[i]] <- outflows
    }
  }

  # Get stock names for position lookup (Julia uses positional indexing)
  stock_names <- sfm[["variables"]][stock_idx, "name"]

  for (i in process_stock_indices) {
    row <- sfm[["variables"]][i, ]
    stock_position <- which(stock_names == row[["name"]])

    # Check for delayed stock (delayN indicates it's a delay accumulator)
    # if (!is.null(row[["delayN"]]) && is_defined(row[["delayN"]])) {
    #   sfm[["variables"]][i, "sum_name"] <- lang$format_delay_sum_name(row)
    #   sfm[["variables"]][i, "sum_eqn"] <- ""

    #   # Set unpack_state for Julia
    #   unpack <- lang$format_unpack_state(row, stock_position, stock_names)
    #   if (!is.null(unpack)) {
    #     sfm[["variables"]][i, "unpack_state"] <- unpack
    #   }

    #   # Set sum_units for R
    #   sum_units <- lang$format_sum_units(row, keep_unit)
    #   if (!is.null(sum_units)) {
    #     sfm[["variables"]][i, "sum_units"] <- sum_units
    #   }
    # } else {
      sfm[["variables"]][i, "sum_name"] <- lang$format_sum_name(row, stock_position, stock_names)

      # Set unpack_state for Julia (non-delay stocks don't set this in Julia)
      # Only delay stocks set unpack_state in Julia

      # Build sum equation from inflows and outflows
      inflow_def <- sfm[["variables"]][i, "inflow"]
      outflow_def <- sfm[["variables"]][i, "outflow"]

      # Extract from list columns
      if (is.list(inflow_def)) {
        inflow_def <- inflow_def[[1]]
        if (is.null(inflow_def)) inflow_def <- character(0)
      }
      if (is.list(outflow_def)) {
        outflow_def <- outflow_def[[1]]
        if (is.null(outflow_def)) outflow_def <- character(0)
      }

      if (!is_defined(inflow_def) && !is_defined(outflow_def)) {
        inflow_def <- lang$zero
      }
      # } else {
        inflow <- outflow <- ""
        if (is_defined(inflow_def)) {
          inflow <- paste0(inflow_def, collapse = " + ")
        }
        if (is_defined(outflow_def)) {
          outflow <- paste0(paste0(" - ", outflow_def), collapse = "")
        }
        sum_eqn <- paste0(inflow, outflow)
        sfm[["variables"]][i, "sum_eqn"] <- lang$format_sum_eqn(sum_eqn, row, keep_unit)
      # }

      # # Set sum_units for R
      # sum_units <- lang$format_sum_units(row, keep_unit)
      # if (!is.null(sum_units)) {
      #   sfm[["variables"]][i, "sum_units"] <- sum_units
      # }
    # }
  }

  sfm
}


# ==============================================================================
# Shared compile helpers: equation extraction and ordering
# ==============================================================================

#' Extract and order static equations into a single string
#'
#' Used by both R and Julia branches of compile_static().
#' Extracts gf, constant, and stock equations, then orders them
#' according to the dependency ordering.
#'
#' @param sfm Stock-and-flow model
#' @param ordering Ordering from order_equations()
#' @param separator String to join equations with
#'
#' @returns List with `str` (ordered equation string), `gf_eqn`, `constant_eqn`, `stock_eqn`
#' @noRd
gather_static_equations <- function(sfm, ordering, separator = "\n") {
  gf_eqn <- get_equations_by_type(sfm, "lookup", "eqn_str")
  constant_eqn <- get_equations_by_type(sfm, "constant", "eqn_str")
  stock_eqn <- get_equations_by_type(sfm, "stock", "eqn_str")

  if (!ordering[["static_and_dynamic"]][["issue"]]) {
    static_str <- c(gf_eqn, constant_eqn, stock_eqn)[ordering[["static"]][["order"]]] |>
      unlist() |>
      paste0(collapse = separator)
  } else {
    aux_eqn <- get_equations_by_type(sfm, "aux", "eqn_str")
    flow_eqn <- get_equations_by_type(sfm, "flow", "eqn_str")

    static_str <- c(
      gf_eqn, constant_eqn, stock_eqn,
      aux_eqn, flow_eqn
    )[ordering[["static_and_dynamic"]][["order"]]] |>
      unlist() |>
      paste0(collapse = separator)
  }

  list(
    str = static_str,
    constant_eqn = constant_eqn,
    gf_eqn = gf_eqn,
    stock_eqn = stock_eqn
  )
}


#' Extract and order dynamic equations into a string
#'
#' Used by both R and Julia branches of compile_ode().
#'
#' @param sfm Stock-and-flow model
#' @param ordering Ordering from order_equations()
#' @param separator String to join equations with
#'
#' @returns List with `str` (ordered equation string), `eqns` (named vector of ordered equations)
#' @noRd
gather_dynamic_equations <- function(sfm, ordering, separator = "\n\t\t") {
  aux_eqn <- get_equations_by_type(sfm, "aux", "eqn_str")
  flow_eqn <- get_equations_by_type(sfm, "flow", "eqn_str")

  eqns <- unlist(c(aux_eqn, flow_eqn)[ordering[["dynamic"]][["order"]]])
  str <- paste0(eqns, collapse = separator)

  list(str = str, eqns = eqns)
}


#' Build gf name to gf(source) mapping
#'
#' For each graphical function that has a source, builds a named character
#' vector mapping the bare gf name to `gf_name(source)`. Handles recursive
#' resolution when one gf's source is another gf.
#'
#' @param sfm Stock-and-flow model
#' @returns Named character vector (names = gf name, values = gf(source)),
#'   or NULL if no gf with sources exist.
#' @noRd
build_gf_source_dict <- function(sfm) {
  gf_df <- sfm[["variables"]][sfm[["variables"]][["type"]] == "lookup", ]
  if (nrow(gf_df) == 0) return(NULL)

  gf_sources <- stats::setNames(gf_df[["source"]], gf_df[["name"]])
  gf_sources <- gf_sources[!is.na(gf_sources) & nzchar(gf_sources)]
  if (length(gf_sources) == 0) return(NULL)

  # Base mapping: gf_name -> gf_name(source)
  dict <- paste0(names(gf_sources), "(", unname(gf_sources), ")") |>
    stats::setNames(names(gf_sources))

  # Recursively resolve nested gf sources (gf1 source is gf2)
  dict2 <- paste0("(", names(gf_sources), "(", unname(gf_sources), "))") |>
    stats::setNames(paste0("\\(", stringr::str_escape(names(gf_sources)), "\\)"))
  dict <- stringr::str_replace_all(unname(dict), dict2) |>
    stats::setNames(names(dict))

  dict
}


#' Build graphical function return string for R ODE
#'
#' Creates the string that includes graphical function calls in the ODE
#' return statement, using the shared gf source dictionary.
#'
#' @param sfm Stock-and-flow model
#' @returns Character string to append to R ODE return (empty if no gf)
#' @noRd
build_gf_return_str <- function(sfm) {
  gf_dict <- build_gf_source_dict(sfm)
  if (is.null(gf_dict)) return("")

  paste0(
    paste0("'", unname(gf_dict), "' = "), unname(gf_dict),
    collapse = ", "
  )
}


#' Build stock change equations string
#'
#' Builds the "sum_name = sum_eqn" strings for each stock.
#' Used by both R and Julia branches of compile_ode().
#'
#' @param sfm Stock-and-flow model
#' @param assign_op Assignment operator ("=" or "<-")
#'
#' @returns Named character vector of stock change equations
#' @noRd
gather_stock_changes <- function(sfm, assign_op, language) {
  stock_vars <- get_variables_by_type(sfm, "stock")

  if (language == "R") {
    # # Filter out any stocks with delayN (if that column exists)
    # if ("delayN" %in% colnames(stock_vars)) {
    #   stock_vars <- stock_vars[is.na(stock_vars[["delayN"]]) | stock_vars[["delayN"]] == "", ]
    # }
    paste0(stock_vars[["sum_name"]], " ", assign_op, " ", stock_vars[["sum_eqn"]])
  } else if (language == "Julia") {
    # # Ensure derivatives carry 1/time_units (even when keep_unit is FALSE)
    # set_units_on_flow <- TRUE

    stock_change <- lapply(
      stock_vars[["name"]],
      function(stock_name) {
        stock_row <- sfm[["variables"]][sfm[["variables"]][["name"]] == stock_name, ]
        x <- as.list(stock_row)
        sum_expr <- x[["sum_eqn"]]
        # # Scale the entire derivative by 1/time_units when time is unitful
        # if (set_units_on_flow) {
        #   sum_expr <- paste0("(", sum_expr, ") ./ ", P[["time_units_name"]])
        # }
        paste(
          x[["sum_name"]],
          # Broadcast assignment for delayed variables
          # ifelse(x[["type"]] == "delayN_smoothN", ".=", "="),
          "=",
          sum_expr
        )
      }
    ) |> compact_()

    # Set names on stock_change
    names(stock_change) <- stock_vars[["name"]]
    stock_change
  }
}


#' Pre-assemble script components for later use/modification
#'
#' Populates sfm$assemble with all script components so they can be
#' inspected and modified before simulation. This allows users to change
#' things like stop time, dt, etc. without recompiling from scratch.
#'
#' Called by compile() and compile_ensemble() to ensure the base cache is
#' populated before runtime-specific script generation begins.
#'
#' @param sfm A stock-and-flow model object of class [`sdbuildR`][sdbuildR]
#'
#' @returns A stock-and-flow model object with populated sfm$assemble cache
#' @noRd
#'
pre_assemble_components <- function(sfm) {
  # Skip if no variables defined yet
  if (!is_defined(sfm[["variables"]]) || nrow(sfm[["variables"]]) == 0) {
    return(sfm)
  }

  language <- sfm[["sim_specs"]][["language"]]

  # --- Julia-specific validation ---------------------------------------------
  if (language == "Julia") {
    var_names <- get_model_var(sfm)
    check_no_keyword_arg(sfm, var_names)
  }

  # --- Cache validation ------------------------------------------------------
  no_assemble <- empty_assemble()
  undefined_assemble <- vapply(
    stats::setNames(nm = names(no_assemble)),
    \(comp) identical(sfm[["assemble"]][[comp]], no_assemble[[comp]]),
    logical(1)
  )

  cache_valid <- !undefined_assemble[["language"]] &&
    sfm[["assemble"]][["language"]] == language
  sfm[["assemble"]][["language"]] <- language

  # --- Ordering --------------------------------------------------------------
  if (!cache_valid || undefined_assemble[["ordering"]]) {
    sfm[["assemble"]][["ordering"]] <- order_equations(sfm)
  }

  # --- Compile times ---------------------------------------------------------
  if (!cache_valid || undefined_assemble[["times"]]) {
    sfm[["assemble"]][["times"]] <- compile_times(sfm, language = language)
  }

  # --- Compile funcs ---------------------------------------------------------
  if (!cache_valid || undefined_assemble[["funcs"]]) {
    sfm[["assemble"]][["funcs"]] <- compile_funcs(sfm, language = language)
  }

  # --- Prepare equations and stock changes -----------------------------------
  if (!cache_valid) {
    sfm <- prep_equations_variables(sfm)
    sfm <- prep_stock_change(sfm)
  }

  # --- Julia: intermediaries -------------------------------------------------
  if (language == "Julia" && (!cache_valid || undefined_assemble[["intermediaries"]])) {
    sfm[["assemble"]][["intermediaries"]] <- prep_intermediary_variables(sfm, language = language)
  }

  # --- Compile static equations ----------------------------------------------
  if (!cache_valid || undefined_assemble[["static"]]) {
    sfm[["assemble"]][["static"]] <- compile_static(sfm, language = language)
  }

  # --- Julia: compile units --------------------------------------------------
  if (language == "Julia") {
    sfm[["assemble"]][["units"]] <- compile_units(sfm, language = language)
  }

  # --- R: compile nonneg stocks ----------------------------------------------
  if (language == "R" && (!cache_valid || undefined_assemble[["nonneg_stocks"]])) {
    sfm[["assemble"]][["nonneg_stocks"]] <- compile_nonneg_stocks(sfm, language = language)
  }

  # --- Julia: compile ODE (Julia ODE does not depend on only_stocks) ---------
  if (language == "Julia" && (!cache_valid || undefined_assemble[["ode"]])) {
    sfm[["assemble"]][["ode"]] <- compile_ode(sfm,
      only_stocks = FALSE,
      language = language,
      is_ensemble = FALSE
    )
  }

  # --- Populate validation caches --------------------------------------------
  if (is.null(sfm[["assemble"]][["diagnose"]])) {
    sfm[["assemble"]][["diagnose"]] <- diagnose(sfm)
  }
  if (is.null(sfm[["assemble"]][["unit_strings"]])) {
    sfm[["assemble"]][["unit_strings"]] <- find_unit_strings(sfm)
  }

  sfm
}
