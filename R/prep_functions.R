#' Prepare equations and variables for R simulation
#'
#' @inheritParams build
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#' @noRd
#'
prep_equations_variables <- function(sfm) {
  keep_nonnegative_flow <- sfm[["sim_specs"]][["keep_nonnegative_flow"]]
  # Add eqn_str column if it doesn't exist
  if (!"eqn_str" %in% colnames(sfm[["variables"]])) {
    sfm[["variables"]][["eqn_str"]] <- ""
  }
  
  # Process graphical functions
  gf_idx <- sfm[["variables"]][["type"]] == "gf"
  for (i in which(gf_idx)) {
    xpts <- sfm[["variables"]][i, "xpts"][[1]]
    ypts <- sfm[["variables"]][i, "ypts"][[1]]
    
    if (!is.null(xpts) && length(xpts) > 0) {
      xpts_str <- paste0("c(", paste0(as.character(xpts), collapse = ", "), ")")
      
      if (is.null(ypts) || length(ypts) == 0) {
        ypts_str <- ""
      } else {
        ypts_str <- sprintf("\n\t\ty = c(%s),", paste0(as.character(ypts), collapse = ", "))
      }
      
      sfm[["variables"]][i, "eqn_str"] <- sprintf(
        "%s = stats::approxfun(x = %s,%s\n\t\tmethod = '%s', rule = %s)",
        sfm[["variables"]][i, "name"], xpts_str, ypts_str,
        sfm[["variables"]][i, "interpolation"],
        ifelse(sfm[["variables"]][i, "extrapolation"] == "nearest", 2,
          ifelse(sfm[["variables"]][i, "extrapolation"] == "NA", 1, sfm[["variables"]][i, "extrapolation"]))
      )
    }
  }
  
  # Constant equations
  const_idx <- sfm[["variables"]][["type"]] == "constant"
  sfm[["variables"]][const_idx, "eqn_str"] <- paste0(
    sfm[["variables"]][const_idx, "name"], " = ",
    sfm[["variables"]][const_idx, "eqn"]
  )
  
  # Initial states of Stocks
  stock_idx <- sfm[["variables"]][["type"]] == "stock"
  sfm[["variables"]][stock_idx, "eqn_str"] <- paste0(
    sfm[["variables"]][stock_idx, "name"], " = ",
    sfm[["variables"]][stock_idx, "eqn"]
  )
  
  # Auxiliary equations
  aux_idx <- sfm[["variables"]][["type"]] == "aux"
  sfm[["variables"]][aux_idx, "eqn_str"] <- sprintf(
    "%s <- %s",
    sfm[["variables"]][aux_idx, "name"],
    sfm[["variables"]][aux_idx, "eqn"]
  )
  
  # Flow equations
  flow_idx <- sfm[["variables"]][["type"]] == "flow"
  sfm[["variables"]][flow_idx, "eqn_str"] <- sprintf(
    "%s <- %s%s%s # Flow%s%s",
    sfm[["variables"]][flow_idx, "name"],
    ifelse(sfm[["variables"]][flow_idx, "non_negative"], "nonnegative(", ""),
    sfm[["variables"]][flow_idx, "eqn"],
    ifelse(sfm[["variables"]][flow_idx, "non_negative"], "\n\t\t)", ""),
    ifelse(nzchar(sfm[["variables"]][flow_idx, "from"]), 
           paste0(" from ", sfm[["variables"]][flow_idx, "from"]), ""),
    ifelse(nzchar(sfm[["variables"]][flow_idx, "to"]), 
           paste0(" to ", sfm[["variables"]][flow_idx, "to"]), "")
  )

  return(sfm)
}


#' Prepare for summing change in stocks for R simulation
#'
#' @inheritParams build
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#' @noRd
#'
prep_stock_change <- function(sfm) {
  # Add temporary properties to sum change in Stocks using data frame approach
  stock_idx <- sfm[["variables"]][["type"]] == "stock"
  
  # Add columns if they don't exist (may not exist when called from build())
  if (!"sum_name" %in% colnames(sfm[["variables"]])) {
    sfm[["variables"]][["sum_name"]] <- ""
  }
  if (!"sum_eqn" %in% colnames(sfm[["variables"]])) {
    sfm[["variables"]][["sum_eqn"]] <- ""
  }
  if (!"sum_units" %in% colnames(sfm[["variables"]])) {
    sfm[["variables"]][["sum_units"]] <- ""
  }
  if (!"inflow" %in% colnames(sfm[["variables"]])) {
    sfm[["variables"]][["inflow"]] <- ""
  }
  if (!"outflow" %in% colnames(sfm[["variables"]])) {
    sfm[["variables"]][["outflow"]] <- ""
  }
  
  # Populate inflows and outflows for each stock
  flow_df <- get_flow_df(sfm)
  for (i in which(stock_idx)) {
    stock_name <- sfm[["variables"]][i, "name"]
    inflows <- flow_df[flow_df[["to"]] == stock_name, "name"]
    outflows <- flow_df[flow_df[["from"]] == stock_name, "name"]
    
    sfm[["variables"]][i, "inflow"] <- paste0(inflows, collapse = "|")
    sfm[["variables"]][i, "outflow"] <- paste0(outflows, collapse = "|")
  }
  
  for (i in which(stock_idx)) {
    # Check for delayed stock (delayN indicates it's a delay accumulator)
    if (!is.null(sfm[["variables"]][i, "delayN"]) && is_defined(sfm[["variables"]][i, "delayN"])) {
      sfm[["variables"]][i, "sum_name"] <- paste0(sfm[["variables"]][i, "inflow"], "$update")
      sfm[["variables"]][i, "sum_eqn"] <- ""
      sfm[["variables"]][i, "sum_units"] <- ""
    } else {
      inflow <- outflow <- ""
      sfm[["variables"]][i, "sum_name"] <- paste0(P[["change_prefix"]], sfm[["variables"]][i, "name"])
      
      # In case no inflow and no outflow is defined, update with 0
      inflow_def <- sfm[["variables"]][i, "inflow"]
      outflow_def <- sfm[["variables"]][i, "outflow"]
      
      if (!is_defined(inflow_def) && !is_defined(outflow_def)) {
        sfm[["variables"]][i, "sum_eqn"] <- "0"
      } else {
        # Split pipe-delimited strings into vectors
        if (is_defined(inflow_def)) {
          inflow_vec <- strsplit(inflow_def, "\\|")[[1]]
          inflow <- paste0(inflow_vec, collapse = " + ")
        }
        if (is_defined(outflow_def)) {
          outflow_vec <- strsplit(outflow_def, "\\|")[[1]]
          outflow <- paste0(" - ", paste0(outflow_vec, collapse = " - "))
        }
        sfm[["variables"]][i, "sum_eqn"] <- sprintf("%s%s", inflow, outflow)
      }
      sfm[["variables"]][i, "sum_units"] <- ""
    }
  }
  
  sfm <- validate_xmile(sfm)
  
  return(sfm)
}


#' Prepare equations and variables for Julia simulation
#'
#' @inheritParams build
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#' @noRd
#'
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
      flow_non_neg <- sfm[["variables"]][i, "non_negative"]

      eqn_str <- sprintf(
        "\n\t# Flow%s%s\n\t%s = %s%s%s",
        # Add comment
        ifelse(is_defined(flow_from), paste0(" from ", flow_from), ""),
        ifelse(is_defined(flow_to), paste0(" to ", flow_to), ""),
        flow_name,
        ifelse(flow_non_neg && keep_nonnegative_flow, "max.(0.0, ", ""),
        flow_eqn_julia,
        ifelse(flow_non_neg && keep_nonnegative_flow, ")", "")
      )

      if (!is.null(sfm[["variables"]][i, "preceding_eqn"])) {
        eqn_str <- c(sfm[["variables"]][i, "preceding_eqn"], eqn_str)
      }
      
      sfm[["variables"]][i, "eqn_str"] <- list(eqn_str)
    }
  }

  return(sfm)
}


#' Prepare for summing change in stocks for Julia simulation
#'
#' @inheritParams build
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#' @noRd
#'
prep_stock_change_julia <- function(sfm) {
  keep_unit <- sfm[["sim_specs"]][["keep_unit"]]
  
  # Add temporary columns to stocks in data frame
  stock_idx <- sfm[["variables"]][["type"]] == "stock"
  
  # Add columns if they don't exist (may not exist when called from build())
  if (!"sum_name" %in% colnames(sfm[["variables"]])) {
    sfm[["variables"]][["sum_name"]] <- ""
  }
  if (!"sum_eqn" %in% colnames(sfm[["variables"]])) {
    sfm[["variables"]][["sum_eqn"]] <- ""
  }
  if (!"unpack_state" %in% colnames(sfm[["variables"]])) {
    sfm[["variables"]][["unpack_state"]] <- ""
  }
  if (!"inflow" %in% colnames(sfm[["variables"]])) {
    sfm[["variables"]][["inflow"]] <- list(NULL)
  }
  if (!"outflow" %in% colnames(sfm[["variables"]])) {
    sfm[["variables"]][["outflow"]] <- list(NULL)
  }
  
  if (any(stock_idx)) {
    stock_names <- sfm[["variables"]][stock_idx, "name"]
    
    for (i in which(stock_idx)) {
      x <- sfm[["variables"]][i, ]
      inflow <- outflow <- ""

      # Get stock position in alphabet
      stock_position <- which(stock_names == x[["name"]])
      
      if (!is.null(x[["delayN"]]) && is_defined(x[["delayN"]])) {
        sfm[["variables"]][i, "sum_name"] <- paste0(P[["change_state_name"]], "[", P[["model_setup_name"]], ".", P[["delay_idx_name"]], ".", x[["name"]], "]")
        sfm[["variables"]][i, "unpack_state"] <- paste0(P[["state_name"]], "[", P[["model_setup_name"]], ".", P[["delay_idx_name"]], ".", x[["name"]], "]")
      } else {
        sfm[["variables"]][i, "sum_name"] <- paste0(P[["change_state_name"]], "[", stock_position, "]")
      }

      # In case no inflow and no outflow is defined, update with 0
      # inflow_def and outflow_def should be character or NULL/empty list
      inflow_def <- sfm[["variables"]][i, "inflow"]
      outflow_def <- sfm[["variables"]][i, "outflow"]
      
      # Handle list columns
      if (is.list(inflow_def)) {
        inflow_def <- inflow_def[[1]]
      }
      if (is.list(outflow_def)) {
        outflow_def <- outflow_def[[1]]
      }
      
      if (!is_defined(inflow_def) & !is_defined(outflow_def)) {
        sfm[["variables"]][i, "sum_eqn"] <- "0.0"
      } else {
        if (is_defined(inflow_def)) {
          inflow <- paste0(inflow_def, collapse = " + ")
        }
        if (is_defined(outflow_def)) {
          outflow <- paste0(paste0(" - ", outflow_def), collapse = "")
        }
        sfm[["variables"]][i, "sum_eqn"] <- sprintf("%s%s", inflow, outflow)
      }

      # Add units if defined
      if (keep_unit & is_defined(x[["units"]]) & x[["units"]] != "1") {
        if (!is.null(x[["delayN"]]) && is_defined(x[["delayN"]])) {
          sfm[["variables"]][i, "sum_eqn"] <- paste0(
            sfm[["variables"]][i, "sum_eqn"], " ./ ",
            P[["time_units_name"]]
          )
        } else {
          sfm[["variables"]][i, "sum_eqn"] <- paste0(
            P[["convert_u_func"]],
            "(", sfm[["variables"]][i, "sum_eqn"],
            ", Unitful.unit.(",
            x[["name"]], ")/",
            P[["time_units_name"]],
            ") ./ Unitful.unit.(", x[["name"]], ")"
          )
        }
      }
    }
  }

  return(sfm)
}
