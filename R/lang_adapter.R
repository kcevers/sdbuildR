# ==============================================================================
# Language adapter pattern for R and Julia code generation
# ==============================================================================
# Provides language-specific formatting functions so that shared assembly
# logic can generate code for either language without if/else branching.

#' Create a language adapter
#'
#' Returns a list of language-specific formatting functions used by
#' the shared assembly pipeline.
#'
#' @param language Character, either "R" or "Julia"
#' @returns A named list of formatting functions and constants
#' @noRd
lang_adapter <- function(language) {
  if (language == "R") {
    lang_adapter_r()
  } else if (language == "Julia") {
    lang_adapter_julia()
  } else {
    cli::cli_abort(c("x" = "Unsupported language: {language}"))
  }
}


# --- R adapter ----------------------------------------------------------------

#' @noRd
lang_adapter_r <- function() {
  list(
    language = "R",

    # No equation conversion needed for R
    convert_eqn = function(type, name, eqn, var_names) {
      eqn
    },

    # Graphical function formatting
    format_lookup = function(row, names_df) {
      xpts <- row[["xpts"]][[1]]
      ypts <- row[["ypts"]][[1]]

      if (is.null(xpts) || length(xpts) == 0) {
        return(NULL)
      }

      xpts_str <- paste0("c(", paste0(as.character(xpts), collapse = ", "), ")")

      if (is.null(ypts) || length(ypts) == 0) {
        ypts_str <- ""
      } else {
        ypts_str <- sprintf("\n\t\ty = c(%s),", paste0(as.character(ypts), collapse = ", "))
      }

      sprintf(
        "%s = stats::approxfun(x = %s,%s\n\t\tmethod = '%s', rule = %s)",
        row[["name"]], xpts_str, ypts_str,
        row[["interpolation"]],
        ifelse(row[["extrapolation"]] == "nearest", 2,
          ifelse(row[["extrapolation"]] == "NA", 1, row[["extrapolation"]])
        )
      )
    },
    assign_op = "<-",

    # Simple assignment: name = eqn (for constants and stock init)
    format_static = function(name, eqn_converted, row) {
      paste0(name, " = ", eqn_converted)
    },

    # Auxiliary: name <- eqn
    format_aux = function(name, eqn_converted, row) {
      sprintf("%s <- %s", name, eqn_converted)
    },

    # Flow: name <- [nonneg(]eqn[)] # Flow from X to Y
    format_flow = function(name, eqn_converted, row, keep_nonnegative_flow) {
      sprintf(
        "%s <- %s%s%s # Flow%s%s",
        name,
        ifelse(row[["non_negative"]] && keep_nonnegative_flow, "nonnegative(", ""),
        eqn_converted,
        ifelse(row[["non_negative"]] && keep_nonnegative_flow, "\n\t\t)", ""),
        ifelse(is_defined(row[["from"]]) && nzchar(row[["from"]]),
          paste0(" from ", row[["from"]]), ""
        ),
        ifelse(is_defined(row[["to"]]) && nzchar(row[["to"]]),
          paste0(" to ", row[["to"]]), ""
        )
      )
    },

    # Whether aux/flow eqn_str can be multi-line (list column)
    eqn_str_as_list = FALSE,

    # Stock change: sum_name
    format_sum_name = function(row, stock_position, stock_names) {
      paste0(P[["change_prefix"]], row[["name"]])
    },

    # Stock change: unpack_state (R doesn't use this)
    format_unpack_state = function(row, stock_position, stock_names) {
      NULL
    },

    # Zero literal
    zero = "0",

    # Whether to call sanitize_sdbuildR after stock change prep
    validate_after_stock_change = FALSE,

    # Func equation formatting: R just pastes name = eqn
    convert_func_eqn = function(name, eqn, var_names) {
      if (nzchar(name) && !startsWith(name, ".")) {
        paste0(name, " = ", eqn)
      } else {
        eqn
      }
    }
  )
}


# --- Julia adapter ------------------------------------------------------------

#' @noRd
lang_adapter_julia <- function() {
  list(
    language = "Julia",

    # Convert R equation syntax to Julia. convert_equations_julia() uses the AST
    # path internally and falls back for constructs it does not handle.
    convert_eqn = function(type, name, eqn, var_names) {
      result <- convert_equations_julia(
        type = type, name = name, eqn = eqn,
        var_names = var_names
      )
      result[["eqn"]]
    },

    # Graphical function formatting
    format_lookup = function(row, names_df) {
      if (!is_defined(row[["xpts"]]) || !is_defined(row[["ypts"]])) {
        return(NULL)
      }

      xpts_val <- row[["xpts"]][[1]]
      if (inherits(xpts_val, "numeric")) {
        xpts_str <- paste0("[", paste0(as.character(xpts_val), collapse = ", "), "]")
      } else {
        xpts_str <- stringr::str_replace_all(xpts_val, "^c\\(", "[") |>
          stringr::str_replace_all("\\)$", "]")
      }

      ypts_val <- row[["ypts"]][[1]]
      if (inherits(ypts_val, "numeric")) {
        ypts_str <- paste0("[", paste0(as.character(ypts_val), collapse = ", "), "]")
      } else {
        ypts_str <- stringr::str_replace_all(ypts_val, "^c\\(", "[") |>
          stringr::str_replace_all("\\)$", "]")
      }

      sprintf(
        "%s = itp(%s,\n\t%s, method = \"%s\", extrapolation = \"%s\")",
        row[["name"]], xpts_str, ypts_str,
        row[["interpolation"]], row[["extrapolation"]]
      )
    },
    assign_op = "=",

    # Static equation
    format_static = function(name, eqn_converted, row) {
      paste0(name, " = ", eqn_converted)
    },

    # Auxiliary
    format_aux = function(name, eqn_converted, row) {
      paste0(name, " = ", eqn_converted)
    },

    # Flow with optional nonneg and comment
    format_flow = function(name, eqn_converted, row, keep_nonnegative_flow) {
      sprintf(
        "\n\t# Flow%s%s\n\t%s = %s%s%s",
        ifelse(is_defined(row[["from"]]), paste0(" from ", row[["from"]]), ""),
        ifelse(is_defined(row[["to"]]), paste0(" to ", row[["to"]]), ""),
        name,
        ifelse(row[["non_negative"]] && keep_nonnegative_flow, "max.(0.0, ", ""),
        eqn_converted,
        ifelse(row[["non_negative"]] && keep_nonnegative_flow, ")", "")
      )
    },

    # Aux/flow eqn_str stored as list (supports preceding_eqn)
    eqn_str_as_list = TRUE,

    # Stock change: sum_name using state vector indexing
    format_sum_name = function(row, stock_position, stock_names) {
      paste0(P[["change_state_name"]], "[", stock_position, "]")
    },

    # Stock change: unpack_state
    format_unpack_state = function(row, stock_position, stock_names) {
      paste0(P[["state_name"]], "[", stock_position, "]")
    },

    # Zero literal
    zero = "0.0",

    # Whether to call sanitize_sdbuildR after stock change prep
    validate_after_stock_change = FALSE,

    # Func equation formatting: Julia converts R syntax to Julia
    convert_func_eqn = function(name, eqn, var_names) {
      full_eqn <- if (nzchar(name) && !startsWith(name, ".")) paste0(name, " = ", eqn) else eqn
      result <- convert_equations_julia(
        type = P[["func_name"]],
        name = name,
        eqn = full_eqn,
        var_names = var_names
      )
      result[["eqn"]]
    }
  )
}
