#' Statement conversion helpers for Julia
#'
#' Internal helper functions for converting R statements (if/else, for, while, function)
#' to Julia code


#' Process statement replacements (if/else if/else/for/while/function)
#'
#' Helper for convert_all_statements_julia()
#'
#' @param eqn Current equation string
#' @param pair data.frame with statement indices
#' @returns Modified equation string
#' @noRd
#'
process_julia_statement <- function(eqn, pair, var_names) {
  if (pair[["statement"]] %in% c("if")) {
    if (pair[["next_statement"]] %in% c("else if", "else")) {
      stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) <- ""
    } else {
      stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) <- "end"
    }
    stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) <- ""
    stringr::str_sub(eqn, pair[["end_round"]], pair[["end_round"]]) <- " "
    stringr::str_sub(eqn, pair[["start_round"]], pair[["start_round"]]) <- " "
    stringr::str_sub(eqn, pair[["start"]], pair[["end"]] - 1) <- tolower(stringr::str_sub(eqn, pair[["start"]], pair[["end"]] - 1))
  } else if (pair[["statement"]] %in% c("else if")) {
    if (pair[["next_statement"]] %in% c("else if", "else")) {
      stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) <- ""
    } else {
      stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) <- "end"
    }
    stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) <- ""
    stringr::str_sub(eqn, pair[["end_round"]], pair[["end_round"]]) <- " "
    stringr::str_sub(eqn, pair[["start"]], pair[["end"]]) <- "elseif "
  } else if (pair[["statement"]] %in% c("else")) {
    stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) <- "end"
    stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) <- ""
    stringr::str_sub(eqn, pair[["start"]], pair[["end"]] - 1) <- tolower(stringr::str_sub(eqn, pair[["start"]], pair[["end"]] - 1))
  } else if (pair[["statement"]] %in% c("for", "while")) {
    stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) <- "end"
    stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) <- ""
    stringr::str_sub(eqn, pair[["end_round"]], pair[["end_round"]]) <- " "
    stringr::str_sub(eqn, pair[["start_round"]], pair[["start_round"]]) <- " "
    stringr::str_sub(eqn, pair[["start"]], pair[["end"]] - 1) <- tolower(stringr::str_sub(eqn, pair[["start"]], pair[["end"]] - 1))
  } else if (pair[["statement"]] %in% c("function")) {
    stringr::str_sub(eqn, pair[["end_curly"]], pair[["end_curly"]]) <- "end"
    stringr::str_sub(eqn, pair[["start_curly"]], pair[["start_curly"]]) <- ""

    # Parse arguments
    arg <- parse_args(stringr::str_sub(eqn, pair[["start_round"]] + 1, pair[["end_round"]] - 1))

    # All default arguments have to be at the end; if not, throw error
    contains_name <- stringr::str_detect(arg, "=")
    arg_split <- stringr::str_split_fixed(arg, "=", n = 2)
    names_arg <- ifelse(contains_name, arg_split[, 1], NA) |> trimws()

    # error when there are non-default arguments between default arguments
    if (any(!is.na(names_arg))) {
      if (any(diff(which(!is.na(names_arg))) > 1) || max(which(!is.na(names_arg))) != length(names_arg)) {
        cli::cli_abort(c(
          "x" = "All arguments with defaults have to be placed at the end of the function arguments.",
          ">" = paste0("Change the function definition of ", pair[["func_name"]])
        ), call. = FALSE)
      }
    }

    arg <- paste0(arg, collapse = ", ") |>
      # Varargs (Variable Arguments): , ... -> ...
      stringr::str_replace_all(",[ ]*\\.\\.\\.", "...")

    stringr::str_sub(eqn, pair[["start_word"]], pair[["end_round"]]) <- paste0(
      "function ", pair[["func_name"]], "(",
      arg, ")"
    )
  }

  return(eqn)
}


#' Process one-liner function definitions
#'
#' Helper for convert_all_statements_julia()
#'
#' @param pair data.frame with statement indices
#' @param var_names Character vector of variable names or NULL
#' @returns Modified match string
#' @noRd
#'
process_oneliners_julia <- function(pair, var_names) {
  if (pair[["second_word"]] == "function") {
    pair[["match"]] <- pair[["match"]] |>
      stringr::str_replace(
        paste0(pair[["second_word"]], "[ ]*\\("),
        paste0(pair[["first_word"]], "(")
      ) |>
      # Replace assignment operator too
      stringr::str_replace(
        paste0(stringr::str_escape(pair[["first_word"]]), "[ ]*(=|<-)"),
        paste0(pair[["second_word"]], " ")
      )

    # Get all round brackets
    round_brackets <- get_range_all_pairs(pair[["match"]], var_names, type = "round")

    # Find first opening bracket
    chosen_bracket <- round_brackets[["start"]] == min(round_brackets[["start"]])
    end_idx <- round_brackets[chosen_bracket, ][["end"]]

    # Parse arguments
    arg <- parse_args(stringr::str_sub(pair[["match"]], round_brackets[chosen_bracket, "start"] + 1, end_idx - 1))

    # All default arguments have to be at the end
    contains_name <- stringr::str_detect(arg, "=")
    arg_split <- stringr::str_split_fixed(arg, "=", n = 2)
    names_arg <- ifelse(contains_name, arg_split[, 1], NA) |> trimws()

    if (any(!is.na(names_arg))) {
      if (any(diff(which(!is.na(names_arg))) > 1) | max(which(!is.na(names_arg))) != length(names_arg)) {
        cli::cli_abort(c(
          "x" = "All arguments with defaults have to be placed at the end of the function arguments.",
          ">" = paste0("Change the function definition of ", pair[["first_word"]])
        ), call. = FALSE)
      }
    }

    stringr::str_sub(pair[["match"]], end_idx, end_idx) <- ")\n"
    pair[["match"]] <- paste0(pair[["match"]], "\nend")
  }

  return(pair[["match"]])
}
