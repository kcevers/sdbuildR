#' Get indices of digits in string
#'
#' @inheritParams convert_equations_julia
#'
#' @returns data.frame with start and end indices of digits
#' @noRd
#'
get_range_digits_julia <- function(eqn, var_names) {
  # Get indices in variable names or quotations to exclude later
  idxs_exclude <- get_seq_exclude(eqn, var_names, names_with_brackets = FALSE)

  # Locate all integers
  idx_df <- as.data.frame(stringr::str_locate_all(eqn, "(?<![a-zA-Z0-9\\.])[0-9]+(?![a-zA-Z0-9\\.])")[[1]])

  if (nrow(idx_df) > 0) {
    # Remove matches within variable names or quotations
    idx_df <- idx_df[!(idx_df[["start"]] %in% idxs_exclude | idx_df[["end"]] %in% idxs_exclude), ]

    if (nrow(idx_df) > 0) {
      # Extract substrings vectorized
      sub_formulas <- stringr::str_sub(eqn, idx_df[["start"]], idx_df[["end"]])

      # Filter idx_df where substrings contain only digits
      idx_df <- idx_df[grepl("^[0-9]+$", sub_formulas), ]
    }
  }

  return(idx_df)
}


#' Replace digits with floats in string
#'
#' @inheritParams convert_equations_julia
#'
#' @returns Updated string
#' @noRd
#'
replace_digits_with_floats_julia <- function(eqn, var_names) {
  idx_df <- get_range_digits_julia(eqn, var_names)

  if (nrow(idx_df) > 0) {
    # Replace digit with float in each case
    for (i in rev(idx_df[["end"]])) {
      eqn <- stringr::str_c(stringr::str_sub(eqn, 1, i), ".0", stringr::str_sub(eqn, i + 1, -1))
    }
  }

  return(eqn)
}


#' Translate R operators to Julia
#'
#' @inheritParams convert_equations_julia
#' @returns Updated eqn
#' @importFrom rlang .data
#' @noRd
#'
replace_op_julia_impl <- function(eqn, var_names) {
  # Define logical operators in R and replacements in Julia
  logical_op_words <- c(
    "TRUE" = "true", "FALSE" = "false", "T" = "true",
    "F" = "false", "NULL" = "nothing", "NA" = "missing"
  )
  # Cannot be preceded or followed by a letter
  names(logical_op_words) <- paste0(
    "(?:^|(?<=\\W))",
    stringr::str_escape(names(logical_op_words)), "(?=(?:\\W|$))"
  )

  logical_op_signs <- c(
    # Default: broadcast operations
    "*" = " .* ",
    "/" = " ./ ",
    "+" = " .+ ",
    "^" = " .^ ",
    "<=" = " .<= ",
    ">=" = " .>= ",
    "==" = " .== ",
    "!=" = " .!= ",
    "%%" = "\\u2295",
    "%REM%" = "%",
    "<-" = " = ",
    "%*%" = " * ",
    "%in%" = " in "
  )

  names(logical_op_signs) <- paste0("(?<![\\.%])", stringr::str_escape(names(logical_op_signs)))
  logical_op <- c(logical_op_words, logical_op_signs)

  # Add additional operators to replace, which require special regex
  logical_op <- c(
    logical_op,
    c("(?<!<)-(?!>)" = " .- "),
    c("(?<!\\.|%)<(?!-|=)" = " .< "),
    c("(?<!\\.|-|%)>(?!=)" = " .> "),
    c("(?<!&)&(?!&)" = " && ")
  )

  # Find indices of logical operators
  idxs_logical_op <- stringr::str_locate_all(eqn, names(logical_op))

  if (length(unlist(idxs_logical_op)) > 0) {
    # Get match and replacement
    df_logical_op <- as.data.frame(do.call(rbind, idxs_logical_op))
    df_logical_op[["match"]] <- stringr::str_sub(eqn, df_logical_op[["start"]], df_logical_op[["end"]])
    df_logical_op[["replacement"]] <- rep(
      unname(logical_op),
      vapply(idxs_logical_op, nrow, numeric(1))
    )
    df_logical_op <- df_logical_op[order(df_logical_op[["start"]]), ]

    # Remove those that are in quotation marks or names
    idxs_exclude <- get_seq_exclude(eqn, var_names)

    if (nrow(df_logical_op) > 0) df_logical_op <- df_logical_op[!(df_logical_op[["start"]] %in% idxs_exclude | df_logical_op[["end"]] %in% idxs_exclude), ]
    # Remove matches that are the same as the logical operator
    if (nrow(df_logical_op) > 0) df_logical_op <- df_logical_op[df_logical_op[["replacement"]] != df_logical_op[["match"]], ]

    if (nrow(df_logical_op) > 0) {
      # Replace in reverse order
      for (i in rev(seq_len(nrow(df_logical_op)))) {
        stringr::str_sub(eqn, df_logical_op[i, ][["start"]], df_logical_op[i, ][["end"]]) <- df_logical_op[i, ][["replacement"]]
      }
      # Remove double spaces
      eqn <- stringr::str_replace_all(eqn, "[ ]+", " ")
    }
  }

  return(eqn)
}


#' Find all round brackets for statements
#'
#' Helper for convert_all_statements_julia()
#'
#' @param df data.frame with indices
#' @param round_brackets data.frame with indices of round brackets
#' @inheritParams convert_equations_julia
#'
#' @returns Modified data.frame
#' @noRd
#'
find_round_brackets_julia <- function(df, round_brackets, eqn, var_names) {
  statements <- c("if", "else if", "for", "while", "function")
  if (df[["statement"]] %in% c(statements, toupper(statements))) {
    matching <- round_brackets[match(df[["end"]], round_brackets[["start"]]), ]
    start_round <- matching[["start"]]
    end_round <- matching[["end"]]
  } else {
    start_round <- end_round <- NA
  }

  start_word <- end_word <- func_name <- NA

  if (df[["statement"]] %in% c("function", "FUNCTION")) {
    # Get words before statement
    words <- get_words(stringr::str_sub(eqn, 1, df[["start"]] - 1))
    if (nrow(words) > 0) {
      # Pick last word
      word <- words[nrow(words), ]
      start_word <- word[["start"]]
      end_word <- word[["end"]]
      func_name <- word[["word"]]
    }
  }
  return(cbind(df, data.frame(
    start_round = start_round, end_round = end_round,
    start_word = start_word, end_word = end_word,
    func_name = func_name
  )))
}


#' Find all curly brackets for statements
#'
#' Helper for convert_all_statements_julia()
#'
#' @param df data.frame with indices
#' @param paired_idxs data.frame with indices
#'
#' @returns Modified data.frame
#' @noRd
#'
find_curly_brackets_julia <- function(df, paired_idxs) {
  statements <- c("if", "else if", "for", "while", "function")
  if (df[["statement"]] %in% c(statements, toupper(statements))) {
    matching <- paired_idxs[which(paired_idxs[["start"]] > df[["end_round"]])[1], ]
  } else {
    matching <- paired_idxs[which(paired_idxs[["start"]] == df[["end"]])[1], ]
  }
  start_curly <- matching[["start"]]
  end_curly <- matching[["end"]]
  return(cbind(df, data.frame(start_curly = start_curly, end_curly = end_curly)))
}


#' Translate vector bracket syntax from R to square brackets in Julia
#'
#' @inheritParams convert_equations_julia
#' @returns Updated eqn
#' @noRd
#'
vector_to_square_brackets_julia <- function(eqn, var_names) {
  # Get indices of all enclosures
  paired_idxs <- get_range_all_pairs(eqn, var_names,
    type = "vector",
    names_with_brackets = FALSE
  )

  # Remove those that are preceded by a letter
  if (nrow(paired_idxs) > 0) paired_idxs <- paired_idxs[!stringr::str_detect(stringr::str_sub(eqn, paired_idxs[["start"]] - 1, paired_idxs[["start"]] - 1), "[[:alpha:]]"), ]

  if (nrow(paired_idxs) > 0) {
    # First replace all closing brackets with ]
    chars <- strsplit(eqn, "", fixed = TRUE)[[1]]
    chars[paired_idxs[["end"]]] <- "]"
    eqn <- paste0(chars, collapse = "")

    # Order paired_idxs by start position
    paired_idxs <- paired_idxs[order(paired_idxs[["start"]]), ]

    # Replace opening brackets c( with [
    for (j in rev(seq_len(nrow(paired_idxs)))) {
      # Replace c( with [
      stringr::str_sub(eqn, paired_idxs[j, "start"], paired_idxs[j, "start"] + 1) <- "["
    }
  }

  return(eqn)
}
