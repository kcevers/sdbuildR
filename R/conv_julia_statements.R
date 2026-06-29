#' Statement conversion helpers for Julia
#'
#' Internal helper functions for converting R statements (if/else, for, while, function)
#' to Julia code


#' Convert all statement syntax from R to Julia
#' Wrapper around convert_statement()
#'
#' @inheritParams convert_equations_IM
#'
#' @returns Updated eqn
#' @noRd
#'
convert_all_statements_julia <- function(eqn, var_names) {
  # If curly brackets surround entire eqn, replace and surround with begin ... end
  if (stringr::str_sub(eqn, 1, 1) == "{" && stringr::str_sub(eqn, nchar(eqn), nchar(eqn)) == "}") {
    stringr::str_sub(eqn, nchar(eqn), nchar(eqn)) <- "\nend"
    stringr::str_sub(eqn, 1, 1) <- "begin\n"
  }

  # Only if there are curly brackets in the equation, look for statements
  if (grepl("\\{", eqn)) {
    done <- FALSE
    i <- 1 # counter

    # Define regular expressions for statements, accounting for whitespace
    statement_regex <- c(
      "for" = "for[ ]*\\(",
      "if" = "if[ ]*\\(",
      "while" = "while[ ]*\\(", "else" = "[ ]*else[ ]*\\{",
      "else if" = "[ ]*else if[ ]*\\(", "function" = "function[ ]*\\("
    )

    while (!done) {
      # Create sequence of indices of curly brackets; update each iteration
      paired_idxs <- get_range_all_pairs(eqn, var_names, type = "curly")

      # Look for statements
      idx_statements <- stringr::str_locate_all(eqn, unname(statement_regex))
      df_statements <- as.data.frame(do.call(rbind, idx_statements))
      df_statements[["statement"]] <- rep(
        names(statement_regex),
        vapply(idx_statements, nrow, numeric(1))
      )

      # # Remove those matches that are in quotation marks or names
      idxs_exclude <- get_seq_exclude(eqn, var_names, type = "quot")
      if (nrow(df_statements) > 0) df_statements <- df_statements[!(df_statements[["start"]] %in% idxs_exclude | df_statements[["end"]] %in% idxs_exclude), ]

      if (!(nrow(paired_idxs) > 0 && nrow(df_statements) > 0)) {
        done <- TRUE
      } else {
        # Sort by start index
        paired_idxs <- paired_idxs[order(paired_idxs[["start"]]), ]

        # Get all round brackets
        round_brackets <- get_range_all_pairs(eqn, var_names, type = "round")

        df_statements <- df_statements[order(df_statements[["start"]]), ]

        # Step 1: Group by 'end' and keep row with minimum 'start' value for each group
        df_grouped <- split(df_statements, df_statements[["end"]])
        df_min_rows <- do.call(rbind, lapply(df_grouped, function(group) {
          min_start_idx <- which.min(group[["start"]])
          group[min_start_idx, ]
        }))

        # Step 2: Add row numbers as 'id' column
        df_min_rows[["id"]] <- seq_len(nrow(df_min_rows))

        # Step 3: Apply find_round_brackets function to each row
        df_with_round <- do.call(rbind, lapply(seq_len(nrow(df_min_rows)), function(i) {
          row_data <- df_min_rows[i, ]
          result <- find_round_brackets(row_data, round_brackets, eqn, var_names)
          result[["id"]] <- i # Preserve the id
          result
        }))

        # Step 4: Apply find_curly_brackets function to each row
        df_statements <- do.call(rbind, lapply(seq_len(nrow(df_with_round)), function(i) {
          row_data <- df_with_round[i, ]
          result <- find_curly_brackets(row_data, paired_idxs)
          result[["id"]] <- i # Preserve the id
          result
        }))

        # Remove row names that might have been created by rbind
        rownames(df_statements) <- NULL

        # Add lead_start column (equivalent to dplyr::lead with default = 0)
        lead_start <- c(df_statements[["start"]][-1], 0) - 1
        df_statements[["lead_start"]] <- lead_start

        # Add next_statement column (equivalent to dplyr::if_else with dplyr::lead)
        lead_statement <- c(df_statements[["statement"]][-1], NA)
        df_statements[["next_statement"]] <- ifelse(
          df_statements[["end_curly"]] == df_statements[["lead_start"]],
          lead_statement,
          NA
        )

        if (nrow(df_statements) == 0) {
          done <- TRUE
        } else {
          # # At first iteration, replace all statement names with uppercase versions.
          if (i == 1) {
            for (i in seq_len(nrow(df_statements))) {
              stringr::str_sub(eqn, df_statements[i, "start"], df_statements[i, "end"]) <- toupper(stringr::str_sub(eqn, df_statements[i, "start"], df_statements[i, "end"]))
            }
            statement_regex <- toupper(statement_regex)
            i <- i + 1
            next
          }

          pair <- df_statements[1, ]
          pair |> as.data.frame()

          eqn <- process_julia_statement(eqn, pair, var_names)
        }
      }
    }
  }

  ### Convert one liner functions
  # Get start of new sentences
  idxs_newline <- rbind(
    data.frame(start = 1, end = 1),
    stringr::str_locate_all(eqn, "\n")[[1]] |> as.data.frame(),
    data.frame(start = nchar(eqn) + 1, end = nchar(eqn) + 1)
  )

  # For each new line, find first two words
  x <- idxs_newline[["end"]]
  pairs <- lapply(seq(length(x) - 1), function(i) {
    pair <- data.frame(start = x[i], end = x[i + 1] - 1)
    pair[["match"]] <- stringr::str_sub(eqn, pair[["start"]], pair[["end"]])
    words <- get_words(pair[["match"]])
    pair[["first_word"]] <- ifelse(nrow(words) > 0, words[1, "word"], "")
    pair[["second_word"]] <- ifelse(nrow(words) > 1, words[2, "word"], "")

    if (pair[["second_word"]] == "function") {
      pair[["match"]] <- process_oneliners_julia(pair, var_names)
    }
    return(pair)
  })

  eqn <- unlist(lapply(pairs, `[[`, "match")) |> paste0(collapse = "")

  return(eqn)
}


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
