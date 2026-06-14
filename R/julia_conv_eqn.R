#' Convert all R equations to Julia code
#'
#' @inheritParams update.sdbuildR
#' @inheritParams simulate_julia
#'
#' @returns Updated object
#' @noRd
#'
convert_equations_julia_wrapper <- function(object) {
  # Get variable names
  var_names <- get_model_var(object)

  # Initialize accumulators for auxiliary variables (similar to IM wrapper)
  accumulated_add_vars <- data.frame()

  # Update equations in variables data frame
  for (i in seq_len(nrow(object[["variables"]]))) {
    if (object[["variables"]][i, "type"] %in% c("stock", "flow", "constant", "aux")) {
      var_name <- object[["variables"]][i, "name"]
      var_type <- object[["variables"]][i, "type"]
      eqn_before <- object[["variables"]][i, "eqn"]

      out <- convert_equations_julia(
        var_type,
        var_name,
        eqn_before,
        var_names
      )

      object[["variables"]][i, "eqn"] <- out[["eqn"]]

      # Accumulate auxiliary variables
      if (nrow(out[["add_vars"]])) {
        accumulated_add_vars <- rbind(accumulated_add_vars, out[["add_vars"]])
      }
    }
  }


  # # Add accumulated auxiliary and graphical function variables to the model
  if (nrow(accumulated_add_vars)) {
    # Some Insight Maker columns may be missing, e.g., eqn_insightmaker
    missing_cols <- setdiff(colnames(object[["variables"]]), colnames(accumulated_add_vars))
    for (col in missing_cols) {
      accumulated_add_vars[[col]] <- NA
    }

    object[["variables"]] <- rbind(object[["variables"]], accumulated_add_vars)
  }

  # Funcs (in the variables data frame with type == "func")
  func_idx <- which(object[["variables"]][["type"]] == "func")
  if (length(func_idx) > 0) {
    for (i in func_idx) {
      row_list <- as.list(object[["variables"]][i, , drop = FALSE])

      # If a name is defined, assign func to that name (necessary for correct conversion of functions)
      if (nzchar(row_list[["name"]]) && !startsWith(row_list[["name"]], ".")) {
        row_list[["eqn"]] <- paste0(row_list[["name"]], " = ", row_list[["eqn"]])
      }

      out <- convert_equations_julia(
        P[["func_name"]],
        P[["func_name"]],
        row_list[["eqn"]],
        var_names
      )

      # Only update the eqn column from the conversion result
      if (!is.null(out[["eqn"]])) {
        object[["variables"]][i, "eqn"] <- out[["eqn"]]
      }
    }
  }

  object
}


#' Transform R code to Julia code
#'
#' @inheritParams update.sdbuildR
#' @inheritParams convert_equations_IM
#'
#' @returns List with flat structure:
#'   - eqn: Converted Julia equation
#'   - add_vars_aux: Auxiliary variables to add
#'   - doc: Documentation from comments
#'
#' @importFrom rlang .data
#' @noRd
#'
convert_equations_julia <- function(type, name, eqn, var_names) {
  if (P[["debug"]]) {
    # cli::cli_inform("")
    # cli::cli_inform(type)
    # cli::cli_inform(name)
    cli::cli_inform(eqn)
  }

  if (length(eqn) > 1) {
    cli::cli_abort(c(
      "Invalid {.arg eqn} length.",
      "x" = "Must be length 1."
    ), call. = FALSE)
  }

  default_out <- list(
    eqn = "0.0",
    add_vars = data.frame(),
    doc = ""
  )

  # Check whether eqn is empty or NULL
  if (is.null(eqn) || !nzchar(eqn)) {
    return(default_out)
  }

  if (eqn == "0" || eqn == "0.0") {
    return(default_out)
  }

  # Try to parse the code
  out <- tryCatch(
    {
      parse(text = eqn)
      TRUE
    },
    error = function(e) {
      return(e)
    }
  )

  if ("error" %in% class(out)) {
    cli::cli_abort(paste0(
      "Parsing equation of \"",
      name, "\" failed:\n", out[["message"]]
    ), call. = FALSE)
  }

  if (any(grepl("%%", eqn))) {
    cli::cli_abort(c(
      "Modulus operator not supported.",
      "x" = "The operator {.code a %% b} is not supported.",
      ">" = "Use {.fn mod}(a, b) instead."
    ), call. = FALSE)
  }

  if (any(grepl("na\\.rm", eqn))) {
    cli::cli_abort(c(
      "Argument {.arg na.rm} not supported.",
      ">" = "Use {.fn na.omit}(x) instead."
    ), call. = FALSE)
  }

  # Remove comments we don't keep these
  eqn <- remove_comments(eqn)[["eqn"]]

  # If equation is now empty, don't run rest of functions but set equation to zero
  if (!nzchar(eqn) || eqn == "0" || eqn == "0.0") {
    return(default_out)
  } else {
    # Ensure there is no scientific notation
    eqn <- scientific_notation(eqn)

    # Step 2. Syntax (bracket types, destructuring assignment, time units {1 Month})
    eqn <- eqn |>
      # Translate vector brackets, i.e.,c() -> []
      vector_to_square_brackets(var_names) |>
      # Ensure integers are floats
      # Julia can throw InexactError errors in case e.g., an initial condition is defined as an integer
      replace_digits_with_floats(var_names)

    # # Destructuring assignment, e.g., x, y <- {a, b}
    # **to do
    # conv_destructuring_assignment()

    # Step 3. Statements (if, for, while, functions, try)
    eqn <- convert_all_statements_julia(eqn, var_names)

    # Step 4. Operators (booleans, logical operators, addition of strings)
    eqn <- eqn |>
      # # Convert addition of strings to paste0
      # conv_addition_of_strings(var_names) |>
      # # Replace logical operators (true, false, = (but not if in function()))
      replace_op_julia(var_names) #|>
    # # Replace range, e.g., range(0, 10, 2) -> 0:2:10
    # replace_range_julia(var_names)

    # Step 5. Replace R functions to Julia functions
    conv_list <- convert_builtin_functions_julia(type, name, eqn, var_names)
    eqn <- conv_list[["eqn"]]
    add_vars <- conv_list[["add_vars"]]


    # **to do:
    #     <<- --> global
    # <- -> =

    # Remove spaces in front of new lines
    eqn <- stringr::str_replace_all(eqn, "[ ]*\n", "\n")

    # Replace single with double quotation marks
    eqn <- stringr::str_replace_all(eqn, "\'", "\"")

    return(list(
      eqn = eqn,
      add_vars = add_vars,
      doc = ""
    ))
  }
}


#' Get indices of digits in string
#'
#' @inheritParams convert_equations_julia
#'
#' @returns data.frame with start and end indices of digits
#' @noRd
#' @keywords internal
#'
get_range_digits <- function(eqn, var_names) {
  get_range_digits_julia(eqn, var_names)
}


#' Replace digits with floats in string
#'
#' @inheritParams convert_equations_julia
#'
#' @returns Updated string
#' @noRd
#' @keywords internal
#'
replace_digits_with_floats <- function(eqn, var_names) {
  replace_digits_with_floats_julia(eqn, var_names)
}


#' Translate R operators to Julia
#'
#' @inheritParams convert_equations_julia
#' @returns Updated eqn
#' @importFrom rlang .data
#' @noRd
#' @keywords internal
#'
replace_op_julia <- function(eqn, var_names) {
  replace_op_julia_impl(eqn, var_names)
}


#' Find all round brackets
#'
#' Helper for convert_all_statements_julia()
#'
#' @param df data.frame with indices
#' @param round_brackets data.frame with indices of round brackets
#' @inheritParams convert_equations_julia
#'
#' @returns Modified data.frame
#' @noRd
#' @keywords internal
#'
find_round_brackets <- function(df, round_brackets, eqn, var_names) {
  find_round_brackets_julia(df, round_brackets, eqn, var_names)
}


#' Find all curly brackets
#'
#' Helper for convert_all_statements_julia()
#'
#' @param df data.frame with indices
#' @param paired_idxs data.frame with indices
#'
#' @returns Modified data.frame
#' @noRd
#' @keywords internal
#'
find_curly_brackets <- function(df, paired_idxs) {
  find_curly_brackets_julia(df, paired_idxs)
}


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
          # # At first iteration, replace all with uppercase versions, as the statement names are the same in R and Julia. This is necessart because someone may have enclosed their if statement etc. in extra round brackets, such that it still matches
          if (i == 1) {
            # Replace all statement names with uppercase versions
            for (i in seq_len(nrow(df_statements))) {
              stringr::str_sub(eqn, df_statements[i, "start"], df_statements[i, "end"]) <- toupper(stringr::str_sub(eqn, df_statements[i, "start"], df_statements[i, "end"]))
            }
            statement_regex <- toupper(statement_regex)
            i <- i + 1
            next
          }


          # Start with first pair
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
    # Get surrounding words
    pair <- data.frame(start = x[i], end = x[i + 1] - 1)
    pair[["match"]] <- stringr::str_sub(eqn, pair[["start"]], pair[["end"]])
    words <- get_words(pair[["match"]])
    pair[["first_word"]] <- ifelse(nrow(words) > 0, words[1, "word"], "")
    pair[["second_word"]] <- ifelse(nrow(words) > 1, words[2, "word"], "")

    # If second word is function, replace
    if (pair[["second_word"]] == "function") {
      pair[["match"]] <- process_oneliners_julia(pair, var_names)
    }
    return(pair)
  })

  eqn <- unlist(lapply(pairs, `[[`, "match")) |> paste0(collapse = "")

  return(eqn)
}


#' Create list of default arguments
#'
#' @param arg List with parsed arguments
#'
#' @returns List with named default arguments
#' @noRd
#'
create_default_arg <- function(arg) {
  # Find names and values of arguments
  contains_value <- stringr::str_detect(arg, "=")
  arg_split <- stringr::str_split_fixed(arg, "=", n = 2)
  values_arg <- ifelse(contains_value, arg_split[, 1], NA) |> trimws()
  names_arg <- ifelse(contains_value, arg_split[, 2], arg_split[, 1]) |> trimws()
  default_arg <- lapply(as.list(stats::setNames(values_arg, names_arg)), as.character)

  return(default_arg)
}


#' Convert R built-in functions to Julia
#'
#' @returns List with transformed eqn and list with additional R code needed to make the eqn function
#' @inheritParams convert_equations_IM
#' @noRd
#' @importFrom rlang .data
#'
convert_builtin_functions_julia <- function(type, name, eqn, var_names) {
  # Check if equation contains letters and opening and closing brackets
  # (all translated R functions have brackets)
  contains_letters <- grepl("[[:alpha:]]", eqn) && grepl("\\(", eqn) && grepl("\\)", eqn)
  if (contains_letters) {
    # data.frame with regular expressions for each built-in R function
    syntax_df <- syntax_julia[["syntax_df"]]

    # Prefilter: only consider built-in functions whose name actually appears as
    # a token in the equation. Each function's regex has a word boundary, so a
    # function that does not appear as a whole-word token cannot match -- this is
    # a safe superset of what the per-regex gregexpr scan below would find, and
    # avoids running ~180 regexes when an equation uses only a handful. (Base
    # name strips any namespace, e.g. stringr::str_to_title -> str_to_title.)
    eqn_tokens <- unique(unlist(
      regmatches(eqn, gregexpr("[A-Za-z_.][A-Za-z0-9_.]*", eqn, perl = TRUE))
    ))
    base_names <- sub(".*::", "", syntax_df[["R_first_iter"]])
    syntax_df <- syntax_df[base_names %in% eqn_tokens, , drop = FALSE]

    if (nrow(syntax_df) == 0) {
      return(list(eqn = eqn, add_vars = data.frame(), doc = ""))
    }

    # Preparation for first iteration
    done <- FALSE
    i <- 1
    R_regex <- syntax_df[["R_regex_first_iter"]]

    while (!done) {
      # Remove those matches that are in quotation marks or names
      idxs_exclude <- get_seq_exclude(eqn, var_names)

      # Update location indices of functions in eqn
      idx_df <- lapply(seq_along(R_regex), function(i) {
        matches <- gregexpr(R_regex[i], eqn, perl = TRUE, ignore.case = FALSE)[[1]]

        if (matches[1] == -1) {
          return(NULL) # Return NULL instead of empty data.frame
        } else {
          # Use cbind instead of dplyr::bind_cols for speed
          cbind(
            syntax_df[rep(i, length(matches)), , drop = FALSE],
            data.frame(
              start = as.integer(matches),
              end = as.integer(matches + attr(matches, "match.length") - 1)
            )
          )
        }
      })

      # Remove NULL entries
      idx_keep <- !vapply(idx_df, is.null, logical(1))
      idx_df <- idx_df[idx_keep]

      if (length(idx_df) == 0) {
        done <- TRUE
        next
      }

      idx_df <- do.call(rbind, idx_df)

      if (nrow(idx_df) > 0) {
        idx_df <- idx_df[!(idx_df[["start"]] %in% idxs_exclude |
          idx_df[["end"]] %in% idxs_exclude), ]
      }

      if (nrow(idx_df) == 0) {
        done <- TRUE
        next
      }

      # For the first iteration, add _replace to all detected functions, so we don't end in an infinite loop (some Julia and R functions have the same name)
      if (i == 1 && nrow(idx_df) > 0) {
        idx_df <- idx_df[order(idx_df[["start"]]), ]
        idx_df[["R_regex"]] <- stringr::str_replace_all(
          idx_df[["R_regex"]],
          stringr::fixed(c("(?<!\\.)\\b" = "", "\\(" = "(", "\\)" = ")"))
        )

        eqn <- apply_replacements_reversed(eqn, idx_df, idx_df[["R_regex"]])
      }

      if (i == 1) {
        # Switch from R_regex_first_iter to R_regex
        # Also only keep those functions that were detected on the first iteration.
        # No new functions to be translated will be added.
        syntax_df <- syntax_df[idx_keep, , drop = FALSE]
        R_regex <- syntax_df[["R_regex"]]
        i <- i + 1
        # Stop first iteration
        next
      }

      if (nrow(idx_df) == 0) {
        done <- TRUE
      } else {
        # Pair functions to brackets and pick the most nested one (syntax1b needs no brackets)
        idx_func <- select_innermost_function(eqn, idx_df, var_names,
          bracketless_syntaxes = "syntax1b",
          pair_args = list(add_custom = "paste0()")
        )

        if (P[["debug"]]) {
          cli::cli_inform(c("i" = "idx_func:"))
          cli::cli_inform(c("i" = toString(idx_func)))
        }

        # Extract argument between brackets (excluding brackets)
        bracket_arg <- stringr::str_sub(eqn, idx_func[["start_bracket"]] + 1, idx_func[["end"]] - 1)

        arg <- parse_args(bracket_arg)
        named_arg <- sort_args(arg, idx_func[["R_first_iter"]],
          var_names = var_names,
          fill_defaults = as.logical(idx_func[["fill_defaults"]])
        )
        arg <- unname(unlist(named_arg))

        # Indices of replacement in eqn
        start_idx <- idx_func[["start"]]
        end_idx <- idx_func[["end"]]

        if (idx_func[["syntax"]] == "syntax0") {
          replacement <- idx_func[["julia"]]
        } else if (idx_func[["syntax"]] == "syntax1") {
          arg <- paste0(arg, collapse = ", ")

          replacement <- sprintf(
            "%s%s(%s%s%s%s%s)",
            idx_func[["julia"]],
            ifelse(idx_func[["add_broadcast"]], ".", ""),
            idx_func[["add_first_arg"]],
            ifelse(nzchar(idx_func[["add_first_arg"]]) & nzchar(arg), ", ", ""),
            arg,
            idx_func[["add_second_arg"]],
            ifelse(nzchar(idx_func[["add_second_arg"]]) & nzchar(arg), ", ", "")
          )
        } else if (idx_func[["syntax"]] == "syntaxD") {
          # Convert random number generation. Pass the *named* argument list so
          # rate/scale reparameterization (Exponential, Gamma) can match by name.
          replacement <- conv_distribution(
            named_arg,
            idx_func[["R_first_iter"]],
            idx_func[["julia"]],
            idx_func[["add_first_arg"]]
          )
        } else if (idx_func[["syntax"]] == "syntax_seq") {
          # Convert sequence
          replacement <- conv_seq(
            named_arg,
            idx_func[["R_first_iter"]],
            idx_func[["julia"]]
          )
        } else if (idx_func[["syntax"]] == "syntax_sample") {
          # Convert sequence
          replacement <- conv_sample(
            named_arg,
            idx_func[["R_first_iter"]],
            idx_func[["julia"]]
          )
        }

        if (P[["debug"]]) {
          cli::cli_inform(c("i" = stringr::str_sub(eqn, start_idx, end_idx)))
          cli::cli_inform(c("i" = replacement))
          cli::cli_inform(c(" " = ""))
        }

        # Replace eqn
        stringr::str_sub(eqn, start_idx, end_idx) <- replacement
      }
    }
  }

  add_vars <- data.frame()

  return(list(eqn = eqn, add_vars = add_vars, doc = ""))
}


#' Convert random number generation in R to Julia
#'
#' @inheritParams sort_args
#' @param julia_func String with Julia function
#' @param R_func String with R function, e.g., "rnorm()".
#' @param distribution String with Julia distribution call
#'
#' @returns String with Julia code
#' @noRd
#' @keywords internal
#'
conv_distribution <- function(arg, R_func, julia_func, distribution) {
  conv_distribution_julia(arg, R_func, julia_func, distribution)
}


#' Convert sequence in R to Julia
#'
#' @inheritParams sort_args
#' @param R_func String with R function, e.g., "seq", "seq_along"
#' @param julia_func String with Julia function
#'
#' @returns String with Julia code
#' @noRd
#'
conv_seq <- function(arg, R_func, julia_func) {
  if (R_func == "seq_along") {
    julia_str <- paste0(julia_func, "(1.0, length(", arg[["along.with"]], "))")
  } else if (R_func == "seq_len") {
    julia_str <- paste0(julia_func, "(1.0, ", arg[["length.out"]], ")")
  } else {
    # If nothing is specified, specify by
    if (!is_defined(arg[["by"]]) && !is_defined(arg[["length.out"]]) &&
      !is_defined(arg[["along.with"]])) {
      arg[["by"]] <- "1.0" # Default value for by
    }

    if (is_defined(arg[["by"]])) {
      julia_str <- sprintf(
        "%s(%s, %s, step=%s)",
        julia_func, arg[["from"]], arg[["to"]], arg[["by"]]
      )
    } else if (is_defined(arg[["length.out"]])) {
      # Julia throws an error in this case
      if (as.numeric(arg[["length.out"]]) == 1 &&
        as.numeric(arg[["from"]]) != as.numeric(arg[["to"]])) {
        julia_str <- arg[["from"]]
      } else {
        # length.out should be an integer
        julia_str <- sprintf(
          "%s(%s, %s, round_(%s))",
          julia_func, arg[["from"]], arg[["to"]], arg[["length.out"]]
        )
      }
    } else if (is_defined(arg[["along.with"]])) {
      julia_str <- sprintf(
        "%s(%s, %s, length(%s))",
        julia_func, arg[["from"]], arg[["to"]], arg[["along.with"]]
      )
    }
  }

  return(julia_str)
}


#' Convert R sample() to Julia StatsBase.sample()
#'
#' @inheritParams conv_seq
#'
#' @returns String with Julia code
#' @noRd
conv_sample <- function(arg, R_func, julia_func) {
  # Order in StatsBase.sample() is different
  if (R_func == "sample.int") {
    arg[["x"]] <- paste0("seq(1.0, ", arg[["n"]], ")")
  }

  arg[["replace"]] <- ifelse(tolower(arg[["replace"]]) == "true", "true", "false")

  if (is_defined(arg[["prob"]])) {
    julia_str <- sprintf(
      "%s(%s, StatsBase.pweights(%s), round_(%s), replace=%s)",
      julia_func, arg[["x"]], arg[["prob"]], arg[["size"]], arg[["replace"]]
    )
  } else {
    julia_str <- sprintf(
      "%s(%s, round_(%s), replace=%s)",
      julia_func, arg[["x"]], arg[["size"]], arg[["replace"]]
    )
  }

  julia_str
}


#' Translate vector bracket syntax from R to square brackets in Julia
#'
#' @inheritParams convert_equations_IM
#' @returns Updated eqn
#' @noRd
#'
vector_to_square_brackets <- function(eqn, var_names) {
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


#' Remove scientific notation from string
#'
#' @inheritParams convert_equations_IM
#' @param task String with either "remove" or "add" to remove or add scientific notation
#' @param digits_max Number of digits after which to use scientific notation; ignored if task = "remove"; defaults to 15
#'
#' @returns Updated eqn
#' @noRd
#'
scientific_notation <- function(eqn, task = c("remove", "add")[1], digits_max = 15) {
  eqn <- as.character(eqn)

  if (task == "remove") {
    # scientific <- FALSE
    # Regex for scientific notation
    pattern <- "-?(?:\\d+\\.?\\d*|\\.\\d+)[eE][+-]?\\d+"
  } else if (task == "add") {
    # scientific <- TRUE
    # pattern = "\\d+"
    pattern <- "-?(?:\\d+\\.?\\d*|\\.\\d+)"
  }

  # Function to reformat scientific notation to fixed format
  reformat_scientific <- function(match) {
    # Convert digit match to numeric
    num <- as.numeric(match)

    # Keep any white space padding
    leading_whitespace <- stringr::str_extract(match, "^[ ]*")
    following_whitespace <- stringr::str_extract(match, "[ ]*$ ")

    # Format to scientific notation if maximum digits are exceeded
    if (task == "add") {
      # Vectorized check - use ifelse instead of if
      exceeds_max <- nchar(format(num, scientific = FALSE)) > digits_max

      replacement <- ifelse(
        exceeds_max,
        paste0(
          ifelse(is.na(leading_whitespace), "", leading_whitespace),
          format(num, scientific = TRUE, trim = TRUE),
          ifelse(is.na(following_whitespace), "", following_whitespace)
        ),
        match # Change nothing if not exceeding max
      )
    } else if (task == "remove") {
      replacement <- paste0(
        ifelse(is.na(leading_whitespace), "", leading_whitespace),
        format(num, scientific = FALSE),
        ifelse(is.na(following_whitespace), "", following_whitespace)
      )
    }

    return(replacement) # Convert back to fixed string
  }

  # Replace scientific notation in the string
  eqn <- stringr::str_replace_all(
    eqn,
    pattern = pattern,
    replacement = reformat_scientific
  )

  return(eqn)
}
