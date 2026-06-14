# ==============================================================================
# String-based fallback R -> Julia function conversion
# ==============================================================================

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
