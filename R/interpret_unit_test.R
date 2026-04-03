#' Recursively interpret a parsed R expression
#'
#' @param e A language object (from parse())
#' @param parent_op The operator of the parent call (used for precedence decisions)
#' @return A human-readable string
#' @keywords internal
interpret <- function(e, parent_op = NULL) {
  # --- Atomic values ---
  if (is.numeric(e)) {
    return(as.character(e))
  }
  if (is.logical(e)) {
    return(tolower(as.character(e)))
  }
  if (is.symbol(e)) {
    return(as.character(e))
  }
  if (is.character(e)) {
    return(paste0('"', e, '"'))
  }

  if (!is.call(e)) {
    return(deparse(e))
  }

  fn <- as.character(e[[1]])
  args <- as.list(e[-1])

  # --- Parentheses: handled via precedence now ---
  # R's parser inserts explicit `(` calls in the AST when the user
  # wrote parentheses. We pass the inner expression through but
  # mark that the user explicitly grouped it.
  #   if (fn == "(") {
  #     inner <- interpret(args[[1]], parent_op = parent_op)
  #     # If the inner expression is a logical connective that differs
  #     # from the parent, wrap in parens to preserve grouping
  #     inner_fn <- if (is.call(args[[1]])) as.character(args[[1]][[1]]) else NULL
  #     if (needs_parens(inner_fn, parent_op)) {
  #       return(paste0("(", inner, ")"))
  #     }
  #     return(inner)
  #   }
  if (fn == "(") {
    return(interpret(args[[1]], parent_op = parent_op))
  }

  # --- Unary minus ---
  if (fn == "-" && length(args) == 1) {
    return(paste0("-", interpret(args[[1]], parent_op = fn)))
  }

  # --- Unary NOT ---
  if (fn == "!") {
    return(paste("it is not the case that", interpret(args[[1]], parent_op = fn)))
  }

  # --- Comparison operators ---
  comp_ops <- c(
    ">" = "is greater than", "<" = "is less than",
    ">=" = "is at least", "<=" = "is at most",
    "==" = "is equal to", "!=" = "is not equal to"
  )
  if (fn %in% names(comp_ops)) {
    result <- paste(
      interpret(args[[1]], parent_op = fn),
      comp_ops[[fn]],
      interpret(args[[2]], parent_op = fn)
    )
    return(maybe_wrap(result, fn, parent_op))
  }

  # --- Logical connectives ---
  if (fn %in% c("&", "&&")) {
    result <- paste(
      interpret(args[[1]], parent_op = fn),
      "and",
      interpret(args[[2]], parent_op = fn)
    )
    return(maybe_wrap(result, fn, parent_op))
  }
  if (fn %in% c("|", "||")) {
    result <- paste(
      interpret(args[[1]], parent_op = fn),
      "or",
      interpret(args[[2]], parent_op = fn)
    )
    return(maybe_wrap(result, fn, parent_op))
  }

  # --- Arithmetic operators ---
  if (fn == "+" && length(args) == 2) {
    result <- paste(
      interpret(args[[1]], parent_op = fn),
      "plus",
      interpret(args[[2]], parent_op = fn)
    )
    return(maybe_wrap(result, fn, parent_op))
  }
  if (fn == "-" && length(args) == 2) {
    result <- paste(
      interpret(args[[1]], parent_op = fn),
      "minus",
      interpret(args[[2]], parent_op = fn)
    )
    return(maybe_wrap(result, fn, parent_op))
  }
  if (fn == "*") {
    result <- paste(
      interpret(args[[1]], parent_op = fn),
      "times",
      interpret(args[[2]], parent_op = fn)
    )
    return(maybe_wrap(result, fn, parent_op))
  }
  if (fn == "/") {
    result <- paste(
      interpret(args[[1]], parent_op = fn),
      "divided by",
      interpret(args[[2]], parent_op = fn)
    )
    return(maybe_wrap(result, fn, parent_op))
  }
  if (fn == "^") {
    base_str <- interpret(args[[1]], parent_op = fn)
    if (is.numeric(args[[2]]) && args[[2]] == 2) {
      result <- paste(base_str, "squared")
    } else if (is.numeric(args[[2]]) && args[[2]] == 3) {
      result <- paste(base_str, "cubed")
    } else {
      result <- paste(
        base_str, "raised to the power of",
        interpret(args[[2]], parent_op = fn)
      )
    }
    return(maybe_wrap(result, fn, parent_op))
  }

  # --- Quantifiers ---
  if (fn == "all") {
    return(paste(interpret(args[[1]], parent_op = fn), "(for all values)"))
  }
  if (fn == "any") {
    return(paste(interpret(args[[1]], parent_op = fn), "(for at least one value)"))
  }

  # --- Correlation and covariance ---
  if (fn == "cor") {
    return(paste0(
      "the correlation between ", interpret(args[[1]], parent_op = fn),
      " and ", interpret(args[[2]], parent_op = fn)
    ))
  }
  if (fn == "cov") {
    return(paste0(
      "the covariance between ", interpret(args[[1]], parent_op = fn),
      " and ", interpret(args[[2]], parent_op = fn)
    ))
  }

  # --- Summary statistics ---
  summary_fns <- list(
    "mean" = "the mean of", "median" = "the median of",
    "sd" = "the standard deviation of", "var" = "the variance of",
    "sum" = "the sum of", "max" = "the maximum of",
    "min" = "the minimum of", "range" = "the range of",
    "length" = "the length of"
  )
  if (fn %in% names(summary_fns)) {
    arg_interp <- interpret(args[[1]], parent_op = fn)
    comp_logi_ops <- c(">", "<", ">=", "<=", "==", "!=", "&", "&&", "|", "||", "!")
    if (is.call(args[[1]]) && as.character(args[[1]][[1]]) %in% comp_logi_ops) {
      arg_interp <- paste0("(", arg_interp, ")")
    }
    return(paste(summary_fns[[fn]], arg_interp))
  }

  # --- Transformations ---
  transform_fns <- list(
    "abs" = "the absolute value of", "sqrt" = "the square root of",
    "sign" = "the sign of", "cumsum" = "the cumulative sum of",
    "cummax" = "the cumulative maximum of", "cummin" = "the cumulative minimum of",
    "exp" = "e raised to the power of"
  )
  if (fn %in% names(transform_fns)) {
    return(paste(transform_fns[[fn]], interpret(args[[1]], parent_op = fn)))
  }

  if (fn == "log") {
    if (length(args) == 1) {
      return(paste("the natural log of", interpret(args[[1]], parent_op = fn)))
    }
    return(paste0(
      "the log (base ", interpret(args[[2]], parent_op = fn),
      ") of ", interpret(args[[1]], parent_op = fn)
    ))
  }
  if (fn == "round") {
    if (length(args) == 1) {
      return(paste(interpret(args[[1]], parent_op = fn), "rounded"))
    }
    return(paste(
      interpret(args[[1]], parent_op = fn), "rounded to",
      interpret(args[[2]], parent_op = fn), "decimal places"
    ))
  }

  # --- diff() with optional lag ---
  if (fn == "diff") {
    named_args <- names(args)
    lag_val <- NULL
    main_arg <- args[[1]]
    if (!is.null(named_args)) {
      lag_idx <- which(named_args == "lag")
      if (length(lag_idx) > 0) lag_val <- interpret(args[[lag_idx]], parent_op = fn)
    }
    if (!is.null(lag_val) && lag_val != "1") {
      return(paste0(
        "the successive differences (lag ", lag_val, ") of ",
        interpret(main_arg, parent_op = fn)
      ))
    }
    return(paste("the successive differences of", interpret(main_arg, parent_op = fn)))
  }

  # --- head / tail ---
  if (fn == "head") {
    n_val <- if (length(args) >= 2) interpret(args[[2]], parent_op = fn) else "6"
    return(paste0("the first ", n_val, " values of ", interpret(args[[1]], parent_op = fn)))
  }
  if (fn == "tail") {
    n_val <- if (length(args) >= 2) interpret(args[[2]], parent_op = fn) else "6"
    return(paste0("the last ", n_val, " values of ", interpret(args[[1]], parent_op = fn)))
  }

  # --- Indexing with [ ---
  if (fn == "[") {
    obj <- interpret(args[[1]], parent_op = fn)
    idx <- args[[2]]
    if (is.call(idx) && as.character(idx[[1]]) == ":") {
      return(paste0(
        obj, " from index ", interpret(idx[[2]], parent_op = fn),
        " to ", interpret(idx[[3]], parent_op = fn)
      ))
    }
    if (is.call(idx) && as.character(idx[[1]]) == "length") {
      return(paste("the final value of", obj))
    }
    if (is.numeric(idx)) {
      if (idx == 1) {
        return(paste("the initial value of", obj))
      }
      return(paste0(obj, " at index ", idx))
    }
    return(paste0(obj, " where ", interpret(idx, parent_op = fn)))
  }

  # --- : operator ---
  if (fn == ":") {
    return(paste(
      interpret(args[[1]], parent_op = fn), "to",
      interpret(args[[2]], parent_op = fn)
    ))
  }

  # --- $ extraction ---
  if (fn == "$") {
    return(paste0(interpret(args[[1]], parent_op = fn), "'s ", as.character(args[[2]])))
  }

  # --- near() ---
  if (fn == "near") {
    base <- paste(
      interpret(args[[1]], parent_op = fn), "is approximately equal to",
      interpret(args[[2]], parent_op = fn)
    )
    if (length(args) >= 3) {
      base <- paste0(base, " (within tolerance ", interpret(args[[3]], parent_op = fn), ")")
    }
    return(base)
  }

  # --- ifelse / if ---
  if (fn == "ifelse" || fn == "if") {
    result <- paste0(
      "if ", interpret(args[[1]], parent_op = fn),
      " then ", interpret(args[[2]], parent_op = fn)
    )
    if (length(args) >= 3) {
      result <- paste0(result, " otherwise ", interpret(args[[3]], parent_op = fn))
    }
    return(result)
  }

  # --- which.max / which.min ---
  if (fn == "which.max") {
    return(paste("the index of the peak of", interpret(args[[1]], parent_op = fn)))
  }
  if (fn == "which.min") {
    return(paste("the index of the trough of", interpret(args[[1]], parent_op = fn)))
  }

  # --- identical ---
  if (fn == "identical") {
    if (is.call(args[[1]]) && as.character(args[[1]][[1]]) == "sort" &&
      identical(args[[1]][[2]], args[[2]])) {
      return(paste(interpret(args[[2]], parent_op = fn), "is sorted in ascending order"))
    }
    return(paste(
      interpret(args[[1]], parent_op = fn), "is identical to",
      interpret(args[[2]], parent_op = fn)
    ))
  }

  # =========================================================================
  # TESTTHAT EXPECTATIONS
  # =========================================================================

  if (fn == "expect_true") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "is true"))
  }
  if (fn == "expect_false") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "is false"))
  }

  if (fn == "expect_equal") {
    base <- paste(
      interpret(args[[1]], parent_op = fn), "equals",
      interpret(args[[2]], parent_op = fn)
    )
    tol <- find_named_arg(args, "tolerance")
    if (!is.null(tol)) base <- paste0(base, " (within tolerance ", interpret(tol, parent_op = fn), ")")
    return(paste("expect that", base))
  }
  if (fn == "expect_identical") {
    return(paste(
      "expect that", interpret(args[[1]], parent_op = fn),
      "is identical to", interpret(args[[2]], parent_op = fn)
    ))
  }

  if (fn == "expect_gt") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "is greater than", interpret(args[[2]], parent_op = fn)))
  }
  if (fn == "expect_lt") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "is less than", interpret(args[[2]], parent_op = fn)))
  }
  if (fn == "expect_gte") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "is at least", interpret(args[[2]], parent_op = fn)))
  }
  if (fn == "expect_lte") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "is at most", interpret(args[[2]], parent_op = fn)))
  }

  if (fn == "expect_length") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "has length", interpret(args[[2]], parent_op = fn)))
  }

  if (fn == "expect_named") {
    if (length(args) >= 2) {
      return(paste("expect that", interpret(args[[1]], parent_op = fn), "has names", interpret(args[[2]], parent_op = fn)))
    }
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "is named"))
  }

  if (fn == "expect_type") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "is of type", interpret(args[[2]], parent_op = fn)))
  }
  if (fn == "expect_s3_class") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "is an S3 object of class", interpret(args[[2]], parent_op = fn)))
  }
  if (fn == "expect_s4_class") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "is an S4 object of class", interpret(args[[2]], parent_op = fn)))
  }
  if (fn == "expect_null") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "is NULL"))
  }

  if (fn == "expect_match") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "matches the pattern", interpret(args[[2]], parent_op = fn)))
  }
  if (fn == "expect_no_match") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "does not match the pattern", interpret(args[[2]], parent_op = fn)))
  }

  if (fn == "expect_error") {
    if (length(args) >= 2) {
      return(paste("expect that", interpret(args[[1]], parent_op = fn), "throws an error matching", interpret(args[[2]], parent_op = fn)))
    }
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "throws an error"))
  }
  if (fn == "expect_warning") {
    if (length(args) >= 2) {
      return(paste("expect that", interpret(args[[1]], parent_op = fn), "raises a warning matching", interpret(args[[2]], parent_op = fn)))
    }
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "raises a warning"))
  }
  if (fn == "expect_message") {
    if (length(args) >= 2) {
      return(paste("expect that", interpret(args[[1]], parent_op = fn), "produces a message matching", interpret(args[[2]], parent_op = fn)))
    }
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "produces a message"))
  }
  if (fn == "expect_output") {
    if (length(args) >= 2) {
      return(paste("expect that", interpret(args[[1]], parent_op = fn), "prints output matching", interpret(args[[2]], parent_op = fn)))
    }
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "prints output"))
  }

  if (fn == "expect_silent") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "runs without messages, warnings, or errors"))
  }
  if (fn == "expect_no_error") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "does not throw an error"))
  }
  if (fn == "expect_no_warning") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "does not raise a warning"))
  }
  if (fn == "expect_no_message") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "does not produce a message"))
  }
  if (fn == "expect_condition") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "signals a condition"))
  }
  if (fn == "expect_no_condition") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "signals no conditions"))
  }
  if (fn == "expect_invisible") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "returns invisibly"))
  }
  if (fn == "expect_visible") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "returns visibly"))
  }
  if (fn == "expect_setequal") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "contains the same elements as", interpret(args[[2]], parent_op = fn)))
  }
  if (fn == "expect_contains") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "contains", interpret(args[[2]], parent_op = fn)))
  }
  if (fn == "expect_in") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "is a subset of", interpret(args[[2]], parent_op = fn)))
  }
  if (fn == "expect_mapequal") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "has the same name-value pairs as", interpret(args[[2]], parent_op = fn)))
  }

  if (fn == "expect_vector") {
    parts <- paste("expect that", interpret(args[[1]], parent_op = fn), "is a vector")
    ptype <- find_named_arg(args, "ptype")
    size <- find_named_arg(args, "size")
    if (!is.null(ptype)) parts <- paste0(parts, " of type ", interpret(ptype, parent_op = fn))
    if (!is.null(size)) parts <- paste0(parts, " with size ", interpret(size, parent_op = fn))
    return(parts)
  }

  if (fn == "expect_snapshot") {
    return(paste("expect that", interpret(args[[1]], parent_op = fn), "matches its stored snapshot"))
  }
  if (fn == "expect_snapshot_value") {
    return(paste("expect that the value of", interpret(args[[1]], parent_op = fn), "matches its stored snapshot"))
  }

  # --- c() ---
  if (fn == "c") {
    interpreted_args <- vapply(args, function(a) interpret(a, parent_op = fn), character(1))
    return(paste0("[", paste(interpreted_args, collapse = ", "), "]"))
  }

  # --- Fallback ---
  interpreted_args <- vapply(args, function(a) interpret(a, parent_op = fn), character(1))
  paste0(fn, "(", paste(interpreted_args, collapse = ", "), ")")
}


# ============================================================================
# ARGUMENT HELPERS
# ============================================================================

#' Find a named argument in a list of call arguments
#'
#' Searches by name; also handles positional fallback for the second argument
#' when the name is absent (e.g., `expect_equal(x, y, tolerance = 0.01)`).
#'
#' @param args List of call arguments (from `as.list(e[-1])`)
#' @param name Character name of the argument to find
#' @return The argument value, or NULL if not found
#' @keywords internal
find_named_arg <- function(args, name) {
  nms <- names(args)
  if (!is.null(nms)) {
    idx <- which(nms == name)
    if (length(idx) > 0) {
      return(args[[idx[[1]]]])
    }
  }
  NULL
}


# ============================================================================
# PRECEDENCE HELPERS
# ============================================================================

#' Get the precedence level of an operator
#'
#' Higher number = binds tighter. Based on R's actual operator precedence.
#' @keywords internal
op_precedence <- function(op) {
  if (is.null(op)) {
    return(-1L)
  }
  switch(op,
    "||" = 1L,
    "|" = 2L,
    "&&" = 3L,
    "&" = 4L,
    "!" = 5L,
    ">" = ,
    "<" = ,
    ">=" = ,
    "<=" = ,
    "==" = ,
    "!=" = 6L,
    "+" = ,
    "-" = 7L,
    "*" = ,
    "/" = 8L,
    "^" = 9L,
    -1L # unknown / not an infix op
  )
}

#' Determine whether parentheses are needed to preserve meaning
#'
#' Parentheses are needed when the inner operator has lower precedence
#' than the parent, because without them the human reader might
#' misinterpret the grouping.
#'
#' @param inner_op The operator inside the parentheses (or NULL)
#' @param outer_op The operator outside (parent context, or NULL)
#' @return logical
#' @keywords internal
needs_parens <- function(inner_op, outer_op) {
  if (is.null(inner_op) || is.null(outer_op)) {
    return(FALSE)
  }

  inner_prec <- op_precedence(inner_op)
  outer_prec <- op_precedence(outer_op)

  # Unknown operators: preserve parens to be safe
  if (inner_prec < 0 || outer_prec < 0) {
    return(FALSE)
  }

  # Inner binds less tightly than outer → parens needed
  # Also preserve when same precedence but different operator
  # (e.g., `(a - b) + c` vs `a - b + c` can differ for non-assoc ops)
  if (inner_prec < outer_prec) {
    return(TRUE)
  }
  if (inner_prec == outer_prec && inner_op != outer_op) {
    return(TRUE)
  }

  FALSE
}

#' Conditionally wrap a result string in parentheses
#'
#' Used by infix operators to add parens when the current operator
#' has lower precedence than the parent context.
#'
#' @param result The human-readable string for this sub-expression
#' @param current_op The current operator
#' @param parent_op The parent operator (from the recursive call)
#' @return Possibly parenthesized string
#' @keywords internal
maybe_wrap <- function(result, current_op, parent_op) {
  if (needs_parens(current_op, parent_op)) {
    return(paste0("(", result, ")"))
  }
  result
}
