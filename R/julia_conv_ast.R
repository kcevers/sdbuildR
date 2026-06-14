# ==============================================================================
# AST-based R -> Julia equation translation (fast path)
# ==============================================================================
# Translates an R equation to Julia by walking R's parse tree and emitting Julia
# directly, instead of the regex/position string-surgery used by
# convert_equations_julia(). It is O(nodes), needs no per-equation regex sweeps
# or data-frame position tables, and reuses the existing function-mapping
# semantics (syntax_julia$syntax_df, sort_args(), conv_distribution/seq/sample).
#
# It returns NULL for any construct it does not handle (control flow, blocks,
# unusual nodes), so the caller transparently falls back to the proven
# convert_equations_julia(). Correctness is therefore preserved by construction;
# this is purely a speed fast-path for the common case (arithmetic + function
# calls), which covers essentially all real model equations.
#
# NOTE: the lower-level regex helpers (get_range_*, parse_args,
# select_innermost_function, ...) are shared with the InsightMaker -> R
# converter and are intentionally left untouched.

# Binary operator -> broadcast Julia operator.
.ast_binary_ops <- c(
  "+" = ".+", "-" = ".-", "*" = ".*", "/" = "./", "^" = ".^",
  "==" = ".==", "!=" = ".!=", "<" = ".<", ">" = ".>",
  "<=" = ".<=", ">=" = ".>=",
  "&" = "&&", "|" = "||", "&&" = "&&", "||" = "||"
)

#' Translate one R equation to Julia via its parse tree
#'
#' @param eqn Length-1 character R equation.
#' @param var_names Character vector of model variable names.
#' @returns Julia string, or `NULL` if the equation contains a construct the AST
#'   path does not handle (caller should fall back to convert_equations_julia()).
#' @noRd
convert_eqn_ast_julia <- function(eqn, var_names) {
  if (is.null(eqn) || length(eqn) != 1L || !nzchar(eqn)) {
    return(NULL)
  }
  parsed <- tryCatch(parse(text = eqn, keep.source = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed) || length(parsed) != 1L) {
    return(NULL)
  }
  # Catch ANY error (not just the deliberate bail), so the AST path can never do
  # worse than the legacy translator: on any hiccup we return NULL and the caller
  # falls back, where a genuinely invalid equation surfaces its canonical error.
  tryCatch(emit_julia_node(parsed[[1]], var_names),
    error = function(e) NULL
  )
}


#' Signal that the AST path cannot handle a node (triggers fallback)
#' @noRd
.ast_bail <- function() {
  rlang::abort("unsupported AST node", class = "sdbuildR_ast_unsupported")
}


#' Format an R numeric/logical literal as a Julia literal
#' @noRd
num_to_julia <- function(x) {
  if (is.logical(x)) {
    if (is.na(x)) .ast_bail() # NA -> let the legacy translator decide
    return(if (isTRUE(x)) "true" else "false")
  }
  if (is.nan(x)) {
    return("NaN")
  }
  if (is.infinite(x)) {
    return(if (x > 0) "Inf" else "-Inf")
  }
  if (is.na(x)) .ast_bail() # NA_real_ / NA_integer_ -> fall back
  s <- format(x, scientific = FALSE, trim = TRUE)
  # Ensure a Float literal so Julia never infers Int (avoids InexactError).
  if (!grepl("[.eE]", s)) {
    s <- paste0(s, ".0")
  }
  s
}


#' Recursively emit Julia for a parsed R node
#' @noRd
emit_julia_node <- function(node, var_names) {
  # Literals: numbers, logicals, strings
  if (rlang::is_syntactic_literal(node)) {
    if (is.character(node)) {
      return(paste0("\"", node, "\""))
    }
    return(num_to_julia(node))
  }

  # Bare names (variables, constants like pi)
  if (is.symbol(node)) {
    return(as.character(node))
  }

  if (!is.call(node)) {
    .ast_bail()
  }

  fname <- tryCatch(rlang::call_name(node), error = function(e) NULL)
  args <- as.list(node)[-1L]

  # --- Control flow / assignment: hand off to the fallback translator -------
  # These are identifier-like call heads, so they would otherwise slip past the
  # operator guard below and be emitted as bogus function calls (if(a, b, c)).
  if (!is.null(fname) && fname %in% c(
    "if", "for", "while", "function", "repeat",
    "return", "break", "next", "{", "<-", "<<-", "="
  )) {
    .ast_bail()
  }

  # --- Parentheses ----------------------------------------------------------
  if (identical(fname, "(")) {
    return(paste0("(", emit_julia_node(args[[1L]], var_names), ")"))
  }

  # --- Unary +/- and logical not -------------------------------------------
  if (length(args) == 1L && !is.null(fname) &&
    fname %in% c("-", "+", "!")) {
    return(paste0(fname, emit_julia_node(args[[1L]], var_names)))
  }

  # --- Binary operators -----------------------------------------------------
  if (length(args) == 2L && !is.null(fname) &&
    fname %in% names(.ast_binary_ops)) {
    return(paste0(
      emit_julia_node(args[[1L]], var_names), " ",
      .ast_binary_ops[[fname]], " ",
      emit_julia_node(args[[2L]], var_names)
    ))
  }

  # --- c(...) -> [...] ------------------------------------------------------
  if (identical(fname, "c")) {
    items <- vapply(args, emit_julia_node, character(1), var_names = var_names)
    return(paste0("[", paste(items, collapse = ", "), "]"))
  }

  # --- Function calls -------------------------------------------------------
  # Bail on non-identifier heads: unhandled infix/operators (%%, %/%, %in%, :,
  # indexing [, ...) and odd heads like (f)(x). Falling back lets the proven
  # translator handle them (and emit its targeted error messages, e.g. for %%).
  if (is.null(fname) || !grepl("^[A-Za-z.][A-Za-z0-9._]*$", fname)) {
    .ast_bail()
  }
  emit_julia_call(node, fname, args, var_names)
}


#' Emit a Julia function call, reusing syntax_df mapping + sort_args/conv_*
#' @noRd
emit_julia_call <- function(node, fname, args, var_names) {
  syntax_df <- syntax_julia[["syntax_df"]]
  # Match by bare name or full namespaced head (syntax_df carries some of both,
  # e.g. stringr::str_to_title); call_name() already strips the namespace.
  head_full <- paste(deparse(node[[1L]]), collapse = "")
  row_idx <- which(syntax_df[["R_first_iter"]] == fname |
    syntax_df[["R_first_iter"]] == head_full)

  # Recursively emit every argument first (innermost-first comes for free).
  emitted <- vapply(args, emit_julia_node, character(1), var_names = var_names)
  arg_names <- names(args)
  if (is.null(arg_names)) arg_names <- rep("", length(args))

  if (length(row_idx) == 0L) {
    # Unknown function (user custom func or graphical function reference): emit
    # verbatim so later gf-source substitution still applies.
    return(paste0(fname, "(", paste(emitted, collapse = ", "), ")"))
  }

  x <- as.list(syntax_df[row_idx[1L], ])

  # Rebuild the "name = value" argument strings sort_args() expects.
  arg_strings <- ifelse(nzchar(arg_names),
    paste0(arg_names, " = ", emitted), emitted
  )

  named_arg <- sort_args(arg_strings, x[["R_first_iter"]],
    var_names = var_names,
    fill_defaults = as.logical(x[["fill_defaults"]])
  )

  syntax <- x[["syntax"]]
  if (syntax == "syntax0") {
    return(x[["julia"]])
  } else if (syntax == "syntax1") {
    arg <- paste0(unname(unlist(named_arg)), collapse = ", ")
    return(sprintf(
      "%s%s(%s%s%s%s%s)",
      x[["julia"]],
      ifelse(isTRUE(as.logical(x[["add_broadcast"]])), ".", ""),
      x[["add_first_arg"]],
      ifelse(nzchar(x[["add_first_arg"]]) & nzchar(arg), ", ", ""),
      arg,
      x[["add_second_arg"]],
      ifelse(nzchar(x[["add_second_arg"]]) & nzchar(arg), ", ", "")
    ))
  } else if (syntax == "syntaxD") {
    return(conv_distribution(
      named_arg, x[["R_first_iter"]], x[["julia"]], x[["add_first_arg"]]
    ))
  } else if (syntax == "syntax_seq") {
    return(conv_seq(named_arg, x[["R_first_iter"]], x[["julia"]]))
  } else if (syntax == "syntax_sample") {
    return(conv_sample(named_arg, x[["R_first_iter"]], x[["julia"]]))
  }

  .ast_bail()
}
