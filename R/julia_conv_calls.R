# ==============================================================================
# AST function-call emission for R -> Julia conversion
# ==============================================================================

#' Emit a Julia function call, reusing syntax_df mapping + sort_args/conv_*
#' @noRd
emit_julia_call <- function(node, fname, args, var_names) {
  syntax_row <- find_julia_syntax_row(node, fname)
  arg_strings <- emit_julia_call_args(args, var_names)

  if (is.null(syntax_row)) {
    # Unknown function (user custom func, graphical function reference, or a
    # passthrough like return()): emit verbatim so later substitution applies.
    return(paste0(fname, "(", paste(arg_strings, collapse = ", "), ")"))
  }

  emit_mapped_julia_call(syntax_row, arg_strings, var_names)
}


#' Find Julia syntax table row for a call head
#' @noRd
find_julia_syntax_row <- function(node, fname) {
  syntax_df <- syntax_julia[["syntax_df"]]
  # Match by bare name or full namespaced head (syntax_df carries some of both,
  # e.g. stringr::str_to_title); call_name() already strips the namespace.
  head_full <- paste(deparse(node[[1L]]), collapse = "")
  row_idx <- which(syntax_df[["R_first_iter"]] == fname |
    syntax_df[["R_first_iter"]] == head_full)

  if (length(row_idx) == 0L) {
    return(NULL)
  }

  as.list(syntax_df[row_idx[1L], ])
}


#' Recursively emit arguments for a Julia call
#' @noRd
emit_julia_call_args <- function(args, var_names) {
  emitted <- vapply(args, emit_julia_node, character(1), var_names = var_names)
  arg_names <- names(args)
  if (is.null(arg_names)) arg_names <- rep("", length(args))
  if ("na.rm" %in% arg_names) {
    .ast_bail()
  }

  ifelse(nzchar(arg_names), paste0(arg_names, " = ", emitted), emitted)
}


#' Emit a known mapped Julia function call
#' @noRd
emit_mapped_julia_call <- function(x, arg_strings, var_names) {
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
