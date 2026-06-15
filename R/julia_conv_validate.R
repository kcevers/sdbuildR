# ==============================================================================
# R -> Julia conversion validation
# ==============================================================================

#' Validate parsed R constructs before R -> Julia conversion
#' @noRd
validate_julia_conversion_ast <- function(parsed) {
  syntax_names <- syntax_julia[["syntax_df"]][["R_first_iter"]]
  syntax_base_names <- sub(".*::", "", syntax_names)

  walk <- function(node, in_index = FALSE) {
    if (!is.call(node)) {
      return(invisible(NULL))
    }

    fname <- tryCatch(rlang::call_name(node), error = function(e) NULL)
    args <- as.list(node)[-1L]

    check_namespace <- function(ns_node) {
      parts <- as.list(ns_node)
      namespace <- as.character(parts[[2L]])
      fun <- as.character(parts[[3L]])
      full_name <- paste0(namespace, "::", fun)
      allowed <- full_name %in% syntax_names ||
        (namespace %in% c("base", "stats") && fun %in% syntax_base_names)
      if (!allowed) {
        cli::cli_abort(c(
          "x" = "Unsupported namespaced function in Julia equation: {.fn {full_name}}.",
          "i" = "Use a supported base/stats function or define a custom function without namespace qualification."
        ), class = "stockflow_julia_conversion_error", call. = FALSE)
      }
    }

    if (is.call(node[[1L]]) && identical(as.character(node[[1L]][[1L]]), "::")) {
      check_namespace(node[[1L]])
    }

    if (!is.null(fname) && fname %in% c("[", "[[")) {
      for (idx in args[-1L]) {
        if (is.call(idx) && identical(rlang::call_name(idx), "-") &&
          length(as.list(idx)) == 2L && is.numeric(as.list(idx)[[2L]])) {
          cli::cli_abort(c(
            "x" = "Negative R-style indices are not supported in Julia equations.",
            "i" = "Julia uses different indexing semantics; select explicit variables or rewrite the expression."
          ), class = "stockflow_julia_conversion_error", call. = FALSE)
        }
        if (rlang::is_syntactic_literal(idx) && is.logical(idx)) {
          cli::cli_abort(c(
            "x" = "Logical R-style indices are not supported in Julia equations.",
            "i" = "Julia uses different indexing semantics; select explicit variables or rewrite the expression."
          ), class = "stockflow_julia_conversion_error", call. = FALSE)
        }
        if (rlang::is_syntactic_literal(idx) && is.numeric(idx) && idx != round(idx)) {
          cli::cli_abort(c(
            "x" = "Non-integer indices are not supported in Julia equations.",
            "i" = "Use integer indices or rewrite the expression before simulation."
          ), class = "stockflow_julia_conversion_error", call. = FALSE)
        }
        walk(idx, in_index = TRUE)
      }
      walk(args[[1L]], in_index = FALSE)
      return(invisible(NULL))
    }

    if (!is.null(fname) && fname %in% c("$", "@")) {
      cli::cli_abort(c(
        "x" = "Field and slot access are not supported in Julia equations.",
        "i" = "Use a model variable, custom function, or explicit scalar expression instead."
      ), class = "stockflow_julia_conversion_error", call. = FALSE)
    }

    if (!is.null(fname) && fname == "::") {
      check_namespace(node)
    }

    for (arg in args) {
      walk(arg, in_index = FALSE)
    }
    invisible(NULL)
  }

  for (expr in parsed) {
    walk(expr)
  }
  invisible(NULL)
}
