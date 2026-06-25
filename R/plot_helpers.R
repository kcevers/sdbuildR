#' Abort if a plot argument is non-NULL but of the wrong type
#'
#' Helper for validate_plot_params() that collapses the repeated
#' "check type, otherwise cli_abort" blocks into a single call.
#'
#' @param value The argument value (skipped when `NULL`).
#' @param arg Argument name, used in the error message.
#' @param type One of "logical", "character", "numeric"; the required type.
#' @param hint Optional final bullet with a usage hint.
#' @noRd
.assert_plot_type <- function(value, arg, type, hint = NULL) {
  predicate <- switch(type,
    logical = is.logical,
    character = is.character,
    numeric = is.numeric
  )
  if (!is.null(value) && !predicate(value)) {
    bullets <- c(
      "x" = "Invalid {.arg {arg}} argument.",
      "i" = "The {.arg {arg}} argument must be {.cls {type}}."
    )
    if (!is.null(hint)) bullets <- c(bullets, ">" = hint)
    cli::cli_abort(bullets)
  }
}


#' Resolve a preference vector against the available options
#'
#' Returns the first element of `pref` that is in `available` (or the literal
#' `"none"`), falling back to `"none"` when nothing matches. Used by
#' [plot.ensemble_stockflow()] to pick the central tendency and spread band from
#' user preference vectors, given which statistics are present in the summary.
#'
#' @param pref Character preference vector, in priority order.
#' @param available Character vector of options that can actually be used.
#' @returns A single character value: the first usable option, or `"none"`.
#' @noRd
resolve_summary_choice <- function(pref, available) {
  usable <- pref[pref %in% c(available, "none")]
  if (length(usable) == 0) {
    return("none")
  }
  usable[1]
}


#' Validate plot parameters
#'
#' Validate common parameters used across all plotting functions (plot.simulate_stockflow,
#' plot.stockflow, plot.ensemble_stockflow).
#'
#' @param showlegend Logical, whether to show legend.
#' @param vars Character vector of variable names to plot, or NULL.
#' @param palette Character, color palette name.
#' @param colors Character vector of colors, or NULL.
#' @param line_width Numeric line width (single value or one per variable), or NULL.
#' @param central_line_width Numeric central-tendency line width (single value or one per variable), or NULL.
#' @param alpha Numeric, transparency.
#' @param font_family Character, font family name.
#' @param font_size Numeric, font size in points.
#' @param wrap_width Integer, text wrap width for labels.
#' @param label_subplots Logical, whether to label subplots with condition names.
#'
#' @returns Invisibly returns a list of validation results. Throws cli errors if validation fails.
#' @noRd
#'
validate_plot_params <- function(showlegend = NULL,
                                 vars = NULL,
                                 palette = NULL,
                                 colors = NULL,
                                 line_width = NULL,
                                 central_line_width = NULL,
                                 alpha = NULL,
                                 font_family = NULL,
                                 font_size = NULL,
                                 wrap_width = NULL,
                                 label_subplots = NULL,
                                 webgl = NULL) {
  .assert_plot_type(showlegend, "showlegend", "logical", "Use {.code TRUE} or {.code FALSE}.")

  .assert_plot_type(webgl, "webgl", "logical", "Use {.code TRUE} or {.code FALSE}.")

  if (!is.null(vars)) {
    if (!is.character(vars)) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg vars} argument.",
        "i" = "Received: {.cls {typeof(vars)}}.",
        ">" = "Provide a character vector of variable names."
      ))
    }

    if (length(vars) == 0) {
      cli::cli_abort(c(
        "x" = "Empty {.arg vars} vector.",
        ">" = "Provide at least one variable name."
      ))
    }
  }

  .assert_plot_type(palette, "palette", "character", "Use {.code hcl.pals()} to see available palettes.")

  .assert_plot_type(colors, "colors", "character", "Provide a character vector of valid color names or hex codes.")

  .assert_plot_type(line_width, "line_width", "numeric", "Provide a single value or one value per variable.")

  if (!is.null(line_width) && any(line_width <= 0)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg line_width} argument.",
      ">" = "All {.arg line_width} values must be positive."
    ))
  }

  .assert_plot_type(central_line_width, "central_line_width", "numeric", "Provide a single value or one value per variable.")

  if (!is.null(central_line_width) && any(central_line_width <= 0)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg central_line_width} argument.",
      ">" = "All {.arg central_line_width} values must be positive."
    ))
  }

  .assert_plot_type(alpha, "alpha", "numeric", "Provide a numeric value between 0 and 1.")

  if (!is.null(alpha) && (alpha < 0 | alpha > 1)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg alpha} argument.",
      "i" = "The {.arg alpha} argument must be between 0 and 1.",
      ">" = "Provide a numeric value between 0 and 1."
    ))
  }

  .assert_plot_type(font_family, "font_family", "character")

  if (!is.null(font_size) && (!is.numeric(font_size) || font_size <= 0)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg font_size} argument.",
      ">" = "The {.arg font_size} argument must be a positive number."
    ))
  }

  if (!is.null(wrap_width) && (!is.numeric(wrap_width) || wrap_width <= 0)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg wrap_width} argument.",
      ">" = "The {.arg wrap_width} argument must be a positive integer."
    ))
  }

  .assert_plot_type(label_subplots, "label_subplots", "logical")

  invisible(TRUE)
}


#' Prepare and format variable labels
#'
#' Consolidate label formatting logic: handle duplicates, wrapping, special characters,
#' and create name-to-label dictionaries for variables.
#'
#' @param names_df Data frame with columns "name", "label", and "type".
#' @param wrap_width Integer, text wrap width.
#' @param format_label Logical, whether to apply default formatting (remove underscores/periods).
#'   Only applied if name equals label.
#'
#' @returns Data frame with prepared labels (original names_df plus modified "label" column).
#' @noRd
#'
prepare_labels <- function(names_df, wrap_width, format_label = FALSE, deduplicate = TRUE) {
  # Apply default formatting if requested (remove underscores and periods)
  if (format_label) {
    formatted_label <- gsub("_", " ", names_df[["label"]], fixed = TRUE)
    formatted_label <- gsub(".", " ", formatted_label, fixed = TRUE)
    formatted_label <- gsub("  ", " ", formatted_label, fixed = TRUE)
    names_df[["label"]] <- ifelse(names_df[["name"]] == names_df[["label"]],
      formatted_label, names_df[["label"]]
    )
  }

  # Escape single quotes for Graphviz/DiagrammeR compatibility
  names_df[["label"]] <- gsub("'", "\\\\'", names_df[["label"]])

  # Text wrap to prevent long labels from squishing plots
  names_df[["label"]] <- str_wrap_(names_df[["label"]], width = wrap_width)

  # Detect and handle duplicate labels by appending variable name in parentheses
  if (deduplicate && nrow(names_df) > 1) {
    labels <- names_df[["label"]]
    dup_indices <- which(labels %in% labels[duplicated(labels) |
      duplicated(labels, fromLast = TRUE)])

    if (length(dup_indices) > 0) {
      names_df[dup_indices, "label"] <- paste0(
        names_df[dup_indices, "label"], " (",
        names_df[dup_indices, "name"], ")"
      )
    }
  }

  return(names_df)
}


#' Escape characters that are special in Graphviz HTML-like labels
#'
#' @param x Character vector.
#' @returns Character vector with &, <, and > escaped.
#' @noRd
#'
escape_html_ <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}


#' Build an HTML-like node label that shows the label and its equation
#'
#' Produce a Graphviz HTML-like label body that places the variable label on one
#' line and its equation underneath, in a smaller font and a given colour. The
#' returned value is the inner HTML, WITHOUT the surrounding angle brackets, so
#' callers assign it via `label=<...>` or `xlabel=<...>`.
#'
#' @param label Character vector of variable labels (may contain "\\n" from wrapping).
#' @param eqn Character vector of equations (same length as label).
#' @param eqn_font_size Numeric, font size for the equation line.
#' @param eqn_col Character, colour of the equation text.
#' @param wrap_width Integer, text wrap width applied to the equation.
#' @param italic Logical, if TRUE wrap the label portion in <I></I> (constants).
#' @returns Character vector of HTML-like label bodies (no surrounding angle brackets).
#' @noRd
#'
make_eqn_label <- function(label, eqn, eqn_font_size, eqn_col, wrap_width, italic = FALSE) {
  # Labels arrive from prepare_labels() with single quotes escaped for quoted
  # Graphviz strings; undo that escaping for HTML-like labels.
  label <- gsub("\\'", "'", label, fixed = TRUE)
  label_html <- gsub("\n", "<BR/>", escape_html_(label), fixed = TRUE)
  if (italic) {
    label_html <- paste0("<I>", label_html, "</I>")
  }

  # Wrap the equation to the same width as the labels, then break and escape.
  eqn <- str_wrap_(eqn, width = wrap_width)
  eqn_html <- gsub("\n", "<BR/>", escape_html_(eqn), fixed = TRUE)

  sprintf(
    "%s<BR/><FONT POINT-SIZE=\"%s\" COLOR=\"%s\">eqn = %s</FONT>",
    label_html, eqn_font_size, eqn_col, eqn_html
  )
}


#' Compose an informative tooltip for a stock-and-flow diagram node
#'
#' Build a multi-line tooltip string describing a single variable: its type and
#' label, its name (only when it differs from the label), its equation/value,
#' and its structural role (inflows/outflows for stocks, from/to for flows).
#' Lines are joined with the literal escape "\\n", which Graphviz renders as a
#' line break in the SVG tooltip.
#'
#' @param type Variable type ("stock", "flow", "aux", or "constant").
#' @param label Human-readable (unwrapped) label.
#' @param name Variable name.
#' @param eqn Equation string (may be NA or "").
#' @param inflows,outflows Character vectors of flow labels (stocks only).
#' @param from_label,to_label Source/destination labels (flows only); NA to omit.
#' @returns A single tooltip string with "\\n"-separated lines.
#' @noRd
#'
node_tooltip <- function(type, label, name, eqn,
                         inflows = character(0), outflows = character(0),
                         from_label = NA_character_, to_label = NA_character_) {
  type_title <- switch(type,
    stock = "Stock",
    flow = "Flow",
    aux = "Auxiliary",
    constant = "Constant",
    type
  )

  lines <- sprintf("%s: %s", type_title, label)

  if (!identical(name, label)) {
    lines <- c(lines, sprintf("Name: %s", name))
  }

  eqn_field <- switch(type,
    stock = "Initial value",
    constant = "Value",
    "Equation"
  )
  if (!is.na(eqn) && nzchar(eqn)) {
    lines <- c(lines, sprintf("%s: %s", eqn_field, eqn))
  }

  none <- "\u2014" # em dash
  if (type == "stock") {
    lines <- c(
      lines,
      sprintf("Inflows: %s", if (length(inflows)) paste(inflows, collapse = ", ") else none),
      sprintf("Outflows: %s", if (length(outflows)) paste(outflows, collapse = ", ") else none)
    )
  }

  if (type == "flow") {
    if (!is.na(from_label)) lines <- c(lines, sprintf("From: %s", from_label))
    if (!is.na(to_label)) lines <- c(lines, sprintf("To: %s", to_label))
  }

  paste(lines, collapse = "\\n")
}


#' Create plotly theme with consistent styling
#'
#' Generate a reusable theme configuration for plotly plots, including fonts,
#' margins, legend, and axis styling.
#'
#' @param font_family Character, font family name.
#' @param font_size Numeric, base font size in points.
#' @param margin_t Numeric, top margin in pixels.
#' @param margin_b Numeric, bottom margin in pixels.
#' @param margin_l Numeric, left margin in pixels.
#' @param margin_r Numeric, right margin in pixels.
#' @param legend_font_scale Numeric, scale factor for legend font size relative to base font_size.
#'   Defaults to 0.85.
#'
#' @returns List with plotly layout specifications.
#' @noRd
#'
plotly_theme <- function(font_family = "Times New Roman",
                         font_size = 16,
                         margin_t = 50,
                         margin_b = 50,
                         margin_l = 50,
                         margin_r = 50,
                         legend_font_scale = 0.85) {
  list(
    font = list(family = font_family, size = font_size),
    margin = list(t = margin_t, b = margin_b, l = margin_l, r = margin_r),
    legend = list(
      traceorder = "reversed",
      font = list(size = ceiling(font_size * legend_font_scale))
    ),
    xaxis = list(font = list(size = font_size)),
    yaxis = list(font = list(size = font_size))
  )
}


#' Create DiagrammeR/Graphviz theme with consistent styling
#'
#' Generate styling parameters for diagram plots (plot.stockflow).
#'
#' @param font_family Character, font family name.
#' @param font_size Numeric, base font size in points.
#' @param aux_font_scale Numeric, scale factor for auxiliary/constant fonts.
#'   Defaults to 0.89 (font_size - 2).
#'
#' @returns List with diagram styling parameters.
#' @noRd
#'
diagram_theme <- function(font_family = "Times New Roman",
                          font_size = 18,
                          aux_font_scale = 0.89) {
  list(
    font_family = font_family,
    font_size = font_size,
    aux_font_size = max(8, font_size - 2), # Minimum 8pt for readability
    stock_col = "#83d3d4",
    flow_col = "#f48153",
    dependency_col = "#999999"
  )
}


#' Apply optional parameters from ... to layout
#'
#' Centralize the handling of ... arguments for optional parameters like
#' main, xlab, ylab, xlim, ylim, alpha, and sub across plot functions.
#'
#' @param dots List from \code{list(...)} in calling function.
#' @param defaults Named list of default values for optional parameters.
#'   Keys should match parameter names (e.g., "main", "xlab", "ylab").
#'
#' @returns List with finalized parameter values, using provided values from dots
#'   where available, otherwise defaults.
#' @noRd
#'
extract_plot_params <- function(dots, defaults) {
  result <- defaults

  for (name in names(defaults)) {
    if (name %in% names(dots)) {
      result[[name]] <- dots[[name]]
    }
  }

  result
}


#' Validate variable names against a model
#'
#' Check whether specified variables exist in the model and are present in
#' the data frame.
#'
#' @param vars Character vector of variable names to check, or NULL.
#' @param names_df Data frame with column "name" containing valid variable names.
#' @param df Data frame with column "variable" containing variables in data.
#' @param context Character, brief context for error message (e.g., "simulation", "diagram").
#'
#' @returns Invisibly returns TRUE if validation passes. Throws cli errors otherwise.
#' @noRd
#'
validate_vars_in_model <- function(vars, names_df, df = NULL, context = "model") {
  if (is.null(vars)) {
    return(invisible(TRUE))
  }

  # Check whether specified variables exist in names_df
  idx <- !(vars %in% names_df[["name"]])
  if (any(idx)) {
    cli::cli_abort(
      c(
        "!" = paste0(
          paste0(vars[idx], collapse = ", "),
          ifelse(sum(idx) == 1, " is not a variable", " are not variables"),
          " in the ", context, "."
        ),
        "i" = paste0(
          "Model variables: ",
          paste0(sort(names_df[["name"]]), collapse = ", ")
        )
      )
    )
  }

  # Check whether variables are in the data frame (if provided)
  if (!is.null(df) && "variable" %in% colnames(df)) {
    not_saved <- vars[!(vars %in% df[["variable"]])]
    if (length(not_saved) > 0) {
      vars_not_saved(not_saved, names_df, arg = "vars", action = "abort")
    }
  }

  invisible(TRUE)
}


#' Normalize and validate a layout-grouping argument for plot.stockflow()
#'
#' Used for the `align` and `order` arguments. Accepts either a single character
#' vector (one group) or a list of character vectors (several groups), normalizes
#' both to a list of character vectors, and validates the variable names:
#' \itemize{
#'   \item names absent from the model are treated as typos and abort;
#'   \item names that exist but are not currently drawn (hidden by `vars`,
#'     `show_constants`, or `show_aux`) are dropped with a warning;
#'   \item within-group whitespace is trimmed and duplicates removed (order kept);
#'   \item groups with fewer than `min_len` drawn members are dropped (a group of
#'     one has nothing to align or order).
#' }
#'
#' @param x The `align`/`order` argument: `NULL`, a character vector, or a list
#'   of character vectors.
#' @param plot_var Character vector of variable names actually drawn in the diagram.
#' @param model_var Character vector of all variable names in the model (typo check).
#' @param arg Argument name for messages ("align" or "order").
#' @param min_len Minimum number of drawn members for a group to be kept. Defaults to 2.
#'
#' @returns A list of character vectors, each with at least `min_len` drawn names,
#'   in the user-specified order. Returns an empty list when `x` is `NULL` or
#'   nothing survives filtering.
#' @noRd
prepare_layout_groups <- function(x, plot_var, model_var, arg, min_len = 2L) {
  if (is.null(x)) {
    return(list())
  }

  # A single character vector is one group
  if (is.character(x)) {
    x <- list(x)
  }

  if (!is.list(x) || length(x) == 0L || !all(vapply(x, is.character, logical(1)))) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg {arg}} argument.",
      ">" = "{.arg {arg}} must be a character vector or a list of character vectors."
    ))
  }

  # Trim, drop blanks, de-duplicate within each group (preserving order)
  x <- lapply(x, function(g) {
    g <- trimws(g)
    unique(g[nzchar(g)])
  })

  flat <- unique(unlist(x, use.names = FALSE))

  # Typo protection: names absent from the model abort
  unknown <- setdiff(flat, model_var)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "!" = paste0(
        "{.arg {arg}}: ",
        paste0(unknown, collapse = ", "),
        ifelse(length(unknown) == 1, " is not a variable", " are not variables"),
        " in the model."
      ),
      "i" = paste0("Model variables: ", paste0(sort(model_var), collapse = ", "))
    ))
  }

  # Known but not currently drawn: drop with a warning
  not_drawn <- setdiff(flat, plot_var)
  if (length(not_drawn) > 0) {
    cli::cli_warn(c(
      "!" = paste0(
        "{.arg {arg}}: ",
        paste0(not_drawn, collapse = ", "),
        ifelse(length(not_drawn) == 1, " is", " are"),
        " not shown in the diagram and will be ignored."
      ),
      "i" = "Hidden by {.arg vars}, {.arg show_constants}, or {.arg show_aux}."
    ))
    x <- lapply(x, function(g) g[g %in% plot_var])
  }

  # Keep only groups with enough drawn members to matter
  x[vapply(x, function(g) length(g) >= min_len, logical(1))]
}


#' Generate or validate colors for variables
#'
#' Centralize color generation from palettes or validation of custom color vectors.
#'
#' @param n_vars Integer, number of variables needing colors.
#' @param colors Character vector of custom colors, or NULL.
#' @param palette Character, palette name (from hcl.pals()).
#'
#' @returns Character vector of colors (length = n_vars).
#' @noRd
#'
generate_colors <- function(n_vars, colors = NULL, palette = "Dark 2") {
  if (!is.null(colors)) {
    if (length(colors) < n_vars) {
      cli::cli_abort(c(
        "x" = "Insufficient colors provided.",
        "i" = "The {.arg colors} vector has length {.val {length(colors)}}, but {.val {n_vars}} variables need colors.",
        ">" = "Provide at least {.val {n_vars}} colors or use {.arg palette} instead."
      ))
    }
    # Normalize provided colors to canonical hex (#RRGGBB)
    norm <- vapply(colors[seq_len(n_vars)], function(col) {
      # Try col2rgb for names/hex/rgb; fall back to original string
      rgb_val <- tryCatch(grDevices::col2rgb(col), error = function(e) NULL)
      if (!is.null(rgb_val)) {
        grDevices::rgb(rgb_val[1, 1], rgb_val[2, 1], rgb_val[3, 1], maxColorValue = 255)
      } else {
        toupper(as.character(col))
      }
    }, character(1))
    return(norm)
  }

  # Ensure minimum of 3 colors for palette generation
  n_colors <- max(n_vars, 3)
  generated <- grDevices::hcl.colors(n = n_colors, palette = palette)

  # hcl.colors returns hex strings; normalize to uppercase #RRGGBB
  generated <- toupper(substr(generated, 1, 7))

  generated[seq_len(n_vars)]
}


#' Generate or validate line widths for variables
#'
#' Centralize line-width handling: recycle a single value across variables or
#' validate a custom vector, mirroring \code{\link{generate_colors}}.
#'
#' @param n_vars Integer, number of variables needing line widths.
#' @param line_width Numeric, either a single value applied to all variables or a
#'   vector with one value per variable, or NULL to use \code{default}.
#' @param default Numeric, the line width used when \code{line_width} is NULL.
#'   Defaults to 2 (plotly's default).
#' @param arg Character, the argument name used in error messages. Defaults to
#'   "line_width".
#'
#' @returns Numeric vector of line widths (length = n_vars).
#' @noRd
#'
generate_line_width <- function(n_vars, line_width = NULL, default = 2,
                                arg = "line_width") {
  if (is.null(line_width)) {
    return(rep(default, n_vars))
  }

  if (!is.numeric(line_width)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg {arg}} argument.",
      "i" = "The {.arg {arg}} argument must be {.cls numeric}."
    ))
  }

  if (any(line_width <= 0)) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg {arg}} argument.",
      "i" = "All {.arg {arg}} values must be positive."
    ))
  }

  # A single value is recycled across all variables.
  if (length(line_width) == 1) {
    return(rep(unname(line_width), n_vars))
  }

  if (length(line_width) < n_vars) {
    cli::cli_abort(c(
      "x" = "Insufficient line widths provided.",
      "i" = "The {.arg {arg}} vector has length {.val {length(line_width)}}, but {.val {n_vars}} variables need line widths.",
      ">" = "Provide a single value, {.val {n_vars}} values, or omit {.arg {arg}}."
    ))
  }

  unname(line_width[seq_len(n_vars)])
}


#' Filter variables in simulation data and metadata
#'
#' Keep only specified variables in both the names_df and simulation data frame.
#' Validates that all requested variables exist.
#'
#' @param vars Character vector of variable names to keep.
#' @param names_df Data frame with "name" and "type" columns.
#' @param df Data frame with "variable" column (simulation data).
#'
#' @returns List with elements $names_df and $df, filtered to only include vars.
#' @noRd
#'
filter_variables <- function(vars, names_df, df) {
  # Check whether specified variables are in the model
  validate_vars_in_model(vars, names_df, NULL, context = "model")

  vars_in_df <- vars[vars %in% df[["variable"]]]
  vars_missing_df <- setdiff(vars, vars_in_df)

  if (length(vars_in_df) == 0) {
    # None of the requested variables were saved: abort with guidance
    vars_not_saved(vars_missing_df, names_df, arg = "vars", action = "abort")
  }

  if (length(vars_missing_df) > 0) {
    # Some were saved: warn about the rest and continue with what is available
    vars_not_saved(vars_missing_df, names_df, arg = "vars", action = "warn")
  }

  # Filter both dataframes to include only specified variables
  names_df <- names_df[match(vars_in_df, names_df[["name"]]), , drop = FALSE]
  df <- df[df[["variable"]] %in% vars_in_df, , drop = FALSE]

  list(names_df = names_df, df = df)
}


#' Prepare constants for plotting
#'
#' Add constant values to the simulation/ensemble data frame, formatting them
#' as long-format rows for each time point.
#'
#' @param df Data frame with "variable" and "time" columns (simulation data).
#' @param constants Named list (for sim) or data frame (for ensemble) of constants.
#' @param names_df Data frame with variable metadata.
#' @param type_sim Character, either "sim", "ensemble", or "verify".
#'
#' @returns List with elements $df (updated data frame) and $names_df (updated metadata,
#'   with non-function constants removed).
#' @noRd
#'
prep_constants <- function(df, constants, names_df, type_sim = "sim") {
  # Find time vector from first variable
  times <- df[df[["variable"]] == df[["variable"]][1], "time"]

  if (type_sim == "sim") {
    # Ensure functions are not added
    idx_func <- vapply(constants, is.function, logical(1), USE.NAMES = FALSE)
    constants <- constants[!idx_func]

    # Remove functions from names_df
    names_df <- names_df[!names_df[["name"]] %in% names(idx_func[idx_func]), ,
      drop = FALSE
    ]

    # Duplicate long format for each constant
    if (length(constants) > 0) {
      temp <- lapply(names(constants), function(y) {
        data.frame(
          time = times,
          variable = y,
          value = constants[[y]]
        )
      }) |>
        do.call(rbind, args = _) |>
        as.data.frame()
      df <- bind_rows_(df, temp)
      rm(temp)
    }
  } else if (type_sim %in% c("ensemble", "verify")) {
    # Constants is a data frame
    if (nrow(constants) > 0) {
      # df <- bind_rows_(df, constants)
      constant_names <- constants[["variable"]]
      n_times <- length(times)

      constants_repeated <- constants[rep(seq_len(nrow(constants)), times = n_times), ]
      row.names(constants_repeated) <- NULL
      rep_times <- rep(times, each = length(constant_names))

      df <- bind_rows_(df, cbind(data.frame(time = rep_times), constants_repeated))
    }
  }

  list(df = df, names_df = names_df)
}


#' Determine which variables should be highlighted in plots
#'
#' Identify "highlight" variables (default: stocks) vs "nonhighlight" variables
#' (flows, auxiliaries). Used to determine initial visibility in interactive plots.
#'
#' @param names_df Data frame with "name" and "type" columns.
#' @param highlight_strategy Character. One of:
#'   - "auto": Highlight stocks (default behavior)
#'   - "all": Highlight all variables
#'   - "none": Highlight no variables
#'   - A character vector of variable names to highlight explicitly
#'
#' @returns Character vector of names to highlight.
#' @noRd
#'
determine_highlight_vars <- function(names_df, highlight_strategy = "auto") {
  if (is.character(highlight_strategy) && length(highlight_strategy) > 1) {
    # Custom variable list provided
    return(highlight_strategy)
  }

  highlight_strategy <- tolower(highlight_strategy)
  if (highlight_strategy == "auto") {
    return(names_df[names_df[["type"]] == "stock", "name"])
  } else if (highlight_strategy == "all") {
    return(names_df[["name"]])
  } else if (highlight_strategy == "none") {
    return(character(0))
  } else {
    cli::cli_abort(c(
      "Invalid {.arg highlight_strategy}.",
      "x" = "Must be 'auto', 'all', 'none', or a character vector of variable names."
    ))
  }
}


#' Add a nonhighlight/highlight trace pair differing only in data and visibility
#'
#' The ensemble plot draws each layer (confidence ribbons, central-tendency lines,
#' error-bar markers, raw simulations) twice: once for nonhighlight variables
#' (\code{visible = "legendonly"}) and once for highlight variables
#' (\code{visible = TRUE}). The two calls are otherwise identical. This helper runs
#' both guarded calls in that order, forwarding the shared trace arguments via
#' \code{...}. Formulas in \code{...} (e.g. \code{x = ~time}) are evaluated by
#' plotly against \code{data} in the caller's environment, as before.
#'
#' @param pl Plotly object.
#' @param add_fn Trace-adding function, e.g. \code{plotly::add_trace} or
#'   \code{plotly::add_ribbons}.
#' @param data_nonhighlight,data_highlight Data frames for each trace.
#' @param plot_nonhighlight,plot_highlight Logical guards; the trace is skipped when
#'   \code{FALSE}.
#' @param ... Shared arguments forwarded to \code{add_fn}.
#'
#' @returns Updated plotly object.
#' @noRd
#'
add_visibility_pair <- function(pl, add_fn,
                                data_nonhighlight, data_highlight,
                                plot_nonhighlight, plot_highlight, ...) {
  if (plot_nonhighlight) {
    pl <- add_fn(pl, data = data_nonhighlight, ..., visible = "legendonly")
  }
  if (plot_highlight) {
    pl <- add_fn(pl, data = data_highlight, ..., visible = TRUE)
  }
  pl
}


#' Add trace pair to plotly object
#'
#' Consolidate the pattern of adding highlight and nonhighlight traces to a plotly
#' plot. Both traces use the same aesthetic mappings but with different visibility.
#'
#' @param pl Plotly object to add traces to.
#' @param df_highlight Data frame with highlight variables.
#' @param df_nonhighlight Data frame with nonhighlight variables.
#' @param colors Character vector of colors for variables.
#' @param x_col Character, name of x-axis column (e.g., "time").
#' @param showlegend Logical, whether to show legend.
#' @param mode Character, "lines", "markers", or "lines+markers".
#' @param type Character, trace type (default "scatter").
#' @param opacity Numeric, opacity/transparency (0-1).
#' @param line_width Numeric line width for "lines" mode. Either a single value
#'   applied to all variables, or a named vector (keyed by variable label) giving
#'   one width per variable. \code{NULL} uses plotly's default.
#' @param marker_size Numeric, marker size for "markers" mode.
#' @param split Optional formula for splitting traces (e.g., \code{~interaction(variable, i)}).
#' @param frame Optional formula (e.g. \code{~.frame}) mapping traces to animation
#'   frames. When \code{NULL} (default), no animation frame is added.
#'
#' @returns Updated plotly object.
#' @noRd
#'
add_trace_pair <- function(pl,
                           df_highlight = NULL,
                           df_nonhighlight = NULL,
                           colors = NULL,
                           x_col = "time",
                           y_col = "value",
                           showlegend = TRUE,
                           mode = "lines",
                           type = "scatter",
                           opacity = 1,
                           line_width = NULL,
                           marker_size = NULL,
                           split = NULL,
                           frame = NULL,
                           visible_highlight = TRUE,
                           visible_nonhighlight = "legendonly") {
  # Build an explicit variable->color mapping for variables actually present in
  # this trace pair. This avoids Plotly domain warnings when factor levels or
  # wrapped labels differ across traces.
  vars_nonhighlight <- if (!is.null(df_nonhighlight) && nrow(df_nonhighlight) > 0) {
    as.character(df_nonhighlight[["variable"]])
  } else {
    character(0)
  }
  vars_highlight <- if (!is.null(df_highlight) && nrow(df_highlight) > 0) {
    as.character(df_highlight[["variable"]])
  } else {
    character(0)
  }

  vars_present <- unique(c(vars_nonhighlight, vars_highlight))
  vars_present <- vars_present[!is.na(vars_present) & nzchar(vars_present)]

  if (!is.null(colors) && length(vars_present) > 0) {
    if (length(colors) < length(vars_present)) {
      cli::cli_abort(c(
        "x" = "Insufficient colors provided for traces.",
        "i" = "Need {.val {length(vars_present)}} colors for plotted variables but got {.val {length(colors)}}."
      ))
    }
    if (isTRUE(attr(colors, "sdbuildR_preserve_names")) && all(vars_present %in% names(colors))) {
      colors <- colors[vars_present]
    } else {
      colors <- unname(colors[seq_along(vars_present)])
      names(colors) <- vars_present
    }
  }

  # Resolve per-variable line widths for the variables present in this trace pair.
  # line_width may be a single value (applied to all variables), a named vector
  # keyed by variable label, or NULL (use plotly's default width).
  lw <- NULL
  if (!is.null(line_width) && length(vars_present) > 0) {
    if (length(line_width) == 1 && is.null(names(line_width))) {
      lw <- stats::setNames(rep(unname(line_width), length(vars_present)), vars_present)
    } else {
      lw <- line_width[vars_present]
    }
  }
  # When all widths are identical, a single color-mapped trace suffices; otherwise
  # each variable needs its own trace so it can carry its own width.
  uniform_width <- is.null(lw) || length(unique(unname(lw))) == 1L

  # Build the trace(s) for one data frame (highlight or nonhighlight), injecting
  # split/frame only when supplied so behaviour is identical to before when
  # neither is requested.
  add_one <- function(pl, data, showlegend_val, visible_val) {
    if (uniform_width) {
      args <- list(
        pl,
        data = data,
        x = ~ get(x_col),
        y = ~ get(y_col),
        color = ~variable,
        legendgroup = ~variable,
        type = type,
        mode = mode,
        opacity = opacity,
        colors = colors,
        showlegend = showlegend_val,
        visible = visible_val
      )
      if (!is.null(lw)) args[["line"]] <- list(width = unname(lw)[1])
      if (!is.null(split)) args[["split"]] <- split
      if (!is.null(frame)) args[["frame"]] <- frame
      return(do.call(plotly::add_trace, args))
    }

    # Per-variable widths: add one trace per variable, setting line colour and
    # width explicitly (plotly cannot map width across colour levels).
    for (v in levels(droplevels(data[["variable"]]))) {
      dv <- data[data[["variable"]] == v, , drop = FALSE]
      if (nrow(dv) == 0) next
      args <- list(
        pl,
        data = dv,
        x = ~ get(x_col),
        y = ~ get(y_col),
        name = v,
        legendgroup = v,
        type = type,
        mode = mode,
        opacity = opacity,
        line = list(width = unname(lw[v]), color = unname(colors[v])),
        showlegend = showlegend_val,
        visible = visible_val
      )
      if (!is.null(split)) args[["split"]] <- split
      if (!is.null(frame)) args[["frame"]] <- frame
      pl <- do.call(plotly::add_trace, args)
    }
    pl
  }

  # Add nonhighlight traces first (will be hidden behind highlight traces)
  if (!is.null(df_nonhighlight) && nrow(df_nonhighlight) > 0) {
    # When splitting, individual trajectories must not each add a legend entry.
    pl <- add_one(pl, df_nonhighlight,
      showlegend_val = if (is.null(split)) showlegend else FALSE,
      visible_val = visible_nonhighlight
    )
  }

  # Add highlight traces (will be visible by default)
  if (!is.null(df_highlight) && nrow(df_highlight) > 0) {
    pl <- add_one(pl, df_highlight,
      showlegend_val = if (is.null(split)) showlegend else FALSE,
      visible_val = visible_highlight
    )
  }

  pl
}


#' Set the export format for Plotly's "Download plot as a png/svg/jpeg/webp" button
#'
#' @param pl Plotly object to configure.
#' @param format Character, one of "png", "svg", "jpeg", or "webp". Defaults to "svg" for better quality and scalability.
#' @returns Updated Plotly object with configured export format.
#' @noRd
set_plotly_export_format <- function(pl, format = "svg") {
  # Check format
  format <- match.arg(format, choices = c("png", "svg", "jpeg", "webp"))

  plotly::config(pl,
    toImageButtonOptions = list(
      format = format
      # Use currently-rendered size by not specifying width/height
      # width = width,
      # height = height
    )
  )
}


#' Validate the animation argument
#'
#' @param animation Character, one of "none" or "time".
#' @returns The matched value.
#' @noRd
.clean_animation <- function(animation) {
  choices <- c("none", "time")
  if (length(animation) > 1) animation <- animation[1]
  if (!is.character(animation) || length(animation) != 1 || !animation %in% choices) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg animation} value.",
      "i" = "The {.arg animation} argument must be {.code 'none'} or {.code 'time'}."
    ))
  }
  animation
}


#' Validate the condition_display argument
#'
#' @param condition_display Character, one of "subplots", "slider", or "dropdown".
#' @returns The matched value.
#' @noRd
.clean_condition_display <- function(condition_display) {
  choices <- c("subplots", "slider", "dropdown")
  if (length(condition_display) > 1) condition_display <- condition_display[1]
  if (!is.character(condition_display) || length(condition_display) != 1 ||
    !condition_display %in% choices) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg condition_display} value.",
      "i" = "The {.arg condition_display} argument must be {.code 'subplots'}, {.code 'slider'}, or {.code 'dropdown'}."
    ))
  }
  condition_display
}


#' Accumulate rows cumulatively by time for a line-drawing animation
#'
#' Duplicates the data so that the frame for time `t` contains every row with
#' `time <= t`. This produces the "line drawing itself" effect when the frame
#' column is mapped to a Plotly animation frame. All other columns (e.g.
#' `variable`, `condition`, `sim`) are preserved, including factor levels.
#'
#' @param df Data frame with a time column, or NULL.
#' @param time_col Name of the time column. Defaults to "time".
#' @param frame_col Name of the frame column to create. Defaults to ".frame".
#' @param max_frames Maximum number of animation frames. When the data has more
#'   unique time points than this, evenly spaced thresholds are used (always
#'   keeping the last time so the final frame is complete). This keeps the
#'   animation responsive: a naive one-frame-per-time-point expansion is
#'   quadratic in the number of time points and can freeze plotly for finely
#'   sampled simulations. Defaults to 50.
#' @returns Data frame with a `frame_col` column, or `df` unchanged if empty/NULL.
#' @noRd
accumulate_by_time <- function(df, time_col = "time", frame_col = ".frame",
                               max_frames = 50) {
  if (is.null(df) || nrow(df) == 0L) {
    return(df)
  }

  times <- sort(unique(df[[time_col]]))

  # Cap the number of frames for performance. Use "nice" rounded breakpoints
  # (snapped to the nearest available time) so the slider tick labels read like
  # ordinary x-axis ticks (e.g. 0, 5, 10, ...) instead of raw sample times. The
  # line itself still draws at full resolution within each frame, since each
  # frame includes every row up to its threshold.
  if (length(times) > max_frames) {
    rng <- range(times)
    nice <- pretty(rng, n = min(max_frames, 20))
    nice <- nice[nice > rng[1] & nice < rng[2]]
    snapped <- vapply(nice, function(v) times[which.min(abs(times - v))], numeric(1))
    times <- sort(unique(c(rng[1], snapped, rng[2])))
  }

  out <- lapply(times, function(time_value) {
    d <- df[df[[time_col]] <= time_value, , drop = FALSE]
    d[[frame_col]] <- time_value
    d
  })

  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}


#' Add Plotly animation controls (play button and time slider)
#'
#' @param pl Plotly object with animation frames.
#' @param time_unit Time unit (e.g. "weeks"), used to build the slider's
#'   dynamic title, formatted like the x-axis title (e.g. "Time (20 weeks)").
#' @param font_family,font_size Font of the slider title, matched to the x-axis title.
#' @param frame_ms Frame duration in milliseconds. Defaults to 100.
#' @param transition_ms Transition duration in milliseconds. Defaults to 0.
#' @param show_slider Whether to add an animation slider. Defaults to TRUE.
#' @param show_button Whether to add a play button. Defaults to TRUE.
#' @returns Updated plotly object.
#' @noRd
add_time_animation_controls <- function(pl,
                                        time_unit = "",
                                        font_family = "Times New Roman",
                                        font_size = 16,
                                        frame_ms = 100,
                                        transition_ms = 0,
                                        show_slider = TRUE,
                                        show_button = TRUE) {
  pl <- plotly::animation_opts(
    pl,
    frame = frame_ms,
    transition = transition_ms,
    redraw = FALSE
  )

  # The slider replaces the x-axis, so drop the axis title and ticks. The slider
  # step labels (the frame times) then act as the x-axis tick labels. Margin is
  # merged recursively, so existing top/left/right margins are preserved.
  pl <- plotly::layout(pl,
    xaxis = list(title = "", showticklabels = FALSE, ticks = ""),
    margin = list(b = 120)
  )

  # Dynamic slider title in the x-axis-title style, e.g. "Time (20 weeks)".
  if (nzchar(time_unit)) {
    cv_prefix <- "Time ("
    cv_suffix <- paste0(" ", time_unit, ")")
  } else {
    cv_prefix <- "Time "
    cv_suffix <- ""
  }

  # Place the controls below where the x-axis title used to sit (paper y < 0 is
  # the bottom margin).
  control_y <- -0.18

  if (show_slider) {
    pl <- plotly::animation_slider(
      pl,
      currentvalue = list(
        prefix = cv_prefix,
        suffix = cv_suffix,
        xanchor = "center",
        font = list(family = font_family, size = font_size)
      ),
      x = 0,
      xanchor = "left",
      len = 1,
      y = control_y,
      yanchor = "top",
      pad = list(t = 0, b = 0),
      font = list(family = font_family, size = ceiling(font_size * 0.85))
    )
  }

  if (show_button) {
    pl <- plotly::animation_button(
      pl,
      x = 1,
      xanchor = "right",
      y = control_y,
      yanchor = "top"
    )
  }

  pl
}


#' Drop the bookkeeping `condition` index column from a conditions table
#'
#' `ensemble()` prepends a `condition` index column to `x$conditions`
#' (see `R/ensemble_r.R` / `R/ensemble_julia.R`). Only the actual parameter
#' columns should drive sliders/dropdowns.
#'
#' @param conditions The `conditions` element of an ensemble object (a data
#'   frame, or `NULL`).
#' @returns A data frame of parameter columns only (zero columns if none).
#' @noRd
condition_param_table <- function(conditions) {
  if (is.null(conditions)) {
    return(data.frame())
  }
  cond <- as.data.frame(conditions)
  cond[, setdiff(names(cond), "condition"), drop = FALSE]
}


#' Build a name -> human-readable label lookup from a stockflow model
#'
#' @param object A stockflow model (or `NULL`).
#' @returns A function mapping a character vector of variable names to their
#'   model labels, falling back to the name when no label is found.
#' @noRd
model_label_lookup <- function(object) {
  ld <- NULL
  if (!is.null(object)) {
    ld <- tryCatch(
      as.data.frame(object, properties = c("name", "label")),
      error = function(e) NULL
    )
  }
  if (is.null(ld) || !all(c("name", "label") %in% names(ld))) {
    return(function(nm) nm)
  }
  function(nm) {
    out <- ld[["label"]][match(nm, ld[["name"]])]
    ifelse(is.na(out), nm, out)
  }
}


#' Rich per-condition labels for ensemble condition controls
#'
#' Produces labels that surface the parameter values of each condition, e.g.
#' `"Contact rate = 1"` (single parameter) or
#' `"Condition 3 (Contact rate = 1, Recovery rate = 0.05)"` (multiple).
#'
#' @param param_tbl Parameter table from condition_param_table().
#' @param object Stockflow model, used to map parameter names to labels.
#' @param condition_ids Condition indices (rows of `param_tbl`) in display order.
#' @returns Character vector of labels, one per `condition_ids`.
#' @noRd
ensemble_condition_labels <- function(param_tbl, object, condition_ids) {
  if (is.null(param_tbl) || ncol(param_tbl) == 0L) {
    return(paste0("Condition ", condition_ids))
  }
  labof <- model_label_lookup(object)
  cvars <- names(param_tbl)
  plabs <- labof(cvars)
  vapply(condition_ids, function(i) {
    vals <- vapply(cvars, function(cn) {
      formatC(param_tbl[i, cn], format = "g")
    }, character(1))
    parts <- paste0(plabs, " = ", vals)
    if (length(cvars) == 1L) {
      paste(parts)
    } else {
      paste0("Condition ", i, " (", paste(parts, collapse = ", "), ")")
    }
  }, character(1))
}


#' Capture per-condition trace `y` arrays for client-side swapping
#'
#' Builds each condition's plotly object and extracts, per trace, the `y` array.
#' Because every condition shares the same variables and time grid, the trace
#' structure and `x` arrays are identical across conditions, so only `y` (which
#' also encodes ribbon polygons and NA-broken trajectories) needs swapping.
#'
#' @param pl_list List of single-condition plotly objects.
#' @returns A list over conditions, each a list of `y` arrays (one per trace).
#' @noRd
capture_swapdata <- function(pl_list) {
  built <- lapply(pl_list, function(p) plotly::plotly_build(p)[["x"]][["data"]])
  n_tr <- length(built[[1]])
  ylen1 <- vapply(built[[1]], function(tr) length(tr[["y"]]), integer(1))

  for (c in seq_along(built)) {
    if (length(built[[c]]) != n_tr) {
      cli::cli_abort(c(
        "x" = "Conditions produced different numbers of traces ({length(built[[c]])} vs {n_tr}).",
        "i" = "Interactive condition controls require the same variables in every condition."
      ))
    }
    ylc <- vapply(built[[c]], function(tr) length(tr[["y"]]), integer(1))
    if (!identical(ylc, ylen1)) {
      cli::cli_abort(c(
        "x" = "Conditions produced series of different lengths.",
        "i" = "Interactive condition controls assume a shared time grid across conditions."
      ))
    }
  }

  lapply(built, function(traces) lapply(traces, function(tr) tr[["y"]]))
}


#' Resolve and validate the `control_options` list for condition controls
#'
#' Users pass a named list to fine-tune the slider/dropdown without bloating the
#' `plot()` signature (cf. the `control` lists of [stats::optim()] /
#' [stats::loess()]). Unknown names are rejected so typos surface early.
#'
#' Supported options:
#' * `max_labels` -- maximum number of slider tick labels to keep visible when
#'   many conditions are varied (a positive integer; the slider always keeps one
#'   step per condition). Defaults to 10.
#'
#' @param control_options A named list, or `NULL`/empty for defaults.
#' @returns A list with all supported options filled in.
#' @noRd
resolve_control_options <- function(control_options = list()) {
  defaults <- list(max_labels = 10L)

  if (is.null(control_options)) {
    return(defaults)
  }
  if (!is.list(control_options)) {
    cli::cli_abort(c(
      "x" = "{.arg control_options} must be a named list.",
      "i" = "For example, {.code control_options = list(max_labels = 8)}."
    ))
  }

  unknown <- setdiff(names(control_options), names(defaults))
  if (length(control_options) > 0 &&
    (is.null(names(control_options)) || length(unknown) > 0)) {
    bad <- if (is.null(names(control_options))) "<unnamed>" else unknown
    cli::cli_abort(c(
      "x" = "Unknown {.arg control_options}: {.val {bad}}.",
      "i" = "Supported options: {.val {names(defaults)}}."
    ))
  }

  out <- utils::modifyList(defaults, control_options)

  ml <- out[["max_labels"]]
  if (!is.numeric(ml) || length(ml) != 1L || is.na(ml) || ml < 1) {
    cli::cli_abort(c(
      "x" = "{.arg control_options$max_labels} must be a single positive number.",
      "i" = "You supplied {.val {ml}}."
    ))
  }
  out[["max_labels"]] <- as.integer(ml)

  out
}


#' Thin slider tick labels to at most `max_labels`, keeping the endpoints
#'
#' A slider keeps one step (jump) per parameter value, but printing every tick
#' label overprints when many values are varied. This blanks intermediate
#' labels so at most `max_labels` evenly spaced ticks (always including the
#' first and last) are labelled, while the number of steps is unchanged.
#'
#' @param labels Character vector of tick labels, one per step.
#' @param max_labels Maximum number of labels to keep visible.
#' @returns `labels` with intermediate entries blanked when there are more than
#'   `max_labels` of them.
#' @noRd
thin_slider_labels <- function(labels, max_labels = 10L) {
  n <- length(labels)
  if (n <= max_labels) {
    return(labels)
  }
  keep <- unique(round(seq(1, n, length.out = max_labels)))
  out <- rep("", n)
  out[keep] <- labels[keep]
  out
}


#' Build per-parameter controls (sliders or dropdowns) for a crossed ensemble
#'
#' One control per condition variable. The browser-side handler reads each
#' control's active value and finds the matching condition (cross-product).
#'
#' @param param_tbl Full parameter table from condition_param_table().
#' @param condition_ids Condition indices built into `pl_list`/`ydata`, in order.
#' @param type Either "slider" or "dropdown".
#' @param object Stockflow model, for parameter labels.
#' @param max_labels Maximum number of slider tick labels to keep visible.
#' @returns List with `layout` (named list for [plotly::layout()]), `condVals`
#'   (per built condition, the parameter values), and `levels` (per parameter,
#'   its sorted unique values) for the JS handler.
#' @noRd
build_param_controls <- function(param_tbl, condition_ids, type, object,
                                 max_labels = 10L) {
  labof <- model_label_lookup(object)
  cvars <- names(param_tbl)
  plabs <- labof(cvars)
  steps_per <- lapply(param_tbl, function(x) sort(unique(x)))

  controls <- lapply(seq_along(cvars), function(j) {
    vals <- steps_per[[cvars[j]]]
    nm <- as.character(j - 1L) # 0-based index tag read by the JS handler
    if (type == "slider") {
      tick_labels <- thin_slider_labels(formatC(vals, format = "g"), max_labels)
      list(
        active = 0, name = nm,
        x = 0, len = 0.9, y = -0.18 - 0.22 * (j - 1),
        pad = list(t = 10, b = 10),
        currentvalue = list(prefix = paste0(plabs[j], " = ")),
        steps = lapply(seq_along(vals), function(k) {
          list(label = tick_labels[k], method = "skip")
        })
      )
    } else {
      list(
        type = "dropdown", active = 0, showactive = TRUE, name = nm,
        direction = "up", x = 0, xanchor = "left",
        y = -0.1 - 0.16 * (j - 1), yanchor = "top",
        buttons = lapply(vals, function(v) {
          list(
            label = paste0(plabs[j], " = ", formatC(v, format = "g")),
            method = "skip"
          )
        })
      )
    }
  })

  layout <- if (type == "slider") {
    list(sliders = controls)
  } else {
    list(updatemenus = controls)
  }

  # Values aligned with the built conditions (= ydata order).
  condVals <- lapply(condition_ids, function(i) {
    as.list(as.numeric(param_tbl[i, cvars, drop = TRUE]))
  })
  levels <- lapply(cvars, function(cn) as.list(as.numeric(steps_per[[cn]])))

  list(layout = layout, condVals = condVals, levels = levels)
}


#' JavaScript handler that swaps trace `y` arrays on control change
#'
#' Used only for the per-parameter (crossed) layout, where independent controls
#' must be combined into a single condition via a cross-product lookup. Sliders
#' emit `plotly_sliderchange`; dropdowns update via `plotly_relayout`.
#'
#' @returns A length-one character vector with the handler source.
#' @noRd
swap_onrender_js <- function() {
  "
  function(el, x, data) {
    var gd = el;
    var K = data.levels.length;
    var state = new Array(K).fill(0);
    var idx = []; for (var t = 0; t < data.nTraces; t++) idx.push(t);

    function apply() {
      var chosen = state.map(function(a, j){ return data.levels[j][a]; });
      var ci = -1;
      for (var c = 0; c < data.condVals.length; c++) {
        var ok = true;
        for (var j = 0; j < K; j++) {
          if (Math.abs(data.condVals[c][j] - chosen[j]) > 1e-9) { ok = false; break; }
        }
        if (ok) { ci = c; break; }
      }
      if (ci < 0 || !data.ydata[ci]) return; // combination not simulated
      Plotly.restyle(gd, {y: data.ydata[ci]}, idx);
    }

    function readActives(arr) {
      if (!arr) return;
      for (var i = 0; i < arr.length; i++) {
        var nm = arr[i].name;
        state[nm != null ? +nm : i] = arr[i].active || 0;
      }
    }

    gd.on('plotly_sliderchange', function(e) {
      if (e && e.slider && e.slider.name != null && e.slider.active != null) {
        state[+e.slider.name] = e.slider.active;
      } else {
        readActives(gd._fullLayout && gd._fullLayout.sliders);
      }
      apply();
    });

    gd.on('plotly_relayout', function() {
      if (!(gd._fullLayout && gd._fullLayout.updatemenus &&
            gd._fullLayout.updatemenus.length)) return;
      readActives(gd._fullLayout.updatemenus);
      apply();
    });
  }
  "
}


#' Assemble a single plot with condition selection controls
#'
#' Builds each condition's traces (via `pl_list`, one plotly object per
#' condition), keeps only the first condition's traces in the live figure, and
#' swaps their `y` arrays client-side as the user changes the control(s). For a
#' crossed ensemble (`cross = TRUE`, >= 2 parameters) one control is shown per
#' parameter; otherwise a single control steps through the conditions.
#'
#' @param pl_list List of single-condition plotly objects (one per condition).
#' @param condition_ids Vector of condition values, aligned with `pl_list`.
#' @param type Either "slider" or "dropdown".
#' @param labels Character vector of condition labels for the single-control
#'   layout.
#' @param theme Theme list from `plotly_theme()`.
#' @param main,xlab,ylab Title and axis labels.
#' @param font_family,font_size Font settings.
#' @param condition_table Optional parameter table (from
#'   condition_param_table()) enabling per-parameter controls.
#' @param cross Whether the ensemble conditions were crossed.
#' @param object Optional stockflow model, for parameter labels.
#' @param max_labels Maximum number of slider tick labels to keep visible.
#' @returns A combined plotly object with condition controls.
#' @noRd
assemble_condition_control_plot <- function(pl_list, condition_ids, type,
                                            labels, theme,
                                            main, xlab, ylab,
                                            font_family, font_size,
                                            condition_table = NULL,
                                            cross = FALSE,
                                            object = NULL,
                                            max_labels = 10L) {
  ydata <- capture_swapdata(pl_list)
  n_traces <- length(ydata[[1]])

  use_param <- isTRUE(cross) && !is.null(condition_table) &&
    ncol(condition_table) >= 2L
  n_controls <- if (use_param) ncol(condition_table) else 1L

  # Live figure: the first condition's traces, with shared title/axes/theme.
  combined <- plotly::layout(pl_list[[1]],
    title = list(text = main),
    xaxis = list(title = xlab),
    yaxis = list(title = ylab),
    font = list(family = font_family, size = font_size),
    margin = utils::modifyList(theme[["margin"]],
      list(b = max(theme[["margin"]][["b"]], 40 + 60 * n_controls))),
    legend = theme[["legend"]]
  )

  if (use_param) {
    ctrl <- build_param_controls(
      condition_table, condition_ids, type, object, max_labels
    )
    combined <- do.call(plotly::layout, c(list(combined), ctrl[["layout"]]))
    combined <- htmlwidgets::onRender(combined, swap_onrender_js(),
      data = list(
        nTraces = n_traces, ydata = ydata,
        condVals = ctrl[["condVals"]], levels = ctrl[["levels"]]
      )
    )
    return(combined)
  }

  # Single control: each step/button natively restyles to its condition's y
  # arrays (no JS needed). For a slider over a single parameter, the parameter
  # label lives in the slider title (currentvalue prefix) and the tick labels
  # carry only the values; otherwise the full condition labels are used.
  trace_idx <- seq_len(n_traces) - 1L

  slider_prefix <- ""
  step_labels <- as.character(unlist(labels))
  single_param <- !is.null(condition_table) && ncol(condition_table) == 1L
  if (type == "slider" && single_param) {
    cn <- names(condition_table)[1]
    slider_prefix <- paste0(model_label_lookup(object)(cn), " = ")
    step_labels <- formatC(condition_table[condition_ids, cn], format = "g")
  }
  if (type == "slider") {
    step_labels <- thin_slider_labels(step_labels, max_labels)
  }

  steps <- lapply(seq_along(condition_ids), function(k) {
    list(
      method = "restyle",
      args = list(list(y = ydata[[k]]), as.list(trace_idx)),
      label = step_labels[[k]]
    )
  })

  if (type == "slider") {
    plotly::layout(combined,
      sliders = list(list(
        active = 0, x = 0, len = 0.9, y = -0.18,
        pad = list(t = 10, b = 10),
        currentvalue = list(prefix = slider_prefix),
        steps = steps
      ))
    )
  } else {
    plotly::layout(combined,
      updatemenus = list(list(
        type = "dropdown", active = 0, showactive = TRUE,
        direction = "up", x = 0, xanchor = "left",
        y = -0.1, yanchor = "top",
        buttons = steps
      ))
    )
  }
}


#' Build informative condition labels for verify plots
#'
#' Uses the `test` and `conditions` columns from
#' `as.data.frame.verify_stockflow(which = "sims")` to make labels such as
#' "Test 1" or "Test 2: rate = 0", falling back to "Condition j" when no test
#' or condition information is available.
#'
#' @param df Verify simulation data frame with `test`, `condition`, and
#'   optionally `conditions` columns.
#' @param condition_ids Vector of condition values to label.
#' @returns Character vector of labels, one per `condition_ids`.
#' @noRd
make_verify_condition_labels <- function(df, condition_ids) {
  vapply(condition_ids, function(j) {
    rows <- df[df[["condition"]] == j, , drop = FALSE]
    if (nrow(rows) == 0L) {
      return(paste0("Condition ", j))
    }

    test_str <- as.character(rows[["test"]][1L])
    lab <- if (!is.na(test_str) && nzchar(test_str)) {
      paste0("Test ", test_str)
    } else {
      paste0("Condition ", j)
    }

    if ("conditions" %in% colnames(rows)) {
      cond_str <- as.character(rows[["conditions"]][1L])
      if (!is.na(cond_str) && nzchar(cond_str)) {
        lab <- paste0(lab, ": ", cond_str)
      }
    }

    lab
  }, character(1))
}
