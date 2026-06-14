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


#' Validate plot parameters
#'
#' Validate common parameters used across all plotting functions (plot.simulate_sdbuildR,
#' plot.sdbuildR, plot.ensemble_sdbuildR).
#'
#' @param showlegend Logical, whether to show legend.
#' @param vars Character vector of variable names to plot, or NULL.
#' @param palette Character, color palette name.
#' @param colors Character vector of colors, or NULL.
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
                                 alpha = NULL,
                                 font_family = NULL,
                                 font_size = NULL,
                                 wrap_width = NULL,
                                 label_subplots = NULL) {
  .assert_plot_type(showlegend, "showlegend", "logical", "Use {.code TRUE} or {.code FALSE}.")

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
#' Generate styling parameters for diagram plots (plot.sdbuildR).
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

  return(result)
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
    idx <- !(vars %in% df[["variable"]])
    if (any(idx)) {
      cli::cli_abort(
        c(
          "x" = paste0(
            paste0(vars[idx], collapse = ", "),
            ifelse(sum(idx) == 1, " is", " are"),
            " in the model, but not in the simulated data frame."
          ),
          ">" = "Run simulate() with only_stocks = FALSE."
        )
      )
    }
  }

  invisible(TRUE)
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
    cli::cli_abort(c(
      "x" = "Requested variables are not available in the simulated data.",
      ">" = "Run {.fn simulate} with {.code only_stocks = FALSE} or choose different {.arg vars}."
    ))
  }

  if (length(vars_missing_df) > 0) {
    cli::cli_warn(c(
      "!" = "Some requested variables are not available in the simulated data and will be ignored.",
      "i" = "Missing: {paste0(vars_missing_df, collapse = ', ')}"
    ))
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
#' @param line_width Numeric, line width for "lines" mode.
#' @param marker_size Numeric, marker size for "markers" mode.
#' @param split Optional formula for splitting traces (e.g., \code{~interaction(variable, i)}).
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
    colors <- unname(colors[seq_along(vars_present)])
    names(colors) <- vars_present
  }

  # Add nonhighlight traces first (will be hidden behind highlight traces)
  if (!is.null(df_nonhighlight) && nrow(df_nonhighlight) > 0) {
    if (is.null(split)) {
      pl <- plotly::add_trace(pl,
        data = df_nonhighlight,
        x = ~ get(x_col),
        y = ~ get(y_col),
        color = ~variable,
        legendgroup = ~variable,
        type = type,
        mode = mode,
        opacity = opacity,
        colors = colors,
        showlegend = showlegend,
        visible = visible_nonhighlight
      )
    } else {
      pl <- plotly::add_trace(pl,
        data = df_nonhighlight,
        x = ~ get(x_col),
        y = ~ get(y_col),
        color = ~variable,
        legendgroup = ~variable,
        type = type,
        mode = mode,
        opacity = opacity,
        colors = colors,
        split = split,
        showlegend = FALSE,
        visible = visible_nonhighlight
      )
    }

    # Add line width or marker size if specified
    if (!is.null(line_width) && mode %in% c("lines", "lines+markers")) {
      pl <- plotly::layout(pl, xaxis = list(title = "")) # Placeholder to avoid error
    }
    if (!is.null(marker_size) && mode %in% c("markers", "lines+markers")) {
      pl <- plotly::layout(pl, xaxis = list(title = "")) # Placeholder to avoid error
    }
  }

  # Add highlight traces (will be visible by default)
  if (!is.null(df_highlight) && nrow(df_highlight) > 0) {
    if (is.null(split)) {
      pl <- plotly::add_trace(pl,
        data = df_highlight,
        x = ~ get(x_col),
        y = ~ get(y_col),
        color = ~variable,
        legendgroup = ~variable,
        type = type,
        mode = mode,
        opacity = opacity,
        colors = colors,
        showlegend = showlegend,
        visible = visible_highlight
      )
    } else {
      pl <- plotly::add_trace(pl,
        data = df_highlight,
        x = ~ get(x_col),
        y = ~ get(y_col),
        color = ~variable,
        legendgroup = ~variable,
        type = type,
        mode = mode,
        opacity = opacity,
        colors = colors,
        split = split,
        showlegend = FALSE,
        visible = visible_highlight
      )
    }
  }

  return(pl)
}
