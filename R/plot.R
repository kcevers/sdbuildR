#' Save plot to a file
#'
#' Save a plot of a stock-and-flow diagram or a simulation to a specified file path. Note that saving plots requires additional packages to be installed (see below).
#'
#' @param pl Plot object. Can be a `grViz` object from the DiagrammeR package (for stock-and-flow diagrams) or a `plotly` object from the plotly package (for (ensemble) simulation results).
#' @param file File path to save plot to, including a file extension. For plotting a stock-and-flow model, the file extension can be one of png, pdf, svg, ps, eps, webp. For plotting a simulation, the file extension can be one of png, pdf, jpg, jpeg, webp. For plotting a qgraph graph, the file extension can be one of png, pdf, svg, ps, eps, jpg, jpeg, tiff, bmp. If no file extension is specified, it will default to png.
#' @param width Width of image in units.
#' @param height Height of image in units.
#' @param units Units in which width and height are specified. Either "cm", "in", or "px".
#' @param dpi Resolution of image. Only used if units is not "px".
#' @param font_family Font family used for qgraph exports. For PDF/PS/EPS exports,
#'   this is applied when the graphics device is opened.
#'
#' @returns Returns `NULL` invisibly, called for side effects.
#' @export
#' @concept convenience
#'
#' @examples
#'
#' # Only if dependencies are installed
#' if (requireNamespace("DiagrammeRsvg", quietly = TRUE) &&
#'   requireNamespace("rsvg", quietly = TRUE)) {
#'   sfm <- sdbuildR("SIR")
#'   file <- tempfile(fileext = ".png")
#'   export_plot(plot(sfm), file)
#'
#'   # Remove plot
#'   file.remove(file)
#' }
#'
#' @examplesIf has_internet()
#' \dontrun{
#' # requires internet
#' # Only if suggested dependencies are installed
#' if (requireNamespace("htmlwidgets", quietly = TRUE) &&
#'   requireNamespace("webshot2", quietly = TRUE)) {
#'   # Requires Chrome to save plotly plot:
#'   sim <- simulate(sfm)
#'   export_plot(plot(sim), file)
#'
#'   # Remove plot
#'   file.remove(file)
#' }
#' }
export_plot <- function(pl, file, width = 3, height = 4, units = "cm", dpi = 300, font_family = "") {
  # Auto-detect format
  format <- tolower(tools::file_ext(file))

  if (!nzchar(format)) {
    # Default to png
    format <- "png"
    file <- paste0(file, ".", format)
  }

  width_px <- width
  height_px <- height

  # Convert dimensions to pixels
  if (units == "in") {
    width_px <- width * dpi
    height_px <- height * dpi
  } else if (units == "cm") {
    width_px <- width * dpi / 2.54
    height_px <- height * dpi / 2.54
  }

  if ("grViz" %in% class(pl)) {
    export_diagram(pl, file, format,
      width = width_px, height = height_px
    )
  } else if ("plotly" %in% class(pl)) {
    export_plotly(pl, file,
      format = format,
      width = width_px, height = height_px
    )
  } else {
    cli::cli_abort(c(
      "x" = "Unsupported plot object class.",
      "i" = "The {.fn export_plot} function does not support plot objects of class {.cls {class(pl)}}.",
      ">" = "Use a {.cls grViz}, {.cls plotly}, or {.cls qgraph} object."
    ))
  }

  invisible(file)
}


#' Export diagram
#'
#' @inheritParams export_plot
#' @param format Output format.
#'
#' @returns Returns `NULL` invisibly.
#' @noRd
#'
export_diagram <- function(pl, file, format, width, height) {
  rlang::check_installed("rsvg", reason = "to export stock-and-flow diagrams to image files.")

  rlang::check_installed("DiagrammeRsvg", reason = "to export stock-and-flow diagrams to image files.")

  temp <- charToRaw(DiagrammeRsvg::export_svg(pl))

  if (format == "webp") {
    rsvg::rsvg_webp(temp, file, width = width, height = height)
  } else if (format == "png") {
    rsvg::rsvg_png(temp, file, width = width, height = height)
  } else if (format == "pdf") {
    rsvg::rsvg_pdf(temp, file, width = width, height = height)
  } else if (format == "svg") {
    rsvg::rsvg_svg(temp, file, width = width, height = height)
  } else if (format == "ps") {
    rsvg::rsvg_ps(temp, file, width = width, height = height)
  } else if (format == "eps") {
    rsvg::rsvg_eps(temp, file, width = width, height = height)
  } else {
    cli::cli_abort(c(
      "x" = "Unsupported {.arg format} value.",
      "i" = "The format {.val {format}} is not supported.",
      ">" = "Use one of: {.code c('webp', 'png', 'pdf', 'svg', 'ps', 'eps')}."
    ))
  }

  invisible(file)
}


#' Export plotly object
#'
#' @inheritParams export_plot
#' @param format Output format.
#'
#' @returns Returns `NULL` invisibly.
#' @noRd
#'
export_plotly <- function(pl, file, format, width, height) {
  rlang::check_installed("htmlwidgets",
    reason = "to export plotly visualizations to image files."
  )

  rlang::check_installed("webshot2",
    reason = "to export plotly visualizations to image files."
  )

  # Create temporary HTML file
  temp_html <- tempfile(fileext = ".html")
  on.exit(
    {
      if (file.exists(temp_html)) {
        file.remove(temp_html)
      }
    },
    add = TRUE
  )
  htmlwidgets::saveWidget(pl, temp_html, selfcontained = TRUE)

  # Set webshot2 parameters based on format
  webshot_params <- list(
    url = temp_html,
    file = file,
    vwidth = width,
    vheight = height,
    delay = 1,
    quiet = TRUE # Doesn't seem to work
  )

  # Format-specific settings
  if (!format %in% c("jpg", "jpeg", "webp", "png", "pdf")) {
    cli::cli_warn(c(
      "!" = "The format {.val {format}} may not be supported by {.pkg webshot2}.",
      "i" = "Attempting export anyway..."
    ))
  }

  # Overwrite quiet option temporarily
  old_option <- getOption("webshot.quiet")
  options("webshot.quiet" = TRUE)
  on.exit(
    {
      if (is.null(old_option)) {
        options("webshot.quiet" = NULL)
      } else {
        options("webshot.quiet" = old_option)
      }
    },
    add = TRUE
  )


  # Convert to specified format
  tryCatch(
    do.call(webshot2::webshot, webshot_params),
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to export plotly visualization.",
        ">" = "This typically means Chrome/Chromium could not be launched. Try again.",
        "i" = conditionMessage(e)
      ), class = "stockflow_export_error")
    }
  )

  invisible(file)
}


#' Plot stock-and-flow diagram
#'
#' Visualize a stock-and-flow diagram using the R package DiagrammeR. Stocks are represented as boxes. Flows are represented as arrows between stocks and/or double circles, where the latter represent what it outside of the model boundary. Thin grey edges indicate dependencies between variables. By default, constants (indicated by italic labels) are not shown. Hover over the variables to see their equations.
#'
#' @param x A stock-and-flow model object of class [`sdbuildR`][sdbuildR].
#' @param vars Variables to plot. Defaults to NULL to plot all variables.
#' @param format_label If TRUE, apply default formatting (removing periods and underscores) to labels if labels are the same as variable names.
#' @param wrap_width Width of text wrapping for labels. Must be an integer. Defaults to 20.
#' @param font_size Font size. Defaults to 18.
#' @param font_family Font name. Defaults to "Times New Roman".
#' @param stock_col Colour of stocks. Defaults to "#83d3d4".
#' @param flow_col Colour of flows. Defaults to "#f48153".
#' @param dependency_col Colour of dependency arrows. Defaults to "#999999".
#' @param show_dependencies If TRUE, show dependencies between variables. Defaults to TRUE.
#' @param show_constants If TRUE, show constants. Defaults to FALSE.
#' @param show_aux If TRUE, show auxiliary variables. Defaults to TRUE.
#' @param minlen Minimum length of edges; must be an integer. Defaults to 2.
#' @param pad Padding around the graph. Defaults to 0.1.
#' @param nodesep Minimum distance between nodes. Defaults to 0.3.
#' @param ... Optional arguments
#'
#' @returns Stock-and-flow diagram
#' @export
#' @concept build
#' @method plot sdbuildR
#' @seealso [import_insightmaker()], [sdbuildR()], [plot.simulate_sdbuildR()]
#'
#' @examples
#' sfm <- sdbuildR("SIR")
#' plot(sfm)
#'
#' # Don't show constants or auxiliaries
#' plot(sfm, show_constants = FALSE, show_aux = FALSE)
#'
#' # Only show specific variables
#' plot(sfm, vars = "susceptible")
#'
plot.sdbuildR <- function(x,
                          vars = NULL,
                          format_label = TRUE,
                          wrap_width = 20,
                          font_size = 18,
                          font_family = "Times New Roman",
                          stock_col = "#83d3d4",
                          flow_col = "#f48153",
                          dependency_col = "#999999",
                          show_dependencies = TRUE,
                          show_constants = FALSE,
                          show_aux = TRUE,
                          minlen = 2,
                          pad = 0.1,
                          nodesep = 0.3,
                          ...) {
  sfm <- x
  rm(x)
  check_sdbuildR(sfm)

  # Get property dataframe
  df <- as.data.frame(sfm, properties = c("type", "name", "label", "eqn"))

  # Check whether there are any variables
  if (nrow(df) == 0) {
    cli::cli_warn(c(
      "i" = "Model contains no variables.",
      ">" = "Add variables using {.fn stock}, {.fn flow}, {.fn constant}, and {.fn aux}."
    ))
    return(invisible(NULL))
  }

  # Get dependencies
  dep <- dependencies(sfm)
  flow_df <- get_flow_df(sfm)

  if (!is.null(vars)) {
    vars <- clean_vars(vars)

    # Check whether specified variables are in the model
    validate_vars_in_model(vars, df, context = "model")

    # Only keep these variables in flow_df, dep, and df
    df <- df[df[["name"]] %in% vars, , drop = FALSE]
    dep <- dep[names(dep) %in% vars]
    flow_df <- flow_df[flow_df[["name"]] %in% vars, , drop = FALSE]

    # Set stocks not in vars to ''
    flow_df[["to"]][!flow_df[["to"]] %in% vars] <- ""
    flow_df[["from"]][!flow_df[["from"]] %in% vars] <- ""

    # Set show_aux to TRUE if any variables are aux
    if (any(vars %in% df[df[["type"]] == "aux", "name"])) {
      show_aux <- TRUE
    }

    # Set show_constants to TRUE if any variables are constants
    if (any(vars %in% df[df[["type"]] == "constant", "name"])) {
      show_constants <- TRUE
    }
  }

  if (format_label) {
    df[["label"]] <- ifelse(df[["name"]] == df[["label"]],
      stringr::str_replace_all(
        df[["label"]],
        c("_" = " ", "\\." = " ", "  " = " ")
      ), df[["label"]]
    )
  }

  # Prepare and format labels using centralized helper
  df <- prepare_labels(df, wrap_width = wrap_width, format_label = FALSE, deduplicate = FALSE)
  dict <- stats::setNames(df[["label"]], df[["name"]])

  # Get equations and remove quotation marks from unit strings
  dict_eqn <- stats::setNames(stringr::str_replace_all(
    df[["eqn"]],
    c("'" = "", "\"" = "")
  ), df[["name"]])

  # Categorize variables by type
  stock_names <- df[df[["type"]] == "stock", "name"]
  flow_names <- df[df[["type"]] == "flow", "name"]
  aux_names <- df[df[["type"]] == "aux", "name"]
  const_names <- df[df[["type"]] == "constant", "name"]

  # Filter based on show parameters
  if (!show_constants) const_names <- character(0)
  if (!show_aux) aux_names <- character(0)

  plot_var <- c(stock_names, flow_names)
  if (show_constants) {
    plot_var <- c(plot_var, const_names)
  }
  if (show_aux) {
    plot_var <- c(plot_var, aux_names)
  }

  # Shared node style
  style_node <- sprintf("node [fontsize=%s,fontname='%s']", font_size, font_family)

  # Prepare stock nodes
  if (length(stock_names) > 0) {
    style_stock <- sprintf("node [shape=box,style=filled,fillcolor='%s']", 
      stock_col)

    stock_nodes <- sprintf(
      "%s [id=%s,label='%s',tooltip = 'eqn = %s']",
      paste0("'", stock_names, "'"),
      paste0("'", stock_names, "'"),
      dict[stock_names],
      dict_eqn[stock_names]
    )
  } else {
    style_stock <- stock_nodes <- ""
  }

  # Prepare auxiliary nodes
  if (length(aux_names) > 0) {
    style_aux <- sprintf("node [shape=circle,fontsize=%s, width=0.15, height=0.15, fixedsize=true, style=filled, fillcolor='grey90']",
      font_size - 2)

    aux_nodes <- sprintf(
      "%s [id=%s,xlabel='%s',label='',tooltip = 'eqn = %s']",
      paste0("'", aux_names, "'"),
      paste0("'", aux_names, "'"),
      dict[aux_names],
      dict_eqn[aux_names]
    )
  } else {
    style_aux <- aux_nodes <- ""
  }

  # Prepare constant nodes (italic font)
  if (length(const_names) > 0) {
    # Format labels: convert \n to <BR/> and add italics
    formatted_labels <- vapply(dict[const_names], function(label) {
      label_with_html_breaks <- gsub("\n", "<BR/>", label, fixed = TRUE)
      paste0("<I>", label_with_html_breaks, "</I>")
    }, character(1), USE.NAMES = FALSE)

    style_const <- sprintf("node [shape=diamond,fontsize=%s,width=0.15, height=0.15, fixedsize=true, style=filled, fillcolor='grey90']",
      font_size - 2)

    const_nodes <- sprintf(
      "%s [id=%s,xlabel=<%s>,label='', tooltip = 'eqn = %s']",
      paste0("'", const_names, "'"),
      paste0("'", const_names, "'"),
      formatted_labels,
      dict_eqn[const_names]
    )
  } else {
    style_const <- const_nodes <- ""
  }


  # Create rank groupings based on dependencies
  rank_groups <- list()

  # Reverse dependencies, because we want to couple constants/aux with the first variable that depends on them
  dep_rev <- reverse_dep(dep)

  # Helper function
  get_first_dep <- function(var_names, dep_rev, plot_var) {
    if (length(var_names) == 0) {
      return(list())
    }

    dep_var <- dep_rev[var_names]

    # Only keep those in plot_var
    dep_var <- lapply(dep_var, function(x) {
      intersect(x, plot_var)
    })

    dep_var <- dep_var[lengths(dep_var) != 0]

    if (length(dep_var) > 0) {
      lapply(dep_var, `[[`, 1)
    } else {
      list()
    }
  }

  if (length(const_names) > 0) {
    dep_var <- get_first_dep(const_names, dep_rev, plot_var)
    rank_groups <- append(rank_groups, dep_var)
  }

  if (length(aux_names) > 0) {
    dep_var <- get_first_dep(aux_names, dep_rev, plot_var)
    rank_groups <- append(rank_groups, dep_var)
  }


  # Create rank statements
  rank_statements <- ""
  if (length(rank_groups) > 0) {
    rank_statements <- vapply(names(rank_groups), function(rank_node) {
      vars_in_rank <- rank_groups[[rank_node]]
      sprintf(
        "      {rank=same; %s }",
        paste0("'", c(rank_node, vars_in_rank), "'", collapse = "; ")
      )
    }, character(1), USE.NAMES = FALSE)
    rank_statements <- paste(rank_statements, collapse = "\n")
  }

  cloud_nodes <- flow_edges_from_source <- flow_edges_to_destination <- flow_nodes <- dependency_edges <- ""
  style_cloud <- style_flow_node <- style_flow_edges_from_source <- style_flow_edges_to_destination <- ""

  if (length(flow_names) > 0) {
    # If the flow is to a stock that doesn't exist, remove
    flow_df[["from"]] <- ifelse(flow_df[["from"]] %in% stock_names,
      flow_df[["from"]], ""
    )
    flow_df[["to"]] <- ifelse(flow_df[["to"]] %in% stock_names,
      flow_df[["to"]], ""
    )
    flow_df <- as.matrix(flow_df)

    # Fill in NA in flow_df with numbered clouds
    idxs <- which(flow_df == "")
    cloud_names <- character(0)
    if (length(idxs) > 0) {
      cloud_names <- paste0("Cloud", seq_along(idxs))
      flow_df[idxs] <- cloud_names

      # Find whether cloud is a source or a sink
      # flow_df has three columns; idxs are row-based
      labels <- cloud_names
      labels[idxs <= (nrow(flow_df) * 2)] <- "Unspecified source"
      labels[idxs > (nrow(flow_df) * 2)] <- "Unspecified sink"

      # Style for cloud nodes
      style_cloud <- sprintf("node [shape=doublecircle, fixedsize=true, width = .25, height = .25, orientation=15]")

      # External environment is represented as a cloud
      cloud_nodes <- sprintf(
        "%s [label='', tooltip = %s]",
        paste0("'", cloud_names, "'"),
        paste0("'", labels, "'")
      )
    }

    # Flow node style
    style_flow_node <- sprintf("node [style = '',shape=plaintext, fontsize=%s, width=0.6, height=0.3]",
      font_size - 2)

    # Create intermediate flow nodes (small nodes that flows pass through)
    flow_nodes <- sprintf(
      "%s [id=%s,label='%s', tooltip = 'eqn = %s']",
      paste0("'", flow_names, "'"),
      paste0("'", flow_names, "'"),
      dict[flow_names],
      dict_eqn[flow_names]
    )

    # Create edges: from -> flow_node -> to
    flow_edges_from_source <- c()
    flow_edges_to_destination <- c()

    style_flow_edges_from_source <- sprintf(
      "edge [style = '', arrowhead='none', color='%s', penwidth=1.1, minlen=%s, splines=false, tailport='e', headport='w']",
      paste0(
          "black:", flow_col, ":black"
        ),
        minlen
    )
    style_flow_edges_to_destination <- sprintf(
      "edge [style = '', arrowhead='normal', color='%s', arrowsize=1.5, penwidth=1.1, minlen=%s, splines=ortho, tailport='e', headport='w']",
      paste0(
          "black:", flow_col, ":black"
        ),
        minlen
    )

    # # Recycle flow_col if needed
    # flow_cols <- rep_len(flow_col, nrow(flow_df))

    for (i in seq_len(nrow(flow_df))) {
      flow_name <- flow_df[i, "name"]
      flow_node <- flow_name
      from_node <- flow_df[i, "from"]
      to_node <- flow_df[i, "to"]

      # Edge from source to flow node
      flow_edges_from_source <- c(flow_edges_from_source, sprintf(
        "%s -> %s",
        paste0("'", from_node, "'"),
        paste0("'", flow_node, "'")
      ))

      # Edge from flow node to destination
      flow_edges_to_destination <- c(flow_edges_to_destination, sprintf(
        "%s -> %s",
        paste0("'", flow_node, "'"),
        paste0("'", to_node, "'")
      ))
    }
  }

  # Add dependency arrows if requested
  style_dependency <- dependency_edges <- ""
  if (show_dependencies) {

    style_dependency <- sprintf("edge [style = '', color='%s', arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport = '_', headport='_']",
      dependency_col)

    # Only keep dependencies in plot_var
    dep <- lapply(dep, function(x) {
      intersect(x, plot_var)
    })

    # Only keep entries in plot_var
    dep <- dep[names(dep) %in% plot_var]

    if (length(dep) > 0) {
      dependency_edges <- unlist(lapply(names(dep), function(x) {
        if (length(dep[[x]]) > 0) {
          vapply(dep[[x]], function(y) {
            sprintf(
              "%s -> %s",
              paste0("'", y, "'"),
              paste0("'", x, "'")
            )
          }, character(1), USE.NAMES = FALSE)
        }
      }))
    }
  }

  # Compile string for diagram
  viz_str <- sprintf(
    "
    digraph sfm {

      graph [layout = dot, rankdir = LR, center=true, outputorder='edgesfirst', pad=%s, nodesep= %s]

      # Shared across all nodes (persists until overridden)
      %s

      # Define stock nodes
      %s
      %s

      # Define flow nodes (intermediate nodes for flows)
      %s
      %s

      # Define external cloud nodes
      %s
      %s

      # Define auxiliary nodes
      %s
      %s

      # Define constant nodes
      %s
      %s

      # Define flow edges (stock -> flow_node)
      %s
      %s

      # Define flow edges (flow_node -> stock)
      %s
      %s

      # Define dependency edges
      %s
      %s

      
      # Rank groupings
      %s

    }
          ",
          pad = as.character(pad),
          nodesep = as.character(nodesep),
    style_node,
    style_stock,
    stock_nodes |> paste0(collapse = "\n\t"),
    # stock_nodes |> rev() |> paste0(collapse = "\n\t"),
    style_flow_node,
    flow_nodes |> paste0(collapse = "\n\t"),
    style_cloud,
    cloud_nodes |> paste0(collapse = "\n\t"),
    style_aux,
    aux_nodes |> paste0(collapse = "\n\t"),
    style_const,
    const_nodes |> paste0(collapse = "\n\t"),
    style_flow_edges_from_source,
    flow_edges_from_source |> paste0(collapse = "\n\t"),
    style_flow_edges_to_destination,
    flow_edges_to_destination |> paste0(collapse = "\n\t"),
    style_dependency,
    dependency_edges |> paste0(collapse = "\n\t"),
    rank_statements

  )

  pl <- DiagrammeR::grViz(viz_str)

  pl
}



.plot_sdbuildR_old <- function(x,
                          vars = NULL,
                          format_label = TRUE,
                          wrap_width = 20,
                          font_size = 18,
                          font_family = "Times New Roman",
                          stock_col = "#83d3d4",
                          flow_col = "#f48153",
                          dependency_col = "#999999",
                          show_dependencies = TRUE,
                          show_constants = FALSE,
                          show_aux = TRUE,
                          minlen = 2,
                          ...) {
  sfm <- x
  rm(x)
  check_sdbuildR(sfm)

  # Get property dataframe
  df <- as.data.frame(sfm, properties = c("type", "name", "label", "eqn"))

  # Check whether there are any variables
  if (nrow(df) == 0) {
    cli::cli_warn(c(
      "i" = "Model contains no variables.",
      ">" = "Add variables using {.fn stock}, {.fn flow}, {.fn constant}, and {.fn aux}."
    ))
    return(invisible(NULL))
  }

  # Get dependencies
  dep <- dependencies(sfm)
  flow_df <- get_flow_df(sfm)

  if (!is.null(vars)) {
    vars <- clean_vars(vars)

    # Check whether specified variables are in the model
    validate_vars_in_model(vars, df, context = "model")

    # Only keep these variables in flow_df, dep, and df
    df <- df[df[["name"]] %in% vars, , drop = FALSE]
    dep <- dep[names(dep) %in% vars]
    flow_df <- flow_df[flow_df[["name"]] %in% vars, , drop = FALSE]

    # Set stocks not in vars to ''
    flow_df[["to"]][!flow_df[["to"]] %in% vars] <- ""
    flow_df[["from"]][!flow_df[["from"]] %in% vars] <- ""

    # Set show_aux to TRUE if any variables are aux
    if (any(vars %in% df[df[["type"]] == "aux", "name"])) {
      show_aux <- TRUE
    }

    # Set show_constants to TRUE if any variables are constants
    if (any(vars %in% df[df[["type"]] == "constant", "name"])) {
      show_constants <- TRUE
    }
  }

  if (format_label) {
    df[["label"]] <- ifelse(df[["name"]] == df[["label"]],
      stringr::str_replace_all(
        df[["label"]],
        c("_" = " ", "\\." = " ", "  " = " ")
      ), df[["label"]]
    )
  }

  # Prepare and format labels using centralized helper
  df <- prepare_labels(df, wrap_width = wrap_width, format_label = FALSE, deduplicate = FALSE)
  dict <- stats::setNames(df[["label"]], df[["name"]])

  # Get equations and remove quotation marks from unit strings
  dict_eqn <- stats::setNames(stringr::str_replace_all(
    df[["eqn"]],
    c("'" = "", "\"" = "")
  ), df[["name"]])

  # Categorize variables by type
  stock_names <- df[df[["type"]] == "stock", "name"]
  flow_names <- df[df[["type"]] == "flow", "name"]
  aux_names <- df[df[["type"]] == "aux", "name"]
  const_names <- df[df[["type"]] == "constant", "name"]

  # Filter based on show parameters
  if (!show_constants) const_names <- character(0)
  if (!show_aux) aux_names <- character(0)

  plot_var <- c(stock_names, flow_names)
  if (show_constants) {
    plot_var <- c(plot_var, const_names)
  }
  if (show_aux) {
    plot_var <- c(plot_var, aux_names)
  }

  # Prepare stock nodes
  if (length(stock_names) > 0) {
    # Recycle stock_col if needed
    stock_cols <- rep_len(stock_col, length(stock_names))
    stock_nodes <- sprintf(
      "%s [id=%s,label='%s',tooltip = 'eqn = %s',shape=box,style=filled,fillcolor='%s',fontsize=%s,fontname='%s']",
      paste0("'", stock_names, "'"),
      paste0("'", stock_names, "'"),
      dict[stock_names],
      dict_eqn[stock_names],
      stock_cols, font_size, font_family
    )
  } else {
    stock_nodes <- ""
  }

  # Prepare auxiliary nodes
  if (length(aux_names) > 0) {
    aux_nodes <- sprintf(
      # "%s [id=%s,label='%s',tooltip = 'eqn = %s',shape=plaintext,fontsize=%s,fontname='%s', width=0.6, height=0.3]",
      "%s [id=%s,xlabel='%s',label='',tooltip = 'eqn = %s',shape=circle,fontsize=%s,fontname='%s', width=0.15, height=0.15, fixedsize=true, style=filled, fillcolor='grey90']",
      paste0("'", aux_names, "'"),
      paste0("'", aux_names, "'"),
      dict[aux_names],
      dict_eqn[aux_names],
      font_size - 2, font_family
    )
  } else {
    aux_nodes <- ""
  }

  # Prepare constant nodes (italic font)
  if (length(const_names) > 0) {
    # Format labels: convert \n to <BR/> and add italics
    formatted_labels <- vapply(dict[const_names], function(label) {
      label_with_html_breaks <- gsub("\n", "<BR/>", label, fixed = TRUE)
      paste0("<I>", label_with_html_breaks, "</I>")
    }, character(1), USE.NAMES = FALSE)

    const_nodes <- sprintf(
      # "%s [id=%s,label=<%s>, tooltip = 'eqn = %s',
      #                    shape=plaintext,fontsize=%s,fontname='%s',
      #                    width=0.6, height=0.3]",
      "%s [id=%s,xlabel=<%s>,label='', tooltip = 'eqn = %s',
                         shape=diamond,fontsize=%s,fontname='%s',
                         width=0.15, height=0.15, fixedsize=true, style=filled, fillcolor='grey90']",
      paste0("'", const_names, "'"),
      paste0("'", const_names, "'"),
      formatted_labels,
      dict_eqn[const_names],
      font_size - 2, font_family
    )
  } else {
    const_nodes <- ""
  }


  # Create rank groupings based on dependencies
  rank_groups <- list()

  # Reverse dependencies, because we want to couple constants/aux with the first variable that depends on them
  dep_rev <- reverse_dep(dep)

  # Helper function
  get_first_dep <- function(var_names, dep_rev, plot_var) {
    if (length(var_names) == 0) {
      return(list())
    }

    dep_var <- dep_rev[var_names]

    # Only keep those in plot_var
    dep_var <- lapply(dep_var, function(x) {
      intersect(x, plot_var)
    })

    dep_var <- dep_var[lengths(dep_var) != 0]

    if (length(dep_var) > 0) {
      lapply(dep_var, `[[`, 1)
    } else {
      list()
    }
  }

  if (length(const_names) > 0) {
    dep_var <- get_first_dep(const_names, dep_rev, plot_var)
    rank_groups <- append(rank_groups, dep_var)
  }

  if (length(aux_names) > 0) {
    dep_var <- get_first_dep(aux_names, dep_rev, plot_var)
    rank_groups <- append(rank_groups, dep_var)
  }


  # Create rank statements
  rank_statements <- ""
  if (length(rank_groups) > 0) {
    rank_statements <- vapply(names(rank_groups), function(rank_node) {
      vars_in_rank <- rank_groups[[rank_node]]
      sprintf(
        "      {rank=same; %s }",
        paste0("'", c(rank_node, vars_in_rank), "'", collapse = "; ")
      )
    }, character(1), USE.NAMES = FALSE)
    rank_statements <- paste(rank_statements, collapse = "\n")
  }

  cloud_nodes <- flow_edges <- flow_nodes <- dependency_edges <- ""

  if (length(flow_names) > 0) {
    # If the flow is to a stock that doesn't exist, remove
    flow_df[["from"]] <- ifelse(flow_df[["from"]] %in% stock_names,
      flow_df[["from"]], ""
    )
    flow_df[["to"]] <- ifelse(flow_df[["to"]] %in% stock_names,
      flow_df[["to"]], ""
    )
    flow_df <- as.matrix(flow_df)

    # Fill in NA in flow_df with numbered clouds
    idxs <- which(flow_df == "")
    cloud_names <- character(0)
    if (length(idxs) > 0) {
      cloud_names <- paste0("Cloud", seq_along(idxs))
      flow_df[idxs] <- cloud_names

      # Find whether cloud is a source or a sink
      # flow_df has three columns; idxs are row-based
      labels <- cloud_names
      labels[idxs <= (nrow(flow_df) * 2)] <- "Unspecified source"
      labels[idxs > (nrow(flow_df) * 2)] <- "Unspecified sink"

      # External environment is represented as a cloud
      cloud_nodes <- sprintf(
        "%s [label='', tooltip = %s,shape=doublecircle, fixedsize=true, width = .25, height = .25, orientation=15]",
        paste0("'", cloud_names, "'"),
        paste0("'", labels, "'")
      )
    }

    # Create intermediate flow nodes (small nodes that flows pass through)
    flow_nodes <- sprintf(
      "%s [id=%s,label='%s', tooltip = 'eqn = %s', shape = plaintext, fontsize=%s, fontname='%s', width=0.6, height=0.3]",
      paste0("'", flow_names, "'"),
      paste0("'", flow_names, "'"),
      dict[flow_names],
      dict_eqn[flow_names],
      font_size - 2,
      font_family
    )

    # Create edges: from -> flow_node -> to
    flow_edges <- c()
    # Recycle flow_col if needed
    flow_cols <- rep_len(flow_col, nrow(flow_df))

    for (i in seq_len(nrow(flow_df))) {
      flow_name <- flow_df[i, "name"]
      flow_node <- flow_name
      from_node <- flow_df[i, "from"]
      to_node <- flow_df[i, "to"]

      # Edge from source to flow node
      flow_edges <- c(flow_edges, sprintf(
        "%s -> %s [arrowhead='none', color='%s', penwidth=1.1, minlen=%s, splines=false]",
        paste0("'", from_node, "'"),
        paste0("'", flow_node, "'"),
        paste0(
          "black:",
          # flow_col,":",
          flow_cols[i], ":black"
        ),
        minlen
      ))

      # Edge from flow node to destination
      flow_edges <- c(flow_edges, sprintf(
        "%s -> %s [arrowhead='normal', color='%s', arrowsize=1.5, penwidth=1.1, minlen=%s, splines=ortho]",
        paste0("'", flow_node, "'"),
        paste0("'", to_node, "'"),
        paste0(
          "black:",
          # flow_col,":",
          flow_cols[i], ":black"
        ),
        minlen
      ))
    }
  }

  # Add dependency arrows if requested
  if (show_dependencies) {
    # Only keep dependencies in plot_var
    dep <- lapply(dep, function(x) {
      intersect(x, plot_var)
    })

    # Only keep entries in plot_var
    dep <- dep[names(dep) %in% plot_var]

    if (length(dep) > 0) {
      dependency_edges <- unlist(lapply(names(dep), function(x) {
        if (length(dep[[x]]) > 0) {
          vapply(dep[[x]], function(y) {
            sprintf(
              "%s -> %s [color='%s', arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport='_']",
              paste0("'", y, "'"),
              paste0("'", x, "'"),
              dependency_col
            )
          }, character(1), USE.NAMES = FALSE)
        }
      }))
    }
  }

  # Compile string for diagram
  viz_str <- sprintf(
    "
    digraph sfm {

      graph [layout = dot, rankdir = LR, center=true, outputorder='edgesfirst', pad=.1, nodesep= .3]

      # Rank groupings
      %s

      # Define stock nodes
      %s

      # Define flow nodes (intermediate nodes for flows)
      %s

      # Define external cloud nodes
      %s

      # Define flow edges (stock -> flow_node -> stock)
      %s

      # Define dependency edges
      %s

      # Define auxiliary nodes
      %s

      # Define constant nodes
      %s
    }
          ",
    rank_statements,
    stock_nodes |> rev() |> paste0(collapse = "\n\t\t"),
    flow_nodes |> paste0(collapse = "\n\t\t"),
    cloud_nodes |> paste0(collapse = "\n\t\t"),
    flow_edges |> paste0(collapse = "\n\t\t"),
    dependency_edges |> paste0(collapse = "\n\t\t"),
    aux_nodes |> paste0(collapse = "\n\t\t"),
    const_nodes |> paste0(collapse = "\n\t\t")
  )

  pl <- DiagrammeR::grViz(viz_str)

  pl
}


#' Prepare for plotting simulation
#'
#' @param type_sim Either "sim", "ensemble", or "verify"
#' @param df data.frame to plot
#' @param constants Constants to plot
#' @inheritParams plot.simulate_sdbuildR
#' @inheritParams update.sdbuildR
#'
#' @returns List
#' @noRd
#'
prep_plot <- function(
  object, type_sim, df, constants,
  show_constants, vars, palette, colors, wrap_width
) {
  # Get names of stocks and non-stock variables
  names_df <- get_names(object)

  # Validate variable parameters
  validate_plot_params(vars = vars)

  if (!is.null(vars)) {
    vars <- clean_vars(vars)
  }

  # If vars is specified and contains constants, enable show_constants
  if (!is.null(vars)) {
    constant_names <- names_df[
      names_df[["type"]] %in% c("constant", "lookup"),
      "name"
    ]
    vars_constants <- intersect(constant_names, vars)
    constants_not_in_vars <- setdiff(constant_names, vars_constants)

    show_constants <- length(vars_constants) > 0

    if (show_constants) {
      # Remove non-selected constants from names_df
      names_df <- names_df[!(names_df[["name"]] %in% constants_not_in_vars), ,
        drop = FALSE
      ]

      # Filter constants to only those in vars
      if (type_sim == "sim") {
        constants <- constants[vars_constants]
      } else if (type_sim == "ensemble") {
        constants <- constants[!(constants[["variable"]] %in% constants_not_in_vars), ,
          drop = FALSE
        ]
      } else if (type_sim == "verify") {
        constants <- constants[!(constants[["variable"]] %in% constants_not_in_vars), ,
          drop = FALSE
        ]
      }
    }
  }

  # Add constants to dataframe if requested
  if (show_constants && length(constants) > 0) {
    result <- prep_constants(df, constants, names_df, type_sim = type_sim)
    df <- result$df
    names_df <- result$names_df
  }

  # Filter to specified variables if provided
  if (!is.null(vars)) {
    result <- filter_variables(vars, names_df, df)
    names_df <- result$names_df
    df <- result$df
    highlight_these_names <- names_df[["name"]]
  } else {
    # Default: highlight stocks
    highlight_these_names <- determine_highlight_vars(names_df, highlight_strategy = "auto")
  }

  # Ensure only variables which are in the dataframe are included
  names_df <- names_df[names_df[["name"]] %in% unique(df[["variable"]]), ,
    drop = FALSE
  ]

  # Prepare and standardize labels (handle duplicates, wrapping, special characters)
  names_df <- prepare_labels(names_df, wrap_width = wrap_width, format_label = FALSE)

  # Create dictionaries: name -> label (for visible and hidden variables)
  highlight_names <- names_df[match(highlight_these_names, names_df[["name"]]), , drop = FALSE]
  highlight_names <- stats::setNames(highlight_names[["name"]], highlight_names[["label"]])
  nonhighlight_names <- names_df[!names_df[["name"]] %in% highlight_these_names, , drop = FALSE]
  nonhighlight_names <- stats::setNames(nonhighlight_names[["name"]], nonhighlight_names[["label"]])

  # Split dataframe and recode labels
  df_highlight <- df[df[["variable"]] %in% unname(highlight_names), , drop = FALSE]
  df_nonhighlight <- df[df[["variable"]] %in% unname(nonhighlight_names), , drop = FALSE]

  df_highlight[["variable"]] <- factor(
    df_highlight[["variable"]],
    levels = unname(highlight_names),
    labels = names(highlight_names)
  )
  df_nonhighlight[["variable"]] <- factor(
    df_nonhighlight[["variable"]],
    levels = unname(nonhighlight_names),
    labels = names(nonhighlight_names)
  )

  # Generate colors
  nr_var <- length(unique(df[["variable"]]))
  colors <- generate_colors(nr_var, colors = colors, palette = palette)
  colors <- stats::setNames(colors, c(names(highlight_names), names(nonhighlight_names)))

  list(
    highlight_names = highlight_names,
    nonhighlight_names = nonhighlight_names,
    df_highlight = df_highlight,
    df_nonhighlight = df_nonhighlight,
    colors = colors
  )
}


#' Plot timeseries of simulation
#'
#' Visualize simulation results of a stock-and-flow model. Plot the evolution of stocks over time, with the option of also showing other model variables.
#'
#' @param x Output of [`simulate()`][simulate.sdbuildR()].
#' @param show_constants If TRUE, include constants in plot. Defaults to FALSE.
#' @param vars Variables to plot. Defaults to NULL to plot all variables.
#' @param palette Colour palette. Must be one of hcl.pals().
#' @param colors Vector of colours. If NULL, the color palette will be used. If specified, will override palette. The number of colours must be equal to the number of variables in the simulation data frame. Defaults to NULL.
#' @param font_family Font family. Defaults to "Times New Roman".
#' @param font_size Font size. Defaults to 16.
#' @param wrap_width Width of text wrapping for labels. Must be an integer. Defaults to 25.
#' @param showlegend Whether to show legend. Must be TRUE or FALSE. Defaults to TRUE.
#' @param ... Optional parameters
#'
#' @returns Plotly object
#' @export
#' @concept simulate
#' @seealso [`simulate()`][simulate.sdbuildR()], [as.data.frame.simulate_sdbuildR()], [plot.simulate_sdbuildR()]
#' @method plot simulate_sdbuildR
#'
#' @examples
#' sfm <- sdbuildR("SIR")
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # The default plot title and axis labels can be changed like so:
#' plot(sim, main = "Simulated trajectory", xlab = "Time", ylab = "Value")
#'
#' # Add constants to the plot
#' plot(sim, show_constants = TRUE)
#'
plot.simulate_sdbuildR <- function(x,
                                   show_constants = FALSE,
                                   vars = NULL,
                                   palette = "Dark 2",
                                   colors = NULL,
                                   font_family = "Times New Roman",
                                   font_size = 16,
                                   wrap_width = 25,
                                   showlegend = TRUE,
                                   ...) {
  if (missing(x)) {
    cli::cli_abort(c(
      "x" = "No simulation data available.",
      ">" = "Run a simulation first with {.fn simulate}."
    ))
  }

  check_simulate_sdbuildR(x)

  if (!x[["success"]]) {
    cli::cli_abort(c(
      "x" = "Simulation failed.",
      ">" = "Check your model specification and try again."
    ))
  }

  if (nrow(x[["df"]]) == 0) {
    cli::cli_abort(c("x" = "Simulation data frame has no rows"))
  }

  # Validate common plot parameters
  validate_plot_params(
    showlegend = showlegend,
    vars = vars,
    palette = palette,
    colors = colors,
    font_family = font_family,
    font_size = font_size,
    wrap_width = wrap_width
  )

  dots <- list(...)

  # Extract optional parameters with defaults
  time_unit <- x[["object"]][["sim_settings"]][["time_units"]]
  params <- extract_plot_params(dots, defaults = list(
    main = x[["object"]][["meta"]][["name"]],
    xlab = paste0("Time (", time_unit, ")"),
    ylab = ""
  ))
  main <- params$main
  xlab <- params$xlab
  ylab <- params$ylab

  out <- prep_plot(
    x[["object"]], "sim", x[["df"]], x[["constants"]], show_constants,
    vars, palette, colors, wrap_width
  )
  highlight_names <- out[["highlight_names"]]
  nonhighlight_names <- out[["nonhighlight_names"]]
  df_highlight <- out[["df_highlight"]]
  df_nonhighlight <- out[["df_nonhighlight"]]
  colors <- out[["colors"]]

  # Initialize plotly object
  pl <- plotly::plot_ly()

  # Add traces for highlight and nonhighlight variables
  pl <- add_trace_pair(pl,
    df_highlight = df_highlight,
    df_nonhighlight = df_nonhighlight,
    colors = colors,
    x_col = "time",
    y_col = "value",
    showlegend = showlegend,
    mode = "lines",
    type = "scatter"
  )

  # Customize layout using theme
  theme <- plotly_theme(
    font_family = font_family,
    font_size = font_size
  )

  pl <- plotly::layout(pl,
    legend = theme$legend,
    title = main,
    xaxis = list(title = xlab, font = list(size = font_size)),
    yaxis = list(title = ylab, font = list(size = font_size)),
    font = theme$font,
    margin = theme$margin
  )

  # If there is only one trace, legend doesn't show
  if (showlegend && (length(highlight_names) + length(nonhighlight_names)) == 1) {
    pl <- plotly::layout(pl, showlegend = TRUE)
  }

  # Set x-axis limits if specified
  if ("xlim" %in% names(dots)) {
    pl <- plotly::layout(pl,
      xaxis = list(range = dots[["xlim"]])
    )
  }

  if ("ylim" %in% names(dots)) {
    pl <- plotly::layout(pl,
      yaxis = list(range = dots[["ylim"]])
    )
  }

  pl
}


#' Plot timeseries of ensemble simulation
#'
#' Visualize ensemble simulation results of a stock-and-flow model. Either summary statistics or individual trajectories can be plotted. When multiple conditions j are specified, a grid of subplots is plotted. See [ensemble()] for examples.
#'
#' @param x Output of [ensemble()].
#' @param which Type of plot. Either `"summary"` for a summary plot with mean or median lines and confidence intervals, or `"sims"` for individual simulation trajectories with mean or median lines. Defaults to `"summary"`.
#' @param sim Indices of the individual trajectories to plot if which = `"sims"`. Defaults to 1:10. Including a high number of trajectories will slow down plotting considerably.
#' @param condition Indices of the condition to plot. Defaults to 1:9. If only one condition is specified, the plot will not be a grid of subplots.
#' @param nrows Number of rows in the plot grid. Defaults to ceiling(sqrt(n_conditions)).
#' @param margin Margin between subplots. Either a single numeric or a vector of length four(left, right, top, bottom). See `?plotly::subplot()` for more details. Defaults to 0.05.
#' @param shareX If `TRUE`, share the x-axis across subplots. Defaults to `TRUE`.
#' @param shareY If `TRUE`, share the y-axis across subplots. Defaults to `TRUE`.
#' @param palette Colour palette. Must be one of hcl.pals().
#' @param colors Vector of colours. If NULL, the color palette will be used. If specified, will override palette. The number of colours must be equal to the number of variables in the simulation data frame. Defaults to NULL.
#' @param alpha Trajectory opacity. Defaults to `1`.
#' @param font_family Font family. Defaults to "Times New Roman".
#' @param font_size Font size. Defaults to 16.
#' @param wrap_width Width of text wrapping for labels. Must be an integer. Defaults to 25.
#' @param showlegend Whether to show legend. Must be TRUE or FALSE. Defaults to TRUE.
#' @param label_subplots Whether to plot labels indicating the condition of the subplot.
#' @param central_tendency Central tendency to use for the mean line. Either "mean", "median", or FALSE to not plot the central tendency. Defaults to "mean".
#' @param central_tendency_width Line width of central tendency. Defaults to 3.
#' @param ... Optional parameters
#' @inheritParams plot.simulate_sdbuildR
#'
#' @returns Plotly object
#' @export
#' @concept ensemble
#' @seealso [ensemble()]
#' @method plot ensemble_sdbuildR
#'
plot.ensemble_sdbuildR <- function(x,
                                   which = c("summary", "sims")[1],
                                   sim = seq(1, min(c(x[["n"]], 10))),
                                   condition = seq(1, min(c(x[["n_conditions"]], 9))),
                                   vars = NULL,
                                   show_constants = FALSE,
                                   nrows = ceiling(sqrt(max(condition))),
                                   margin = .05,
                                   shareX = TRUE,
                                   shareY = TRUE,
                                   palette = "Dark 2",
                                   alpha = 0.3,
                                   colors = NULL,
                                   font_family = "Times New Roman",
                                   font_size = 16,
                                   wrap_width = 25,
                                   showlegend = TRUE,
                                   label_subplots = TRUE,
                                   central_tendency = c("mean", "median", FALSE)[1],
                                   central_tendency_width = 3,
                                   ...) {

  check_ensemble_sdbuildR(x)

  # Validate common plot parameters
  validate_plot_params(
    showlegend = showlegend,
    vars = vars,
    palette = palette,
    colors = colors,
    alpha = alpha,
    font_family = font_family,
    font_size = font_size,
    wrap_width = wrap_width,
    label_subplots = label_subplots
  )

  which <- .clean_which(which)

  # Check central tendency
  if (!isFALSE(central_tendency)) {
    central_tendency <- trimws(tolower(central_tendency))
    if (!central_tendency %in% c("mean", "median")) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg central_tendency} value.",
        "i" = "The {.arg central_tendency} argument must be {.code 'mean'}, {.code 'median'}, or {.code FALSE}."
      ))
    }
  }

  # Get passed arguments
  passed_arg <- names(as.list(match.call())[-1])

  dots <- list(...)

  # Build default subtitle based on plot type and central tendency
  default_sub <- if (which == "summary") {
    paste0(
      ifelse(isFALSE(central_tendency), "",
        paste0(stringr::str_to_title(central_tendency), " with ")
      ), "[",
      min(x[["quantiles"]]), ", ", max(x[["quantiles"]]),
      "] confidence interval of ", x[["n"]], " simulation",
      ifelse(x[["n"]] == 1, "", "s")
    )
  } else if (which == "sims") {
    paste0(
      ifelse(isFALSE(central_tendency), "",
        paste0(stringr::str_to_title(central_tendency), " with ")
      ),
      length(sim), "/", x[["n"]], " simulation",
      ifelse(x[["n"]] == 1, "", "s")
    )
  }

  # Extract optional parameters with defaults
  time_unit <- x[["object"]][["sim_settings"]][["time_units"]]
  params <- extract_plot_params(dots, defaults = list(
    main = paste0("Ensemble of ", x[["object"]][["meta"]][["name"]]),
    xlab = paste0("Time (", time_unit, ")"),
    ylab = "",
    sub = default_sub,
    alpha = alpha
  ))
  main <- params$main
  xlab <- params$xlab
  ylab <- params$ylab
  sub <- params$sub
  alpha <- params$alpha

  # Append subtitle to main title
  main <- paste0(main, "<span style='font-size:", font_size, "px;'>\n", sub, "</span>")

  if (!is.null(x[["summary"]])) {
    summary_df <- x[["summary"]]
  } else {
    cli::cli_abort(c(
      "x" = "The ensemble object does not contain summary statistics."
    ))
  }

  # Validate condition index
  if ("condition" %in% passed_arg) {
    .check_condition_index(condition, x[["n_conditions"]])
  }

  # Filter to the selected condition(s). This is a no-op for the default (all
  # conditions); without it, selecting a single condition would still draw every
  # condition's summary overlaid in one plot.
  summary_df <- summary_df[summary_df[["condition"]] %in% condition, , drop = FALSE]

  # Ensure there aren't more rows than condition
  nrows <- min(nrows, length(condition))

  # Whether to create subplots or not
  create_subplots <- length(condition) > 1

  # To plot individual simulation trajectories, extract df
  if (which == "sims") {
    if (!is.null(x[["df"]])) {
      df <- x[["df"]]

      # Validate and apply sim filter
      if ("sim" %in% passed_arg) {
        .check_sim_index(sim, x[["n"]])
      }

      # Filter by simulation index and selected condition(s)
      df <- df[df[["sim"]] %in% sim & df[["condition"]] %in% condition, , drop = FALSE]
    } else {
      cli::cli_abort(c(
        "x" = "No simulation data available.",
        "i" = "Individual simulation data is required for {.code which = 'sims'}.",
        ">" = "Run {.fn ensemble} with {.code save_sims = TRUE}."
      ))
    }
  } else if (which == "summary") {
    if ("sim" %in% passed_arg) {
      cli::cli_inform(c(
        "i" = "The {.arg sim} argument is ignored when {.code which = 'summary'}.",
        ">" = "Set {.code which = 'sims'} to plot individual trajectories."
      ))
    }

    df <- NULL
  }

  # Prepare for plotting
  out <- prep_plot(x[["object"]], "ensemble", summary_df,
    constants = x[["constants"]][["summary"]], show_constants = show_constants,
    vars = vars, palette = palette, colors = colors, wrap_width = wrap_width
  )
  summary_df_highlight <- out[["df_highlight"]]
  summary_df_nonhighlight <- out[["df_nonhighlight"]]
  colors <- out[["colors"]]

  if (which == "sims") {
    out <- prep_plot(x[["object"]], "ensemble", df,
      constants = x[["constants"]][["df"]], show_constants = show_constants,
      vars = vars, palette = palette, colors = colors, wrap_width = wrap_width
    )
    df_highlight <- out[["df_highlight"]]
    df_nonhighlight <- out[["df_nonhighlight"]]
  } else {
    df_highlight <- df_nonhighlight <- NULL
  }

  # Find qlow and qhigh
  q_cols <- colnames(summary_df)[grepl("^q", colnames(summary_df))]
  q_num <- as.numeric(gsub("^q", "", q_cols))
  q_low <- q_cols[which.min(q_num)]
  q_high <- q_cols[which.max(q_num)]

  # Check whether there are multiple time points
  mode <- ifelse(length(unique(summary_df[["time"]])) == 1, "markers", "lines")

  # Per-subplot theme is invariant across conditions; compute once.
  subplot_theme <- plotly_theme(font_family = font_family, font_size = font_size)

  # Plot
  if (!create_subplots) {
    j_idx <- 1
    j_name <- condition[j_idx]
    pl <- plot_ensemble_helper(
      subplot_label = ifelse(label_subplots, paste0("Condition ", j_name), ""),
      which = which,
      create_subplots = create_subplots,
      summary_df_highlight = summary_df_highlight,
      summary_df_nonhighlight = summary_df_nonhighlight,
      df_highlight = df_highlight,
      df_nonhighlight = df_nonhighlight,
      central_tendency = central_tendency,
      central_tendency_width = central_tendency_width,
      q_low = q_low,
      q_high = q_high,
      mode = mode,
      colors = colors,
      showlegend = showlegend,
      dots = dots,
      main = main,
      xlab = xlab, ylab = ylab,
      font_family = font_family,
      font_size = font_size,
      alpha = alpha,
      theme = subplot_theme
    )
  } else {
    # Pre-split every condition-filtered frame once instead of re-scanning the
    # full frame with a `condition == j_name` filter on every iteration.
    hl_by <- split_by_cond(df_highlight)
    nhl_by <- split_by_cond(df_nonhighlight)
    shl_by <- split_by_cond(summary_df_highlight)
    snhl_by <- split_by_cond(summary_df_nonhighlight)

    # Create a list of plotly objects for each condition
    pl_list <- list()
    for (j_idx in seq_along(condition)) {
      j_name <- condition[j_idx]

      pl_list[[j_idx]] <- plot_ensemble_helper(
        subplot_label = ifelse(label_subplots, paste0("Condition ", j_name), ""),
        which = which,
        create_subplots = create_subplots,
        summary_df_highlight = get_cond(shl_by, j_name, summary_df_highlight),
        summary_df_nonhighlight = get_cond(snhl_by, j_name, summary_df_nonhighlight),
        df_highlight = get_cond(hl_by, j_name, df_highlight),
        df_nonhighlight = get_cond(nhl_by, j_name, df_nonhighlight),
        central_tendency = central_tendency,
        central_tendency_width = central_tendency_width,
        q_low = q_low,
        q_high = q_high,
        mode = mode,
        colors = colors,
        # Only show legend if it's the last subplot
        showlegend = ifelse(j_idx != length(condition), FALSE, showlegend),
        dots = dots,
        main = main,
        xlab = xlab, ylab = ylab,
        font_family = font_family,
        font_size = font_size,
        alpha = alpha,
        theme = subplot_theme
      )
    }

    theme <- plotly_theme(
      font_family = font_family,
      font_size = font_size,
      margin_t = 100 # Extra space for ensemble title
    )

    pl <- plotly::subplot(pl_list,
      nrows = nrows,
      margin = margin,
      shareX = shareX,
      shareY = shareY,
      titleY = FALSE,
      titleX = FALSE
    ) |>
      plotly::layout(
        title = list(text = main),
        yaxis = list(title = list(text = ylab)),
        font = theme$font,
        margin = theme$margin,
        legend = list(
          orientation = "h",
          x = 0.5,
          y = -0.2,
          xanchor = "center",
          yanchor = "top"
        )
      )

    if (nzchar(xlab)) {
      pl <- pl |>
        plotly::layout(
          annotations = list(
            list(
              text = xlab,
              font = list(
                family = font_family
                # size = font_size
              ),
              xref = "paper",
              yref = "paper",
              xanchor = "center", yanchor = "top",
              x = 0.5, y = -.1,
              showarrow = FALSE
            )
          )
        )
    }
  }

  pl
}


#' Order rows by sim then time and insert a spacer row between sims
#'
#' This lets a single scatter trace render each simulation as a separate line:
#' the spacer row carries NA in the x and y columns (so the line breaks between
#' simulations instead of connecting them) while keeping every other column,
#' notably the grouping/colour column, intact. This is applied per variable so
#' that `color = ~variable` still splits the data into one trace per variable
#' and the spacers stay within each variable's group.
#'
#' @param d data.frame for a single variable.
#' @param group Column(s) identifying a single trajectory. A trajectory is one
#'   `(condition, sim)` pair, so multiple columns may be supplied. Defaults to
#'   "sim".
#' @param x Column mapped to the x aesthetic. Defaults to "time".
#' @param y Column mapped to the y aesthetic. Defaults to "value".
#'
#' @returns data.frame with NA spacer rows inserted between trajectories.
#' @noRd
break_by_sim <- function(d, group = "sim", x = "time", y = "value") {
  if (nrow(d) == 0) return(d)
  # Composite key over the grouping columns so e.g. the same sim index in
  # different conditions counts as two distinct trajectories.
  key <- interaction(d[, group, drop = FALSE], drop = TRUE, lex.order = TRUE)
  ord <- order(key, d[[x]])
  d <- d[ord, , drop = FALSE]
  parts <- split(d, key[ord], drop = TRUE)
  if (length(parts) <= 1L) return(d) # nothing to separate
  spacer <- d[1, , drop = FALSE]
  spacer[[x]] <- NA # NA in x and y breaks the line; keep grouping columns
  spacer[[y]] <- NA
  out <- vector("list", 2L * length(parts) - 1L)
  out[seq(1L, length(out), by = 2L)] <- parts
  out[seq(2L, length(out), by = 2L)] <- rep(list(spacer), length(parts) - 1L)
  do.call(rbind, out)
}


#' Insert NA spacers between simulations within each variable
#'
#' Applies break_by_sim() to each variable level present so that a single
#' `color = ~variable` scatter trace renders each simulation as a separate
#' line. Returns the recombined data.frame (variable order preserved).
#'
#' @param d data.frame with a factor "variable" column.
#' @returns data.frame with NA spacer rows inserted between simulations.
#' @noRd
break_sims_by_variable <- function(d) {
  if (nrow(d) == 0) return(d)
  # A trajectory is one (condition, sim) pair; fall back to "sim" alone if no
  # condition column is present.
  group <- intersect(c("condition", "sim"), colnames(d))
  if (length(group) == 0L) group <- "sim"
  parts <- lapply(
    levels(droplevels(d[["variable"]])),
    function(v) break_by_sim(d[d[["variable"]] == v, , drop = FALSE], group = group)
  )
  do.call(rbind, parts)
}


#' Split a data.frame by its "condition" column
#'
#' Pre-splitting once before the subplot loop avoids re-scanning the full frame
#' with a `condition == j_name` filter on every iteration. List names are
#' `as.character()` of the condition values.
#'
#' @param d data.frame with a "condition" column, or NULL.
#' @returns Named list of per-condition data.frames, or NULL if `d` is NULL.
#' @noRd
split_by_cond <- function(d) {
  if (is.null(d)) return(NULL)
  split(d, d[["condition"]])
}


#' Retrieve one condition's rows from a pre-split list
#'
#' Returns an empty-but-correctly-typed frame (same columns and factor levels as
#' `template`) for conditions with no rows, so behaviour matches the old
#' `d[d[["condition"]] == key, ]` filter exactly.
#'
#' @param lst Named list produced by split_by_cond(), or NULL.
#' @param key Condition value to look up.
#' @param template Full data.frame to use as a 0-row template when absent, or
#'   NULL (in which case NULL is returned, matching the old `NULL[...]` filter).
#' @returns data.frame of rows for `key`, or NULL.
#' @noRd
get_cond <- function(lst, key, template) {
  if (is.null(template)) return(NULL)
  d <- lst[[as.character(key)]]
  if (is.null(d)) template[0, , drop = FALSE] else d
}


#' Helper function to plot ensemble simulation results
#'
#' @param subplot_label Label for the subplot. Only used if create_subplots = TRUE.
#' @param which Type of plot. Must be one of "summary" or "sims". Defaults to "summary". If "summary", the plot will show the mean and confidence intervals of the simulation results. If "sims", the plot will show all individual simulation runs in sim.
#' @param create_subplots If TRUE, create subplots for each condition. If FALSE, plot all conditions in one plot.
#' @param summary_df_highlight data.frame with summary statistics of the ensemble simulation results (stocks). Must contain columns "condition", "variable", "mean", and confidence interval columns (e.g., "q0.025", "q0.975").
#' @param summary_df_nonhighlight data.frame with summary statistics of the ensemble simulation results (non-stocks). Must contain columns "condition", "variable", "mean", and confidence interval columns (e.g., "q0.025", "q0.975").
#' @param df_highlight data.frame with individual simulation results (stocks). Must contain columns "sim", "condition", "variable", and "value". Only used if which = "sims".
#' @param df_nonhighlight data.frame with individual simulation results (non-stocks). Must contain columns "sim", "condition", "variable", and "value". Only used if which = "sims".
#' @param q_low Column name for the lower bound of the confidence interval (e.g., "q0.025").
#' @param q_high Column name for the upper bound of the confidence interval (e.g., "q0.975").
#' @param mode Plotting mode. Either "lines" if there are multiple time points or "markers" for a single time point.
#' @param colors Vector of colours. If NULL, the color palette will be used. If specified, will override palette. The number of colours must be equal to the number of variables in the simulation data frame. Defaults to NULL.
#' @param dots List of additional parameters passed to the plotly functions.
#' @param main Main title of the plot. Defaults to the name of the stock-and-flow model and the number of simulations.
#' @param xlab Label on x-axis.
#' @param ylab Label on y-axis.
#' @param alpha Opacity of the confidence bands or individual trajectories.
#' @param theme List of styling parameters from plotly_theme(). Invariant
#'   across conditions, so computed once by the caller and passed in.
#' @inheritParams plot.ensemble_sdbuildR
#'
#' @returns Plotly object
#' @noRd
plot_ensemble_helper <- function(subplot_label,
                                 which, create_subplots,
                                 summary_df_highlight,
                                 summary_df_nonhighlight,
                                 df_highlight,
                                 df_nonhighlight,
                                 central_tendency,
                                 central_tendency_width,
                                 q_low, q_high,
                                 mode,
                                 colors, showlegend, dots,
                                 main, xlab, ylab,
                                 font_family, font_size, alpha, theme) {
  if (which == "sims") {
    plot_highlight <- nrow(df_highlight) > 0
    plot_nonhighlight <- nrow(df_nonhighlight) > 0
  } else if (which == "summary") {
    plot_highlight <- nrow(summary_df_highlight) > 0
    plot_nonhighlight <- nrow(summary_df_nonhighlight) > 0
  }
  plot_summary <- (nrow(summary_df_highlight) > 0) || (nrow(summary_df_nonhighlight) > 0)

  nr_var <- length(colors)

  # Initialize plotly object
  pl <- plotly::plot_ly()

  if (which == "summary") {
    if (mode == "lines") {
      if (plot_nonhighlight) {
        pl <- plotly::add_ribbons(pl,
          data = summary_df_nonhighlight,
          x = ~time,
          ymin = ~ get(q_low),
          ymax = ~ get(q_high),
          color = ~variable,
          legendgroup = ~variable,
          fillcolor = ~variable,
          opacity = alpha,
          type = "scatter",
          mode = mode,
          colors = colors,
          # Add traces for non-stock variables (visible = "legendonly")
          showlegend = showlegend,
          visible = "legendonly"
        )
      }

      # First plot confidence bands
      if (plot_highlight) {
        pl <- plotly::add_ribbons(pl,
          data = summary_df_highlight,
          x = ~time,
          ymin = ~ get(q_low),
          ymax = ~ get(q_high),
          color = ~variable,
          legendgroup = ~variable,
          fillcolor = ~variable,
          opacity = alpha,
          type = "scatter",
          mode = mode,
          colors = colors,
          showlegend = showlegend,
          # Add traces for stock variables (visible = TRUE)
          visible = TRUE
        )
      }
    }
  } else if (which == "sims") {
    # Draw one scatter trace per variable instead of one per (variable, sim).
    # break_sims_by_variable() inserts an NA spacer row between simulations so a
    # single trace renders each simulation as a separate line (connectgaps =
    # FALSE keeps the NA gaps from being bridged). Keeping color = ~variable /
    # colors = colors means the colour mapping is identical to the summary
    # traces, while plotly still splits the data into one trace per variable.

    # Add traces for non-stock variables (visible = "legendonly")
    if (plot_nonhighlight) {
      pl <- plotly::add_trace(pl,
        data = break_sims_by_variable(df_nonhighlight),
        x = ~time,
        y = ~value,
        color = ~variable,
        legendgroup = ~variable,
        type = "scatter",
        mode = mode,
        opacity = alpha,
        colors = colors,
        showlegend = if (plot_summary) FALSE else showlegend, # only show legend if summary is not plotted, otherwise it will be duplicated with the summary traces
        connectgaps = FALSE, # required: NA must break the line, not bridge it
        visible = "legendonly"
      )
    }

    if (plot_highlight) {
      pl <- plotly::add_trace(pl,
        data = break_sims_by_variable(df_highlight),
        x = ~time,
        y = ~value,
        color = ~variable,
        legendgroup = ~variable,
        type = "scatter",
        mode = mode,
        opacity = alpha,
        colors = colors,
        showlegend = if (plot_summary) FALSE else showlegend, # only show legend if summary is not plotted, otherwise it will be duplicated with the summary traces
        connectgaps = FALSE, # required: NA must break the line, not bridge it
        # Add traces for stock variables (visible = TRUE)
        visible = TRUE
      )
    }
  }

  # When central_tendency = FALSE, we still want a legend
  if (isFALSE(central_tendency) && plot_summary) {
    # Overwrite with default
    central_tendency <- "mean"

    # Same trick does not work for mode == "markers" to not plot central_tendency
    if (mode == "markers") {
      # When mode == "markers" and the only value available is Infinity, no trace is plotted but the legend still shows up, which is exactly what we want when central_tendency is FALSE

      summary_df_highlight[[central_tendency]] <- Inf
      summary_df_nonhighlight[[central_tendency]] <- Inf
    } else if (mode == "lines") {
      # When only one time point is available and mode == "lines", no trace is plotted but the legend still shows up, which is exactly what we want when central_tendency is FALSE
      summary_df_highlight <- summary_df_highlight[summary_df_highlight[["time"]] == summary_df_highlight[["time"]][1], ]
      summary_df_nonhighlight <- summary_df_nonhighlight[summary_df_nonhighlight[["time"]] == summary_df_nonhighlight[["time"]][1], ]
    }
  }

  # Plot mean/median points/lines
  if (plot_summary) {
    if (mode == "lines") {
      if (plot_nonhighlight) {
        pl <- plotly::add_trace(pl,
          data = summary_df_nonhighlight,
          x = ~time,
          y = ~ get(central_tendency),
          color = ~variable,
          legendgroup = ~variable,
          type = "scatter",
          mode = mode,
          colors = colors,
          showlegend = showlegend,
          line = list(width = central_tendency_width), # thicker line for mean
          visible = "legendonly"
        )
      }

      if (plot_highlight) {
        pl <- plotly::add_trace(pl,
          data = summary_df_highlight,
          x = ~time,
          y = ~ get(central_tendency),
          color = ~variable,
          legendgroup = ~variable,
          type = "scatter",
          mode = mode,
          line = list(width = central_tendency_width), # thicker line for mean
          showlegend = showlegend,
          colors = colors,
          visible = TRUE
        )
      }
    } else if (mode == "markers" && which == "summary") {
      if (plot_nonhighlight) {
        pl <- plotly::add_trace(pl,
          data = summary_df_nonhighlight,
          x = ~time,
          y = ~ get(central_tendency),
          color = ~variable,
          legendgroup = ~variable,
          type = "scatter",
          error_y = ~ list(
            symmetric = FALSE,
            arrayminus = get(central_tendency) - get(q_low),
            array = get(q_high) - get(central_tendency),
            color = colors
          ),
          mode = mode,
          colors = colors,
          showlegend = showlegend,
          marker = list(size = central_tendency_width * 3), # thicker line for mean
          visible = "legendonly"
        )
      }

      if (plot_highlight) {
        pl <- plotly::add_trace(pl,
          data = summary_df_highlight,
          x = ~time,
          y = ~ get(central_tendency),
          color = ~variable,
          legendgroup = ~variable,
          type = "scatter",
          error_y = ~ list(
            symmetric = FALSE,
            arrayminus = get(central_tendency) - get(q_low),
            array = get(q_high) - get(central_tendency),
            color = colors
          ),
          mode = mode,
          marker = list(size = central_tendency_width * 3), # thicker line for mean
          showlegend = showlegend,
          colors = colors,
          visible = TRUE
        )
      }
    } else if (mode == "markers" && which == "sims") {
      if (plot_nonhighlight) {
        pl <- plotly::add_trace(pl,
          data = summary_df_nonhighlight,
          x = ~time,
          y = ~ get(central_tendency),
          color = ~variable,
          legendgroup = ~variable,
          type = "scatter",
          mode = mode,
          colors = colors,
          showlegend = showlegend,
          marker = list(size = central_tendency_width * 3), # thicker line for mean
          visible = "legendonly"
        )
      }

      if (plot_highlight) {
        pl <- plotly::add_trace(pl,
          data = summary_df_highlight,
          x = ~time,
          y = ~ get(central_tendency),
          color = ~variable,
          legendgroup = ~variable,
          type = "scatter",
          mode = mode,
          marker = list(size = central_tendency_width * 3), # thicker line for mean
          showlegend = showlegend,
          colors = colors,
          visible = TRUE
        )
      }
    }
  }

  # Customize layout (theme is invariant across conditions, computed by caller)
  pl <- plotly::layout(pl,
    margin = theme$margin,
    legend = theme$legend
  )

  # If there is only one trace, legend doesn't show
  if (showlegend && (nr_var == 1)) {
    pl <- plotly::layout(pl, showlegend = TRUE)
  }


  if (!create_subplots) {
    pl <- plotly::layout(pl,
      title = main,
      xaxis = list(title = xlab),
      yaxis = list(title = ylab),
      font = list(family = font_family, size = font_size)
    )
  }


  # Add subplot title
  if (create_subplots && nzchar(subplot_label)) {
    pl <- plotly::layout(pl,
      annotations = list(
        list(
          text = subplot_label,
          font = list(
            family = font_family,
            size = ceiling(font_size * .75)
          ),
          bgcolor = "white",
          xref = "paper",
          yref = "paper",
          xanchor = "center", yanchor = "top",
          x = 0.5, y = 1,
          showarrow = FALSE
        )
      )
    )
  }

  # Set x-axis limits if specified
  if ("xlim" %in% names(dots)) {
    pl <- plotly::layout(pl, xaxis = list(range = dots[["xlim"]]))
  }

  # Set y-axis limits if specified
  if ("ylim" %in% names(dots)) {
    pl <- plotly::layout(pl, yaxis = list(range = dots[["ylim"]]))
  }

  pl
}


#' Plot verify results
#'
#' Visualize the simulation(s) used during [verify()]. Each condition `j` is
#' displayed as a subplot. Simulations are always available since [verify()]
#' unconditionally retains them.
#'
#' @param x Output of [verify()].
#' @param test Integer vector of test numbers to plot.
#' Combines with `label` and `status` as AND intersection.
#' @param label Character vector of regex patterns for partial, case-insensitive
#'   label matching. A test is included if its label matches any pattern.
#' @param ignore_case Logical; whether `label` matching is case-insensitive.
#'   Default `TRUE`.
#' @param condition Integer vector of condition numbers to plot. Defaults to `1:9`. If only one condition is specified, the plot will not be a grid of subplots.
#' @param nrows Number of subplot rows. Defaults to `ceiling(sqrt(condition))`.
#' @param shareX Share the x-axis across subplots. Defaults to `TRUE`.
#' @param shareY Share the y-axis across subplots. Defaults to `TRUE`.
#' @param palette Colour palette (see `hcl.pals()`). Defaults to `"Dark 2"`.
#' @param colors Vector of colours overriding `palette`. Defaults to `NULL`.
#' @param font_family Font family. Defaults to `"Times New Roman"`.
#' @param font_size Font size. Defaults to `16`.
#' @param wrap_width Label wrap width. Defaults to `25`.
#' @param showlegend Whether to show the legend. Defaults to `TRUE`.
#' @param label_subplots Whether to plot labels indicating the test number of the subplot.
#' @param ... Additional arguments passed to [plot.simulate_sdbuildR()].
#' @inheritParams as.data.frame.verify_sdbuildR
#' @inheritParams plot.simulate_sdbuildR
#' @inheritParams plot.ensemble_sdbuildR
#'
#' @returns A plotly object.
#' @export
#' @concept unitTest
#' @method plot verify_sdbuildR
#' @seealso [verify()], [plot.simulate_sdbuildR()], [plot.ensemble_sdbuildR()]
#'
#' @examples
#' sfm <- sdbuildR("SIR") |>
#'   unit_test(expr = all(susceptible >= 0))
#' res <- verify(sfm)
#' plot(res)
plot.verify_sdbuildR <- function(x,
                                 test = NULL,
                                 vars = NULL,
                                 show_constants = FALSE,
                                 label = NULL,
                                 ignore_case = TRUE,
                                 status = c("pass", "fail", "error", "skip"),
                                 condition = seq(1, min(c(x[["n_conditions"]], 9))),
                                 nrows = ceiling(sqrt(max(condition))),
                                 shareX = TRUE,
                                 shareY = TRUE,
                                 palette = "Dark 2",
                                 colors = NULL,
                                 font_family = "Times New Roman",
                                 font_size = 16,
                                 wrap_width = 25,
                                 showlegend = TRUE,
                                 label_subplots = TRUE,
                                 alpha = 1,
                                 margin = .05,
                                 ...) {
  if (missing(x)) {
    cli::cli_abort(c(
      "x" = "No simulation data available.",
      ">" = "Generate a unit test run first with {.fn verify}."
    ))
  }

  # Check whether it is a verify_sdbuildR object
  if (!inherits(x, "verify_sdbuildR")) {
    cli::cli_abort(c(
      "x" = "Invalid object class.",
      "i" = "This is not an object of class {.cls verify_sdbuildR}.",
      ">" = "Generate a unit test run with {.fn verify}."
    ))
  }

  if (!x[["success"]]) {
    cli::cli_abort(c(
      "x" = "Unit test run failed.",
      ">" = "Check your model specification and try again."
    ))
  }

  # Validate common plot parameters
  validate_plot_params(
    showlegend = showlegend,
    vars = vars,
    palette = palette,
    colors = colors,
    alpha = alpha,
    font_family = font_family,
    font_size = font_size,
    wrap_width = wrap_width,
    label_subplots = label_subplots
  )

  # Get passed arguments
  passed_arg <- names(as.list(match.call())[-1])
  dots <- list(...)

  # Filter simulations based on test, label, status, and condition
  df <- as.data.frame(x,
    which = "sims",
    # Pass on filtering arguments to as.data.frame() to filter the simulations before plotting
    test = test, label = label,
    ignore_case = ignore_case, status = status, condition = condition
  )

  # Add sim column for plot_ensemble_helper() if not already present (e.g., when which = "summary")
  if (!"sim" %in% colnames(df)) {
    df[["sim"]] <- 1
  }

  # Find how many tests and conditions are available after filtering
  test_column <- paste0(unique(df[["test"]]), collapse = ", ")

  # Test numbers are stored as 1, 2, 3
  test_nrs <- strsplit(test_column, ", ") |>
    unlist() |>
    trimws() |>
    as.integer()
  n_tests <- length(test_nrs)
  condition_nrs <- unique(df[["condition"]])
  n_conditions <- length(condition_nrs)

  # Ensure there aren't more rows than conditions
  nrows <- min(nrows, n_conditions)

  # Whether to create subplots or not
  create_subplots <- n_conditions > 1

  # Extract constants from verify object to pass to prep_plot()
  constants <- lapply(condition_nrs, function(y) {
    const <- x[["sims"]][[y]][["constants"]]
    data.frame(
      # For verify(), sim = 1
      sim = 1,
      condition = y, variable = names(const), value = unlist(unname(const))
    )
  })
  constants <- as.data.frame(do.call(rbind, constants))

  # Build default subtitle
  default_sub <- paste0(n_tests, " test", ifelse(n_tests > 1, "s", ""), "; ", n_conditions, " condition", ifelse(n_conditions > 1, "s", ""))

  # Extract optional parameters with defaults
  sfm <- x[["object"]]
  time_unit <- sfm[["sim_settings"]][["time_units"]]
  params <- extract_plot_params(dots, defaults = list(
    main = paste0("Unit Tests of ", sfm[["meta"]][["name"]]),
    xlab = paste0("Time (", time_unit, ")"),
    ylab = "",
    sub = default_sub,
    alpha = alpha
  ))
  main <- params$main
  xlab <- params$xlab
  ylab <- params$ylab
  sub <- params$sub
  alpha <- params$alpha

  # Append subtitle to main title
  main <- paste0(main, "<span style='font-size:", font_size, "px;'>\n", sub, "</span>")

  # Prepare for plotting
  out <- prep_plot(sfm, "verify", df,
    constants = constants, show_constants = show_constants,
    vars = vars, palette = palette, colors = colors, wrap_width = wrap_width
  )
  df_highlight <- out[["df_highlight"]]
  df_nonhighlight <- out[["df_nonhighlight"]]
  colors <- out[["colors"]]

  # Check whether there are multiple time points
  mode <- ifelse(length(unique(df[["time"]])) == 1, "markers", "lines")

  # Plot
  which <- "sims"
  summary_df_highlight <- summary_df_nonhighlight <- data.frame()
  central_tendency_width <- q_low <- q_high <- NULL
  central_tendency <- FALSE

  # Per-subplot theme is invariant across conditions; compute once.
  subplot_theme <- plotly_theme(font_family = font_family, font_size = font_size)

  if (!create_subplots) {
    j_idx <- 1
    j_name <- condition_nrs[j_idx]
    pl <- plot_ensemble_helper(
      subplot_label = ifelse(label_subplots, paste0("Condition ", j_name), ""),
      which = which,
      create_subplots = create_subplots,
      summary_df_highlight = summary_df_highlight,
      summary_df_nonhighlight = summary_df_nonhighlight,
      df_highlight = df_highlight,
      df_nonhighlight = df_nonhighlight,
      central_tendency = central_tendency,
      central_tendency_width = central_tendency_width,
      q_low = q_low,
      q_high = q_high,
      mode = mode,
      colors = colors,
      showlegend = showlegend,
      dots = dots,
      main = main,
      xlab = xlab, ylab = ylab,
      font_family = font_family,
      font_size = font_size,
      alpha = alpha,
      theme = subplot_theme
    )
  } else {
    # Pre-split the condition-filtered frames once (summary frames are empty for
    # verify, so they are passed through directly).
    hl_by <- split_by_cond(df_highlight)
    nhl_by <- split_by_cond(df_nonhighlight)

    # Create a list of plotly objects for each condition
    pl_list <- list()
    for (j_idx in seq_along(condition)) {
      j_name <- condition[j_idx]

      pl_list[[j_idx]] <- plot_ensemble_helper(
        subplot_label = ifelse(label_subplots, paste0("Condition ", j_name), ""),
        which = which,
        create_subplots = create_subplots,
        summary_df_highlight = summary_df_highlight,
        summary_df_nonhighlight = summary_df_nonhighlight,
        df_highlight = get_cond(hl_by, j_name, df_highlight),
        df_nonhighlight = get_cond(nhl_by, j_name, df_nonhighlight),
        central_tendency = central_tendency,
        central_tendency_width = central_tendency_width,
        q_low = q_low,
        q_high = q_high,
        mode = mode,
        colors = colors,
        # Only show legend if it's the last subplot
        showlegend = ifelse(j_idx != length(condition), FALSE, showlegend),
        dots = dots,
        main = main,
        xlab = xlab, ylab = ylab,
        font_family = font_family,
        font_size = font_size,
        alpha = alpha,
        theme = subplot_theme
      )
    }

    theme <- plotly_theme(
      font_family = font_family,
      font_size = font_size,
      margin_t = 100 # Extra space for title
    )

    pl <- plotly::subplot(pl_list,
      nrows = nrows,
      margin = margin,
      shareX = shareX,
      shareY = shareY,
      titleY = FALSE,
      titleX = FALSE
    ) |>
      plotly::layout(
        title = list(text = main),
        yaxis = list(title = list(text = ylab)),
        font = theme$font,
        margin = theme$margin,
        legend = list(
          orientation = "h",
          x = 0.5,
          y = -0.2,
          xanchor = "center",
          yanchor = "top"
        )
      )

    if (nzchar(xlab)) {
      pl <- pl |>
        plotly::layout(
          annotations = list(
            list(
              text = xlab,
              font = list(
                family = font_family
                # size = font_size
              ),
              xref = "paper",
              yref = "paper",
              xanchor = "center", yanchor = "top",
              x = 0.5, y = -.1,
              showarrow = FALSE
            )
          )
        )
    }
  }

  pl
}
