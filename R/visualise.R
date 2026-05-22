#' Save plot to a file
#'
#' Save a plot of a stock-and-flow diagram or a simulation to a specified file path. Note that saving plots requires additional packages to be installed (see below).
#'
#' @param pl Plot object.
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
#' @concept simulate
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
#' @examplesIf interactive()
#' \dontrun{
#' # requires internet
#' # Only if dependencies are installed
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
  } else if ("qgraph" %in% class(pl) || is.function(pl)) {
    export_qgraph(pl, file,
      format = format,
      width = width_px,
      height = height_px,
      dpi = dpi,
      font_family = font_family
    )
  } else {
    cli::cli_abort(c(
      "Unsupported plot object class.",
      "x" = "The {.fn export_plot} function does not support plot objects of class {.cls {class(pl)}}.",
      ">" = "Use a {.cls grViz}, {.cls plotly}, or {.cls qgraph} object."
    ))
  }

  return(invisible())
}


#' Export qgraph object
#'
#' @inheritParams export_plot
#' @param format Output format.
#'
#' @returns Returns `NULL` invisibly.
#' @noRd
#'
export_qgraph <- function(pl, file, format, width, height, dpi, font_family = "") {
  rlang::check_installed("qgraph", reason = "to export qgraph visualizations to image files.")

  width_in <- width / dpi
  height_in <- height / dpi
  family <- trimws(font_family)

  resolve_pdf_ps_family <- function(family_name) {
    if (!nzchar(family_name)) {
      return("")
    }

    if (family_name %in% names(grDevices::pdfFonts())) {
      return(family_name)
    }

    if (identical(family_name, "Times New Roman")) {
      return("Times")
    }

    return("")
  }

  if (format == "png") {
    grDevices::png(file, width = width, height = height, units = "px", res = dpi)
  } else if (format %in% c("jpg", "jpeg")) {
    grDevices::jpeg(file, width = width, height = height, units = "px", res = dpi)
  } else if (format == "tiff") {
    grDevices::tiff(file, width = width, height = height, units = "px", res = dpi)
  } else if (format == "bmp") {
    grDevices::bmp(file, width = width, height = height, units = "px", res = dpi)
  } else if (format == "pdf") {
    if (capabilities("cairo")) {
      if (nzchar(family)) {
        grDevices::cairo_pdf(file, width = width_in, height = height_in, family = family)
      } else {
        grDevices::cairo_pdf(file, width = width_in, height = height_in)
      }
    } else {
      pdf_family <- resolve_pdf_ps_family(family)
      if (nzchar(pdf_family)) {
        grDevices::pdf(file, width = width_in, height = height_in, family = pdf_family)
      } else {
        grDevices::pdf(file, width = width_in, height = height_in)
      }
    }
  } else if (format == "svg") {
    grDevices::svg(file, width = width_in, height = height_in)
  } else if (format == "ps") {
    ps_family <- resolve_pdf_ps_family(family)
    if (nzchar(ps_family)) {
      grDevices::postscript(file, width = width_in, height = height_in, horizontal = FALSE, family = ps_family)
    } else {
      grDevices::postscript(file, width = width_in, height = height_in, horizontal = FALSE)
    }
  } else if (format == "eps") {
    ps_family <- resolve_pdf_ps_family(family)
    if (nzchar(ps_family)) {
      grDevices::postscript(file,
        width = width_in, height = height_in,
        horizontal = FALSE, onefile = FALSE, paper = "special", family = ps_family
      )
    } else {
      grDevices::postscript(file,
        width = width_in, height = height_in,
        horizontal = FALSE, onefile = FALSE, paper = "special"
      )
    }
  } else {
    cli::cli_abort(c(
      "Unsupported {.arg format} value.",
      "x" = "The format {.val {format}} is not supported for qgraph export.",
      ">" = "Use one of: {.code c('png', 'pdf', 'svg', 'ps', 'eps', 'jpg', 'jpeg', 'tiff', 'bmp')}."
    ))
  }

  on.exit(grDevices::dev.off(), add = TRUE)

  if (is.function(pl)) {
    pl()
  } else {
    plot(pl)
  }

  invisible()
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
      "Unsupported {.arg format} value.",
      "x" = "The format {.val {format}} is not supported.",
      ">" = "Use one of: {.code c('webp', 'png', 'pdf', 'svg', 'ps', 'eps')}."
    ))
  }

  invisible()
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
  rlang::check_installed("htmlwidgets", reason = "to export plotly visualizations to image files.")

  rlang::check_installed("webshot2", reason = "to export plotly visualizations to image files.")

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
      "Potentially unsupported format.",
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
  do.call(webshot2::webshot, webshot_params)

  return(invisible())
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
#' plot(sfm, vars = "Susceptible")
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
      ">" = "Add variables using {.fn update} before plotting."
    ))
    return(invisible(NULL))
  }

  # Get dependencies
  dep <- dependencies(sfm)
  flow_df <- get_flow_df(sfm)

  if (!is.null(vars)) {
    if (!is.character(vars)) {
      cli::cli_abort(c(
        "Invalid {.arg vars} argument.",
        "x" = "The {.arg vars} argument must be {.cls character}.",
        "i" = "Received: {.cls {typeof(vars)}}.",
        ">" = "Provide a character vector of variable names."
      ))
    }

    vars <- unique(vars)

    if (length(vars) == 0) {
      cli::cli_abort(c(
        "Empty {.arg vars} vector.",
        "x" = "The {.arg vars} argument cannot be of length zero.",
        ">" = "Provide at least one variable name."
      ))
    }

    # Check whether specified variables are in the model
    validate_vars_in_model(vars, df, context = "model")

    # # Add dependencies of vars
    # vars <- c(vars, unname(unlist(dep[vars])))
    #
    # # For all stocks in vars, also include their inflows and outflows,
    # # and the stocks that those inflows and outflows are connected to.
    # stock_vars <- vars[vars %in% df[df[["type"]] == "stock", "name"]]
    # if (length(stock_vars) > 0){
    #   idx <- flow_df[["to"]] %in% stock_vars | flow_df[["from"]] %in% stock_vars
    #   if (any(idx)){
    #     vars <- unique(c(vars, unname(unlist(flow_df[idx, ])) ))
    #   }
    # }

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
#' @param type_sim Either "sim" or "ensemble"
#' @param df data.frame to plot
#' @param constants Constants to plot
#' @inheritParams plot.simulate_sdbuildR
#' @inheritParams update.sdbuildR
#'
#' @returns List
#' @noRd
#'
prep_plot <- function(object, type_sim, df, constants, add_constants, vars, palette, colors, wrap_width) {
  # Get names of stocks and non-stock variables
  names_df <- get_names(object)

  # Validate variable parameters
  validate_plot_params(vars = vars)

  if (!is.null(vars)) {
    vars <- unique(vars)
  }

  # If vars is specified and contains constants, enable add_constants
  if (!is.null(vars)) {
    constant_names <- names_df[
      names_df[["type"]] %in% c("constant", "lookup"),
      "name"
    ]
    vars_constants <- intersect(constant_names, vars)
    constants_not_in_vars <- setdiff(constant_names, vars_constants)

    add_constants <- length(vars_constants) > 0

    if (add_constants) {
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
      }
    }
  }

  # Add constants to dataframe if requested
  if (add_constants && length(constants) > 0) {
    result <- prep_constants(df, constants, names_df, type_sim = type_sim)
    df <- result$df
    names_df <- result$names_df
  }

  # Filter to specified variables if provided
  if (!is.null(vars)) {
    result <- filter_variables(vars, names_df, df, type_sim = type_sim)
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

  return(list(
    highlight_names = highlight_names,
    nonhighlight_names = nonhighlight_names,
    df_highlight = df_highlight,
    df_nonhighlight = df_nonhighlight,
    colors = colors
  ))
}


#' Plot timeseries of simulation
#'
#' Visualize simulation results of a stock-and-flow model. Plot the evolution of stocks over time, with the option of also showing other model variables.
#'
#' @param x Output of [`simulate()`][simulate.sdbuildR()].
#' @param add_constants If TRUE, include constants in plot. Defaults to FALSE.
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
#' plot(sim, add_constants = TRUE)
#'
plot.simulate_sdbuildR <- function(x,
                                   add_constants = FALSE,
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
      "No simulation data available.",
      ">" = "Run a simulation first with {.fn simulate}."
    ))
  }

  check_simulate_sdbuildR(x)

  if (!x[["success"]]) {
    cli::cli_abort(c(
      "Simulation failed.",
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
    x[["object"]], "sim", x[["df"]], x[["constants"]], add_constants,
    vars, palette, colors, wrap_width
  )
  highlight_names <- out[["highlight_names"]]
  nonhighlight_names <- out[["nonhighlight_names"]]
  df_highlight <- out[["df_highlight"]]
  df_nonhighlight <- out[["df_nonhighlight"]]
  colors <- out[["colors"]]
  x_col <- "time"

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


  return(pl)
}


#' Plot timeseries of ensemble
#'
#' Visualize ensemble simulation results of a stock-and-flow model. Either summary statistics or individual trajectories can be plotted. When multiple conditions j are specified, a grid of subplots is plotted. See [ensemble()] for examples.
#'
#' @param x Output of [ensemble()].
#' @param type Type of plot. Either "summary" for a summary plot with mean or median lines and confidence intervals, or "sims" for individual simulation trajectories with mean or median lines. Defaults to "summary".
#' @param i Indices of the individual trajectories to plot if type = "sims". Defaults to 1:10. Including a high number of trajectories will slow down plotting considerably.
#' @param j Indices of the condition to plot. Defaults to 1:9. If only one condition is specified, the plot will not be a grid of subplots.
#' @param nrows Number of rows in the plot grid. Defaults to ceiling(sqrt(n_conditions)).
#' @param margin Margin between subplots. Either a single numeric or a vector of length four(left, right, top, bottom). See `?plotly::subplot()` for more details. Defaults to 0.05.
#' @param shareX If TRUE, share the x-axis across subplots. Defaults to TRUE.
#' @param shareY If TRUE, share the y-axis across subplots. Defaults to TRUE.
#' @param palette Colour palette. Must be one of hcl.pals().
#' @param colors Vector of colours. If NULL, the color palette will be used. If specified, will override palette. The number of colours must be equal to the number of variables in the simulation data frame. Defaults to NULL.
#' @param font_family Font family. Defaults to "Times New Roman".
#' @param font_size Font size. Defaults to 16.
#' @param wrap_width Width of text wrapping for labels. Must be an integer. Defaults to 25.
#' @param showlegend Whether to show legend. Must be TRUE or FALSE. Defaults to TRUE.
#' @param j_labels Whether to plot labels indicating the condition of the subplot.
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
                                   type = c("summary", "sims")[1],
                                   i = seq(1, min(c(x[["n"]], 10))),
                                   j = seq(1, min(c(x[["n_conditions"]], 9))),
                                   vars = NULL,
                                   add_constants = FALSE,
                                   nrows = ceiling(sqrt(max(j))),
                                   margin = .05,
                                   shareX = TRUE,
                                   shareY = TRUE,
                                   palette = "Dark 2",
                                   colors = NULL,
                                   font_family = "Times New Roman",
                                   font_size = 16,
                                   wrap_width = 25,
                                   showlegend = TRUE,
                                   j_labels = TRUE,
                                   central_tendency = c("mean", "median", FALSE)[1],
                                   central_tendency_width = 3,
                                   ...) {
  if (missing(x)) {
    cli::cli_abort(c(
      "No simulation data available.",
      ">" = "Generate an ensemble first with {.fn ensemble}."
    ))
  }

  # Check whether it is an sdbuildR object
  if (!inherits(x, "ensemble_sdbuildR")) {
    cli::cli_abort(c(
      "Invalid object class.",
      "x" = "This is not an object of class {.cls ensemble_sdbuildR}.",
      ">" = "Generate an ensemble simulation with {.fn ensemble}."
    ))
  }

  if (isFALSE(x[["success"]])) {
    cli::cli_abort(c(
      "Ensemble simulation failed.",
      ">" = "Check your model specification and try again."
    ))
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

  if (!is.logical(j_labels)) {
    cli::cli_abort(c(
      "Invalid {.arg j_labels} argument.",
      "x" = "The {.arg j_labels} argument must be {.cls logical}."
    ))
  }

  # Check type
  type <- trimws(tolower(type))
  type <- ifelse(type == "sim", "sims", type)
  if (!type %in% c("summary", "sims")) {
    cli::cli_abort(c(
      "Invalid {.arg type} value.",
      "x" = "The {.arg type} argument must be {.code 'summary'} or {.code 'sims'}."
    ))
  }

  # Check central tendency
  if (!isFALSE(central_tendency)) {
    central_tendency <- trimws(tolower(central_tendency))
    if (!central_tendency %in% c("mean", "median")) {
      cli::cli_abort(c(
        "Invalid {.arg central_tendency} value.",
        "x" = "The {.arg central_tendency} argument must be {.code 'mean'}, {.code 'median'}, or {.code FALSE}."
      ))
    }
  }

  # Get passed arguments
  passed_arg <- names(as.list(match.call())[-1])

  dots <- list(...)

  # Build default subtitle based on plot type and central tendency
  default_sub <- if (type == "summary") {
    paste0(
      ifelse(isFALSE(central_tendency), "",
        stringr::str_to_title(central_tendency)
      ), " with [",
      min(x[["quantiles"]]), ", ", max(x[["quantiles"]]),
      "] confidence interval of ", x[["n"]], " simulation",
      ifelse(x[["n"]] == 1, "", "s")
    )
  } else if (type == "sims") {
    paste0(
      ifelse(isFALSE(central_tendency), "",
        stringr::str_to_title(central_tendency)
      ),
      " with ", length(i), "/", x[["n"]], " simulation",
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
    alpha = 0.3
  ))
  main <- params$main
  xlab <- params$xlab
  ylab <- params$ylab
  sub <- params$sub
  alpha <- params$alpha

  # Append subtitle to main title
  main <- paste0(main, "\n<span style='font-size:", font_size, "px;'>", sub, "</span>")

  if (!is.null(x[["summary"]])) {
    summary_df <- x[["summary"]]
  } else {
    cli::cli_abort(c(
      "x" = "The ensemble object does not contain summary statistics."
    ))
  }

  # Validate j index
  if ("j" %in% passed_arg) {
    .check_j_index(j, x[["n_conditions"]])
  }

  # Ensure there aren't more rows than j
  nrows <- min(nrows, length(j))

  # Whether to create subplots or not
  create_subplots <- length(j) > 1

  # To plot individual simulation trajectories, extract df
  if (type == "sims") {
    if (!is.null(x[["df"]])) {
      df <- x[["df"]]

      # Validate and apply i filter
      if ("i" %in% passed_arg) {
        .check_i_index(i, x[["n"]])
      }

      # Filter condition
      df <- df[df[["i"]] %in% i, , drop = FALSE]
    } else {
      cli::cli_abort(c(
        "No simulation data available.",
        "x" = "Individual simulation data is required for {.code type = 'sims'}.",
        ">" = "Run {.fn ensemble} with {.code return_sims = TRUE}."
      ))
    }
  } else if (type == "summary") {
    if ("i" %in% passed_arg) {
      cli::cli_inform(c(
        "i" = "The {.arg i} argument is ignored when {.code type = 'summary'}.",
        ">" = "Set {.code type = 'sims'} to plot individual trajectories."
      ))
    }

    df <- NULL
  }

  # Prepare for plotting
  out <- prep_plot(x[["object"]], "ensemble", summary_df,
    constants = x[["constants"]][["summary"]], add_constants = add_constants,
    vars = vars, palette = palette, colors = colors, wrap_width = wrap_width
  )
  # highlight_names <- out[["highlight_names"]]
  # nonhighlight_names <- out[["nonhighlight_names"]]
  summary_df_highlight <- out[["df_highlight"]]
  summary_df_nonhighlight <- out[["df_nonhighlight"]]
  colors <- out[["colors"]]

  if (type == "sims") {
    out <- prep_plot(x[["object"]], "ensemble", df,
      constants = x[["constants"]][["df"]], add_constants = add_constants,
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


  # Plot
  if (!create_subplots) {
    j_idx <- 1
    j_name <- j[j_idx]
    pl <- plot_ensemble_helper(
      j_idx = j_idx,
      j_name = j_name, j = j, j_labels = j_labels,
      type = type,
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
      alpha = alpha
    )
  } else {
    # Create a list of plotly objects for each condition
    pl_list <- list()
    for (j_idx in seq_along(j)) {
      j_name <- j[j_idx]

      pl_list[[j_idx]] <- plot_ensemble_helper(
        j_idx = j_idx,
        j_name = j_name, j = j, j_labels = j_labels,
        type = type,
        create_subplots = create_subplots,
        summary_df_highlight = summary_df_highlight[summary_df_highlight[["j"]] == j_name, , drop = FALSE],
        summary_df_nonhighlight = summary_df_nonhighlight[summary_df_nonhighlight[["j"]] == j_name, , drop = FALSE],
        df_highlight = df_highlight[df_highlight[["j"]] == j_name, , drop = FALSE],
        df_nonhighlight = df_nonhighlight[df_nonhighlight[["j"]] == j_name, , drop = FALSE],
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
        alpha = alpha
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

  return(pl)
}


#' Helper function to plot ensemble simulation results
#'
#' @param j_idx Index of the condition to plot.
#' @param j_name Name of the condition to plot.
#' @param j Index of the condition to plot. Used to determine whether to show the legend.
#' @param type Type of plot. Must be one of "summary" or "sims". Defaults to "summary". If "summary", the plot will show the mean and confidence intervals of the simulation results. If "sims", the plot will show all individual simulation runs in i.
#' @param create_subplots If TRUE, create subplots for each condition. If FALSE, plot all conditions in one plot.
#' @param summary_df_highlight data.frame with summary statistics of the ensemble simulation results (stocks). Must contain columns "j", "variable", "mean", and confidence interval columns (e.g., "q0.025", "q0.975").
#' @param summary_df_nonhighlight data.frame with summary statistics of the ensemble simulation results (non-stocks). Must contain columns "j", "variable", "mean", and confidence interval columns (e.g., "q0.025", "q0.975").
#' @param df_highlight data.frame with individual simulation results (stocks). Must contain columns "i", "j", "variable", and "value". Only used if type = "sims".
#' @param df_nonhighlight data.frame with individual simulation results (non-stocks). Must contain columns "i", "j", "variable", and "value". Only used if type = "sims".
#' @param q_low Column name for the lower bound of the confidence interval (e.g., "q0.025").
#' @param q_high Column name for the upper bound of the confidence interval (e.g., "q0.975").
#' @param mode Plotting mode. Either "lines" if there are multiple time points or "markers" for a single time point.
#' @param colors Vector of colours. If NULL, the color palette will be used. If specified, will override palette. The number of colours must be equal to the number of variables in the simulation data frame. Defaults to NULL.
#' @param dots List of additional parameters passed to the plotly functions.
#' @param main Main title of the plot. Defaults to the name of the stock-and-flow model and the number of simulations.
#' @param xlab Label on x-axis.
#' @param ylab Label on y-axis.
#' @param alpha Opacity of the confidence bands or individual trajectories. Defaults to 0.3.
#' @inheritParams plot.ensemble_sdbuildR
#'
#' @returns Plotly object
#' @noRd
plot_ensemble_helper <- function(j_idx, j_name, j, j_labels,
                                 type, create_subplots,
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
                                 font_family, font_size, alpha) {
  # Only show legend if it's the last subplot
  showlegend <- ifelse(j_name != max(j), FALSE, showlegend)
  x_col <- "time"

  plot_highlight <- nrow(summary_df_highlight) > 0
  plot_nonhighlight <- nrow(summary_df_nonhighlight) > 0
  nr_var <- length(colors)

  # Initialize plotly object
  pl <- plotly::plot_ly()

  if (type == "summary") {
    if (mode == "lines") {
      if (plot_nonhighlight) {
        pl <- plotly::add_ribbons(pl,
          data = summary_df_nonhighlight,
          x = ~ get(x_col),
          ymin = ~ get(q_low),
          ymax = ~ get(q_low),
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
          x = ~ get(x_col),
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
  } else if (type == "sims") {
    # Add traces for non-stock variables (visible = "legendonly")
    if (plot_nonhighlight) {
      pl <- plotly::add_trace(pl,
        data = df_nonhighlight,
        x = ~ get(x_col),
        y = ~value,
        color = ~variable,
        legendgroup = ~variable,
        type = "scatter",
        mode = mode,
        opacity = alpha,
        colors = colors,
        split = ~ interaction(variable, i), # ensures each line is treated separately
        showlegend = FALSE,
        visible = "legendonly"
      )
    }

    if (plot_highlight) {
      pl <- plotly::add_trace(pl,
        data = df_highlight,
        x = ~ get(x_col),
        y = ~value,
        color = ~variable,
        legendgroup = ~variable,
        type = "scatter",
        mode = mode,
        opacity = alpha,
        colors = colors,
        split = ~ interaction(variable, i), # ensures each line is treated separately
        showlegend = FALSE,
        # Add traces for stock variables (visible = TRUE)
        visible = TRUE
      )
    }
  }

  # When central_tendency = FALSE, we still want a legend
  if (isFALSE(central_tendency)) {
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
  if (mode == "lines") {
    if (plot_nonhighlight) {
      pl <- plotly::add_trace(pl,
        data = summary_df_nonhighlight,
        x = ~ get(x_col),
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
        x = ~ get(x_col),
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
  } else if (mode == "markers" && type == "summary") {
    if (plot_nonhighlight) {
      pl <- plotly::add_trace(pl,
        data = summary_df_nonhighlight,
        x = ~ get(x_col),
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
        x = ~ get(x_col),
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
  } else if (mode == "markers" && type == "sims") {
    if (plot_nonhighlight) {
      pl <- plotly::add_trace(pl,
        data = summary_df_nonhighlight,
        x = ~ get(x_col),
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
        x = ~ get(x_col),
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


  # Customize layout
  theme <- plotly_theme(
    font_family = font_family,
    font_size = font_size
  )

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
  if (create_subplots && j_labels) {
    pl <- plotly::layout(pl,
      annotations = list(
        list(
          text = paste0("j = ", j_name),
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
#' Visualize the simulation(s) used during [verify()]. Requires that `verify()`
#' was called with `return_sims = TRUE`. Each condition `j` is displayed as a
#' subplot; when `n > 1` (robustness testing), individual trajectories are
#' overlaid within each subplot.
#'
#' @param x Output of [verify()] with `return_sims = TRUE`.
#' @param nr Integer vector of test numbers to plot. Defaults to the first 9
#'   available tests. Combines with `label` and `status` as AND intersection.
#' @param i Integer vector. Trajectories to plot when `n > 1`. Defaults to the
#'   first 10 runs.
#' @param label Character vector of regex patterns for partial, case-insensitive
#'   label matching. A test is included if its label matches any pattern.
#' @param ignore_case Logical; whether `label` matching is case-insensitive.
#'   Default `TRUE`.
#' @param nrows Number of subplot rows. Defaults to `ceiling(sqrt(n_conditions))`.
#' @param shareX Share the x-axis across subplots. Defaults to `TRUE`.
#' @param shareY Share the y-axis across subplots. Defaults to `TRUE`.
#' @param palette Colour palette (see `hcl.pals()`). Defaults to `"Dark 2"`.
#' @param colors Vector of colours overriding `palette`. Defaults to `NULL`.
#' @param font_family Font family. Defaults to `"Times New Roman"`.
#' @param font_size Font size. Defaults to `16`.
#' @param wrap_width Label wrap width. Defaults to `25`.
#' @param showlegend Whether to show the legend. Defaults to `TRUE`.
#' @param alpha Opacity for individual trajectories (when `n > 1`). Defaults
#'   to `0.4`.
#' @param ... Additional arguments passed to [plot.simulate_sdbuildR()].
#' @inheritParams as.data.frame.verify_sdbuildR
#'
#' @returns A plotly object.
#' @export
#' @concept unitTest
#' @method plot verify_sdbuildR
#' @seealso [verify()], [plot.simulate_sdbuildR()], [plot.ensemble_sdbuildR()]
#'
#' @examples
#' sfm <- sdbuildR("SIR") |>
#'   unit_test(expr = all(Susceptible >= 0))
#' res <- verify(sfm, return_sims = TRUE)
#' plot(res)
plot.verify_sdbuildR <- function(x,
                                  nr = NULL,
                                  i = NULL,
                                  label = NULL,
                                  ignore_case = TRUE,
                                  status = c("pass", "fail", "error", "skip"),
                                  nrows = NULL,
                                  shareX = TRUE,
                                  shareY = TRUE,
                                  palette = "Dark 2",
                                  colors = NULL,
                                  font_family = "Times New Roman",
                                  font_size = 16,
                                  wrap_width = 25,
                                  showlegend = TRUE,
                                  alpha = 0.4,
                                  ...) {
  if (is.null(x[["sims"]])) {
    cli::cli_abort(c(
      "No simulation data available.",
      ">" = "Re-run {.fn verify} with {.code return_sims = TRUE}."
    ))
  }

  n_runs       <- x[["n"]]
  available_nr <- if (is.null(x[["test_indices"]])) seq_along(x[["results"]]) else x[["test_indices"]]

  # label filter (narrows available_nr)
  if (!is.null(label)) {
    if (!is.character(label) || length(label) == 0L || any(is.na(label)) || any(!nzchar(label)))
      cli::cli_abort(c(
        "Invalid {.arg label}.",
        "x" = "{.arg label} must be a character vector of non-empty regex pattern(s)."
      ))
    result_labels <- vapply(x[["results"]], function(r) r[["label"]], character(1L))
    hits <- Reduce("|", lapply(label, grepl, x = result_labels, ignore.case = ignore_case))
    available_nr <- available_nr[hits]
    if (length(available_nr) == 0)
      cli::cli_warn(c(
        "No tests matched pattern{?s} {.val {label}}.",
        "i" = "Re-run without {.arg label} to see all tests."
      ))
  }

  # status filter (narrows available_nr, after label)
  if (!is.null(status)) {
    status <- clean_status(status)
    all_nrs      <- if (is.null(x[["test_indices"]])) seq_along(x[["results"]]) else x[["test_indices"]]
    all_statuses <- vapply(x[["results"]], function(r) r[["status"]], character(1L))
    status_nrs   <- all_nrs[all_statuses %in% status]
    available_nr <- available_nr[available_nr %in% status_nrs]
    if (length(available_nr) == 0)
      cli::cli_warn("No tests with status {.val {status}} to plot.")
  }

  # Defaults (applied to filtered available_nr)
  if (is.null(nr)) nr <- head(available_nr, 9L)
  if (is.null(i))  i  <- seq_len(min(n_runs, 10L))

  if (!all(nr %in% available_nr)) {
    bad <- nr[!nr %in% available_nr]
    cli::cli_abort(c(
      "Test number{?s} not found: {.val {bad}}.",
      "i" = "Available: {.val {available_nr}}."
    ))
  }
  .check_i_index(i, n_runs)

  # Map nr -> unique condition indices (deduplicates tests sharing the same simulation)
  keep_pos     <- which(available_nr %in% nr)
  cond_indices <- unique(unname(x[["j"]][keep_pos]))

  validate_plot_params(
    showlegend = showlegend, vars = NULL,
    palette = palette, colors = colors,
    font_family = font_family, font_size = font_size,
    wrap_width = wrap_width
  )

  dots <- list(...)

  time_unit <- x[["object"]][["sim_settings"]][["time_units"]]
  params <- extract_plot_params(dots, defaults = list(
    main = paste0("Unit Tests: ", x[["object"]][["meta"]][["name"]]),
    xlab = paste0("Time (", time_unit, ")"),
    ylab = ""
  ))

  if (is.null(nrows)) nrows <- ceiling(sqrt(length(cond_indices)))
  nrows <- min(nrows, length(cond_indices))
  create_subplots <- length(cond_indices) > 1L
  mode <- "lines"

  # Build one plotly per condition
  pl_list <- lapply(seq_along(cond_indices), function(j_seq) {
    ji <- cond_indices[[j_seq]]
    this_showlegend <- showlegend && (j_seq == length(cond_indices))

    # Combine selected runs into a single long-format df with i column
    runs_ji <- x[["sims"]][[ji]]
    df_combined <- do.call(rbind, lapply(seq_along(i), function(i_seq) {
      ii <- i[[i_seq]]
      sim <- runs_ji[[ii]]
      if (!sim[["success"]]) return(NULL)
      cbind(sim[["df"]], i = ii)
    }))

    if (is.null(df_combined) || nrow(df_combined) == 0) {
      return(plotly::plot_ly())
    }

    out <- prep_plot(
      x[["object"]], "sim", df_combined, constants = NULL,
      add_constants = FALSE, vars = NULL,
      palette = palette, colors = colors, wrap_width = wrap_width
    )
    df_highlight    <- out[["df_highlight"]]
    df_nonhighlight <- out[["df_nonhighlight"]]
    plot_colors     <- out[["colors"]]

    pl <- plotly::plot_ly()

    # Plot nonhighlight variables (flows/auxiliaries - hidden by default)
    if (!is.null(df_nonhighlight) && nrow(df_nonhighlight) > 0) {
      pl <- plotly::add_trace(pl,
        data = df_nonhighlight,
        x = ~time, y = ~value,
        color = ~variable, legendgroup = ~variable,
        type = "scatter", mode = mode,
        opacity = alpha, colors = plot_colors,
        split = ~interaction(variable, i),
        showlegend = FALSE, visible = "legendonly"
      )
    }

    # Plot highlight variables (stocks - visible by default)
    if (!is.null(df_highlight) && nrow(df_highlight) > 0) {
      pl <- plotly::add_trace(pl,
        data = df_highlight,
        x = ~time, y = ~value,
        color = ~variable, legendgroup = ~variable,
        type = "scatter", mode = mode,
        opacity = alpha, colors = plot_colors,
        split = ~interaction(variable, i),
        showlegend = this_showlegend, visible = TRUE
      )
    }

    theme <- plotly_theme(font_family = font_family, font_size = font_size)

    # Build "nr = N (cond)" annotation text for this condition panel
    test_nrs_ji <- available_nr[keep_pos[unname(x[["j"]][keep_pos]) == ji]]
    nr_str      <- paste(test_nrs_ji, collapse = ", ")
    r_idx       <- keep_pos[unname(x[["j"]][keep_pos]) == ji][[1L]]
    conds       <- x[["results"]][[r_idx]][["conditions"]]
    cond_str    <- if (length(conds) == 0) "" else
      paste(names(conds), unlist(conds), sep = " = ", collapse = ", ")
    annotation_text <- if (nzchar(cond_str))
      paste0("nr = ", nr_str, " (", cond_str, ")")
    else
      paste0("nr = ", nr_str)

    pl <- plotly::layout(pl,
      annotations = list(list(
        text      = annotation_text,
        font      = list(family = font_family, size = ceiling(font_size * 0.75)),
        bgcolor   = "white",
        xref      = "paper", yref = "paper",
        xanchor   = "center", yanchor = "top",
        x = 0.5, y = 1,
        showarrow = FALSE
      )),
      xaxis = list(title = params$xlab, font = list(size = font_size)),
      yaxis = list(title = params$ylab, font = list(size = font_size)),
      legend = theme$legend,
      font = theme$font,
      margin = theme$margin,
      showlegend = this_showlegend
    )

    pl
  })

  if (!create_subplots) {
    return(plotly::layout(pl_list[[1L]], title = params$main))
  }

  plotly::subplot(
    pl_list,
    nrows = nrows,
    shareX = shareX,
    shareY = shareY,
    titleX = TRUE,
    titleY = TRUE
  ) |>
    plotly::layout(title = params$main)
}
