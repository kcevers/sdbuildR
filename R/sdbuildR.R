#' Create a new stock-and-flow model
#'
#' Initialize a stock-and-flow model of class [`sdbuildR`][sdbuildR]. You can
#' either create an empty stock-and-flow model or load a template from the model
#' library.
#'
#' Do not edit the object manually; this will likely lead to errors downstream.
#' Rather, use [meta()], [sim_settings()], [update()], and [custom_func()] for safe manipulation.
#'
#' @param template Name of the template to load. If `NULL`, an empty stock-and-flow
#' model will be created with default simulation parameters and a default meta.
#' If specified, `template` should be one of the available templates (case-insensitive):
#' \itemize{
#'   \item \strong{logistic_model}: Population growth with carrying capacity
#'   \item \strong{sir}: Epidemic model (Susceptible-Infected-Recovered)
#'   \item \strong{predator_prey}: Lotka-Volterra dynamics
#'   \item \strong{cusp}: Cusp catastrophe model
#'   \item \strong{crielaard2022}: Eating behavior (doi: 10.1037/met0000484)
#'   \item \strong{coffee_cup}: Temperature equilibration (Meadows)
#'   \item \strong{bank_account}: Compound interest (Meadows)
#'   \item \strong{lorenz}: Lorenz attractor (chaotic)
#'   \item \strong{rossler}: Rossler attractor (chaotic)
#'   \item \strong{vanderpol}: Van der Pol oscillator
#'   \item \strong{duffing}: Forced Duffing oscillator
#'   \item \strong{chua}: Chua's circuit (chaotic)
#'   \item \strong{jdr}: Job Demands-Resources Theory as formalized in Evers et al. (under review)
#' }
#'
#' @returns A stock-and-flow model object of class [`sdbuildR`][sdbuildR]. Its structure is based
#'  on [XML Interchange Language for System Dynamics (XMILE)](https://docs.oasis-open.org/sdbuildR/sdbuildR/v1.0/os/sdbuildR-v1.0-os.html). It is a nested list, containing:
#' \describe{
#'  \item{meta}{Meta-information about model. A list containing arguments listed in [meta()].}
#'  \item{sim_settings}{Simulation specifications. A list containing arguments listed in [sim_settings()].}
#'  \item{model}{Model variables, grouped under the variable types stock, flow, aux (auxiliaries), constant, gf (graphical functions), and func (custom functions). Each variable contains arguments as listed in [update()].}
#'  }
#'
#' Use [summary()] to run model diagnostics, [as.data.frame()] to convert to a data.frame, [plot()] to visualize.
#'
#' @export
#' @concept build
#' @seealso [update()], [meta()], [custom_func()], [sim_settings()]
#'
#' @examples sfm <- sdbuildR()
#' summary(sfm)
#'
#' \dontshow{
#' sfm <- sim_settings(sfm, save_at = 1)
#' }
#'
#' # Load a template
#' sfm <- sdbuildR("lorenz")
#' sim <- simulate(sfm)
#' plot(sim)
sdbuildR <- function(template = NULL) {
  if (!is.null(template)) {
    return(templates(template))
  }

  sfm <- new_sdbuildR()
  sfm
}


#' Create empty assemble cache
#' @returns Empty assemble cache
#' @noRd
empty_assemble <- function() {
  list(
    language = NULL,
    ordering = NULL, # Contains deps_by_name for incremental updates
    funcs = "",
    times = "",
    static = list(script = "", par_names = character(0)),
    nonneg_stocks = empty_nonneg_stocks(),
    ode = "",
    callback = "",
    run = "",
    post = "",
    intermediaries = list(),
    ensemble = list(),
    summary = NULL, # cached diagnostics result
    unit_tests = list(deps = NULL) # cached unit test dependencies (positionally indexed)
  )
}


empty_unit_tests <- function() {
  list()
}


empty_variables <- function() {
  variables_df <- data.frame(
    name = character(0),
    type = character(0),
    eqn = character(0),
    label = character(0),
    doc = character(0),
    non_negative = logical(0),
    # Flow-specific
    to = character(0),
    from = character(0),
    # Graphical function-specific (list-columns)
    source = character(0),
    interpolation = character(0),
    extrapolation = character(0),
    # Prepared equation strings (language-specific, updated in update())
    eqn_str = character(0),
    # Stock accumulation equations and names (language-specific)
    sum_eqn = character(0),
    sum_name = character(0),
    stringsAsFactors = FALSE
  )

  # Add list-columns for xpts and ypts (graphical functions)
  variables_df$xpts <- list()
  variables_df$ypts <- list()

  # Add list-columns for inflow and outflow (stocks)
  variables_df$inflow <- list()
  variables_df$outflow <- list()
  variables_df
}


get_variable_row <- function(name, type,
                             eqn = "0",
                             label = name,
                             doc = "",
                             non_negative = FALSE,
                             to = "",
                             from = "",
                             xpts = NULL,
                             ypts = NULL,
                             source = "",
                             interpolation = "linear",
                             extrapolation = "nearest") {
  row <- data.frame(
    name = name,
    type = type,
    eqn = eqn %||% "0",
    label = label %||% name,
    doc = doc %||% "",
    non_negative = non_negative %||% FALSE,
    to = if (type == "flow") (to %||% "") else "",
    from = if (type == "flow") (from %||% "") else "",
    source = if (type == "lookup") (source %||% "") else "",
    interpolation = if (type == "lookup") (interpolation %||% "linear") else "",
    extrapolation = if (type == "lookup") (extrapolation %||% "nearest") else "",
    eqn_str = "",
    sum_eqn = "",
    sum_name = "",
    stringsAsFactors = FALSE
  )

  if (type == "lookup") {
    row[["xpts"]][[1]] <- xpts %||% numeric(0)
    row[["ypts"]][[1]] <- ypts %||% numeric(0)
  } else {
    row[["xpts"]] <- list(NULL)
    row[["ypts"]] <- list(NULL)
  }

  # Add list-columns for inflow and outflow (stocks)
  row[["inflow"]] <- list(NULL)
  row[["outflow"]] <- list(NULL)

  row
}


#' Add a variable to sfm
#'
#' @param object Stock-and-flow model
#' @inheritParams update.sdbuildR
#'
#' @returns Updated object with variable added
#' @noRd
#'
add_variable_row <- function(object, name, type,
                             eqn,
                             label,
                             doc,
                             non_negative,
                             to,
                             from,
                             xpts,
                             ypts,
                             source,
                             interpolation,
                             extrapolation) {
  # Create new row
  arg <- compact_(as.list(environment()))
  if ("object" %in% names(arg)) {
    arg[["object"]] <- NULL
  }
  new_row <- do.call(get_variable_row, arg)

  # Add to variables data frame
  object[["variables"]] <- rbind(object[["variables"]], new_row)

  object
}


new_meta <- function() {
  meta_defaults <- as.list(formals(meta))
  meta_defaults <- meta_defaults[!names(meta_defaults) %in%
    c("object", "...")]
  meta_defaults[["created"]] <- Sys.time() # Manually overwrite time
  meta_defaults
}

new_sim_settings <- function() {
  spec_defaults <- as.list(formals(sim_settings))
  spec_defaults <- spec_defaults[!names(spec_defaults) %in% c("object", "...")]

  # Flat save fields: save_type discriminates mode; save_at and save_n are NULL
  # (meaning "save all dt steps") by default
  spec_defaults[["save_type"]] <- "all"
  spec_defaults["save_at"] <- list(NULL)
  spec_defaults["save_n"] <- list(NULL)
  spec_defaults
}

empty_nonneg_stocks <- function() {
  list(func_def = "", root_arg = "", check_root = "")
}

#' Create new object of class [`sdbuildR`][sdbuildR]
#'
#' @returns A stock-and-flow model of class [`sdbuildR`][sdbuildR]
#' @noRd
#'
new_sdbuildR <- function() {
  meta_defaults <- new_meta()
  spec_defaults <- new_sim_settings()

  # Create data frame for variables (all types in one data frame, including funcs)
  variables_df <- empty_variables()

  # Create list with fixed structure
  obj <- list(
    meta = meta_defaults,
    sim_settings = spec_defaults,
    variables = variables_df,
    # Cache for pre-assembled simulation components
    assemble = empty_assemble(),
    # Import metadata (NULL for programmatic models, populated for imported models)
    import_metadata = NULL,
    # User-defined unit tests; named list keyed by label
    unit_tests = empty_unit_tests()
  )

  object <- structure(obj, class = "sdbuildR")
  object <- sanitize_sdbuildR(object)
  # Ensure deterministic ordering for presentation/storage: sort by type then name
  if (nrow(object[["variables"]]) > 0) {
    type_levels <- c("stock", "flow", "constant", "aux", "lookup", "func")
    type_rank <- match(object[["variables"]][["type"]], type_levels)
    type_rank[is.na(type_rank)] <- length(type_levels) + 1
    name_lower <- tolower(object[["variables"]][["name"]])
    ord <- order(type_rank, name_lower)
    object[["variables"]] <- object[["variables"]][ord, , drop = FALSE]
    rownames(object[["variables"]]) <- NULL
  }

  object

  return(object)
}


#' Get the sources and destinations of flows
#'
#' @inheritParams update.sdbuildR
#'
#' @returns data.frame with for each flow which stock and flow to and/or from
#' @noRd
get_flow_df <- function(object) {
  check_sdbuildR(object)

  flows <- object[["variables"]][object[["variables"]][["type"]] == "flow", ]

  if (nrow(flows) == 0) {
    return(data.frame(name = character(0), to = character(0), from = character(0)))
  }

  data.frame(
    name = flows[["name"]],
    to = ifelse(is.na(flows[["to"]]), "", flows[["to"]]),
    from = ifelse(is.na(flows[["from"]]), "", flows[["from"]]),
    stringsAsFactors = FALSE
  )
}


#' Check whether object is of class [`sdbuildR`][sdbuildR]
#'
#' @inheritParams update.sdbuildR
#'
#' @returns Invisibly returns TRUE, called for side effects.
#' @noRd
check_sdbuildR <- function(object) {
  # Check whether it is an sdbuildR object
  if (!inherits(object, "sdbuildR")) {
    cli::cli_abort(c(
      "Expected object of class {.cls sdbuildR}.",
      "i" = "Create a stock-and-flow model with {.fn sdbuildR} or {.fn import_insightmaker}."
    ))
  }
  invisible(TRUE)
}


#' Validate sdbuildR class
#'
#' Pure structural validator for stock-and-flow models. Checks that the object
#' has the required fields and correct types, but does NOT modify the object.
#' Use sanitize_sdbuildR() to apply defaults and fix invalid state.
#'
#' @inheritParams update.sdbuildR
#'
#' @returns The stock-and-flow model, unchanged (invisibly)
#' @noRd
#'
validate_sdbuildR <- function(object) {
  check_sdbuildR(object)

  # Check variables data frame has required columns
  if (nrow(object[["variables"]]) > 0) {
    required_var_cols <- colnames(empty_variables())
    missing_cols <- setdiff(required_var_cols, colnames(object[["variables"]]))
    if (length(missing_cols) > 0) {
      cli::cli_warn(c("!" = "Variables is missing columns: {.field {missing_cols}}"))
    }

    # Warn about flow connections to non-stocks (but don't fix)
    flows <- object[["variables"]][object[["variables"]][["type"]] == "flow", ]
    non_stock_names <- object[["variables"]][object[["variables"]][["type"]] != "stock", "name"]

    if (nrow(flows) > 0) {
      flows_to_invalid <- !is.na(flows[["to"]]) & flows[["to"]] != "" & flows[["to"]] %in% non_stock_names
      if (any(flows_to_invalid)) {
        for (i in which(flows_to_invalid)) {
          flow_name <- flows[i, "name"]
          to_name <- flows[i, "to"]
          cli::cli_warn(c(
            "{.code {flow_name}} flows to non-stock variable {.code {to_name}}.",
            "x" = "{.code {to_name}} is not a stock."
          ))
        }
      }

      flows_from_invalid <- !is.na(flows[["from"]]) & flows[["from"]] != "" & flows[["from"]] %in% non_stock_names
      if (any(flows_from_invalid)) {
        for (i in which(flows_from_invalid)) {
          flow_name <- flows[i, "name"]
          from_name <- flows[i, "from"]
          cli::cli_warn(c(
            "{.code {flow_name}} flows from non-stock variable {.code {from_name}}.",
            "x" = "{.code {from_name}} is not a stock."
          ))
        }
      }

      flows_same <- !is.na(flows[["to"]]) & !is.na(flows[["from"]]) &
        flows[["to"]] == flows[["from"]] & flows[["to"]] != ""
      if (any(flows_same)) {
        for (i in which(flows_same)) {
          flow_name <- flows[i, "name"]
          same_name <- flows[i, "to"]
          cli::cli_warn(c(
            "{.code {flow_name}} flows to and from the same variable.",
            "x" = "{.code {same_name}} is both source and target."
          ))
        }
      }
    }
  }

  invisible(object)
}


#' Sanitize sdbuildR object
#'
#' Applies defaults and fixes invalid state in the stock-and-flow model.
#' Called after modifier functions to ensure the object is in a consistent state.
#'
#' @inheritParams update.sdbuildR
#'
#' @returns A stock-and-flow model of class [`sdbuildR`][sdbuildR]
#' @noRd
#'
sanitize_sdbuildR <- function(object) {
  check_sdbuildR(object)

  # Sanitize variables data frame
  if (nrow(object[["variables"]])) {
    # Migrate old "gf" type to "lookup"
    idx_gf <- object[["variables"]][["type"]] == "gf"
    if (any(idx_gf)) {
      object[["variables"]][idx_gf, "type"] <- "lookup"
    }

    # Ensure label is set (defaults to name if missing)
    idx_missing_label <- !vapply(object[["variables"]][["label"]], is_defined, logical(1))
    if (any(idx_missing_label)) {
      object[["variables"]][idx_missing_label, "label"] <- object[["variables"]][idx_missing_label, "name"]
    }

    # Fix flows: ensure to and from only refer to stocks
    flows <- object[["variables"]][object[["variables"]][["type"]] == "flow", ]
    non_stock_names <- object[["variables"]][object[["variables"]][["type"]] != "stock", "name"]

    if (nrow(flows)) {
      # Fix invalid 'to'
      flows_to_invalid <- !is.na(flows[["to"]]) & flows[["to"]] != "" & flows[["to"]] %in% non_stock_names
      if (any(flows_to_invalid)) {
        for (i in which(flows_to_invalid)) {
          flow_name <- flows[i, "name"]
          to_name <- flows[i, "to"]
          cli::cli_warn(c(
            "{.code {flow_name}} flows to non-stock {.code {to_name}}.",
            ">" = "Removed {.code {to_name}} from {.arg to}."
          ))
          object[["variables"]][object[["variables"]][["name"]] == flow_name, "to"] <- ""
        }
      }

      # Fix invalid 'from'
      flows_from_invalid <- !is.na(flows[["from"]]) & flows[["from"]] != "" & flows[["from"]] %in% non_stock_names
      if (any(flows_from_invalid)) {
        for (i in which(flows_from_invalid)) {
          flow_name <- flows[i, "name"]
          from_name <- flows[i, "from"]
          cli::cli_warn(c(
            "{.code {flow_name}} flows from non-stock {.code {from_name}}.",
            ">" = "Removed {.code {from_name}} from {.arg from}."
          ))
          object[["variables"]][object[["variables"]][["name"]] == flow_name, "from"] <- ""
        }
      }

      # Fix to and from being the same
      flows_invalid <- !is.na(flows[["to"]]) & !is.na(flows[["from"]]) &
        flows[["to"]] == flows[["from"]] & flows[["to"]] != ""
      if (any(flows_invalid)) {
        for (i in which(flows_invalid)) {
          flow_name <- flows[i, "name"]
          same_name <- flows[i, "from"]
          cli::cli_warn(c(
            "{.code {flow_name}} flows to and from the same variable {.code {same_name}}.",
            ">" = "Removed {.code {same_name}} from {.arg from}."
          ))
          object[["variables"]][object[["variables"]][["name"]] == flow_name, "from"] <- ""
        }
      }
    }
  }

  # Ensure deterministic ordering for presentation/storage: sort by type then name
  if (nrow(object[["variables"]]) > 0) {
    type_levels <- c("stock", "flow", "constant", "aux", "lookup", "func")
    type_rank <- match(object[["variables"]][["type"]], type_levels)
    type_rank[is.na(type_rank)] <- length(type_levels) + 1
    name_lower <- tolower(object[["variables"]][["name"]])
    ord <- order(type_rank, name_lower)
    object[["variables"]] <- object[["variables"]][ord, , drop = FALSE]
    rownames(object[["variables"]]) <- NULL
  }

  object
}


#' Modify meta of stock-and-flow model
#'
#' The meta of a stock-and-flow model contains metadata about the model, such as the name, author, and version. Modify the meta of an existing model with standard or custom properties.
#'
#' @inheritParams update.sdbuildR
#' @param name Model name. Defaults to "My Model".
#' @param caption Model description. Defaults to "My Model Description".
#' @param created Date the model was created. Defaults to Sys.time().
#' @param author Creator of the model. Defaults to "Me".
#' @param version Model version. Defaults to "1.0".
#' @param URL URL associated with model. Defaults to "".
#' @param doi DOI associated with the model. Defaults to "".
#' @param ... Optional other entries to add to the meta.
#'
#' @returns A stock-and-flow model object of class [`sdbuildR`][sdbuildR]
#' @concept build
#' @export
#'
#' @examples
#' sfm <- sdbuildR() |>
#'   meta(
#'     name = "My first model",
#'     caption = "This is my first model",
#'     author = "Kyra Evers",
#'     version = "1.1"
#'   )
meta <- function(object, name = "My Model", caption = "My Model Description",
                 created = Sys.time(), author = "Me", version = "1.0", URL = "", doi = "", ...) {
  # Basic check
  if (missing(object)) {
    missing_arg("object")
  }

  check_sdbuildR(object)

  argg <- list(...)
  if (!missing(name)) argg$name <- name
  if (!missing(caption)) argg$caption <- caption
  if (!missing(created)) argg$created <- created
  if (!missing(author)) argg$author <- author
  if (!missing(version)) argg$version <- version
  if (!missing(URL)) argg$URL <- URL
  if (!missing(doi)) argg$doi <- doi

  object[["meta"]] <- utils::modifyList(object[["meta"]], argg)

  object <- sanitize_sdbuildR(object)

  object
}


#' Report whether any names were changed
#'
#' @param old_names Vector with old names
#' @param new_names Vector with new names
#'
#' @returns Returns `NULL`, called for side effects
#' @noRd
report_name_change <- function(old_names, new_names) {
  # Warning if specified name changed
  idx <- !is.na(old_names) & !is.na(new_names) & old_names != new_names
  if (any(idx)) {
    n <- sum(idx)
    cli::cli_warn(c(
      "{cli::qty(n)}{?A name was/Names were} changed for syntactic validity or uniqueness.",
      "i" = paste0(
        paste0("{.val ", old_names[idx], "} \u2192 {.code ", new_names[idx], "}"),
        collapse = ", "
      )
    ))
  }

  return(invisible())
}


#' Get possible variable properties per building block type
#'
#' @returns List with default properties per building block type
#' @noRd
#'
get_building_block_prop <- function() {
  list(
    "stock" = c(
      "name", "type", "eqn", "label", "doc",
      "non_negative"
    ),
    "flow" = c(
      "name", "type", "eqn", "to", "from", "label", "doc",
      "non_negative"
    ),
    "constant" = c(
      "name", "type", "eqn", "label", "doc",
      "non_negative"
    ),
    "aux" = c(
      "name", "type", "eqn", "label", "doc",
      "non_negative"
    ),
    "lookup" = c("name", "type", "label", "xpts", "ypts", "source", "interpolation", "extrapolation", "doc"),
    "func" = c("name", "type", "eqn", "label", "doc")
  )
}


#' Convert stock-and-flow model to data frame
#'
#' Create a data frame with properties of all model variables and functions. Specify the variable types, variable names, and/or properties to get a subset of the data frame.
#'
#' @inheritParams plot.sdbuildR
#' @param name Variable names to retain in the data frame. Defaults to `NULL` to include all variables.
#' @param type Variable types to retain in the data frame. Must be one or more of 'stock', 'flow', 'constant', 'aux', 'gf', or 'func'. Defaults to `NULL` to include all types.
#' @param properties Variable properties to retain in the data frame. Defaults to `NULL` to include all properties.
#' @param row.names `NULL` or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional Ignored parameter.
#'
#' @returns A data.frame with one row per model component.
#'   Common columns include \code{type} (component type), \code{name} (variable name),
#'   \code{eqn} (equation), and \code{label} (descriptive label). Additional columns may include \code{to}, \code{from},
#'   \code{non_negative}, and others depending on variable types. The exact columns returned
#'   depend on the \code{type} and \code{properties} arguments. Returns an empty data.frame
#'   if no components match the filters.
#' @export
#' @concept build
#' @method as.data.frame sdbuildR
#'
#' @examples as.data.frame(sdbuildR("SIR"))
#'
#' # Only show stocks
#' as.data.frame(sdbuildR("SIR"), type = "stock")
#'
#' # Only show equation and label
#' as.data.frame(sdbuildR("SIR"), properties = c("eqn", "label"))
#'
as.data.frame.sdbuildR <- function(x,
                                   row.names = NULL, optional = FALSE,
                                   name = NULL, type = NULL,
                                   properties = NULL, ...) {
  name <- .expr_to_char(rlang::enexpr(name))
  check_sdbuildR(x)
  sfm <- x
  rm(x)

  # Check for mutually exclusive parameters
  if (!is.null(name) && !is.null(type)) {
    cli::cli_warn(c("!" = "Both {.arg name} and {.arg type} specified; ignoring {.arg type} and using {.arg name} only."))
    type <- NULL
  }

  # Validate and clean parameters
  if (!is.null(name)) {
    .validate_name_arg(name, arg_name = "name")
  }

  # Only keep specified types
  if (!is.null(type)) {
    type <- .validate_type_arg(type, arg_name = "type")

    if (length(type) == 0) {
      cli::cli_abort(c("x" = "At least one {.arg type} must be specified"))
    }
  }

  df <- data.frame()

  # Add model variables to data frame - already in data frame format!
  core_types <- .sdbuildR_types()
  if ((is.null(type) || any(core_types %in% type)) && nrow(sfm[["variables"]]) > 0) {
    var_df <- sfm[["variables"]]

    # Filter by type if specified
    if (!is.null(type)) {
      var_types <- type[type %in% core_types]
      var_df <- var_df[var_df[["type"]] %in% var_types, , drop = FALSE]
    }

    # Convert list-columns to character strings for display
    if (nrow(var_df) > 0) {
      gf_idx <- var_df[["type"]] == "lookup"
      if (any(gf_idx)) {
        var_df[gf_idx, "xpts"] <- vapply(var_df[gf_idx, "xpts"], function(x) {
          paste0(x, collapse = ", ")
        }, character(1))
        var_df[gf_idx, "ypts"] <- vapply(var_df[gf_idx, "ypts"], function(x) {
          paste0(x, collapse = ", ")
        }, character(1))
      }
    }

    df <- bind_rows_(df, var_df)
  }

  if (nrow(df) == 0) {
    return(df)
  }

  # Only keep specified names
  if (!is.null(name)) {
    # Clean names
    name <- Filter(nzchar, unique(name))

    if (length(name) == 0) {
      cli::cli_abort(c("x" = "At least one {.arg name} must be specified"))
    }

    # Check if names exist
    idx_exist <- name %in% df[["name"]]
    if (!all(idx_exist)) {
      missing_names <- name[!idx_exist]
      cli::cli_abort(c(
        "Variable{cli::qty(length(missing_names))}{?s} not found in model.",
        "x" = "{.code {missing_names}} {cli::qty(length(missing_names))}{?does/do} not exist."
      ))
    }
    df <- df[df[["name"]] %in% name, , drop = FALSE]
    if (nrow(df) == 0) {
      return(df)
    }
  }

  # Only keep columns that correspond to update() parameters
  allowed_props <- names(formals(get_variable_row))
  df <- df[, intersect(allowed_props, names(df)), drop = FALSE]

  # Only keep specified properties
  if (!is.null(properties)) {
    # Check if properties exist
    properties <- Filter(nzchar, unique(tolower(properties)))
    if (length(properties) == 0) {
      cli::cli_abort(c("x" = "At least one property must be specified"))
    }

    # Internal properties that shouldn't be exposed to users
    internal_props <- c("prefix")
    existing_prop <- setdiff(Reduce(union, get_building_block_prop()), internal_props)
    idx_exist <- properties %in% existing_prop
    # prop_in_df <- properties %in% names(df)

    if (!all(idx_exist)) {
      invalid_props <- properties[!idx_exist]
      cli::cli_abort(c(
        "Invalid propert{cli::qty(length(invalid_props))}{?y/ies}.",
        "x" = "{.code {invalid_props}} {?is/are} not valid."
      ))
    }

    # Always show name and type
    properties <- unique(c("type", "name", properties))
    df <- df[, names(df) %in% properties, drop = FALSE]
    if (nrow(df) == 0) {
      return(df)
    }
  }

  # Reorder columns
  order_first <- c("type", "name", "eqn", "label", "to", "from", "non_negative")

  # Get columns to prioritize (in order_first order)
  cols_first <- intersect(order_first, names(df))
  # Get remaining columns (in original order)
  cols_rest <- setdiff(names(df), order_first)
  # Combine columns (handles character(0) safely)
  new_cols <- c(cols_first, cols_rest)
  # Reorder data frame
  df <- df[, new_cols, drop = FALSE]

  # Make sure that for all columns, at least one row is not NA or empty
  # This is especially necessary when only interested in one type, e.g., func

  # Convert empty strings to NA and keep columns with at least one non-NA
  df[] <- lapply(df, function(x) {
    x[x == ""] <- NA
    x
  })
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]

  # Handle row.names if provided
  if (!is.null(row.names)) {
    if (length(row.names) != nrow(df)) {
      cli::cli_abort(c(
        "Length mismatch in {.arg row.names}.",
        "x" = "Got {length(row.names)} name{?s} but {nrow(df)} row{?s}."
      ))
    }
    rownames(df) <- row.names
  } else {
    rownames(df) <- NULL
  }

  df
}


#' Print overview of stock-and-flow model
#'
#' Prints a descriptive overview of the model structure, including stock-flow
#' topology, variable names, and simulation settings. For model diagnostics,
#' use [`summary()`][summary.sdbuildR()].
#'
#' @param x A stock-and-flow model object of class [`sdbuildR`][sdbuildR]
#' @param ... Additional arguments (unused)
#'
#' @returns Invisibly returns `x`
#' @export
#' @concept build
#' @seealso [summary.sdbuildR()], [dependencies()]
#'
#' @examples
#' sfm <- sdbuildR("SIR")
#' print(sfm)
#'
print.sdbuildR <- function(x, ...) {
  # Header
  model_name <- x[["meta"]][["name"]]
  default_name <- formals(meta)[["name"]]
  has_name <- !is.null(model_name) && nzchar(model_name) && model_name != default_name

  if (has_name) {
    cli::cli_h1("Stock-and-Flow Model: {model_name}")
  } else {
    cli::cli_h1("Stock-and-Flow Model")
  }

  # Count line
  vars <- x[["variables"]]
  types <- vars[["type"]]
  n_stocks <- sum(types == "stock")
  n_flows <- sum(types == "flow")
  n_constants <- sum(types == "constant")
  n_aux <- sum(types == "aux")
  n_lookup <- sum(types == "lookup")
  total <- n_stocks + n_flows + n_constants + n_aux + n_lookup

  if (total == 0) {
    cli::cli_alert_info("Empty model without any variables.")
  } else {
    parts <- c()
    if (n_stocks > 0) parts <- c(parts, "{n_stocks} stock{?s}")
    if (n_flows > 0) parts <- c(parts, "{n_flows} flow{?s}")
    if (n_constants > 0) parts <- c(parts, "{n_constants} constant{?s}")
    if (n_aux > 0) parts <- c(parts, "{n_aux} {?auxiliary/auxiliaries}")
    if (n_lookup > 0) parts <- c(parts, "{n_lookup} lookup{?s}")
    cli::cli_text(paste(parts, collapse = " \u2022 "))
  }

  # Stock-flow structure
  if (n_stocks > 0 || n_flows > 0) {
    cli::cli_h2("Stock-Flow Structure")

    stock_rows <- vars[types == "stock", , drop = FALSE]
    all_flow_names <- vars[types == "flow", "name"]

    shown_flows <- character(0)

    for (i in seq_len(nrow(stock_rows))) {
      stock_name <- stock_rows[i, "name"]
      inflows <- unlist(stock_rows[i, "inflow"])
      outflows <- unlist(stock_rows[i, "outflow"])

      if (is.null(inflows)) inflows <- character(0)
      if (is.null(outflows)) outflows <- character(0)

      shown_flows <- c(shown_flows, inflows, outflows)

      parts <- c(
        if (length(inflows) > 0) paste0("+ ", inflows),
        if (length(outflows) > 0) paste0("- ", outflows)
      )

      if (length(parts) == 0) {
        cli::cli_text("  {stock_name}: (no flows)")
      } else {
        cli::cli_text("  {stock_name}: {paste(parts, collapse = ' ')}")
      }
    }

    # Show truly disconnected flows (not in any stock's inflow/outflow)
    disconnected <- setdiff(all_flow_names, shown_flows)
    if (length(disconnected) > 0) {
      cli::cli_text("  {.emph Unconnected flows}: {.code {disconnected}}")
    }
  }

  # Other variables
  has_others <- n_constants > 0 || n_aux > 0 || n_lookup > 0
  if (has_others) {
    cli::cli_h2("Other Variables")
    if (n_constants > 0) {
      const_names <- vars[types == "constant", "name"]
      cli::cli_text("  {.strong Constants}:   {.code {const_names}}")
    }
    if (n_aux > 0) {
      aux_names <- vars[types == "aux", "name"]
      cli::cli_text("  {.strong Auxiliaries}: {.code {aux_names}}")
    }
    if (n_lookup > 0) {
      lookup_names <- vars[types == "lookup", "name"]
      cli::cli_text("  {.strong Lookups}:     {.code {lookup_names}}")
    }
  }

  # Simulation settings
  cli::cli_h2("Simulation Settings")
  ss <- x[["sim_settings"]]
  time_unit <- ss[["time_units"]]

  save_type <- ss[["save_type"]]
  if (save_type == "all") {
    save_suffix <- ""
  } else if (save_type == "at") {
    l_save_at <- length(ss[["save_at"]])
    if (l_save_at > 1) {
      save_suffix <- paste0(", save_at = ", l_save_at, " time points")
    } else {
      save_suffix <- paste0(", save_at = ", ss[["save_at"]])
    }
  } else if (save_type == "n") {
    save_suffix <- paste0(", save_n = ", ss[["save_n"]])
  } else {
    save_suffix <- ""
  }

  seed <- ss[["seed"]]
  if (!is.null(seed)) {
    seed_suffix <- paste0(" \u2022 seed = ", seed)
  } else {
    seed_suffix <- ""
  }

  cli::cli_text(
    "  Time: {ss$start} to {ss$stop} {time_unit} (dt = {ss$dt}{save_suffix}) \u2022 {ss$method} \u2022 {ss$language}{seed_suffix}"
  )

  # Unit tests (only shown when at least one is defined)
  n_ut <- length(x[["unit_tests"]])
  if (n_ut > 0L) {
    n_active <- sum(vapply(x[["unit_tests"]], function(t) isTRUE(t[["active"]]), logical(1)))
    cli::cli_text("  {n_ut} unit test{?s} defined ({n_active} active) \u2022 Run with {.fn verify}.")
  }

  invisible(x)
}
