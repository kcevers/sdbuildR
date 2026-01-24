# ==============================================================================
# sdbuildR: Build interface for stock-and-flow models
# ==============================================================================
# This file contains the build() function and all related helpers for creating
# and modifying stock-and-flow model variables.
#
# Organization:
# 1. Validation functions
# 2. Operation-specific helpers (modular build operations)
# 3. Prep function (model preparation for simulation)
# 4. build() - Main user-facing function
# 5. add_from_df() - Bulk building from data frame

# ==============================================================================
# VALIDATION FUNCTIONS - Pure validators for build() arguments
# ==============================================================================
# These validators check input validity and return structured error messages.

#' Validate name argument
#' @param name Character vector of variable names
#' @param context Character describing context for error messages
#' @return Validated, trimmed name vector (invisibly valid, or error raised)
#' @noRd
.validate_name_arg <- function(name, context = "") {
  if (!all(is.character(name))) {
    cli::cli_abort(c(
      "Invalid {.arg name} argument.",
      "x" = "The {.arg name} argument must be {.cls character}.",
      "i" = "Received: {.cls {typeof(name)}}."
    ))
  }
  
  name <- trimws(name)
  if (!all(nzchar(name))) {
    cli::cli_abort(c(
      "Invalid {.arg name} argument.",
      "x" = "Variable names cannot be empty strings."
    ))
  }
  
  name
}

#' Validate type argument
#' @param type Character vector of types
#' @noRd
.validate_type_arg <- function(type) {
  type <- clean_type(type)
  
  if (!all(type %in% c("stock", "flow", "constant", "aux", "gf"))) {
    cli::cli_abort(c(
      "Invalid {.arg type} argument.",
      "x" = "The {.arg type} must be one of: {.code 'stock'}, {.code 'flow'}, {.code 'constant'}, {.code 'aux'}, or {.code 'gf'}."
    ))
  }
  
  type
}

#' Validate erase argument
#' @param erase Logical value
#' @noRd
.validate_erase_arg <- function(erase) {
  if (!is.null(erase)) {
    if (length(erase) != 1 || !is.logical(erase)) {
      cli::cli_abort(c(
        "Invalid {.arg erase} argument.",
        "x" = "The {.arg erase} argument must be {.cls logical}.",
        "i" = "Use {.code TRUE} or {.code FALSE}."
      ))
    }
  }
  invisible(erase)
}

#' Validate label argument
#' @param label Character vector of labels
#' @noRd
.validate_label_arg <- function(label) {
  label <- trimws(label)
  if (!all(nzchar(label))) {
    cli::cli_abort(c(
      "Invalid {.arg label} argument.",
      "x" = "Labels cannot be empty strings."
    ))
  }
  label
}

#' Validate change_name argument
#' @param change_name New name for variable
#' @noRd
.validate_change_name_arg <- function(change_name) {
  if (!is.null(change_name)) {
    if (!nzchar(trimws(change_name))) {
      cli::cli_abort(c(
        "Invalid {.arg change_name} argument.",
        "x" = "The new name cannot be empty."
      ))
    }
  }
  invisible(change_name)
}

#' Validate change_type argument
#' @param change_type New type for variable
#' @noRd
.validate_change_type_arg <- function(change_type) {
  if (!is.null(change_type)) {
    change_type <- clean_type(change_type)
    if (!change_type %in% c("stock", "flow", "constant", "aux", "gf")) {
      cli::cli_abort(c(
        "Invalid {.arg change_type} argument.",
        "x" = "Must be one of: {.code 'stock'}, {.code 'flow'}, {.code 'constant'}, {.code 'aux'}, or {.code 'gf'}."
      ))
    }
  }
  invisible(change_type)
}

#' Validate flow connections (to/from)
#' @param to Target stock name
#' @param from Source stock name
#' @param name Flow variable name
#' @noRd
.validate_flow_connections <- function(to, from, name) {
  if (!is.null(to)) {
    to[is.na(to)] <- ""
    if (!inherits(to, "character")) {
      cli::cli_abort(c(
        "Invalid {.arg to} argument.",
        "x" = "The {.arg to} argument must be {.cls character}.",
        "i" = "Received: {.cls {typeof(to)}}."
      ))
    }
    if (length(name) == 1 && length(to) > 1) {
      cli::cli_abort(c(
        "Too many {.arg to} targets.",
        "x" = "A single flow can only have one target {.cls character}."
      ))
    }
    if (any(to == name)) {
      cli::cli_abort(c(
        "Invalid {.arg to} target.",
        "x" = "A flow cannot flow to itself."
      ))
    }
  }
  
  if (!is.null(from)) {
    from[is.na(from)] <- ""
    if (!inherits(from, "character")) {
      cli::cli_abort(c(
        "Invalid {.arg from} argument.",
        "x" = "The {.arg from} argument must be {.cls character}.",
        "i" = "Received: {.cls {typeof(from)}}."
      ))
    }
    if (length(name) == 1 && length(from) > 1) {
      cli::cli_abort(c(
        "Too many {.arg from} sources.",
        "x" = "A single flow can only have one source {.cls character}."
      ))
    }
    if (any(from == name)) {
      cli::cli_abort(c(
        "Invalid {.arg from} source.",
        "x" = "A flow cannot flow from itself."
      ))
    }
  }
  
  # Note: allowing to == from here; validate_xmile() will clean this up
  # This permits building models with invalid flow config that can be fixed later
  
  invisible(list(to = to, from = from))
}

#' Validate graphical function properties
#' @param type Variable type
#' @param xpts X points vector
#' @param ypts Y points vector
#' @param source Source variable name
#' @param interpolation Interpolation method
#' @param extrapolation Extrapolation method
#' @noRd
.validate_graphical_function <- function(type, xpts, ypts, source, 
                                         interpolation, extrapolation) {
  if (type != "gf") return(invisible(list()))
  
  # Validate xpts/ypts
  if (!is.null(xpts) && !is.null(ypts)) {
    if (is.character(xpts)) {
      xpts <- trimws(xpts)
      xpts <- gsub("^c\\(", "", xpts)
      xpts <- gsub("\\)$", "", xpts)
      xpts <- as.numeric(trimws(strsplit(xpts, ",")[[1]]))
    }
    if (is.character(ypts)) {
      ypts <- trimws(ypts)
      ypts <- gsub("^c\\(", "", ypts)
      ypts <- gsub("\\)$", "", ypts)
      ypts <- as.numeric(trimws(strsplit(ypts, ",")[[1]]))
    }
    
    if (length(xpts) != length(ypts)) {
      cli::cli_abort(c(
        "Length mismatch between {.arg xpts} and {.arg ypts}.",
        "x" = "Length of {.arg xpts} is {.val {length(xpts)}}, but length of {.arg ypts} is {.val {length(ypts)}}.",
        ">" = "Ensure both arguments have the same length."
      ))
    }
  }
  
  # Validate interpolation
  if (length(interpolation) > 1) {
    cli::cli_abort(c(
      "Invalid {.arg interpolation} argument.",
      "x" = "Must be a single value: {.code 'linear'} or {.code 'constant'}."
    ))
  }
  interpolation <- tolower(interpolation)
  if (!interpolation %in% c("linear", "constant")) {
    cli::cli_abort(c(
      "Invalid {.arg interpolation} value.",
      "x" = "The {.arg interpolation} must be {.code 'linear'} or {.code 'constant'}.",
      "i" = "Received: {.code {interpolation}}."
    ))
  }
  
  # Validate extrapolation
  if (length(extrapolation) > 1) {
    cli::cli_abort(c(
      "Invalid {.arg extrapolation} argument.",
      "x" = "Must be a single value: {.code 'nearest'} or {.code 'NA'}."
    ))
  }
  if (!extrapolation %in% c("nearest", "NA")) {
    cli::cli_abort(c(
      "Invalid {.arg extrapolation} value.",
      "x" = "The {.arg extrapolation} must be {.code 'nearest'} or {.code 'NA'}.",
      "i" = "Received: {.code {extrapolation}}."
    ))
  }
  
  # Validate source
  if (!is.null(source)) {
    if (!inherits(source, "character")) {
      cli::cli_abort(c(
        "Invalid {.arg source} argument.",
        "x" = "The {.arg source} must be {.cls character}."
      ))
    }
    if (length(source) > 1) {
      cli::cli_abort(c(
        "Invalid {.arg source} argument.",
        "x" = "Only one source variable can be specified."
      ))
    }
  }
  
  invisible(list(xpts = xpts, ypts = ypts, 
                 interpolation = interpolation, 
                 extrapolation = extrapolation))
}

#' Validate other property arguments
#' @param non_negative Logical value
#' @param doc Character value
#' @noRd
.validate_property_args <- function(non_negative, doc) {
  if (!is.null(non_negative)) {
    if (!all(is.logical(non_negative))) {
      cli::cli_abort(c(
        "Invalid {.arg non_negative} argument.",
        "x" = "The {.arg non_negative} must be {.cls logical}.",
        "i" = "Use {.code TRUE} or {.code FALSE}."
      ))
    }
  }
  
  if (!is.null(doc)) {
    if (!inherits(doc, "character")) {
      cli::cli_abort(c(
        "Invalid {.arg doc} argument.",
        "x" = "The {.arg doc} must be {.cls character}."
      ))
    }
  }
  
  invisible(list(non_negative = non_negative, doc = doc))
}

# ==============================================================================
# OPERATION-SPECIFIC INTERNAL FUNCTIONS - Modular build operations
# ==============================================================================
# These functions encapsulate specific build operations for clarity and testing.

#' Rename a variable throughout the model
#' @param sfm Stock-and-flow model
#' @param old_name Current variable name
#' @param new_name New variable name
#' @noRd
.rename_variable <- function(sfm, old_name, new_name) {
  var_names <- sfm[["variables"]][["name"]]
  
  # Update name in data frame
  idx_var <- sfm[["variables"]][["name"]] == old_name
  sfm[["variables"]][idx_var, "name"] <- new_name
  
  # Update label if it was same as old name
  if (sfm[["variables"]][idx_var, "label"] == old_name) {
    sfm[["variables"]][idx_var, "label"] <- new_name
  }
  
  # Replace references using word boundaries
  for (col in c("eqn", "eqn_julia")) {
    if (col %in% colnames(sfm[["variables"]])) {
      sfm[["variables"]][[col]] <- gsub(
        paste0("\\b", old_name, "\\b"), new_name,
        sfm[["variables"]][[col]]
      )
    }
  }
  
  # Update to/from/source references
  sfm[["variables"]][sfm[["variables"]][["to"]] == old_name, "to"] <- new_name
  sfm[["variables"]][sfm[["variables"]][["from"]] == old_name, "from"] <- new_name
  sfm[["variables"]][sfm[["variables"]][["source"]] == old_name, "source"] <- new_name
  
  sfm
}

#' Change variable type
#' @param sfm Stock-and-flow model
#' @param name Variable name
#' @param new_type New variable type
#' @noRd
.change_variable_type <- function(sfm, name, new_type) {
  idx_var <- sfm[["variables"]][["name"]] == name
  sfm[["variables"]][idx_var, "type"] <- new_type
  sfm
}

#' Update variable properties in data frame
#' @param sfm Stock-and-flow model
#' @param name Variable name
#' @param i Index in name vector (for vectorized build)
#' @param passed_arg Character vector of properties that were passed
#' @param eqn Equation value
#' @param eqn_julia_list List with eqn_julia values
#' @param units Units value
#' @param label Label value
#' @param doc Documentation value
#' @param non_negative Logical value
#' @param to Target for flow
#' @param from Source for flow
#' @param type Variable type
#' @param xpts X points for graphical function
#' @param ypts Y points for graphical function
#' @param source Source for graphical function
#' @param interpolation Interpolation method
#' @param extrapolation Extrapolation method
#' @noRd
.update_variable_properties <- function(sfm, name, i, passed_arg, 
                                        eqn, eqn_julia_list, units, label, doc, 
                                        non_negative, to, from, type,
                                        xpts, ypts, source, 
                                        interpolation, extrapolation) {
  idx_var <- which(sfm[["variables"]][["name"]] == name)
  
  if (length(idx_var) == 0) {
    return(sfm)  # Variable doesn't exist, skip update
  }
  
  # Update scalar properties
  if ("eqn" %in% passed_arg) {
    sfm[["variables"]][idx_var, "eqn"] <- eqn[i]
    sfm[["variables"]][idx_var, "eqn_julia"] <- eqn_julia_list[[i]][["eqn_julia"]]
  }
  if ("units" %in% passed_arg) sfm[["variables"]][idx_var, "units"] <- units[i]
  if ("label" %in% passed_arg) sfm[["variables"]][idx_var, "label"] <- label[i]
  if ("doc" %in% passed_arg) sfm[["variables"]][idx_var, "doc"] <- doc[i]
  if ("non_negative" %in% passed_arg) sfm[["variables"]][idx_var, "non_negative"] <- non_negative[i]
  
  # Update flow properties
  if ("to" %in% passed_arg && type[i] == "flow") {
    sfm[["variables"]][idx_var, "to"] <- to[i]
  }
  if ("from" %in% passed_arg && type[i] == "flow") {
    sfm[["variables"]][idx_var, "from"] <- from[i]
  }
  
  # Update graphical function properties
  if (type[i] == "gf") {
    if (!is.null(xpts)) sfm[["variables"]][idx_var, "xpts"] <- list(xpts)
    if (!is.null(ypts)) sfm[["variables"]][idx_var, "ypts"] <- list(ypts)
    if (!is.null(source)) sfm[["variables"]][idx_var, "source"] <- source
    if ("interpolation" %in% passed_arg) sfm[["variables"]][idx_var, "interpolation"] <- interpolation
    if ("extrapolation" %in% passed_arg) sfm[["variables"]][idx_var, "extrapolation"] <- extrapolation
  }
  
  sfm
}

#' Create a new variable row
#' @param name Variable name
#' @param type Variable type
#' @param i Index in name vector
#' @param passed_arg Character vector of properties that were passed
#' @param eqn Equation value
#' @param eqn_julia_list List with eqn_julia values
#' @param units Units value
#' @param label Label value
#' @param doc Documentation value
#' @param non_negative Logical value
#' @param to Target for flow
#' @param from Source for flow
#' @param xpts X points for graphical function
#' @param ypts Y points for graphical function
#' @param source Source for graphical function
#' @param interpolation Interpolation method
#' @param extrapolation Extrapolation method
#' @param ref_row Reference row from existing data frame (for column structure)
#' @noRd
.create_variable_row <- function(name, type, i, passed_arg,
                                 eqn, eqn_julia_list, units, label, doc,
                                 non_negative, to, from,
                                 xpts, ypts, source,
                                 interpolation, extrapolation,
                                 ref_row) {
  new_row <- data.frame(
    name = name,
    type = type,
    eqn = if ("eqn" %in% passed_arg) {
      eqn[i]
    } else if (type == "stock") {
      "0.0"
    } else {
      ""
    },
    eqn_julia = if ("eqn" %in% passed_arg) {
      eqn_julia_list[[i]][["eqn_julia"]]
    } else if (type == "stock") {
      "0.0"
    } else {
      ""
    },
    units = if ("units" %in% passed_arg) units[i] else "1",
    label = if ("label" %in% passed_arg) label[i] else name,
    doc = if ("doc" %in% passed_arg) doc[i] else "",
    non_negative = if ("non_negative" %in% passed_arg) non_negative[i] else FALSE,
    to = if ("to" %in% passed_arg && type == "flow") to[i] else "",
    from = if ("from" %in% passed_arg && type == "flow") from[i] else "",
    source = if (type == "gf" && !is.null(source)) source else "",
    interpolation = if (type == "gf") interpolation else "",
    extrapolation = if (type == "gf") extrapolation else "",
    stringsAsFactors = FALSE
  )
  
  # Add list-columns for graphical functions
  if (type == "gf") {
    new_row$xpts <- list(xpts)
    new_row$ypts <- list(ypts)
  } else {
    new_row$xpts <- list(NULL)
    new_row$ypts <- list(NULL)
  }
  
  # Ensure new_row has all columns from reference
  for (col in colnames(ref_row)) {
    if (!col %in% colnames(new_row)) {
      # Add missing column with appropriate default value
      if (is.list(ref_row[[col]])) {
        new_row[[col]] <- list(NULL)
      } else if (is.logical(ref_row[[col]])) {
        new_row[[col]] <- FALSE
      } else if (is.numeric(ref_row[[col]])) {
        new_row[[col]] <- NA_real_
      } else {
        new_row[[col]] <- ""
      }
    }
  }
  
  new_row
}

#' Prepare model for compilation/simulation
#'
#' Reactively prepares all equations, stock changes, and equation ordering.
#' This function is called by build() but can also be called directly if needed.
#' 
#' @param sfm Stock-and-flow model
#' @param validate Logical: whether to run final validation
#' @noRd
.prepare_model_for_assembly <- function(sfm, validate = TRUE) {
  # Ensure eqn_str column exists for reactive assembly
  if (!"eqn_str" %in% colnames(sfm[["variables"]])) {
    sfm[["variables"]][["eqn_str"]] <- ""
  }
  
  # Reactively prepare equations for all modified/new variables
  # This ensures that the model is always in a consistent state
  # Prepare both R and Julia equation strings
  sfm <- prep_equations_variables(sfm)
  # Preserve R-ready equation strings before Julia rewrites
  sfm[["variables"]][["eqn_str_R"]] <- sfm[["variables"]][["eqn_str"]]

  sfm <- prep_equations_variables_julia(sfm)
  sfm <- prep_stock_change(sfm)
  # Preserve R-ready stock sum strings and names before Julia rewrites
  sfm[["variables"]][["sum_eqn_R"]] <- sfm[["variables"]][["sum_eqn"]]
  sfm[["variables"]][["sum_name_R"]] <- sfm[["variables"]][["sum_name"]]

  sfm <- prep_stock_change_julia(sfm)
  # Cache equation ordering for reuse in simulate()/compile steps
  sfm[["ordering"]] <- order_equations(sfm)
  
  # Sort stocks alphabetically
  stock_idx <- sfm[["variables"]][["type"]] == "stock"
  if (any(stock_idx)) {
    stock_rows <- which(stock_idx)
    stock_names <- sfm[["variables"]][stock_rows, "name"]
    alphabetical_order <- order(stock_names)
    
    # Reorder stocks within the data frame
    new_order <- c(which(!stock_idx), stock_rows[alphabetical_order])
    sfm[["variables"]] <- sfm[["variables"]][new_order, ]
  }
  
  if (validate) {
    sfm <- validate_xmile(sfm)
  }
  
  sfm
}

#' Build and modify stock-and-flow model variables
#'
#' @param sfm A stock-and-flow model created with [xmile()]
#' @param name Character: name(s) of variable(s) to add or modify
#' @param type Character: type(s) of variable(s) - one of "stock", "flow",
#'   "constant", "aux", or "gf" (graphical function)
#' @param eqn Character or numeric: equation(s) defining the variable(s)
#' @param units Character: unit(s) for the variable(s)
#' @param label Character: label(s) for the variable(s)
#' @param doc Character: documentation string(s) for the variable(s)
#' @param change_name Character: new name for an existing variable
#' @param change_type Character: change type of an existing variable
#' @param erase Logical: if TRUE, erase variable(s)
#' @param to Character: target stock(s) for flow variable(s)
#' @param from Character: source stock(s) for flow variable(s)
#' @param non_negative Logical: whether variable must remain non-negative
#' @param xpts Numeric vector: X points for graphical function
#' @param ypts Numeric vector: Y points for graphical function
#' @param source Character: source variable for graphical function
#' @param interpolation Character: "linear" or "constant" for graphical function
#' @param extrapolation Character: "nearest" or "NA" for graphical function
#' @param df Data frame: for bulk add/modify operations (columns: type, name, and other properties)
#'
#' @returns A stock-and-flow model object
#' @examples
#' # Create a simple predator-prey model
#' sfm <- xmile() |>
#'   build("prey", "stock", eqn = 100, label = "Prey Population") |>
#'   build("predator", "stock", eqn = 10, label = "Predator Population")
#'
#' # Remove variable
#' sfm <- build(sfm, "prey", erase = TRUE)
#'
#' # To add and/or modify variables more quickly, pass a data.frame.
#' # The data.frame is processed row-wise.
#' # For instance, to create a logistic population growth model:
#' df <- data.frame(
#'   type = c("stock", "flow", "flow", "constant", "constant"),
#'   name = c("X", "inflow", "outflow", "r", "K"),
#'   eqn = c(.01, "r * X", "r * X^2 / K", 0.1, 1),
#'   label = c(
#'     "Population size", "Births", "Deaths", "Growth rate",
#'     "Carrying capacity"
#'   ),
#'   to = c(NA, "X", NA, NA, NA),
#'   from = c(NA, NA, "X", NA, NA)
#' )
#' sfm <- build(xmile(), df = df)
#'
#' # Check for errors in the model
#' debugger(sfm)
#'
#' @export
build <- function(sfm, name, type,
                  eqn = "0.0",
                  units = "1",
                  label = name,
                  doc = "",
                  change_name = NULL,
                  change_type = NULL,
                  erase = FALSE,
                  to = NULL, from = NULL,
                  non_negative = FALSE,
                  xpts = NULL, ypts = NULL,
                  source = NULL,
                  interpolation = "linear",
                  extrapolation = "nearest",
                  df = NULL) {
  # Basic checks
  if (missing(sfm)) cli::cli_abort(c(
    "Missing required {.arg sfm} argument.",
    "x" = "No stock-and-flow model specified.",
    ">" = "Create one with {.fn xmile()} first."
  ))
  check_xmile(sfm)

  # Handle data frame input
  if (!is.null(df)) {
    sfm <- add_from_df(sfm, df)
    return(sfm)
  }

  # Validate inputs
  if (missing(name)) cli::cli_abort(c(
    "Missing required {.arg name} argument.",
    "x" = "Variable name must be specified."
  ))
  
  name <- .validate_name_arg(name)
  label <- .validate_label_arg(label)
  .validate_erase_arg(erase)
  .validate_change_name_arg(change_name)
  .validate_change_type_arg(change_type)

  # Get current variable names
  var_names <- sfm[["variables"]][["name"]]
  
  # Get passed arguments
  passed_arg <- names(as.list(match.call())[-1]) |>
    setdiff(c("sfm", "erase", "change_name", "change_type"))

  # Find which variables already exist
  idx_exist <- name %in% var_names
  
  # For new variables, if eqn wasn't explicitly passed, add the default to passed_arg
  # But only if the type supports eqn (not for gf or other types that don't use eqn)
  if (any(!idx_exist) && !"eqn" %in% passed_arg && !missing(type)) {
    type_to_check <- .validate_type_arg(type)
    type_to_check <- ensure_length(type_to_check, name)
    # Check if any of the new variables have a type that supports eqn
    new_types <- type_to_check[!idx_exist]
    if (any(new_types != "gf")) {
      # Only add eqn default if at least one new var is not a gf
      passed_arg <- c(passed_arg, "eqn")
    }
  }

  # Determine type
  if (missing(type)) {
    # If type not specified, all names must exist
    if (any(!idx_exist)) {
      missing_vars <- paste0(name[!idx_exist], collapse = ", ")
      cli::cli_abort(c(
        "Cannot find variable{ifelse(sum(!idx_exist) > 1, 's', '')} in model.",
        "x" = "The following variable{ifelse(sum(!idx_exist) > 1, 's', '')} {ifelse(sum(!idx_exist) > 1, 'do', 'does')} not exist: {.code {missing_vars}}.",
        "!" = "To add a new variable, specify the {.arg type} argument.",
        ">" = "Use {.fn build}({.arg sfm}, {.code {name[!idx_exist][1]}}, {.arg type} = {.code 'stock'|'flow'|'constant'|'aux'|'gf'})."
      ))
    }
    
    # Get types for existing variables
    type <- sapply(name, function(n) {
      sfm[["variables"]][sfm[["variables"]][["name"]] == n, "type"]
    })
    
    # Now add eqn default for new vars if any exist and eqn not specified
    if (any(!idx_exist) && !"eqn" %in% passed_arg && !any(type[!idx_exist] == "gf")) {
      passed_arg <- c(passed_arg, "eqn")
    }
  } else {
    type <- .validate_type_arg(type)
    type <- ensure_length(type, name)
    
    # Check if existing variables match the specified type
    if (any(idx_exist)) {
      existing_types <- sapply(name[idx_exist], function(n) {
        sfm[["variables"]][sfm[["variables"]][["name"]] == n, "type"]
      })
      
      nonmatching_type <- type[idx_exist] != existing_types
      
      if (any(nonmatching_type)) {
        bad_names <- name[idx_exist][nonmatching_type]
        bad_types <- existing_types[nonmatching_type]
        
        if (erase) {
          msg_types <- paste(sprintf("%s ({.code %s})", bad_names, bad_types), collapse = ", ")
          cli::cli_abort(c(
            "Cannot erase variables with wrong types.",
            "x" = "These variables exist but have different types:",
            "!" = msg_types
          ))
        } else {
          msg_types <- paste(sprintf("%s ({.code %s})", bad_names, bad_types), collapse = ", ")
          cli::cli_abort(c(
            "Cannot modify variables with wrong types.",
            "x" = "These variables exist but have different types:",
            "!" = msg_types,
            ">" = "Either omit the {.arg type} to modify as-is, or use a unique name for a new variable."
          ))
        }
      }
    }
  }

  # Handle erase (check this before cleaning names for new variables)
  if (erase) {
    if (any(!idx_exist)) {
      missing_vars <- paste0(name[!idx_exist], collapse = ", ")
      cli::cli_abort(c(
        "Cannot erase non-existent variable{ifelse(sum(!idx_exist) > 1, 's', '')}.",
        "x" = sprintf(
          "The following variable%s %s not exist: {.code %s}.",
          ifelse(sum(!idx_exist) > 1, "s", ""),
          ifelse(sum(!idx_exist) > 1, "do", "does"),
          missing_vars
        )
      ))
    }
    
    sfm <- erase_var(sfm, name)
    return(sfm)
  }

    # Clean names for new variables
    if (any(!idx_exist)) {
      new_names <- clean_name(name[!idx_exist], var_names)
      report_name_change(name[!idx_exist], new_names)
      name[!idx_exist] <- new_names
    }

  # Check change_name
  if (!is.null(change_name)) {
    if (length(change_name) > 1 || length(name) > 1) {
      cli::cli_abort(c(
        "Cannot rename multiple variables at once.",
        "x" = "Please rename one variable at a time using {.fn build()}."
      ))
    }
  }

  # Check change_type
  if (!is.null(change_type)) {
    if (length(change_type) > 1 || length(name) > 1) {
      cli::cli_abort(c(
        "Cannot change types of multiple variables at once.",
        "x" = "Please change the type of one variable at a time using {.fn build()}."
      ))
    }
    change_type <- .validate_change_type_arg(change_type)
  }

  # Property validation
  keep_prop <- get_building_block_prop()
  type_ <- if (!is.null(change_type)) change_type else type
  
  appr_prop <- Reduce(intersect, keep_prop[type_])
  idx_inappr <- !(passed_arg %in% appr_prop)
  if (any(idx_inappr)) {
    cli::cli_warn(sprintf(
      "These properties are not appropriate for %s specified type%s (%s):\n- %s\nThese will be ignored.",
      ifelse(length(unique(type_)) > 1, "all", "the"),
      ifelse(length(unique(type_)) > 1, "s", ""),
      paste0(unique(type_), collapse = ", "), paste0(passed_arg[idx_inappr], collapse = ", ")
    ))
  }
  
  # Validate flow properties
  if ("to" %in% passed_arg) {
    if (is.null(to)) to <- ""
    to <- ensure_length(to, name)
  }
  
  if ("from" %in% passed_arg) {
    if (is.null(from)) from <- ""
    from <- ensure_length(from, name)
  }
  
  flow_result <- .validate_flow_connections(to, from, name)
  to <- flow_result$to
  from <- flow_result$from
  
  # Validate graphical function properties
  if (any(type == "gf")) {
    if (length(name) != 1) {
      cli::cli_abort(c(
        "Cannot vectorize graphical functions.",
        "x" = "Graphical functions must be built one at a time.",
        ">" = "Build each graphical function separately using {.fn build()}."
      ))
    }
    
    if (!any(idx_exist) && is.null(xpts) && is.null(ypts)) {
      cli::cli_abort(c(
        "Missing graphical function properties.",
        "x" = "Both {.arg xpts} and {.arg ypts} must be specified for new graphical functions."
      ))
    } else if (!any(idx_exist) && is.null(xpts)) {
      cli::cli_abort(c(
        "Missing {.arg xpts} property.",
        "x" = "The {.arg xpts} argument is required for new graphical functions."
      ))
    } else if (!any(idx_exist) && is.null(ypts)) {
      cli::cli_abort(c(
        "Missing {.arg ypts} property.",
        "x" = "The {.arg ypts} argument is required for new graphical functions."
      ))
    } else if (any(idx_exist)) {
      # Get existing xpts/ypts if not provided
      idx_var <- which(sfm[["variables"]][["name"]] == name & sfm[["variables"]][["type"]] == "gf")
      if (is.null(xpts) && !is.null(ypts)) {
        xpts <- sfm[["variables"]][idx_var, "xpts"][[1]]
      } else if (is.null(ypts) && !is.null(xpts)) {
        ypts <- sfm[["variables"]][idx_var, "ypts"][[1]]
      }
    }
    
    gf_result <- .validate_graphical_function(type[1], xpts, ypts, source, 
                                               interpolation, extrapolation)
    xpts <- gf_result$xpts
    ypts <- gf_result$ypts
    interpolation <- gf_result$interpolation
    extrapolation <- gf_result$extrapolation
  }
  
  # Handle change_name
  if (!is.null(change_name)) {
    chosen_new_name <- change_name
    change_name <- clean_name(change_name, var_names)
    report_name_change(chosen_new_name, change_name)
    
    sfm <- .rename_variable(sfm, name, change_name)
    
    # Update tracking variables
    name <- change_name
    var_names <- sfm[["variables"]][["name"]]
    idx_exist <- name %in% var_names
    
    # Update label if not explicitly passed
    if ("label" %in% passed_arg) {
      idx_var <- sfm[["variables"]][["name"]] == name
      sfm[["variables"]][idx_var, "label"] <- label
    }
    
    # Redo equation if not already passed
    if (!"eqn" %in% passed_arg) {
      idx_var <- sfm[["variables"]][["name"]] == name
      eqn <- sfm[["variables"]][idx_var, "eqn"]
      passed_arg <- c(passed_arg, "eqn")
    }
  }
  
  # Handle change_type
  if (!is.null(change_type)) {
    if (type != change_type) {
      sfm <- .change_variable_type(sfm, name, change_type)
      type <- change_type
      
      # Redo equation
      if (!"eqn" %in% passed_arg) {
        idx_var <- sfm[["variables"]][["name"]] == name
        eqn <- sfm[["variables"]][idx_var, "eqn"]
        passed_arg <- c(passed_arg, "eqn")
      }
    }
  }
  
  # Get regex units if needed
  if (any(c("eqn", "units") %in% passed_arg)) {
    regex_units <- get_regex_units()
  }
  
  # Process equation
  if ("eqn" %in% passed_arg) {
    if (is.null(eqn)) {
      cli::cli_warn(c(
        "Null equation detected.",
        ">" = "Setting equation to {.code '0.0'}."
      ))
      eqn <- "0.0"
    }
    if (any(is.na(eqn))) {
      cli::cli_warn(c(
        "NA value{ifelse(length(eqn) > 1, 's', '')} in equation{ifelse(length(eqn) > 1, 's', '')} detected.",
        ">" = "Setting {ifelse(length(eqn) > 1, 'them', 'it')} to {.code '0.0'}."
      ))
      eqn[is.na(eqn)] <- "0.0"
    }
    if (any(!nzchar(eqn))) {
      cli::cli_warn(c(
        "Empty equation{ifelse(sum(!nzchar(eqn)) > 1, 's', '')} detected.",
        ">" = "Setting {ifelse(sum(!nzchar(eqn)) > 1, 'them', 'it')} to {.code '0.0'}."
      ))
      eqn[!nzchar(eqn)] <- "0.0"
    }
    
    eqn <- as.character(eqn)
    
    if (any(grepl("^[ ]*function[ ]*\\(", eqn))) {
      cli::cli_abort(c(
        "Invalid equation format.",
        "x" = "Model variables cannot be defined as functions.",
        ">" = "To add a custom function, use {.fn macro()} instead."
      ))
    }
    
    eqn <- clean_unit_in_u(eqn, regex_units)
    eqn <- ensure_length(eqn, name)
    
    # Convert to Julia
    all_var_names <- c(var_names, name)
    eqn_julia_list <- lapply(seq_along(name), function(i) {
      convert_equations_julia(type[i], name[i], eqn[i], all_var_names,
                             regex_units = regex_units)
    })
  }
  
  # Process units
  if (!is.null(units)) {
    if (!inherits(units, "character")) units <- as.character(units)
    if (any(!nzchar(units))) units[!nzchar(units)] <- "1"
    
    units <- vapply(units, function(x) {
      clean_unit(x, regex_units)
    }, character(1), USE.NAMES = FALSE)
    units <- ensure_length(units, name)
  }
  
  # Process other properties
  if ("non_negative" %in% passed_arg) {
    non_negative <- ensure_length(non_negative, name)
  }
  
  if ("label" %in% passed_arg) {
    label <- ensure_length(label, name)
  }
  
  if ("doc" %in% passed_arg) {
    doc <- ensure_length(doc, name)
  }
  
  prop_result <- .validate_property_args(non_negative, doc)
  
  # Build/update variables in data frame
  for (i in seq_along(name)) {
    idx_var <- which(sfm[["variables"]][["name"]] == name[i])
    
    if (length(idx_var) > 0) {
      # Update existing variable
      sfm <- .update_variable_properties(
        sfm, name[i], i, passed_arg,
        eqn, eqn_julia_list, units, label, doc,
        non_negative, to, from, type,
        xpts, ypts, source,
        interpolation, extrapolation
      )
    } else {
      # Add new variable
      new_row <- .create_variable_row(
        name[i], type[i], i, passed_arg,
        eqn, eqn_julia_list, units, label, doc,
        non_negative, to, from,
        xpts, ypts, source,
        interpolation, extrapolation,
        sfm[["variables"]][1, ]  # Use first row as template
      )
      
      sfm[["variables"]] <- rbind(sfm[["variables"]], new_row)
    }
  }
  
  # Prepare model for assembly/simulation
  sfm <- .prepare_model_for_assembly(sfm, validate = TRUE)
  return(sfm)
}


#' Add and/or modify model from data frame
#'
#' @inheritParams build
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#' @noRd
#'
add_from_df <- function(sfm, df) {
  if (!inherits(df, "data.frame")) {
    cli::cli_abort(c(
      "Invalid {.arg df} argument.",
      "x" = "The {.arg df} must be a {.cls data.frame}.",
      "i" = "Received: {.cls {typeof(df)}}."
    ))
  }

  # Get all properties
  prop <- get_building_block_prop()

  # Check whether dataframe has necessary columns
  nec_prop <- c("type", "name")

  if (!all(nec_prop %in% colnames(df))) {
    missing_cols <- paste0(nec_prop[!nec_prop %in% colnames(df)], collapse = ", ")
    cli::cli_abort(c(
      "Missing required column{ifelse(length(nec_prop[!nec_prop %in% colnames(df)]) > 1, 's', '')} in {.arg df}.",
      "x" = "The following {ifelse(length(nec_prop[!nec_prop %in% colnames(df)]) > 1, 'columns are', 'column is')} required: {.code {missing_cols}}."
    ))
  }

  # Check whether dataframe has columns only in prop
  idx <- !colnames(df) %in% unique(unlist(prop))
  if (any(idx)) {
    invalid_cols <- paste0(colnames(df)[idx], collapse = ", ")
    cli::cli_abort(c(
      "Invalid column name{ifelse(sum(idx) > 1, 's', '')} in {.arg df}.",
      "x" = "The following {ifelse(sum(idx) > 1, 'columns are', 'column is')} not valid properties: {.code {invalid_cols}}.",
      "!" = "Valid properties are: {paste0(unique(unlist(prop)), collapse = ', ')}."
    ))
  }

  # Add each row by calling build
  for (i in seq_len(nrow(df))) {
    arg <- as.list(df[i, ])
    arg <- arg[!is.na(arg)]

    # Only keep appropriate properties for this type
    arg <- arg[names(arg) %in% prop[[arg[["type"]]]]]

    arg[["sfm"]] <- sfm
    sfm <- do.call(build, arg)
  }

  return(sfm)
}
