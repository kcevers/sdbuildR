#' Create a new stock-and-flow model
#'
#' Initialize a stock-and-flow model of class [`sdbuildR_xmile`][xmile]. You can
#' either create an empty stock-and-flow model or load a template from the model
#' library.
#'
#' Do not edit the object manually; this will likely lead to errors downstream.
#' Rather, use [header()], [sim_specs()], [build()], [macro()], and
#' [model_units()] for safe manipulation.
#'
#' @param name Name of the template to load. If `NULL`, an empty stock-and-flow
#' model will be created with default simulation parameters and a default header.
#' If specified, `name` should be one of the available templates:
#' \itemize{
#'   \item \strong{logistic_model}: Population growth with carrying capacity
#'   \item \strong{SIR}: Epidemic model (Susceptible-Infected-Recovered)
#'   \item \strong{predator_prey}: Lotka-Volterra dynamics
#'   \item \strong{cusp}: Cusp catastrophe model
#'   \item \strong{Crielaard2022}: Eating behavior (doi: 10.1037/met0000484)
#'   \item \strong{coffee_cup}: Temperature equilibration (Meadows)
#'   \item \strong{bank_account}: Compound interest (Meadows)
#'   \item \strong{Lorenz}: Lorenz attractor (chaotic)
#'   \item \strong{Rossler}: Rossler attractor (chaotic)
#'   \item \strong{vanderPol}: Van der Pol oscillator
#'   \item \strong{Duffing}: Forced Duffing oscillator
#'   \item \strong{Chua}: Chua's circuit (chaotic)
#'   \item \strong{JDR}: Job Demands-Resources Theory as formalized in Evers et al. (submitted)
#' }
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]. Its structure is based
#'  on [XML Interchange Language for System Dynamics (XMILE)](https://docs.oasis-open.org/xmile/xmile/v1.0/os/xmile-v1.0-os.html). It is a nested list, containing:
#' \describe{
#'  \item{header}{Meta-information about model. A list containing arguments listed in [header()].}
#'  \item{sim_specs}{Simulation specifications. A list containing arguments listed in [sim_specs()].}
#'  \item{model}{Model variables, grouped under the variable types stock, flow, aux (auxiliaries), constant, and gf (graphical functions). Each variable contains arguments as listed in [build()].}
#'  \item{macro}{Global variable or functions. A list containing arguments listed in [macro()].}
#'  \item{model_units}{Custom model units. A list containing arguments listed in [model_units()].}
#'  }
#'
#' Use [summary()] to summarize, [as.data.frame()] to convert to a data.frame, [plot()] to visualize.
#'
#' @export
#' @concept build
#' @seealso [build()], [header()], [macro()], [model_units()], [sim_specs()]
#'
#' @examples sfm <- xmile()
#' summary(sfm)
#'
#' \dontshow{
#' sfm <- sim_specs(sfm, save_at = 1)
#' }
#'
#' # Load a template
#' sfm <- xmile("Lorenz")
#' sim <- simulate(sfm)
#' plot(sim)
xmile <- function(name = NULL) {
  if (!is.null(name)) {
    return(template(name))
  }

  sfm <- new_sdbuildR_xmile()
  return(sfm)
}


#' Create new object of class [`sdbuildR_xmile`][xmile]
#'
#' @returns A stock-and-flow model of class [`sdbuildR_xmile`][xmile]
#' @noRd
#'
new_sdbuildR_xmile <- function() {
  header_defaults <- as.list(formals(header))
  header_defaults <- header_defaults[!names(header_defaults) %in%
    c("sfm", "...")]
  header_defaults[["created"]] <- Sys.time() # Manually overwrite time

  spec_defaults <- as.list(formals(sim_specs))
  spec_defaults <- spec_defaults[!names(spec_defaults) %in% c("sfm", "...")]

  # Manually overwrite these as the defaults of save_at and save_from are
  # defined in terms of other variables
  spec_defaults[["save_at"]] <- spec_defaults[["dt"]]
  spec_defaults[["save_from"]] <- spec_defaults[["start"]]

  # Create data frame for variables (all types in one data frame)
  variables_df <- data.frame(
    name = character(0),
    type = character(0),
    eqn = character(0),
    eqn_julia = character(0),
    units = character(0),
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
    stringsAsFactors = FALSE
  )
  
  # Add list-columns for xpts and ypts (graphical functions)
  variables_df$xpts <- list()
  variables_df$ypts <- list()

  # Create data frame for macros
  macros_df <- data.frame(
    name = character(0),
    eqn = character(0),
    eqn_julia = character(0),
    units = character(0),
    doc = character(0),
    stringsAsFactors = FALSE
  )

  # Create data frame for model_units
  model_units_df <- data.frame(
    name = character(0),
    eqn = character(0),
    doc = character(0),
    prefix = logical(0),
    stringsAsFactors = FALSE
  )

  # Create list
  obj <- list(
    header = header_defaults,
    sim_specs = spec_defaults,
    variables = variables_df,
    macro = macros_df,
    model_units = model_units_df
  )

  sfm <- structure(obj, class = "sdbuildR_xmile")
  sfm <- validate_xmile(sfm)
  return(sfm)
}


#' Get the sources and destinations of flows
#'
#' @inheritParams build
#'
#' @returns data.frame with for each flow which stock and flow to and/or from
#' @noRd
get_flow_df <- function(sfm) {
  check_xmile(sfm)

  flows <- sfm[["variables"]][sfm[["variables"]][["type"]] == "flow", ]
  
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


#' Create data frame of simulation results
#'
#' Convert simulation results to a data.frame.
#'
#' @inheritParams plot.sdbuildR_sim
#' @param direction Format of data frame, either "long" (default) or "wide".
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional Ignored parameter.
#'
#' @returns A data.frame with simulation results. For \code{direction = "long"} (default),
#'   the data frame has three columns: \code{time}, \code{variable}, and \code{value}.
#'   For \code{direction = "wide"}, the data frame has columns \code{time} followed by
#'   one column per variable.
#' @export
#' @seealso [simulate()], [xmile()]
#' @concept build
#' @method as.data.frame sdbuildR_sim
#'
#' @examples
#' sfm <- xmile("SIR")
#' sim <- simulate(sfm)
#' df <- as.data.frame(sim)
#' head(df)
#'
#' # Get results in wide format
#' df_wide <- as.data.frame(sim, direction = "wide")
#' head(df_wide)
#'
as.data.frame.sdbuildR_sim <- function(x,
                                       row.names = NULL, optional = FALSE,
                                       direction = "long", ...) {
  validate_sdbuildR_sim(x)

  direction <- trimws(tolower(direction))
  if (!direction %in% c("long", "wide")) {
    cli::cli_abort(c(
      "Invalid {.arg direction} argument.",
      "x" = "Must be either {.code 'long'} or {.code 'wide'}."
    ))
  }

  if (direction == "long") {
    df <- x[["df"]]
  } else if (direction == "wide") {
    df <- stats::reshape(x[["df"]],
      timevar = "variable",
      idvar = "time",
      direction = "wide"
    )

    # Remove value. prefix
    names(df) <- sub("^value\\.", "", names(df))

    # Remove row names
    rownames(df) <- NULL
  }

  # Handle row.names if provided
  if (!is.null(row.names)) {
    if (length(row.names) != nrow(df)) {
      cli::cli_abort(sprintf(
        "Length of row.names (%d) does not match number of rows (%d)",
        length(row.names), nrow(df)
      ))
    }
    rownames(df) <- row.names
  }

  return(df)
}


#' Find longest regex match
#'
#' @param x Value
#' @param regex_units Regex units dictionary
#'
#' @returns Longest cleaned regex match
#' @noRd
find_matching_regex <- function(x, regex_units) {
  matches <- names(regex_units[regex_units == x])

  # Return empty string if no matches found
  if (length(matches) == 0) {
    return("")
  }

  # Clean regex and select longest match
  matches <- sub("\\$$", "", sub("^\\^", "", matches))
  matches <- sub("\\[s\\]\\?", "s", matches)

  matches <- unique(tolower(stringr::str_replace_all(
    matches,
    "\\[([a-zA-Z])\\|([a-zA-Z])\\]", "\\1"
  )))
  matches[which.max(nchar(matches))] # Return longest match
}


#' Get delayN and smoothN from stock-and-flow model
#'
#' @inheritParams build
#'
#' @returns List with delayN and smoothN functions
#' @noRd
get_delay <- function(sfm, type = c("delayN_smoothN", "past")) {
  type <- match.arg(type)
  
  result <- list()
  
  # Search through equations for delay functions
  if (nrow(sfm[["variables"]]) > 0) {
    for (i in seq_len(nrow(sfm[["variables"]]))) {
      var_name <- sfm[["variables"]][i, "name"]
      var_eqn <- sfm[["variables"]][i, "eqn"]
      
      if (!is.na(var_eqn) && nzchar(var_eqn)) {
        if (type == "past") {
          # Look for past() or delay() functions
          if (grepl("\\bpast\\s*\\(|\\bdelay\\s*\\(", var_eqn)) {
            result[[var_name]] <- var_eqn
          }
        } else if (type == "delayN_smoothN") {
          # Look for delayN(), smoothN() functions
          if (grepl("\\bdelay[0-9]+\\s*\\(|\\bsmooth[0-9]+\\s*\\(", var_eqn)) {
            result[[var_name]] <- var_eqn
          }
        }
      }
    }
  }
  
  return(result)
}


#' Check whether object is of class [`sdbuildR_xmile`][xmile]
#'
#' @inheritParams build
#'
#' @returns Returns `NULL`, called for side effects.
#' @noRd
check_xmile <- function(sfm) {
  # Check whether it is an xmile object
  if (!inherits(sfm, "sdbuildR_xmile")) {
    cli::cli_abort(c(
      "Expected object of class {.cls sdbuildR_xmile}.",
      "i" = "Create a stock-and-flow model with {.fn xmile()} or {.fn insightmaker_to_sfm()}."
    ))
  }
}


#' Validate sdbuildR_xmile class
#'
#' Internally used function to ensure that the stock-and-flow model is valid and contains all necessary properties.
#'
#' @inheritParams build
#'
#' @returns A stock-and-flow model of class [`sdbuildR_xmile`][xmile]
#' @noRd
#'
validate_xmile <- function(sfm) {
  check_xmile(sfm)

  # Ensure model_units data frame has default properties
  if (nrow(sfm[["model_units"]]) > 0) {
    defaults <- as.list(formals(model_units))
    defaults <- defaults[!names(defaults) %in% c(
      "sfm", "name", "erase", "change_name"
    )]
    
    # Ensure all necessary columns exist
    for (col in names(defaults)) {
      if (!col %in% colnames(sfm[["model_units"]])) {
        sfm[["model_units"]][[col]] <- defaults[[col]]
      }
    }
    
    # Ensure prefix is FALSE if not set
    if (!"prefix" %in% colnames(sfm[["model_units"]])) {
      sfm[["model_units"]][["prefix"]] <- FALSE
    }
  }

  # Validate variables data frame
  if (nrow(sfm[["variables"]]) > 0) {
    # Ensure label is set (defaults to name if missing)
    if (any(is.na(sfm[["variables"]][["label"]]) | sfm[["variables"]][["label"]] == "")) {
      idx_missing_label <- is.na(sfm[["variables"]][["label"]]) | sfm[["variables"]][["label"]] == ""
      sfm[["variables"]][idx_missing_label, "label"] <- sfm[["variables"]][idx_missing_label, "name"]
    }
    
    # Ensure eqn_julia is set (defaults to "0.0" if missing)
    if (any(is.na(sfm[["variables"]][["eqn_julia"]]) | sfm[["variables"]][["eqn_julia"]] == "")) {
      idx_missing_eqn_julia <- is.na(sfm[["variables"]][["eqn_julia"]]) | sfm[["variables"]][["eqn_julia"]] == ""
      sfm[["variables"]][idx_missing_eqn_julia, "eqn_julia"] <- "0.0"
    }
    
    # Validate flows: ensure to and from only refer to stocks
    flows <- sfm[["variables"]][sfm[["variables"]][["type"]] == "flow", ]
    stocks <- sfm[["variables"]][sfm[["variables"]][["type"]] == "stock", ]
    stock_names <- stocks[["name"]]
    non_stock_names <- sfm[["variables"]][sfm[["variables"]][["type"]] != "stock", "name"]
    
    if (nrow(flows) > 0) {
      # Check invalid 'to'
      flows_to_invalid <- !is.na(flows[["to"]]) & flows[["to"]] != "" & flows[["to"]] %in% non_stock_names
      if (any(flows_to_invalid)) {
        for (i in which(flows_to_invalid)) {
          cli::cli_warn(sprintf(
            "%s is flowing to a variable which is not a stock (%s)! Removing %s from `to`...",
            flows[i, "name"], flows[i, "to"], flows[i, "to"]
          ))
          sfm[["variables"]][sfm[["variables"]][["name"]] == flows[i, "name"], "to"] <- ""
        }
      }
      
      # Check invalid 'from'
      flows_from_invalid <- !is.na(flows[["from"]]) & flows[["from"]] != "" & flows[["from"]] %in% non_stock_names
      if (any(flows_from_invalid)) {
        for (i in which(flows_from_invalid)) {
          cli::cli_warn(sprintf(
            "%s is flowing from a variable which is not a stock (%s)! Removing %s from `from`...",
            flows[i, "name"], flows[i, "from"], flows[i, "from"]
          ))
          sfm[["variables"]][sfm[["variables"]][["name"]] == flows[i, "name"], "from"] <- ""
        }
      }
      
      # Check if to and from are the same
      flows_invalid <- !is.na(flows[["to"]]) & !is.na(flows[["from"]]) & 
                       flows[["to"]] == flows[["from"]] & flows[["to"]] != ""
      if (any(flows_invalid)) {
        for (i in which(flows_invalid)) {
          cli::cli_warn(sprintf(
            "%s is flowing to and from the same variable (%s)! Removing %s from `from`...",
            flows[i, "name"], flows[i, "from"], flows[i, "from"]
          ))
          sfm[["variables"]][sfm[["variables"]][["name"]] == flows[i, "name"], "from"] <- ""
        }
      }
    }
  }

  # Ensure macros data frame has default properties
  macro_name <- P[["macro_name"]]
  if (nrow(sfm[[macro_name]]) > 0) {
    # Ensure eqn and eqn_julia are set
    if (any(is.na(sfm[[macro_name]][["eqn"]]) | sfm[[macro_name]][["eqn"]] == "")) {
      idx_missing <- is.na(sfm[[macro_name]][["eqn"]]) | sfm[[macro_name]][["eqn"]] == ""
      sfm[[macro_name]][idx_missing, "eqn"] <- "0.0"
    }
    if (any(is.na(sfm[[macro_name]][["eqn_julia"]]) | sfm[[macro_name]][["eqn_julia"]] == "")) {
      idx_missing <- is.na(sfm[[macro_name]][["eqn_julia"]]) | sfm[[macro_name]][["eqn_julia"]] == ""
      sfm[[macro_name]][idx_missing, "eqn_julia"] <- "0.0"
    }
  }

  # To prevent downstream errors, don't:
  # - add inflows and outflows to stocks

  # To prevent massively slowing down code, don't:
  # - translate all equations to Julia here
  # - detect undefined units

  return(sfm)
}


#' Create, modify or remove custom units
#'
#' A large library of units already exists, but you may want to define your own custom units. Use [model_units()] to add, change, or erase custom units from a stock-and-flow model. Custom units may be new base units, or may be defined in terms of other (custom) units. See [u()] for more information on the rules of specifying units. Note that units are only supported in Julia, not in R.
#'
#' @inheritParams build
#' @param name Name of unit. A character vector.
#' @param eqn Definition of unit. String or vector of unit definitions. Defaults to "1" to indicate a base unit not defined in terms of other units.
#' @param doc Documentation of unit.
#' @param erase If TRUE, remove model unit from the model. Defaults to FALSE.
#' @param change_name New name for model unit. Defaults to NULL to indicate no change.
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#'
#' @export
#' @concept units
#' @seealso [unit_prefixes()]
#'
#' @examplesIf julia_status()$status == "ready"
#' # Units are only supported with Julia
#' sfm <- xmile("Crielaard2022")
#' sfm <- model_units(sfm, "BMI", eqn = "kg/m^2", doc = "Body Mass Index")
#'
#' # You may also use words rather than symbols for the unit definition.
#' # The following modifies the unit BMI:
#' sfm <- model_units(sfm, "BMI", eqn = "kilogram/meters^2")
#'
#' # Remove unit:
#' sfm <- model_units(sfm, "BMI", erase = TRUE)
#'
#' # Unit names may need to be changed to be syntactically valid or to avoid
#' # overlap with existing units:
#' sfm <- model_units(xmile(), "C0^2")
#'
model_units <- function(sfm, name, eqn = "1", doc = "",
                        erase = FALSE, change_name = NULL) {
  # Basic check
  if (missing(sfm)) {
    cli::cli_abort("The {.arg sfm} argument is required.")
  }

  if (missing(name)) {
    cli::cli_abort("The {.arg name} argument must be specified.")
  }

  check_xmile(sfm)

  # Handle rename first, before other operations
  if (!is.null(change_name)) {
    # Check if unit exists
    idx_exist <- name %in% sfm[["model_units"]][["name"]]
    if (!idx_exist) {
      cli::cli_abort("Cannot rename a unit that does not exist: {.code {name}}")
    }
    
    # Check if new name would cause a duplicate
    new_name_exists <- change_name %in% sfm[["model_units"]][["name"]] && change_name != name
    if (new_name_exists) {
      cli::cli_abort("A custom unit with this name already exists: {.code {change_name}}")
    }
    
    # Update the unit name in the data frame
    old_idx <- which(sfm[["model_units"]][["name"]] == name)
    if (length(old_idx) > 0) {
      sfm[["model_units"]][old_idx, "name"] <- change_name
    }

    # Ensure the unit is translated in the entire model
    dict <- stats::setNames(change_name, paste0("^", name, "$"))

    # Update units and equations in variables data frame
    var_names <- get_model_var(sfm)
    
    for (i in seq_len(nrow(sfm[["variables"]]))) {
      if (is_defined(sfm[["variables"]][i, "units"])) {
        sfm[["variables"]][i, "units"] <- clean_unit(sfm[["variables"]][i, "units"], dict)
      }
      
      if (is_defined(sfm[["variables"]][i, "eqn"])) {
        old_eqn <- sfm[["variables"]][i, "eqn"]
        new_eqn <- clean_unit_in_u(old_eqn, dict)
        sfm[["variables"]][i, "eqn"] <- new_eqn
        
        # If equation changed, redo Julia translation
        if (old_eqn != new_eqn) {
          sfm[["variables"]][i, "eqn_julia"] <- convert_equations_julia(
            sfm[["variables"]][i, "type"], 
            sfm[["variables"]][i, "name"], 
            new_eqn,
            var_names,
            regex_units = dict
          )
        }
      }
    }
    
    sfm <- validate_xmile(sfm)
    return(sfm)
  }

  idx_nonexist <- which(!name %in% sfm[["model_units"]][["name"]])

  # Remove unit from model
  if (erase) {
    if (length(idx_nonexist) == 0) {
      sfm[["model_units"]] <- sfm[["model_units"]][!sfm[["model_units"]][["name"]] %in% name, ]
    } else {
      cli::cli_abort(
        "Cannot erase non-existent custom unit{ifelse(length(idx_nonexist) > 1, 's', '')}: {.code {paste0(name[idx_nonexist], collapse = ', ')}}"
      )
    }
  } else {
    # Change units to units valid for Julia's Unitful package
    regex_units <- get_regex_units()

    if (!is.null(change_name)) {
      if (length(name) > 1 || length(change_name) > 1) {
        cli::cli_abort(c(
          "Cannot rename multiple custom units at once.",
          "x" = "Please rename one unit at a time using {.fn model_units()}."
        ))
      }

      old_name <- name
      chosen_name <- change_name
    } else {
      chosen_name <- name
    }

    name <- vapply(chosen_name, function(x) {
      clean_unit(x, regex_units, unit_name = TRUE)
    }, character(1), USE.NAMES = FALSE)

    # Keep existing names the same
    name[!idx_nonexist] <- chosen_name[!idx_nonexist]

    idx_changed <- name != chosen_name

    # Check if unit already exists in unit package.
    # Default units cannot be overwritten
    name_in_units <- name %in% unname(regex_units)

    if (any(name_in_units)) {
      cli::cli_abort(sprintf(
        "The custom unit name%s %s match%s the standard unit%s %s, which cannot be overwritten.\nPlease choose %sunique name%s for: %s ",
        ifelse(sum(name_in_units) > 1, "s", ""),
        paste0(chosen_name[name_in_units], collapse = ", "),
        ifelse(sum(name_in_units) > 1, "", "es"),
        ifelse(sum(name_in_units) > 1, "s", ""),
        paste0(name[name_in_units], collapse = ", "),
        ifelse(sum(name_in_units) > 1, "", "a "),
        ifelse(sum(name_in_units) > 1, "s", ""),
        paste0(chosen_name[name_in_units], collapse = ", ")
      ))
    }

    # Check if all unit names contain at least one letter or digit
    idx_invalid <- !grepl("[a-zA-Z0-9]", name)

    if (any(idx_invalid)) {
      cli::cli_abort(sprintf(
        "Each custom unit name needs at least one letter or number.\nPlease choose %sdifferent name%s for: %s ",
        ifelse(sum(name_in_units) > 1, "", "a "),
        ifelse(sum(name_in_units) > 1, "s", ""),
        paste0(chosen_name[idx_invalid], collapse = ", ")
      ))
    }

    if (any(idx_changed)) {
      old_names <- paste0(chosen_name[idx_changed], collapse = ", ")
      new_names <- paste0(name[idx_changed], collapse = ", ")
      cli::cli_warn(c(
        "Custom unit name{ifelse(sum(idx_changed) > 1, 's', '')} modified for Julia compatibility.",
        "i" = "{ifelse(sum(idx_changed) > 1, 'Names', 'Name')} {ifelse(sum(idx_changed) > 1, 'were', 'was')}: {.code {old_names}} → {.code {new_names}}.",
        ">" = "Use {.fn model_units}({.code 'old_name'}, {.arg change_name} = {.code 'new_name'}) to update in your model."
      ))
    }


    if (!is.null(change_name)) {
      # Check if new name is already in use
      unit_exists <- change_name %in% sfm[["model_units"]][["name"]] && change_name != name
      
      if (!unit_exists) {
        # Update the unit name in the data frame
        old_idx <- which(sfm[["model_units"]][["name"]] == old_name)
        if (length(old_idx) > 0) {
          sfm[["model_units"]][old_idx, "name"] <- name
        }

        # Ensure the unit is translated in the entire model
        dict <- stats::setNames(name, paste0("^", old_name, "$"))

        # Update units and equations in variables data frame
        var_names <- get_model_var(sfm)
        
        for (i in seq_len(nrow(sfm[["variables"]]))) {
          if (is_defined(sfm[["variables"]][i, "units"])) {
            sfm[["variables"]][i, "units"] <- clean_unit(sfm[["variables"]][i, "units"], dict)
          }
          
          if (is_defined(sfm[["variables"]][i, "eqn"])) {
            old_eqn <- sfm[["variables"]][i, "eqn"]
            new_eqn <- clean_unit_in_u(old_eqn, dict)
            sfm[["variables"]][i, "eqn"] <- new_eqn
            
            # If equation changed, redo Julia translation
            if (old_eqn != new_eqn) {
              sfm[["variables"]][i, "eqn_julia"] <- convert_equations_julia(
                sfm[["variables"]][i, "type"], 
                sfm[["variables"]][i, "name"], 
                new_eqn,
                var_names,
                regex_units = dict
              )
            }
          }
        }
      } else {
        cli::cli_abort(sprintf(
          "A custom unit with this name already exists: %s",
          change_name
        ))
      }
    }

    # Get names of passed arguments
    passed_arg <- names(as.list(match.call())[-1]) |>
      # Remove some arguments
      setdiff(c("sfm", "erase", "change_name"))
    argg <- list()
    argg[["name"]] <- name

    if ("eqn" %in% passed_arg) {
      # Validate equation
      if (any(!nzchar(eqn))) {
        cli::cli_abort("eqn cannot be an empty string.")
      }
      eqn <- ensure_length(eqn, name)
      argg[["eqn"]] <- eqn
    }

    if ("doc" %in% passed_arg) {
      doc <- ensure_length(doc, name)
      argg[["doc"]] <- doc
    }

    new_units <- transpose_(argg) |> stats::setNames(name)

    # Add or update elements in the model_units data frame
    for (i in seq_along(name)) {
      unit_name_i <- name[i]
      
      # Check if unit already exists
      existing_idx <- which(sfm[["model_units"]][["name"]] == unit_name_i)
      
      if (length(existing_idx) > 0) {
        # Update existing unit row
        for (col in names(new_units[[i]])) {
          sfm[["model_units"]][existing_idx, col] <- new_units[[i]][[col]]
        }
      } else {
        # Add new unit row
        new_row <- as.data.frame(new_units[[i]], stringsAsFactors = FALSE)
        sfm[["model_units"]] <- bind_rows_(sfm[["model_units"]], new_row)
      }
    }
  }

  sfm <- validate_xmile(sfm)

  return(sfm)
}


#' Create, modify or remove a global variable or function
#'
#' Macros are global variables or functions that can be used throughout your stock-and-flow model. [macro()] adds, changes, or erases a macro.
#'
#' @inheritParams build
#' @param name Name of the macro. The equation will be assigned to this name.
#' @param eqn Equation of the macro. A character vector. Defaults to "0.0".
#' @param doc Documentation of the macro. Defaults to "".
#' @param change_name New name for macro (optional). Defaults to NULL to indicate no change.
#' @param erase If TRUE, remove macro from the model. Defaults to FALSE.
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#' @concept build
#' @export
#'
#' @examples
#'
#' # Simple function
#' sfm <- xmile() |>
#'   macro("double", eqn = "function(x) x * 2") |>
#'   build("a", "constant", eqn = "double(2)")
#'
#' # Function with defaults
#' sfm <- xmile() |>
#'   macro("scale", eqn = "function(x, factor = 10) x * factor") |>
#'   build("b", "constant", eqn = "scale(2)")
#'
#' # If the logistic() function did not exist, you could create it yourself:
#' sfm <- macro(xmile(), "func", eqn = "function(x, slope = 1, midpoint = .5){
#'    1 / (1 + exp(-slope*(x-midpoint)))
#'  }") |>
#'   build("c", "constant", eqn = "func(2, slope = 50)")
#'
macro <- function(sfm, name, eqn = "0.0", doc = "", units = NULL, change_name = NULL, erase = FALSE) {
  # Basic check
  if (missing(sfm)) {
    cli::cli_abort("The {.arg sfm} argument is required.")
  }

  check_xmile(sfm)

  if (missing(name)) {
    cli::cli_abort("The {.arg name} argument must be specified.")
  }

  # Check change name of variable
  if (!is.null(change_name)) {
    if (length(change_name) > 1 || length(name) > 1) {
      cli::cli_abort(c(
        "Cannot rename multiple macros at once.",
        "x" = "Please rename one macro at a time using {.fn macro()}."
      ))
    }
  }

  passed_arg <- names(as.list(match.call())[-1]) |>
    # Remove some arguments
    setdiff(c("sfm", "erase", "change_name"))

  names_df <- get_names(sfm)
  var_names <- get_model_var(sfm)

  idx_exist <- name %in% sfm[[P[["macro_name"]]]][["name"]]

  if (erase) {
    if (all(idx_exist)) {
      # Remove rows where name matches
      sfm[[P[["macro_name"]]]] <- sfm[[P[["macro_name"]]]][!sfm[[P[["macro_name"]]]][["name"]] %in% name, ]
    } else {
      missing_macros <- paste0(name[!idx_exist], collapse = ", ")
      existing_msg <- ifelse(nrow(sfm[[P[["macro_name"]]]]) > 0,
        paste0("Existing macros: ", paste0(sfm[[P[["macro_name"]]]][["name"]], collapse = ", ")),
        "Your model has no custom macros."
      )
      cli::cli_abort(c(
        "Cannot erase non-existent macro{ifelse(sum(!idx_exist) > 1, 's', '')}.",
        "x" = "The following {ifelse(sum(!idx_exist) > 1, 'macros do', 'macro does')} not exist: {.code {missing_macros}}.",
        "i" = existing_msg
      ))
    }
  } else {
    # Handle renaming with change_name
    if (!is.null(change_name)) {
      if (!idx_exist) {
        cli::cli_abort("Cannot rename a macro that does not exist: {.code {name}}")
      }
      
      # Check if new name would cause a duplicate BEFORE cleaning
      new_name_exists <- change_name %in% sfm[[P[["macro_name"]]]][["name"]]
      if (new_name_exists && change_name != name) {
        cli::cli_abort("A macro with this name already exists: {.code {change_name}}")
      }
      
      # Ensure new name is syntactically valid
      change_name_original <- change_name
      change_name <- clean_name(change_name, names_df[["name"]])
      
      # Report if name was changed for syntactic reasons
      if (change_name_original != change_name) {
        cli::cli_inform(c(
          "i" = "Name changed for syntactic validity: {.val {change_name_original}} -> {.code {change_name}}"
        ))
      }
      
      # Update the macro name in the data frame
      old_idx <- which(sfm[[P[["macro_name"]]]][["name"]] == name)
      sfm[[P[["macro_name"]]]][old_idx, "name"] <- change_name
      
      # Replace references to old name with new name everywhere in variables data frame
      for (i in seq_len(nrow(sfm[["variables"]]))) {
        if (is_defined(sfm[["variables"]][i, "eqn"])) {
          idx_df <- get_range_names(sfm[["variables"]][i, "eqn"], name, names_with_brackets = FALSE)
          if (nrow(idx_df) > 0) {
            # Reverse indices to replace correctly
            for (j in rev(seq_len(nrow(idx_df)))) {
              stringr::str_sub(sfm[["variables"]][i, "eqn"], idx_df[j, "start"], idx_df[j, "end"]) <- change_name
            }
            
            # Update Julia translation
            idx_df <- get_range_names(sfm[["variables"]][i, "eqn_julia"], name, names_with_brackets = FALSE)
            if (nrow(idx_df) > 0) {
              # Reverse indices to replace correctly
              for (j in rev(seq_len(nrow(idx_df)))) {
                stringr::str_sub(sfm[["variables"]][i, "eqn_julia"], idx_df[j, "start"], idx_df[j, "end"]) <- change_name
              }
            }
          }
        }
      }
      
      # If no equation update requested, exit here
      if (!"eqn" %in% passed_arg) {
        sfm <- validate_xmile(sfm)
        return(sfm)
      }
      
      name <- change_name
      idx_exist <- TRUE
    }

    # Handle new macros or equation updates
    argg <- list()
    
    # Ensure names are valid
    if (any(!idx_exist)) {
      # Create syntactically valid, unique names
      new_names <- clean_name(name[!idx_exist], names_df[["name"]])

      # Report if name was changed for syntactic reasons
      for (j in seq_along(name[!idx_exist])) {
        if (name[!idx_exist][j] != new_names[j]) {
          cli::cli_inform(c(
            "i" = "Name changed for syntactic validity: {.val {name[!idx_exist][j]}} -> {.code {new_names[j]}}"
          ))
        }
      }

      # Change name for new macros only
      name[!idx_exist] <- new_names
    }

    # Process equation if provided
    if ("eqn" %in% passed_arg) {
      regex_units <- get_regex_units()

      # Validate equations
      if (any(is.null(eqn))) {
        cli::cli_abort("Macro equation cannot be NULL.")
      }

      if (any(!nzchar(eqn))) {
        cli::cli_abort("eqn cannot be an empty string.")
      }

      # Change all equations to characters
      eqn <- as.character(eqn)

      # Ensure units are cleaned in u() in eqn
      eqn <- clean_unit_in_u(eqn, regex_units)
      eqn <- ensure_length(eqn, name)

      # Convert equation to Julia
      eqn_julia <- vapply(seq_along(name), function(i) {
        # Assign name already to convert functions correctly
        x <- paste0(name[i], " = ", eqn[i])

        convert_equations_julia(
          type = P[["macro_name"]], name = name[i], eqn = x,
          var_names = var_names,
          regex_units = regex_units
        )[["eqn_julia"]]
        # No need to save $func because delay family cannot be used for macros
      }, character(1), USE.NAMES = FALSE)

      argg[["eqn"]] <- eqn
      argg[["eqn_julia"]] <- eqn_julia
    }

    argg[["name"]] <- name

    if ("doc" %in% passed_arg) {
      doc <- ensure_length(doc, name)
      argg[["doc"]] <- doc
    }

    if ("units" %in% passed_arg) {
      units <- ensure_length(units, name)
      argg[["units"]] <- units
    }

    # Add default units if not provided
    if ("eqn" %in% passed_arg && !"units" %in% names(argg)) {
      argg[["units"]] <- rep("1", length(name))
    }

    # Convert to data frame format
    new_macros <- transpose_(argg) |> stats::setNames(name)

    # Add or update elements in the macro data frame
    for (i in seq_along(name)) {
      macro_name_i <- name[i]
      
      # Check if macro already exists
      existing_idx <- which(sfm[[P[["macro_name"]]]][["name"]] == macro_name_i)
      
      if (length(existing_idx) > 0) {
        # Update existing macro row (only update provided fields)
        for (col in names(new_macros[[i]])) {
          if (!is.null(new_macros[[i]][[col]])) {
            sfm[[P[["macro_name"]]]][existing_idx, col] <- new_macros[[i]][[col]]
          }
        }
      } else {
        # Add new macro row
        new_row <- as.data.frame(new_macros[[i]], stringsAsFactors = FALSE)
        sfm[[P[["macro_name"]]]] <- bind_rows_(sfm[[P[["macro_name"]]]], new_row)
      }
    }
  }

  sfm <- validate_xmile(sfm)

  return(sfm)
}


#' Modify header of stock-and-flow model
#'
#' The header of a stock-and-flow model contains metadata about the model, such as the name, author, and version. Modify the header of an existing model with standard or custom properties.
#'
#' @inheritParams build
#' @param name Model name. Defaults to "My Model".
#' @param caption Model description. Defaults to "My Model Description".
#' @param created Date the model was created. Defaults to Sys.time().
#' @param author Creator of the model. Defaults to "Me".
#' @param version Model version. Defaults to "1.0".
#' @param URL URL associated with model. Defaults to "".
#' @param doi DOI associated with the model. Defaults to "".
#' @param ... Optional other entries to add to the header.
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#' @concept build
#' @export
#'
#' @examples
#' sfm <- xmile() |>
#'   header(
#'     name = "My first model",
#'     caption = "This is my first model",
#'     author = "Kyra Evers",
#'     version = "1.1"
#'   )
header <- function(sfm, name = "My Model", caption = "My Model Description",
                   created = Sys.time(), author = "Me", version = "1.0", URL = "", doi = "", ...) {
  # Basic check
  if (missing(sfm)) {
    cli::cli_abort("The {.arg sfm} argument is required.")
  }

  check_xmile(sfm)

  # Get names of passed arguments
  passed_arg <- names(as.list(match.call())[-1]) |>
    # Remove some arguments
    setdiff(c("sfm", "..."))

  # Collect all arguments
  argg <- c(
    as.list(environment()),
    list(...)
  )[unique(passed_arg)]

  sfm[["header"]] <- utils::modifyList(sfm[["header"]], argg)

  sfm <- validate_xmile(sfm)

  return(sfm)
}


#' Modify simulation specifications
#'
#' Simulation specifications are the settings that determine how the model is simulated, such as the integration method (i.e. solver), start and stop time, and timestep. Modify these specifications for an existing stock-and-flow model.
#'
#' @inheritParams build
#' @param method Integration method. Defaults to "euler".
#' @param start Start time of simulation. Defaults to 0.
#' @param stop End time of simulation. Defaults to 100.
#' @param dt Timestep of solver; controls simulation accuracy. Smaller = more
#'   accurate but slower. Defaults to 0.01.
#' @param save_at Timestep at which to save computed values; controls output size.
#'   Must be >= dt. Use larger than dt to reduce memory without sacrificing accuracy.
#'   Example: dt = 0.01, save_at = 1 gives accurate simulation but only saves
#'   every 100th point. Defaults to dt (save everything).
#' @param save_from Time at which to start saving values. Use to discard initial
#'   transient behavior. Must be >= start. Defaults to start.
#' @param seed Seed number to ensure reproducibility across runs in case of random elements. Must be an integer. Defaults to NULL (no seed).
#' @param time_units Simulation time unit, e.g. 's' (second). Defaults to "s".
#' @param language Coding language in which to simulate model. Either "R" or "Julia". Julia is necessary for using units or delay functions. Defaults to "R".
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#' @concept simulate
#' @seealso [solvers()]
#' @export
#'
#' @examples
#' sfm <- xmile("predator_prey") |>
#'   sim_specs(start = 0, stop = 50, dt = 0.1)
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # Change the simulation method to "rk4"
#' sfm <- sim_specs(sfm, method = "rk4")
#'
#' # Change the time units to "years", such that one time unit is one year
#' sfm <- sim_specs(sfm, time_units = "years")
#'
#' # To save storage but not affect accuracy, use save_at and save_from
#' sfm <- sim_specs(sfm, save_at = 1, save_from = 10)
#' sim <- simulate(sfm)
#' head(as.data.frame(sim))
#'
#' # Add stochastic initial condition but specify seed to obtain same result
#' sfm <- sim_specs(sfm, seed = 1) |>
#'   build(c("predator", "prey"), eqn = "runif(1, 20, 50)")
#'
#' # Change the simulation language to Julia to use units
#' sfm <- sim_specs(sfm, language = "Julia")
#'
sim_specs <- function(sfm,
                      method = "euler",
                      start = "0.0",
                      stop = "100.0",
                      dt = "0.01",
                      save_at = dt,
                      save_from = start,
                      # adaptive = FALSE,
                      seed = NULL,
                      time_units = "s",
                      language = "R",
                      keep_nonnegative_flow = TRUE,
                      keep_nonnegative_stock = FALSE,
                      keep_unit = TRUE) {
  # Basic check
  if (missing(sfm)) {
    cli::cli_abort("The {.arg sfm} argument is required.")
  }

  check_xmile(sfm)

  # Get names of passed arguments
  passed_arg <- names(as.list(match.call())[-1]) |>
    # Remove some arguments
    setdiff(c("sfm"))

  if (!missing(start)) {
    start <- suppressWarnings(as.numeric(start))
    if (is.na(start)) {
      cli::cli_abort(c(
        "Invalid {.arg start} argument.",
        "x" = "The {.arg start} argument must be {.cls numeric}."
      ))
    }
  }

  if (!missing(stop)) {
    stop <- suppressWarnings(as.numeric(stop))
    if (is.na(stop)) {
      cli::cli_abort(c(
        "Invalid {.arg stop} argument.",
        "x" = "The {.arg stop} argument must be {.cls numeric}."
      ))
    }
  }

  if (!missing(dt)) {
    dt <- suppressWarnings(as.numeric(dt))
    if (is.na(dt)) {
      cli::cli_abort(c(
        "Invalid {.arg dt} argument.",
        "x" = "The {.arg dt} argument must be {.cls numeric}."
      ))
    }

    if (dt != 1) {
      if (dt > .1) {
        cli::cli_warn(c(
          "Large timestep detected ({.arg dt} = {.val {dt}}).",
          "i" = "This may lead to simulation inaccuracies.",
          ">" = "Consider using smaller timesteps for better accuracy."
        ))
      }
    }
  }

  if (!missing(save_at)) {
    save_at <- suppressWarnings(as.numeric(save_at))
    if (is.na(save_at)) {
      cli::cli_abort(c(
        "Invalid {.arg save_at} argument.",
        "x" = "The {.arg save_at} argument must be {.cls numeric}."
      ))
    }
  }

  if (!missing(save_from)) {
    save_from <- suppressWarnings(as.numeric(save_from))
    if (is.na(save_from)) {
      cli::cli_abort(c(
        "Invalid {.arg save_from} argument.",
        "x" = "The {.arg save_from} argument must be {.cls numeric}."
      ))
    }
  }

  # Ensure time_units are formatted correctly
  if (!missing(time_units)) {
    if (length(time_units) != 1) {
      cli::cli_abort(c(
        "Invalid {.arg time_units} argument.",
        "x" = "The {.arg time_units} argument must be a single {.cls character} string."
      ))
    }

    # Time units can only contain letters or spaces
    if (any(grepl("[^a-zA-Z _]", time_units))) {
      cli::cli_abort(c(
        "Invalid {.arg time_units} format.",
        "x" = "The {.arg time_units} argument can only contain letters, spaces, or underscores."
      ))
    }
    regex_time_units <- get_regex_time_units()
    time_units <- clean_unit(time_units, regex_time_units) # Units are not used in R, so translate to julia directly

    if (!any(time_units == unname(regex_time_units))) {
      cli::cli_abort(c(
        "Invalid time unit {.val {time_units}}.",
        "i" = "Available time units are: {paste0(unique(unname(regex_time_units)), collapse = ', ')}"
      ))
    }
  }

  if ("method" %in% passed_arg) {
    if (is.null(method) || any(is.na(method)) || !inherits(method, "character") || length(method) > 1) {
      cli::cli_abort(c(
        "Invalid {.arg method} argument.",
        "x" = "The {.arg method} argument must be a single {.cls character} string."
      ))
    }

    method <- trimws(method)
  }

  # Check coding language
  if ("language" %in% passed_arg) {
    language <- clean_language(language)

    # Translate method if method was not specified
    old_language <- sfm[["sim_specs"]][["language"]]
    if (!"method" %in% passed_arg && language != old_language) {
      method <- solvers(sfm[["sim_specs"]][["method"]],
        from = old_language, to = language,
        show_info = TRUE
      )

      if (is.null(method[["translation"]])) {
        method <- method[["alternatives"]][1]
      } else {
        method <- method[["translation"]]
      }
      passed_arg <- c(passed_arg, "method")
    } else if ("method" %in% passed_arg) {
      # If method was specified, check whether it is a valid method in the new coding language
      method <- solvers(method, from = language, show_info = TRUE)
      method <- method[["name"]]
    }
  } else if ("method" %in% passed_arg) {
    # If language was not specified but methods were, check method
    language <- sfm[["sim_specs"]][["language"]]
    method <- solvers(method, from = language, show_info = TRUE)
    method <- method[["name"]]
  }

  # Check whether start is smaller than stop
  if ("start" %in% passed_arg) {
    if (!"stop" %in% passed_arg) {
      stop <- as.numeric(sfm[["sim_specs"]][["stop"]])
    }
    if (start >= stop) {
      cli::cli_abort(c(
        "Invalid time interval.",
        "x" = "The {.arg start} ({.val {start}}) must be smaller than {.arg stop} ({.val {stop}})."
      ))
    }
  }

  if ("stop" %in% passed_arg) {
    if (!"start" %in% passed_arg) {
      start <- as.numeric(sfm[["sim_specs"]][["start"]])
    }
    if (start >= stop) {
      cli::cli_abort(c(
        "Invalid time interval.",
        "x" = "The {.arg start} ({.val {start}}) must be smaller than {.arg stop} ({.val {stop}})."
      ))
    }
  }

  # Check whether dt is smaller than stop; if not, stop
  if ("dt" %in% passed_arg) {
    if (!"stop" %in% passed_arg) {
      stop <- as.numeric(sfm[["sim_specs"]][["stop"]])
    }
    if (!"start" %in% passed_arg) {
      start <- as.numeric(sfm[["sim_specs"]][["start"]])
    }
    if (dt > (stop - start)) {
      cli::cli_abort(c(
        "Invalid {.arg dt} argument.",
        "x" = "The {.arg dt} ({.val {dt}}) must be smaller than the time interval ({.arg stop} - {.arg start} = {.val {stop - start}})."
      ))
    }
  }

  # Check whether save_at is smaller than stop; if not, stop
  if ("save_at" %in% passed_arg) {
    if (!"stop" %in% passed_arg) {
      stop <- as.numeric(sfm[["sim_specs"]][["stop"]])
    }
    if (!"start" %in% passed_arg) {
      start <- as.numeric(sfm[["sim_specs"]][["start"]])
    }
    if (!"save_from" %in% passed_arg) {
      save_from <- as.numeric(sfm[["sim_specs"]][["save_from"]])
    }
    if (save_at > (stop - start)) {
      cli::cli_abort(c(
        "Invalid {.arg save_at} argument.",
        "x" = "The {.arg save_at} ({.val {save_at}}) must be smaller than the time interval ({.arg stop} - {.arg start} = {.val {stop - start}})."
      ))
    }
    if (save_at > (stop - save_from)) {
      cli::cli_abort(c(
        "Invalid {.arg save_at} argument.",
        "x" = "The {.arg save_at} ({.val {save_at}}) must be smaller than the interval from {.arg save_from} ({.val {save_from}}) to {.arg stop} ({.val {stop}})."
      ))
    }
  }

  # Check whether dt is smaller than save_at; if not, set save_at to dt
  if ("dt" %in% passed_arg) {
    if ("save_at" %in% passed_arg) {
      if (dt > save_at) {
        cli::cli_warn(c(
          "Invalid {.arg dt} and {.arg save_at} relationship.",
          "x" = "{.arg dt} ({.val {dt}}) must be <= {.arg save_at} ({.val {save_at}}).",
          "i" = "Automatically setting {.arg save_at} equal to {.arg dt}."
        ))
        save_at <- dt
        passed_arg <- c(passed_arg, "save_at")
      }
    } else if (!"save_at" %in% passed_arg) {
      if (is_defined(sfm[["sim_specs"]][["save_at"]])) {
        if (dt > as.numeric(sfm[["sim_specs"]][["save_at"]])) {
          save_at <- dt
          passed_arg <- c(passed_arg, "save_at")
        }
      } else {
        save_at <- dt
        passed_arg <- c(passed_arg, "save_at")
      }
    }
  } else if ("save_at" %in% passed_arg) {
    # The above ifelse takes care of when save_at and dt are both not NULL; now only save_at can be not NULL
    if (is_defined(sfm[["sim_specs"]][["dt"]])) {
      if (save_at < as.numeric(sfm[["sim_specs"]][["dt"]])) {
        cli::cli_warn(c(
          "Invalid {.arg dt} and {.arg save_at} relationship.",
          "x" = "{.arg dt} must be smaller than or equal to {.arg save_at}.",
          "i" = "Automatically setting {.arg save_at} equal to {.arg dt}."
        ))
        save_at <- dt
        passed_arg <- c(passed_arg, "save_at")
      }
    }
  }

  # Check whether save_from is smaller than stop and larger than start; if not, stop
  if ("save_from" %in% passed_arg) {
    if (!"start" %in% passed_arg) {
      start <- as.numeric(sfm[["sim_specs"]][["start"]])
    }
    if (!"stop" %in% passed_arg) {
      stop <- as.numeric(sfm[["sim_specs"]][["stop"]])
    }

    if (save_from < start || save_from > stop) {
      cli::cli_abort(c(
        "Invalid {.arg save_from} argument.",
        "x" = "The {.arg save_from} ({.val {save_from}}) must be within the simulation time interval.",
        "i" = "Must satisfy: {.val {start}} <= {.arg save_from} <= {.val {stop}}"
      ))
    }
  } else {
    # Ensure that save_from stays within start and stop, also when save_from is not specified
    # When save_from is not specified, it is automatically updated to start
    if ("start" %in% passed_arg) {
      save_from <- start
      passed_arg <- c(passed_arg, "save_from")
    }
  }

  # Seed must be NULL or an integer
  if ("seed" %in% passed_arg) {
    if (!is.null(seed)) {
      if (nzchar(seed)) {
        seed <- strtoi(seed)

        if (is.na(seed)) {
          cli::cli_abort(c(
            "Invalid {.arg seed} argument.",
            "x" = "The {.arg seed} argument must be an {.cls integer}."
          ))
        }
        seed <- as.character(seed)
      } else {
        seed <- NULL
      }
    }
  }

  # Ensure no scientific notation is present
  if ("start" %in% passed_arg) {
    start <- replace_digits_with_floats(scientific_notation(start), NULL)
  }
  if ("stop" %in% passed_arg) {
    stop <- replace_digits_with_floats(scientific_notation(stop), NULL)
  }
  if ("dt" %in% passed_arg) {
    dt <- replace_digits_with_floats(scientific_notation(dt), NULL)
  }
  if ("save_at" %in% passed_arg) {
    save_at <- replace_digits_with_floats(scientific_notation(save_at), NULL)
  }
  if ("save_from" %in% passed_arg) {
    save_from <- replace_digits_with_floats(scientific_notation(save_from), NULL)
  }


  # Collect all arguments
  argg <- c(
    as.list(environment())
  )[unique(passed_arg)]

  # Overwrite simulation specifications
  sfm[["sim_specs"]] <- utils::modifyList(sfm[["sim_specs"]], argg)

  sfm <- validate_xmile(sfm)

  return(sfm)
}


#' Remove variable from stock-and-flow model
#'
#' @inheritParams build
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#' @noRd
#'
erase_var <- function(sfm, name) {
  # Remove variables from the data frame
  sfm[["variables"]] <- sfm[["variables"]][!sfm[["variables"]][["name"]] %in% name, ]
  
  # Remove references to these variables in 'to', 'from', 'source' columns
  if (nrow(sfm[["variables"]]) > 0) {
    sfm[["variables"]][sfm[["variables"]][["to"]] %in% name, "to"] <- ""
    sfm[["variables"]][sfm[["variables"]][["from"]] %in% name, "from"] <- ""
    sfm[["variables"]][sfm[["variables"]][["source"]] %in% name, "source"] <- ""
  }

  sfm <- validate_xmile(sfm)

  return(sfm)
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
  idx <- old_names != new_names
  if (any(idx)) {
    cli::cli_warn(c(
      "Name{ifelse(sum(idx) > 1, 's', '')} changed for syntactic validity or to avoid conflicts:",
      "i" = paste0(paste0("{.val {", old_names[idx], "}} -> {.code ", new_names[idx], "}"), collapse = ", ")
    ))
  }

  return(invisible())
}


#' Create, modify or remove variables
#'
#' Add, change, or erase variables in a stock-and-flow model. Variables may be stocks, flows, constants, auxiliaries, or graphical functions.
#'
#' @section Stocks: Stocks define the state of the system. They accumulate material or information over time, such as people, products, or beliefs, which creates memory and inertia in the system. As such, stocks need not be tangible. Stocks are variables that can increase and decrease, and can be measured at a single moment in time. The value of a stock is increased or decreased by flows. A stock may have multiple inflows and multiple outflows. The net change in a stock is the sum of its inflows minus the sum of its outflows.
#'
#' The obligatory properties of a stock are "name", "type", and "eqn". Optional additional properties are "units", "label", "doc", "non_negative".
#'
#' @section Flows: Flows move material and information through the system. Stocks can only decrease or increase through flows. A flow must flow from and/or flow to a stock. If a flow is not flowing from a stock, the source of the flow is outside of the model boundary. Similarly, if a flow is not flowing to a stock, the destination of the flow is outside the model boundary. Flows are defined in units of material or information moved over time, such as birth rates, revenue, and sales.
#'
#' The obligatory properties of a flow are "name", "type", "eqn", and either "from", "to", or both. Optional additional properties are "units", "label", "doc", "non_negative".
#'
#' @section Constants: Constants are variables that do not change over the course of the simulation - they are time-independent. These may be numbers, but also functions. They can depend only on other constants.
#'
#' The obligatory properties of a constant are "name", "type", and "eqn". Optional additional properties are "units", "label", "doc", "non_negative".
#'
#' @section Auxiliaries: Auxiliaries are dynamic variables that change over time. They are used for intermediate calculations in the system, and can depend on other flows, auxiliaries, constants, and stocks.
#'
#' The obligatory properties of an auxiliary are "name", "type", and "eqn". Optional additional properties are "units", "label", "doc", "non_negative".
#'
#' @section Graphical functions: Graphical functions, also known as table or lookup functions, are interpolation functions used to define the desired output (y) for a specified input (x). They are defined by a set of x- and y-domain points, which are used to create a piecewise linear function. The interpolation method defines the behavior of the graphical function between x-points ("constant" to return the value of the previous x-point, "linear" to linearly interpolate between defined x-points), and the extrapolation method defines the behavior outside of the x-points ("NA" to return NA values outside of defined x-points, "nearest" to return the value of the closest x-point).
#'
#' The obligatory properties of a graphical function are "name", "type", "xpts", and "ypts". "xpts" and "ypts" must be of the same length. Optional additional properties are "units", "label", "doc", "source", "interpolation", "extrapolation".
#'
#' @param sfm Stock-and-flow model, object of class [`sdbuildR_xmile`][xmile].
#' @param name Variable name. Character vector.
#' @param type Type of building block(s); one of 'stock', 'flow', 'constant', 'aux', or 'gf'). Does not need to be specified to modify an existing variable.
#' @param change_name New name for variable (optional). Defaults to NULL to indicate no change.
#' @param change_type New type for variable (optional). Defaults to NULL to indicate no change.
#' @param erase If TRUE, remove variable from model. Defaults to FALSE.
#' @param label Name of variable used for plotting. Defaults to the same as name.
#' @param eqn Equation (or initial value in the case of stocks). Defaults to "0.0".
#' @param to Target of flow. Must be a stock in the model. Defaults to NULL to indicate no target.
#' @param from Source of flow. Must be a stock in the model. Defaults to NULL to indicate no source.
#' @param units Unit of variable, such as 'meter'. Defaults to "1" (no units).
#' @param non_negative If TRUE, variable is enforced to be non-negative (i.e. strictly 0 or positive). Defaults to FALSE.
#' @param xpts Only for graphical functions: vector of x-domain points. Must be of the same length as ypts.
#' @param ypts Only for graphical functions: vector of y-domain points. Must be of the same length as xpts.
#' @param source Only for graphical functions: name of the variable which will serve as the input to the graphical function. Necessary to specify if units are used. Defaults to NULL.
#' @param interpolation Only for graphical functions: interpolation method. Must be either "constant" or "linear". Defaults to "linear".
#' @param extrapolation Only for graphical functions: extrapolation method. Must be either "nearest" or "NA". Defaults to "nearest".
#' @param doc Description of variable. Defaults to "" (no description).
#' @param df A data.frame with variable properties to add and/or modify. Each row represents one variable to build. Required columns depend on the variable type being created:
#'
#' - All types require: 'type', 'name'
#' - Stocks require: 'eqn' (initial value)
#' - Flows require: 'eqn', and at least one of 'from' or 'to'
#' - Constants require: 'eqn'
#' - Auxiliaries require: 'eqn'
#' - Graphical functions require: 'xpts', 'ypts'
#'
#' Optional columns for all types: 'units', 'label', 'doc', 'non_negative'
#' Optional columns for graphical functions: 'source', 'interpolation', 'extrapolation'
#'
#' Columns not applicable to a variable type should be set to NA. See Examples for a complete demonstration.
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#' @seealso [xmile()]
#' @concept build
#' @export
#'
#' @examples
#'
#' # First initialize an empty model
#' sfm <- xmile()
#' summary(sfm)
#' \dontshow{
#' sfm <- sim_specs(sfm, save_at = .5)
#' }
#'
#' # Add two stocks. Specify their initial values in the "eqn" property
#' # and their plotting label.
#' sfm <- build(sfm, "predator", "stock", eqn = 10, label = "Predator") |>
#'   build("prey", "stock", eqn = 50, label = "Prey")
#'
#'
#' # Add four flows: the births and deaths of both the predators and prey. The
#' # "eqn" property of flows represents the rate of the flow. In addition, we
#' # specify which stock the flow is coming from ("from") or flowing to ("to").
#' sfm <- build(sfm, "predator_births", "flow",
#'   eqn = "delta*prey*predator",
#'   label = "Predator Births", to = "predator"
#' ) |>
#'   build("predator_deaths", "flow",
#'     eqn = "gamma*predator",
#'     label = "Predator Deaths", from = "predator"
#'   ) |>
#'   build("prey_births", "flow",
#'     eqn = "alpha*prey",
#'     label = "Prey Births", to = "prey"
#'   ) |>
#'   build("prey_deaths", "flow",
#'     eqn = "beta*prey*predator",
#'     label = "Prey Deaths", from = "prey"
#'   )
#' plot(sfm)
#'
#' # The flows make use of four other variables: "delta", "gamma", "alpha", and
#' # "beta". Define these as constants in a vectorized manner for efficiency.
#' sfm <- build(sfm, c("delta", "gamma", "alpha", "beta"), "constant",
#'   eqn = c(.025, .5, .5, .05),
#'   label = c("Delta", "Gamma", "Alpha", "Beta"),
#'   doc = c(
#'     "Birth rate of predators", "Death rate of predators",
#'     "Birth rate of prey", "Death rate of prey by predators"
#'   )
#' )
#'
#' # We now have a complete predator-prey model which is ready to be simulated.
#' sim <- simulate(sfm)
#' plot(sim)
#'
#' # Modify a variable - note that we no longer need to specify type
#' sfm <- build(sfm, "delta", eqn = .03, label = "DELTA")
#'
#' # Change variable name (throughout the model)
#' sfm <- build(sfm, "delta", change_name = "DELTA")
#'
#' # Change variable type
#' sfm <- build(sfm, "DELTA", change_type = "stock")
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
get_building_block_prop <- function() {
  return(list(
    "stock" = c(
      "name", "type", "eqn", "units", "label", "doc",
      "non_negative",
      "eqn_julia"
    ),
    "flow" = c(
      "name", "type", "eqn", "to", "from", "units", "label", "doc",
      "non_negative",
      "eqn_julia"
    ),
    "constant" = c(
      "name", "type", "eqn", "units", "label", "doc",
      "non_negative",
      "eqn_julia"
    ),
    "aux" = c(
      "name", "type", "eqn", "units", "label", "doc",
      "non_negative",
      "eqn_julia"
    ),
    "gf" = c("name", "type", "units", "label", "xpts", "ypts", "source", "interpolation", "extrapolation", "doc")
  ))
}


#' Debug stock-and-flow model
#'
#' Check for common formulation problems in a stock-and-flow model.
#'
#' The following problems are detected:
#' - An absence of stocks
#' - Flows without a source (`from`) or target (`to`)
#' - Flows connected to a stock that does not exist
#' - Undefined variable references in equations
#' - Circularity in equations
#' - Connected stocks and flows without both having units or no units
#' - Missing unit definitions
#'
#' The following potential problems are detected:
#' - Absence of flows
#' - Stocks without inflows or outflows
#' - Equations with a value of 0
#'
#' @inheritParams build
#' @param quietly If TRUE, don't print problems. Defaults to FALSE.
#'
#' @returns If `quietly = FALSE`, list with problems and potential problems.
#' @concept build
#' @export
#'
#' @examples
#' # No issues
#' sfm <- xmile("SIR")
#' debugger(sfm)
#'
#' # Detect absence of stocks or flows
#' sfm <- xmile()
#' debugger(sfm)
#'
#' # Detect stocks without inflows or outflows
#' sfm <- xmile() |> build("Prey", "stock")
#' debugger(sfm)
#'
#' # Detect circularity in equation definitions
#' sfm <- xmile() |>
#'   build("Prey", "stock", eqn = "Predator") |>
#'   build("Predator", "stock", eqn = "Prey")
#' debugger(sfm)
#'
debugger <- function(sfm, quietly = FALSE) {
  check_xmile(sfm)

  if (!is.logical(quietly)) {
    cli::cli_abort(c(
      "Invalid {.arg quietly} argument.",
      "x" = "Must be {.code TRUE} or {.code FALSE}."
    ))
  }

  problems <- c()
  potential_problems <- c()

  # Get stock and flow names
  stock_names <- sfm[["variables"]][sfm[["variables"]][["type"]] == "stock", "name"]
  flow_df <- get_flow_df(sfm)
  flow_names <- flow_df[["name"]]

  ### Check whether all Stocks have inflows and/or outflows
  if (length(stock_names) > 0 && nrow(flow_df) > 0) {
    idx <- stock_names %in% flow_df[["to"]] | stock_names %in% flow_df[["from"]]

    if (any(!idx)) {
      potential_problems <- c(potential_problems, paste0(
        "* These stocks are not connected to any flows:\n- ",
        paste0(stock_names[!idx], collapse = ", ")
      ))
    }
  } else if (length(stock_names) == 0) {
    problems <- c(problems, "* Your model has no stocks.")
  }

  ### Check whether all flows either have a from or to property
  if (length(flow_names) > 0) {
    idx <- !nzchar(flow_df[["from"]]) & !nzchar(flow_df[["to"]])

    if (any(idx)) {
      problems <- c(problems, paste0(
        "* These flows are not connected to any stock:\n- ",
        paste0(flow_names[idx], collapse = ", "), "\nConnect a flow to a stock using 'to' and/or 'from' in build()."
      ))
    }

    ### Find whether the from and to stocks exist
    idx_to <- (!flow_df[["to"]] %in% stock_names) & nzchar(flow_df[["to"]])
    idx_from <- (!flow_df[["from"]] %in% stock_names) & nzchar(flow_df[["from"]])

    if (any(idx_to) || any(idx_from)) {
      problems <- c(problems, paste0(
        "* These flows are connected to a stock that does not exist:\n - ",
        paste0(c(flow_names[idx_to], flow_names[idx_from]), collapse = ", ")
      ))
    }
  } else {
    potential_problems <- c(potential_problems, "* Your model has no flows.")
  }

  ### Check equations with zero
  zero_idx <- sfm[["variables"]][["eqn"]] %in% c("0", "0.0")
  if (any(zero_idx)) {
    zero_eqn <- sfm[["variables"]][zero_idx, "name"]
    potential_problems <- c(
      potential_problems,
      paste0(
        "* These variables have an equation of 0:\n- ",
        paste0(zero_eqn, collapse = ", ")
      )
    )
  }

  ### Check for missing equations in dynamic variables
  eqn_vec <- sfm[["variables"]][["eqn"]]
  eqn_vec[is.na(eqn_vec)] <- ""

  missing_eqn_idx <- sfm[["variables"]][["type"]] %in% c("flow", "aux", "constant", "gf") &
    (!nzchar(eqn_vec))

  if (any(missing_eqn_idx)) {
    missing_eqn <- sfm[["variables"]][missing_eqn_idx, "name"]
    problems <- c(problems, paste0(
      "* eqn cannot be an empty string. Missing for: ",
      paste0(missing_eqn, collapse = ", ")
    ))
  }

  ### Detect undefined variable references in equations
  out <- detect_undefined_var(sfm)
  if (out[["issue"]]) {
    problems <- c(problems, paste0("* ", out[["msg"]]))
  }

  ### Detect circularity in equations
  out <- order_equations(sfm, print_msg = FALSE)
  if (out[["static"]][["issue"]]) {
    problems <- c(
      problems,
      paste0("* ", out[["static"]][["msg"]], collapse = "")
    )
  }
  if (out[["dynamic"]][["issue"]]) {
    problems <- c(
      problems,
      paste0("* ", out[["dynamic"]][["msg"]], collapse = "")
    )
  }

  ### Find missing unit definitions
  regex_units <- get_regex_units()

  # Check whether all units are defined
  macro_eqns <- if (nrow(sfm[[P[["macro_name"]]]]) > 0 && "eqn_julia" %in% names(sfm[[P[["macro_name"]]]])) {
    sfm[[P[["macro_name"]]]][["eqn_julia"]]
  } else {
    character(0)
  }

  add_model_units <- detect_undefined_units(
    sfm,
    new_eqns = c(sfm[["variables"]][["eqn_julia"]], macro_eqns),
    new_units = sfm[["variables"]][["units"]],
    regex_units = regex_units,
    R_or_Julia = "Julia"
  )
  if (length(add_model_units) > 0) {
    problems <- c(problems, paste0(
      "* These units are not defined:\n- ",
      paste0(names(add_model_units), collapse = ", ")
    ))
  }

  if (!quietly && length(problems) > 0) {
    cli::cli_inform("Problems:")
    cli::cli_inform(paste0(problems, collapse = "\n\n"))
  } else if (!quietly) {
    cli::cli_inform("No problems detected!")
  }

  if (!quietly && length(potential_problems) > 0) {
    prefix <- ifelse(!quietly & length(problems) > 0, "\n", "")
    cli::cli_inform(paste0(prefix, "Potentially problematic:"))
    cli::cli_inform(paste0(potential_problems, collapse = "\n\n"))
  }

  if (quietly) {
    return(list(
      problems = paste0(problems, collapse = "\n\n"),
      potential_problems = paste0(potential_problems, collapse = "\n\n")
    ))
  } else {
    return(invisible())
  }
}


#' Check whether static variables (stock's initial values, constants) depend on dynamic variables
#'
#' @inheritParams build
#'
#' @noRd
#' @returns Logical value
static_depend_on_dyn <- function(sfm) {
  # Get static and dynamic variable names
  static_idx <- sfm[["variables"]][["type"]] %in% c("stock", "constant")
  dynamic_idx <- sfm[["variables"]][["type"]] %in% c("aux", "flow")
  
  static_vars <- sfm[["variables"]][static_idx, ]
  dynamic_var <- sfm[["variables"]][dynamic_idx, "name"]
  
  # Check dependencies
  dependencies <- find_dependencies_(sfm, eqns = static_vars[["eqn"]], only_model_var = TRUE)
  names(dependencies) <- static_vars[["name"]]
  
  static_with_dyn_dep <- lapply(dependencies, function(x) {
    x[x %in% dynamic_var]
  }) |> compact_()

  if (length(static_with_dyn_dep) > 0) {
    static_with_dyn_dep <- vapply(static_with_dyn_dep, paste0, character(1), collapse = ", ")
    stock_or_constant <- sfm[["variables"]][match(names(static_with_dyn_dep), sfm[["variables"]][["name"]]), "type"]

    msg <- paste0(
      c(
        "Simulation impossible: static variables depend on dynamic variables!",
        paste0(
          paste0(
            "- ",
            ifelse(stock_or_constant == "stock", "The initial value of stock ", "The constant "),
            names(static_with_dyn_dep), " depends on ", static_with_dyn_dep
          ),
          collapse = "\n"
        )
      ),
      collapse = "\n"
    )

    return(list(issue = TRUE, msg = msg))
  } else {
    return(list(issue = FALSE))
  }
}


#' Convert stock-and-flow model to data frame
#'
#' Create a data frame with properties of all model variables, model units, and macros. Specify the variable types, variable names, and/or properties to get a subset of the data frame.
#'
#' @inheritParams plot.sdbuildR_xmile
#' @param type Variable types to retain in the data frame. Must be one or more of 'stock', 'flow', 'constant', 'aux', 'gf', 'macro', or 'model_units'. Defaults to NULL to include all types.
#' @param name Variable names to retain in the data frame. Defaults to NULL to include all variables.
#' @param properties Variable properties to retain in the data frame. Defaults to NULL to include all properties.
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional Ignored parameter.
#'
#' @returns A data.frame with one row per model component (variable, unit definition, or macro).
#'   Common columns include \code{type} (component type), \code{name} (variable name),
#'   \code{eqn} (equation), \code{units} (units of measurement), and \code{label}
#'   (descriptive label). Additional columns may include \code{to}, \code{from},
#'   \code{non_negative}, and others depending on variable types. The exact columns returned
#'   depend on the \code{type} and \code{properties} arguments. Returns an empty data.frame
#'   if no components match the filters.
#' @export
#' @concept build
#' @method as.data.frame sdbuildR_xmile
#'
#' @examples as.data.frame(xmile("SIR"))
#'
#' # Only show stocks
#' as.data.frame(xmile("SIR"), type = "stock")
#'
#' # Only show equation and label
#' as.data.frame(xmile("SIR"), properties = c("eqn", "label"))
#'
as.data.frame.sdbuildR_xmile <- function(x,
                                         row.names = NULL, optional = FALSE,
                                         type = NULL, name = NULL,
                                         properties = NULL, ...) {
  check_xmile(x)
  sfm <- x

  # Only keep specified types
  if (!is.null(type)) {
    type <- clean_type(type)

    if (length(type) == 0) {
      cli::cli_abort("At least one {.arg type} must be specified")
    }

    if (!all(type %in% c("stock", "flow", "constant", "aux", "gf", "model_units", "macro"))) {
      cli::cli_abort(c(
        "Invalid {.arg type} value.",
        "x" = "Must be one or more of {.code 'stock'}, {.code 'flow'}, {.code 'constant'}, {.code 'aux'}, {.code 'gf'}, {.code 'macro'}, or {.code 'model_units'}."
      ))
    }
  }

  df <- data.frame()

  # Add model variables - already in data frame format!
  if ((is.null(type) || any(c("stock", "flow", "constant", "aux", "gf") %in% type)) && nrow(sfm[["variables"]]) > 0) {
    var_df <- sfm[["variables"]]
    
    # Filter by type if specified
    if (!is.null(type)) {
      var_types <- type[type %in% c("stock", "flow", "constant", "aux", "gf")]
      var_df <- var_df[var_df[["type"]] %in% var_types, , drop = FALSE]
    }
    
    # Convert list-columns to character strings for display
    if (nrow(var_df) > 0) {
      gf_idx <- var_df[["type"]] == "gf"
      if (any(gf_idx)) {
        var_df[gf_idx, "xpts"] <- lapply(var_df[gf_idx, "xpts"], function(x) {
          paste0(x, collapse = ", ")
        })
        var_df[gf_idx, "ypts"] <- lapply(var_df[gf_idx, "ypts"], function(x) {
          paste0(x, collapse = ", ")
        })
      }
    }
    
    df <- bind_rows_(df, var_df)
  }

  # Add model units
  if ((is.null(type) || "model_units" %in% type) && nrow(sfm[["model_units"]]) > 0) {
    units_df <- sfm[["model_units"]]
    units_df[["prefix"]] <- NULL
    units_df[["type"]] <- "model_units"
    df <- bind_rows_(df, units_df)
  }

  # Add macros
  if ((is.null(type) || P[["macro_name"]] %in% type) && nrow(sfm[[P[["macro_name"]]]]) > 0) {
    macro_df <- sfm[[P[["macro_name"]]]]
    macro_df[["type"]] <- P[["macro_name"]]
    df <- bind_rows_(df, macro_df)
  }

  if (nrow(df) == 0) {
    return(df)
  }

  # Only keep specified names
  if (!is.null(name)) {

    # Clean names
    name <- Filter(nzchar, unique(name))

    if (length(name) == 0) {
      cli::cli_abort("At least one {.arg name} must be specified")
    }

    # Check if names exist
    idx_exist <- name %in% df[["name"]]
    if (!all(idx_exist)) {
      cli::cli_abort(sprintf(
        "The variable%s %s %s not exist in your model!",
        ifelse(length(name[!idx_exist]) > 1, "s", ""),
        paste0(name[!idx_exist], collapse = ", "),
        ifelse(length(name[!idx_exist]) > 1, "do", "does")
      ))
    }
    df <- df[df[["name"]] %in% name, , drop = FALSE]
    if (nrow(df) == 0) {
      return(df)
    }
  }

  # Only keep columns that correspond to build() parameters
  allowed_props <- names(formals(build)) |>
    setdiff(c("sfm", "change_name", "change_type", "erase", "df", "..."))
  df <- df[, intersect(allowed_props, names(df)), drop = FALSE]

  # Only keep specified properties
  if (!is.null(properties)) {
    # Check if properties exist
    properties <- Filter(nzchar, unique(tolower(properties)))
    if (length(properties) == 0) {
      cli::cli_abort("At least one property must be specified")
    }

    # Internal properties that shouldn't be exposed to users
    internal_props <- c("eqn_julia", "prefix")
    existing_prop <- setdiff(Reduce(union, get_building_block_prop()), internal_props)
    idx_exist <- properties %in% existing_prop
    # prop_in_df <- properties %in% names(df)

    if (!all(idx_exist)) {
      cli::cli_abort(sprintf(
        "%s %s!",
        paste0(properties[!idx_exist], collapse = ", "),
        ifelse(length(properties[!idx_exist]) > 1, "are not existing properties", "is not an existing property")
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
  order_first <- c("type", "name", "eqn", "units", "label", "to", "from", "non_negative")

  # Get columns to prioritize (in order_first order)
  cols_first <- intersect(order_first, names(df))
  # Get remaining columns (in original order)
  cols_rest <- setdiff(names(df), order_first)
  # Combine columns (handles character(0) safely)
  new_cols <- c(cols_first, cols_rest)
  # Reorder data frame
  df <- df[, new_cols, drop = FALSE]

  # Make sure that for all columns, at least one row is not NA or empty
  # This is especially necessary when only interested in one type, e.g. macro or model_units

  # Convert empty strings to NA and keep columns with at least one non-NA
  df[] <- lapply(df, function(x) {
    x[x == ""] <- NA
    x
  })
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]

  # Handle row.names if provided
  if (!is.null(row.names)) {
    if (length(row.names) != nrow(df)) {
      cli::cli_abort(sprintf(
        "Length of row.names (%d) does not match number of rows (%d)",
        length(row.names), nrow(df)
      ))
    }
    rownames(df) <- row.names
  }

  return(df)
}


#' Print overview of stock-and-flow model
#'
#' Print summary of stock-and-flow model, including number of stocks, flows, constants, auxiliaries, graphical functions, macros, and custom model units, as well as simulation specifications and use of delay functions.
#'
#' @param object A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#' @inheritParams plot.sdbuildR_xmile
#'
#' @returns Summary object of class [summary.sdbuildR_xmile]
#' @concept build
#' @export
#' @seealso [build()]
#'
#' @examples
#' sfm <- xmile("SIR")
#' summary(sfm)
#'
summary.sdbuildR_xmile <- function(object, ...) {
  # Extract model components from data frame
  stocks <- object[["variables"]][object[["variables"]][["type"]] == "stock", "name"]
  flows <- object[["variables"]][object[["variables"]][["type"]] == "flow", "name"]
  constants <- object[["variables"]][object[["variables"]][["type"]] == "constant", "name"]
  auxs <- object[["variables"]][object[["variables"]][["type"]] == "aux", "name"]
  gfs <- object[["variables"]][object[["variables"]][["type"]] == "gf", "name"]
  
  # Handle empty data frames safely
  model_units_str <- if (nrow(object[["model_units"]]) > 0) object[["model_units"]][["name"]] else character(0)
  macro_str <- if (nrow(object[[P[["macro_name"]]]]) > 0) object[[P[["macro_name"]]]][["name"]] else character(0)

  # Check for delay functions
  delay_past <- get_delay(object, type = "past")
  delay_func <- get_delay(object, type = "delayN_smoothN")
  matched_time_unit <- find_matching_regex(
    object[["sim_specs"]][["time_units"]],
    get_regex_time_units()
  )

  # Create structured summary object - flat structure for easier access
  has_delays <- length(delay_past) > 0 || length(delay_func) > 0
  
  summary_obj <- list(
    stocks = stocks,
    flows = flows,
    constants = constants,
    aux = auxs,
    gf = gfs,
    macros = macro_str,
    model_units = model_units_str,
    delay_past = if (length(delay_past) > 0) unique(names(delay_past)) else character(0),
    delay_func = if (length(delay_func) > 0) unique(names(delay_func)) else character(0),
    has_delays = has_delays,
    sim_specs = list(
      start = object[["sim_specs"]][["start"]],
      stop = object[["sim_specs"]][["stop"]],
      dt = object[["sim_specs"]][["dt"]],
      save_at = object[["sim_specs"]][["save_at"]],
      save_from = object[["sim_specs"]][["save_from"]],
      time_units = matched_time_unit,
      method = object[["sim_specs"]][["method"]],
      seed = object[["sim_specs"]][["seed"]],
      language = object[["sim_specs"]][["language"]]
    )
  )

  class(summary_obj) <- "summary.sdbuildR_xmile"
  return(summary_obj)
}


#' Print method for summary.sdbuildR_xmile
#'
#' @param x A summary object of class [summary.sdbuildR_xmile]
#' @param ... Additional arguments (unused)
#'
#' @returns Invisibly returns the summary object of class [summary.sdbuildR_xmile]
#' @export
#' @concept build
print.summary.sdbuildR_xmile <- function(x, ...) {
  cli::cli_h1("Stock-and-Flow Model Summary")

  # Count and summarize components
  total_vars <- length(x$stocks) + length(x$flows) + length(x$constants) + 
                length(x$aux) + length(x$gf)
  total_macros <- length(x$macros)
  total_units <- length(x$model_units)

  # Quick summary line
  if (total_vars == 0 && total_macros == 0 && total_units == 0) {
    cli::cli_alert_info("Empty model")
  } else {
    summary_parts <- c()
    if (total_vars > 0) summary_parts <- c(summary_parts, "{.emph {total_vars}} variable{?s}")
    if (total_macros > 0) summary_parts <- c(summary_parts, "{.emph {total_macros}} macro{?s}")
    if (total_units > 0) summary_parts <- c(summary_parts, "{.emph {total_units}} custom unit{?s}")
    
    cli::cli_text(paste0(summary_parts, collapse = " • "))
  }

  # Model elements section - all components in one place
  if (length(x$stocks) > 0 || length(x$flows) > 0 || length(x$constants) > 0 || 
      length(x$aux) > 0 || length(x$gf) > 0 || length(x$macros) > 0) {
    cli::cli_h2("Model Elements")
    
    if (length(x$stocks) > 0) {
      cli::cli_bullets(c(
        "s" = "{.strong Stocks} ({length(x$stocks)}): {.code {x$stocks}}"
      ))
    }
    
    if (length(x$flows) > 0) {
      cli::cli_bullets(c(
        "s" = "{.strong Flows} ({length(x$flows)}): {.code {x$flows}}"
      ))
    }
    
    if (length(x$constants) > 0) {
      cli::cli_bullets(c(
        "s" = "{.strong Constants} ({length(x$constants)}): {.code {x$constants}}"
      ))
    }
    
    if (length(x$aux) > 0) {
      cli::cli_bullets(c(
        "s" = "{.strong Auxiliaries} ({length(x$aux)}): {.code {x$aux}}"
      ))
    }
    
    if (length(x$gf) > 0) {
      cli::cli_bullets(c(
        "s" = "{.strong Graphical Functions} ({length(x$gf)}): {.code {x$gf}}"
      ))
    }
    
    if (length(x$macros) > 0) {
      cli::cli_bullets(c(
        "s" = "{.strong Macros} ({length(x$macros)}): {.code {x$macros}}"
      ))
    }
  }

  # Custom units section
  if (length(x$model_units) > 0) {
    cli::cli_h2("Custom Units")
    cli::cli_bullets(c(
      "s" = "{.code {x$model_units}}"
    ))
  }

  # Delay functions section
  if (length(x$delay_past) > 0 || length(x$delay_func) > 0) {
    cli::cli_h2("Delay Functions")
    
    if (length(x$delay_past) > 0) {
      cli::cli_bullets(c(
        "i" = "{length(x$delay_past)} variable{?s} {?uses/use} {.code past()} or {.code delay()}: {.code {x$delay_past}}"
      ))
    }
    
    if (length(x$delay_func) > 0) {
      cli::cli_bullets(c(
        "i" = "{length(x$delay_func)} variable{?s} {?uses/use} {.code delayN()} or {.code smoothN()}: {.code {x$delay_func}}"
      ))
    }
  }

  # Simulation specifications
  cli::cli_h2("Simulation Settings")
  
  # Build simulation specs preserving cli formatting
  bullets <- c()
  
  bullets["i"] <- paste0(
    "{.strong Time}: ",
    x$sim_specs$start, " to ", x$sim_specs$stop, " ",
    x$sim_specs$time_units
  )
  
  stepping_parts <- c(paste0("dt = {.code ", x$sim_specs$dt, "}"))
  if (x$sim_specs$save_at != x$sim_specs$dt) {
    stepping_parts <- c(stepping_parts, paste0("save_at = {.code ", x$sim_specs$save_at, "}"))
  }
  if (x$sim_specs$save_from != x$sim_specs$start) {
    stepping_parts <- c(stepping_parts, paste0("save_from = {.code ", x$sim_specs$save_from, "}"))
  }
  
  bullets <- c(bullets, c("i" = paste0("{.strong Stepping}: ", paste0(stepping_parts, collapse = ", "))))
  bullets <- c(bullets, c("i" = paste0("{.strong Solver}: {.code ", x$sim_specs$method, "} in {.code ", x$sim_specs$language, "}")))
  
  if (is_defined(x$sim_specs$seed)) {
    bullets <- c(bullets, c("i" = paste0("{.strong Seed}: {.code ", x$sim_specs$seed, "}")))
  }
  
  cli::cli_bullets(bullets)

  invisible(x)
}
