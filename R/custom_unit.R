#' Create or modify custom units
#'
#' Add or change custom units in a stock-and-flow model. Custom units offer greater flexibility in defining units that are not part of the standard library. Custom units may be new base units, or may be defined in terms of other (custom) units. See [u()] for more information on the rules of specifying units. Note that units are only supported in Julia, not in R.
#'
#' @inheritParams build
#' @param name Name of unit. A character vector.
#' @param eqn Definition of unit. String or vector of unit definitions. Defaults to `1` to indicate a base unit not defined in terms of other units.
#' @param doc Documentation of unit.
#'
#' @returns A stock-and-flow model object of class [`sdbuildR`][sdbuildR].
#'
#' @export
#' @concept units
#' @seealso [unit_prefixes()], [discard()], [change_name()]
#'
#' @examplesIf is_julia_ready()
#' # Units are only supported with Julia
#' sfm <- sdbuildR("Crielaard2022")
#' sfm <- custom_unit(sfm, BMI, eqn = kg/m^2, doc = "Body Mass Index")
#'
#' # You may also use words rather than symbols for the unit definition.
#' # The following modifies the unit BMI:
#' sfm <- custom_unit(sfm, BMI, eqn = kilogram/meters^2)
#'
#' # Rename unit:
#' sfm <- change_name(sfm, BMI, BodyMassIndex)
#' 
#' # Remove unit:
#' sfm <- discard(sfm, BodyMassIndex)
#'
#' # Unit names may need to be changed to be syntactically valid or to avoid
#' # overlap with existing units:
#' sfm <- custom_unit(sdbuildR(), C0^2)
#'
custom_unit <- function(sfm, name, eqn = 1, doc = "") {
  # Basic check
  if (missing(sfm)) {
    missing_arg("sfm")
  }
  check_sdbuildR(sfm)

  if (missing(name)) {
    missing_arg("name")
  }
  name_expr <- rlang::enexpr(name)
  .check_name_not_sdbuildR(name_expr, rlang::caller_env())
  name <- .expr_to_char(name_expr)
  eqn  <- .expr_to_char(rlang::enexpr(eqn))

  # Gather arguments for validation and processing
  passed_arg <- setdiff(names(match.call()[-1]), "sfm")
  args <- mget(passed_arg)
  args <- do.call(.validate_build_args, args)

  # Ensure length of vector arguments matches length of name
  args <- .ensure_length_build_args(args)

  unit_names <- sfm[["custom_unit"]][["name"]]
  idx_nonexist <- which(!name %in% unit_names)

  chosen_name <- name
  regex_units <- get_regex_units()

  # Change units to units valid for Julia's Unitful package
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
    n <- sum(name_in_units)
    cli::cli_abort(c(
      "x" = "{cli::qty(n)}Unallowed name{?s} for custom unit{?s}.",
      "i" = "Names of custom units cannot be the same as standard units.",
      ">" = "Choose new name{?s} for: {.val {chosen_name[name_in_units]}}." 
    ))

  }

  # Check if all unit names contain at least one letter or digit
  idx_invalid <- !grepl("[a-zA-Z0-9]", name)

  if (any(idx_invalid)) {
    bad_names <- chosen_name[idx_invalid]
    n <- length(bad_names)
    cli::cli_abort(c(
      "x" = "{cli::qty(n)}Invalid custom unit name{?s}.",
      "i" = "{.code {bad_names}} {?does/do} not contain any letters or numbers.",
      ">" = "{cli::qty(n)}Choose a different name{?s}."
    ))
  }

  # Enforce unique names: new unit names must not clash with variable names
  if (length(idx_nonexist) > 0) {
    var_names <- sfm[["variables"]][["name"]]
    clashing <- name[idx_nonexist] %in% var_names
    if (any(clashing)) {
      clash_names <- name[idx_nonexist][clashing]
      n <- length(clash_names)
      cli::cli_abort(c(
        "x" = "{cli::qty(n)}Custom unit name{?s} {.val {clash_names}} conflict{?s/} with existing variable{?s}.",
        "i" = "Variable and unit names must be unique."
      ))
    }
  }

  if (any(idx_changed)) {
    n_changed <- sum(idx_changed)
    cli::cli_warn(c(
      "{cli::qty(n_changed)}{?A custom unit name was/Custom unit names were} modified for Julia compatibility.",
      "i" = paste0(
        paste0("{.code ", chosen_name[idx_changed], "} \u2192 {.code ", name[idx_changed], "}"),
        collapse = ", "
      ),
      ">" = "Use {.fn change_name} to rename units."
    ))
  }

  # Get names of passed arguments
  passed_arg <- names(as.list(match.call())[-1]) |>
    setdiff("sfm")
  argg <- list()
  argg[["name"]] <- name

  if ("eqn" %in% passed_arg) {
    eqn <- .validate_eqn_arg(eqn)
    eqn <- ensure_length(eqn, name)
    argg[["eqn"]] <- eqn
  }

  if ("doc" %in% passed_arg) {
    doc <- ensure_length(doc, name)
    argg[["doc"]] <- doc
  }

  new_units <- transpose_(argg) |> stats::setNames(name)

  # Add or update elements in the custom_unit data frame
  for (i in seq_along(name)) {
    unit_name_i <- name[i]

    # Check if unit already exists
    existing_idx <- which(sfm[["custom_unit"]][["name"]] == unit_name_i)

    if (length(existing_idx) > 0) {
      # Update existing unit row
      for (col in names(new_units[[i]])) {
        sfm[["custom_unit"]][existing_idx, col] <- new_units[[i]][[col]]
      }
    } else {
      # Add new unit row
      new_row <- as.data.frame(new_units[[i]], stringsAsFactors = FALSE)
      sfm[["custom_unit"]] <- bind_rows_(sfm[["custom_unit"]], new_row)
    }
  }

  # Clear assemble cache only for Julia (units only affect Julia scripts)
  if (sfm[["sim_specs"]][["language"]] == "Julia") {
    sfm <- invalidate_assemble(sfm, "units")
  }

  sfm <- sanitize_sdbuildR(sfm)
  validate_sdbuildR(sfm)
  sfm
}
