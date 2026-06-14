#' Convert all R equations to Julia code
#'
#' @inheritParams update.sdbuildR
#' @inheritParams simulate_julia
#'
#' @returns Updated object
#' @noRd
#'
convert_equations_julia_wrapper <- function(object) {
  # Get variable names
  var_names <- get_model_var(object)

  # Initialize accumulators for auxiliary variables (similar to IM wrapper)
  accumulated_add_vars <- data.frame()

  # Update equations in variables data frame
  for (i in seq_len(nrow(object[["variables"]]))) {
    if (object[["variables"]][i, "type"] %in% c("stock", "flow", "constant", "aux")) {
      var_name <- object[["variables"]][i, "name"]
      var_type <- object[["variables"]][i, "type"]
      eqn_before <- object[["variables"]][i, "eqn"]

      out <- convert_equations_julia(
        var_type,
        var_name,
        eqn_before,
        var_names
      )

      object[["variables"]][i, "eqn"] <- out[["eqn"]]

      # Accumulate auxiliary variables
      if (nrow(out[["add_vars"]])) {
        accumulated_add_vars <- rbind(accumulated_add_vars, out[["add_vars"]])
      }
    }
  }


  # # Add accumulated auxiliary and graphical function variables to the model
  if (nrow(accumulated_add_vars)) {
    # Some Insight Maker columns may be missing, e.g., eqn_insightmaker
    missing_cols <- setdiff(colnames(object[["variables"]]), colnames(accumulated_add_vars))
    for (col in missing_cols) {
      accumulated_add_vars[[col]] <- NA
    }

    object[["variables"]] <- rbind(object[["variables"]], accumulated_add_vars)
  }

  # Funcs (in the variables data frame with type == "func")
  func_idx <- which(object[["variables"]][["type"]] == "func")
  if (length(func_idx) > 0) {
    for (i in func_idx) {
      row_list <- as.list(object[["variables"]][i, , drop = FALSE])

      # If a name is defined, assign func to that name (necessary for correct conversion of functions)
      if (nzchar(row_list[["name"]]) && !startsWith(row_list[["name"]], ".")) {
        row_list[["eqn"]] <- paste0(row_list[["name"]], " = ", row_list[["eqn"]])
      }

      out <- convert_equations_julia(
        P[["func_name"]],
        P[["func_name"]],
        row_list[["eqn"]],
        var_names
      )

      # Only update the eqn column from the conversion result
      if (!is.null(out[["eqn"]])) {
        object[["variables"]][i, "eqn"] <- out[["eqn"]]
      }
    }
  }

  object
}


#' Transform R code to Julia code
#'
#' @inheritParams update.sdbuildR
#' @inheritParams convert_equations_IM
#'
#' @returns List with flat structure:
#'   - eqn: Converted Julia equation
#'   - add_vars_aux: Auxiliary variables to add
#'   - doc: Documentation from comments
#'
#' @importFrom rlang .data
#' @noRd
#'
convert_equations_julia <- function(type, name, eqn, var_names) {
  if (P[["debug"]]) {
    # cli::cli_inform("")
    # cli::cli_inform(type)
    # cli::cli_inform(name)
    cli::cli_inform(eqn)
  }

  if (length(eqn) > 1) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg eqn} length.",
      ">" = "Must be length 1."
    ), call. = FALSE)
  }

  default_out <- list(
    eqn = "0.0",
    add_vars = data.frame(),
    doc = ""
  )

  # Check whether eqn is empty or NULL
  if (is.null(eqn) || !nzchar(eqn)) {
    return(default_out)
  }

  if (eqn == "0" || eqn == "0.0") {
    return(default_out)
  }

  # Try to parse the code
  parsed <- NULL
  out <- tryCatch(
    {
      parsed <- parse(text = eqn)
      TRUE
    },
    error = function(e) {
      e
    }
  )

  if ("error" %in% class(out)) {
    cli::cli_abort(c("x" = paste0(
      "Parsing equation of \"",
      name, "\" failed:\n", out[["message"]]
    ), call. = FALSE))
  }

  validate_julia_conversion_ast(parsed)

  if (any(grepl("na\\.rm", eqn))) {
    cli::cli_abort(c(
      "Argument {.arg na.rm} not supported.",
      ">" = "Use {.fn na.omit}(x) instead."
    ), call. = FALSE)
  }

  # Remove comments we don't keep these
  eqn <- remove_comments(eqn)[["eqn"]]

  # If equation is now empty, don't run rest of functions but set equation to zero
  if (!nzchar(eqn) || eqn == "0" || eqn == "0.0") {
    return(default_out)
  } else {
    if (!identical(getOption("sdbuildR.use_ast", TRUE), FALSE)) {
      ast <- convert_eqn_ast_julia(eqn, var_names)
      if (!is.null(ast)) {
        return(list(
          eqn = ast,
          add_vars = data.frame(),
          doc = ""
        ))
      }
    }

    # Ensure there is no scientific notation
    eqn <- scientific_notation(eqn)

    # Step 2. Syntax (bracket types, destructuring assignment, time units {1 Month})
    eqn <- eqn |>
      # Translate vector brackets, i.e.,c() -> []
      vector_to_square_brackets(var_names) |>
      # Ensure integers are floats
      # Julia can throw InexactError errors in case e.g., an initial condition is defined as an integer
      replace_digits_with_floats(var_names)

    # # Destructuring assignment, e.g., x, y <- {a, b}
    # **to do
    # conv_destructuring_assignment()

    # Step 3. Statements (if, for, while, functions, try)
    eqn <- convert_all_statements_julia(eqn, var_names)

    # Step 4. Operators (booleans, logical operators, addition of strings)
    eqn <- eqn |>
      # # Convert addition of strings to paste0
      # conv_addition_of_strings(var_names) |>
      # # Replace logical operators (true, false, = (but not if in function()))
      replace_op_julia(var_names) #|>
    # # Replace range, e.g., range(0, 10, 2) -> 0:2:10
    # replace_range_julia(var_names)

    # Step 5. Replace R functions to Julia functions
    conv_list <- convert_builtin_functions_julia(type, name, eqn, var_names)
    eqn <- conv_list[["eqn"]]
    add_vars <- conv_list[["add_vars"]]


    # **to do:
    #     <<- --> global
    # <- -> =

    # Remove spaces in front of new lines
    eqn <- gsub("[ ]*\n", "\n", eqn)

    # Replace single with double quotation marks
    eqn <- gsub("'", "\"", eqn, fixed = TRUE)

    return(list(
      eqn = eqn,
      add_vars = add_vars,
      doc = ""
    ))
  }
}


#' Remove scientific notation from string
#'
#' @inheritParams convert_equations_IM
#' @param task String with either "remove" or "add" to remove or add scientific notation
#' @param digits_max Number of digits after which to use scientific notation; ignored if task = "remove"; defaults to 15
#'
#' @returns Updated eqn
#' @noRd
#'
scientific_notation <- function(eqn, task = c("remove", "add")[1], digits_max = 15) {
  eqn <- as.character(eqn)

  if (task == "remove") {
    # scientific <- FALSE
    # Regex for scientific notation
    pattern <- "-?(?:\\d+\\.?\\d*|\\.\\d+)[eE][+-]?\\d+"
  } else if (task == "add") {
    # scientific <- TRUE
    # pattern = "\\d+"
    pattern <- "-?(?:\\d+\\.?\\d*|\\.\\d+)"
  }

  # Function to reformat scientific notation to fixed format
  reformat_scientific <- function(match) {
    # Convert digit match to numeric
    num <- as.numeric(match)

    # Keep any white space padding
    leading_whitespace <- stringr::str_extract(match, "^[ ]*")
    following_whitespace <- stringr::str_extract(match, "[ ]*$ ")

    # Format to scientific notation if maximum digits are exceeded
    if (task == "add") {
      # Vectorized check - use ifelse instead of if
      exceeds_max <- nchar(format(num, scientific = FALSE)) > digits_max

      replacement <- ifelse(
        exceeds_max,
        paste0(
          ifelse(is.na(leading_whitespace), "", leading_whitespace),
          format(num, scientific = TRUE, trim = TRUE),
          ifelse(is.na(following_whitespace), "", following_whitespace)
        ),
        match # Change nothing if not exceeding max
      )
    } else if (task == "remove") {
      replacement <- paste0(
        ifelse(is.na(leading_whitespace), "", leading_whitespace),
        format(num, scientific = FALSE),
        ifelse(is.na(following_whitespace), "", following_whitespace)
      )
    }

    return(replacement) # Convert back to fixed string
  }

  # Replace scientific notation in the string
  eqn <- stringr::str_replace_all(
    eqn,
    pattern = pattern,
    replacement = reformat_scientific
  )

  return(eqn)
}
