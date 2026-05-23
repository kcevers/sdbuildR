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
      "Invalid {.arg eqn} length.",
      "x" = "Must be length 1."
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
  out <- tryCatch(
    {
      parse(text = eqn)
      TRUE
    },
    error = function(e) {
      return(e)
    }
  )

  if ("error" %in% class(out)) {
    cli::cli_abort(paste0(
      "Parsing equation of \"",
      name, "\" failed:\n", out[["message"]]
    ), call. = FALSE)
  }

  if (any(grepl("%%", eqn))) {
    cli::cli_abort(c(
      "Modulus operator not supported.",
      "x" = "The operator {.code a %% b} is not supported.",
      ">" = "Use {.fn mod}(a, b) instead."
    ), call. = FALSE)
  }

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
    eqn <- stringr::str_replace_all(eqn, "[ ]*\n", "\n")

    # Replace single with double quotation marks
    eqn <- stringr::str_replace_all(eqn, "\'", "\"")

    return(list(
      eqn = eqn,
      add_vars = add_vars,
      doc = ""
    ))
  }
}


#' Get indices of digits in string
#'
#' @inheritParams convert_equations_julia
#'
#' @returns data.frame with start and end indices of digits
#' @noRd
#' @keywords internal
#'
get_range_digits <- function(eqn, var_names) {
  get_range_digits_julia(eqn, var_names)
}


#' Replace digits with floats in string
#'
#' @inheritParams convert_equations_julia
#'
#' @returns Updated string
#' @noRd
#' @keywords internal
#'
replace_digits_with_floats <- function(eqn, var_names) {
  replace_digits_with_floats_julia(eqn, var_names)
}


#' Translate R operators to Julia
#'
#' @inheritParams convert_equations_julia
#' @returns Updated eqn
#' @importFrom rlang .data
#' @noRd
#' @keywords internal
#'
replace_op_julia <- function(eqn, var_names) {
  replace_op_julia_impl(eqn, var_names)
}


#' Find all round brackets
#'
#' Helper for convert_all_statements_julia()
#'
#' @param df data.frame with indices
#' @param round_brackets data.frame with indices of round brackets
#' @inheritParams convert_equations_julia
#'
#' @returns Modified data.frame
#' @noRd
#' @keywords internal
#'
find_round_brackets <- function(df, round_brackets, eqn, var_names) {
  find_round_brackets_julia(df, round_brackets, eqn, var_names)
}


#' Find all curly brackets
#'
#' Helper for convert_all_statements_julia()
#'
#' @param df data.frame with indices
#' @param paired_idxs data.frame with indices
#'
#' @returns Modified data.frame
#' @noRd
#' @keywords internal
#'
find_curly_brackets <- function(df, paired_idxs) {
  find_curly_brackets_julia(df, paired_idxs)
}


#' Convert all statement syntax from R to Julia
#' Wrapper around convert_statement()
#'
#' @inheritParams convert_equations_IM
#'
#' @returns Updated eqn
#' @noRd
#'
convert_all_statements_julia <- function(eqn, var_names) {
  # eqn_old <- eqn

  # If curly brackets surround entire eqn, replace and surround with begin ... end
  if (stringr::str_sub(eqn, 1, 1) == "{" && stringr::str_sub(eqn, nchar(eqn), nchar(eqn)) == "}") {
    stringr::str_sub(eqn, nchar(eqn), nchar(eqn)) <- "\nend"
    stringr::str_sub(eqn, 1, 1) <- "begin\n"
  }

  # Only if there are curly brackets in the equation, look for statements
  if (grepl("\\{", eqn)) {
    done <- FALSE
    i <- 1 # counter

    # Define regular expressions for statements, accounting for whitespace
    statement_regex <- c(
      "for" = "for[ ]*\\(",
      "if" = "if[ ]*\\(",
      "while" = "while[ ]*\\(", "else" = "[ ]*else[ ]*\\{",
      "else if" = "[ ]*else if[ ]*\\(", "function" = "function[ ]*\\("
    )

    while (!done) {
      # Create sequence of indices of curly brackets; update each iteration
      paired_idxs <- get_range_all_pairs(eqn, var_names, type = "curly")

      # Look for statements
      idx_statements <- stringr::str_locate_all(eqn, unname(statement_regex))
      df_statements <- as.data.frame(do.call(rbind, idx_statements))
      df_statements[["statement"]] <- rep(
        names(statement_regex),
        vapply(idx_statements, nrow, numeric(1))
      )

      # # Remove those matches that are in quotation marks or names
      idxs_exclude <- get_seq_exclude(eqn, var_names, type = "quot")
      if (nrow(df_statements) > 0) df_statements <- df_statements[!(df_statements[["start"]] %in% idxs_exclude | df_statements[["end"]] %in% idxs_exclude), ]

      if (!(nrow(paired_idxs) > 0 && nrow(df_statements) > 0)) {
        done <- TRUE
      } else {
        # Sort by start index
        paired_idxs <- paired_idxs[order(paired_idxs[["start"]]), ]

        # Get all round brackets
        round_brackets <- get_range_all_pairs(eqn, var_names, type = "round")

        df_statements <- df_statements[order(df_statements[["start"]]), ]

        # Step 1: Group by 'end' and keep row with minimum 'start' value for each group
        df_grouped <- split(df_statements, df_statements[["end"]])
        df_min_rows <- do.call(rbind, lapply(df_grouped, function(group) {
          min_start_idx <- which.min(group[["start"]])
          group[min_start_idx, ]
        }))

        # Step 2: Add row numbers as 'id' column
        df_min_rows[["id"]] <- seq_len(nrow(df_min_rows))

        # Step 3: Apply find_round_brackets function to each row
        df_with_round <- do.call(rbind, lapply(seq_len(nrow(df_min_rows)), function(i) {
          row_data <- df_min_rows[i, ]
          result <- find_round_brackets(row_data, round_brackets, eqn, var_names)
          result[["id"]] <- i # Preserve the id
          result
        }))

        # Step 4: Apply find_curly_brackets function to each row
        df_statements <- do.call(rbind, lapply(seq_len(nrow(df_with_round)), function(i) {
          row_data <- df_with_round[i, ]
          result <- find_curly_brackets(row_data, paired_idxs)
          result[["id"]] <- i # Preserve the id
          result
        }))

        # Remove row names that might have been created by rbind
        rownames(df_statements) <- NULL

        # Add lead_start column (equivalent to dplyr::lead with default = 0)
        lead_start <- c(df_statements[["start"]][-1], 0) - 1
        df_statements[["lead_start"]] <- lead_start

        # Add next_statement column (equivalent to dplyr::if_else with dplyr::lead)
        lead_statement <- c(df_statements[["statement"]][-1], NA)
        df_statements[["next_statement"]] <- ifelse(
          df_statements[["end_curly"]] == df_statements[["lead_start"]],
          lead_statement,
          NA
        )

        if (nrow(df_statements) == 0) {
          done <- TRUE
        } else {
          # # At first iteration, replace all with uppercase versions, as the statement names are the same in R and Julia. This is necessart because someone may have enclosed their if statement etc. in extra round brackets, such that it still matches
          if (i == 1) {
            # Replace all statement names with uppercase versions
            for (i in seq_len(nrow(df_statements))) {
              stringr::str_sub(eqn, df_statements[i, "start"], df_statements[i, "end"]) <- toupper(stringr::str_sub(eqn, df_statements[i, "start"], df_statements[i, "end"]))
            }
            statement_regex <- toupper(statement_regex)
            i <- i + 1
            next
          }


          # Start with first pair
          pair <- df_statements[1, ]
          pair |> as.data.frame()

          eqn <- process_julia_statement(eqn, pair, var_names)
        }
      }
    }
  }


  ### Convert one liner functions
  # Get start of new sentences
  idxs_newline <- rbind(
    data.frame(start = 1, end = 1),
    stringr::str_locate_all(eqn, "\n")[[1]] |> as.data.frame(),
    data.frame(start = nchar(eqn) + 1, end = nchar(eqn) + 1)
  )

  # For each new line, find first two words
  x <- idxs_newline[["end"]]
  pairs <- lapply(seq(length(x) - 1), function(i) {
    # Get surrounding words
    pair <- data.frame(start = x[i], end = x[i + 1] - 1)
    pair[["match"]] <- stringr::str_sub(eqn, pair[["start"]], pair[["end"]])
    words <- get_words(pair[["match"]])
    pair[["first_word"]] <- ifelse(nrow(words) > 0, words[1, "word"], "")
    pair[["second_word"]] <- ifelse(nrow(words) > 1, words[2, "word"], "")

    # If second word is function, replace
    if (pair[["second_word"]] == "function") {
      pair[["match"]] <- process_oneliners_julia(pair, var_names)
    }
    return(pair)
  })

  eqn <- unlist(lapply(pairs, `[[`, "match")) |> paste0(collapse = "")

  return(eqn)
}


#' Create list of default arguments
#'
#' @param arg List with parsed arguments
#'
#' @returns List with named default arguments
#' @noRd
#'
create_default_arg <- function(arg) {
  # Find names and values of arguments
  contains_value <- stringr::str_detect(arg, "=")
  arg_split <- stringr::str_split_fixed(arg, "=", n = 2)
  values_arg <- ifelse(contains_value, arg_split[, 1], NA) |> trimws()
  names_arg <- ifelse(contains_value, arg_split[, 2], arg_split[, 1]) |> trimws()
  default_arg <- lapply(as.list(stats::setNames(values_arg, names_arg)), as.character)

  return(default_arg)
}


#' Get regular expressions for Julia functions
#'
#' @noRd
#' @returns data.frame
get_syntax_julia <- function() {
  # Custom function to replace each (nested) function; necessary because regex in stringr unfortunately doesn't seem to handle nested functions
  conv_df <- matrix(
    c(
      # Statistics
      "min", "min", "syntax1", "", "", FALSE,
      "max", "max", "syntax1", "", "", FALSE,
      "pmin", "min", "syntax1", "", "", FALSE,
      "pmax", "max", "syntax1", "", "", FALSE,
      "mean", "Statistics.mean", "syntax1", "", "", FALSE,
      "median", "Statistics.median", "syntax1", "", "", FALSE,
      "prod", "prod", "syntax1", "", "", FALSE,
      "sum", "sum", "syntax1", "", "", FALSE,
      "sd", "Statistics.std", "syntax1", "", "", FALSE,
      "cor", "Statistics.cor", "syntax1", "", "", FALSE,
      "cov", "Statistics.cov", "syntax1", "", "", FALSE,
      "var", "Statistics.var", "syntax1", "", "", FALSE,
      "range", "extrema", "syntax1", "", "", FALSE,
      "as.logical", "Bool", "syntax1", "", "", TRUE,
      "seq", "range", "syntax_seq", "", "", FALSE,
      "seq.int", "range", "syntax_seq", "", "", FALSE,
      "seq_along", "range", "syntax_seq", "", "", FALSE,
      "seq_len", "range", "syntax_seq", "", "", FALSE,
      "sample", "StatsBase.sample", "syntax_sample", "", "", FALSE,
      "sample.int", "StatsBase.sample", "syntax_sample", "", "", FALSE,
      "cumsum", "cumsum", "syntax1", "", "", FALSE,
      "cumprod", "cumprod", "syntax1", "", "", FALSE,
      "diff", "diff", "syntax1", "", "", FALSE,
      "abs", "abs", "syntax1", "", "", TRUE,
      "sign", "sign", "syntax1", "", "", TRUE,
      "cos", "cos", "syntax1", "", "", TRUE,
      "sin", "sin", "syntax1", "", "", TRUE,
      "tan", "tan", "syntax1", "", "", TRUE,
      "acos", "acos", "syntax1", "", "", TRUE,
      "asin", "asin", "syntax1", "", "", TRUE,
      "atan", "atan", "syntax1", "", "", TRUE,
      "cospi", "cospi", "syntax1", "", "", TRUE,
      "sinpi", "sinpi", "syntax1", "", "", TRUE,
      "tanpi", "tanpi", "syntax1", "", "", TRUE,
      "nchar", "length", "syntax1", "", "", FALSE,
      "cor", "cor", "syntax1", "", "", FALSE,
      "floor", "floor", "syntax1", "", "", TRUE,
      "ceiling", "ceil", "syntax1", "", "", TRUE,
      "round", "round_", "syntax1", "", "", TRUE,
      "trunc", "trunc", "syntax1", "", "", TRUE,

      # Find
      # "which", "findall", "syntax1", "", "",
      # findmax(arr): Returns (max_value, index).
      # findmin(arr): Returns (min_value, index).

      "which.min", "argmin", "syntax1", "", "", FALSE,
      "which.max", "argmax", "syntax1", "", "", FALSE,
      "exp", "exp", "syntax1", "", "", TRUE,
      "expm1", "expm1", "syntax1", "", "", TRUE,
      # "log", "log", "syntax1", "", "", TRUE, # **to do, put base first!
      # "logb", "logb", "syntax1", "", "", TRUE,
      "log2", "log2", "syntax1", "", "", TRUE,
      "log10", "log10", "syntax1", "", "", TRUE,
      "sqrt", "sqrt", "syntax1", "", "", TRUE,
      "dim", "size", "syntax1", "", "", FALSE,
      "nrow", "size", "syntax1", "", "1", FALSE,
      "ncol", "size", "syntax1", "", "2", FALSE,
      "cbind", "hcat", "syntax1", "", "", FALSE,
      "rbind", "vcat", "syntax1", "", "", FALSE,

      # Matrix functions
      "diag", "LinearAlgebra.diag", "syntax1", "", "", FALSE,
      "upper.tri", "LinearAlgebra.UpperTriangular", "syntax1", "", "", FALSE,
      "lower.tri", "LinearAlgebra.LowerTriangular", "syntax1", "", "", FALSE,
      "norm", "LinearAlgebra.norm", "syntax1", "", "", FALSE,
      "det", "LinearAlgebra.det", "syntax1", "", "", FALSE,
      "t", "transpose", "syntax1", "", "", FALSE,
      "rev", "reverse", "syntax1", "", "", FALSE,
      "print", "println", "syntax1", "", "", FALSE,
      "na.omit", "skipmissing", "syntax1", "", "", FALSE,
      "eigen", "eig", "syntax1", "", "", FALSE,
      "getcd", "getcwd", "syntax1", "", "", FALSE,
      "setwd", "setcwd", "syntax1", "", "", FALSE,
      "Filter", "filter", "syntax1", "", "", TRUE,
      "which", "findall", "syntax1", "", "", FALSE,
      "class", "typeof", "syntax1", "", "", FALSE,
      # String manipulation
      "grep", "match", "syntax1", "", "", FALSE,
      "strsplit", "split", "syntax1", "", "", FALSE,
      "paste0", "join", "syntax1", "", "", FALSE,
      "toupper", "uppercase", "syntax1", "", "", TRUE,
      "tolower", "lowercase", "syntax1", "", "", TRUE,
      "stringr::str_to_title", "uppercasefirst", "syntax1", "", "", TRUE,
      # Sets
      "union", "union", "syntax1", "", "", FALSE,
      "intersect", "intersect", "syntax1", "", "", FALSE,
      "setdiff", "setdiff", "syntax1", "", "", FALSE,
      "setequal", "setequal", "syntax1", "", "", FALSE,
      # is....()
      "rlang::is_empty", "isempty", "syntax1", "", "", FALSE,
      "all", "all", "syntax1", "", "", FALSE,
      "any", "any", "syntax1", "", "", FALSE,
      "is.infinite", "isinf", "syntax1", "", "", TRUE,
      "is.finite", "isfinite", "syntax1", "", "", TRUE,
      "is.nan", "ismissing", "syntax1", "", "", TRUE,
      # https://docs.julialang.org/en/v1/base/collections
      # Julia: indexin, sortperm, findfirst
      "sort", "sort", "syntax1", "", "", FALSE,
      # Complex numbers
      "Re", "real", "syntax1", "", "", TRUE,
      "Im", "imag", "syntax1", "", "", TRUE,
      "Mod", "", "syntax1", "", "", TRUE,
      "Arg", "", "syntax1", "", "", TRUE,
      "Conj", "conj", "syntax1", "", "", TRUE,
      # Custom functions
      "logistic", "logistic", "syntax1", "", "", TRUE,
      "sigmoid", "logistic", "syntax1", "", "", TRUE,
      "hill", "hill", "syntax1", "", "", TRUE,
      "logit", "logit", "syntax1", "", "", TRUE,
      "expit", "expit", "syntax1", "", "", TRUE,
      # step() is already an existing function in Julia, so we use make_step()
      # instead, as well as for the others for consistency
      "step", "make_step", "syntax1", "", "", FALSE,
      "pulse", "make_pulse", "syntax1", "", "", FALSE,
      "ramp", "make_ramp", "syntax1", "", "", FALSE,
      "seasonal", "make_seasonal", "syntax1", "", "", FALSE,
      "length_IM", "length", "syntax1", "", "", FALSE,

      # Random Number Functions (13)
      "runif", "rand", "syntaxD", "Distributions.Uniform", "", FALSE,
      "rnorm", "rand", "syntaxD", "Distributions.Normal", "", FALSE,
      "rlnorm", "rand", "syntaxD", "Distributions.LogNormal", "", FALSE,
      "rbool", "rbool", "syntax1", "", "", FALSE,
      "rbinom", "rand", "syntaxD", "Distributions.Binomial", "", FALSE,
      "rnbinom", "rand", "syntaxD", "Distributions.NegativeBinomial", "", FALSE,
      "rpois", "rand", "syntaxD", "Distributions.Poisson", "", FALSE,
      # "EnvStats::rtri", "", "syntaxD", "", "", FALSE,
      "rexp", "rand", "syntaxD", "Distributions.Exponential", "", FALSE,
      "rgamma", "rand", "syntaxD", "Distributions.Gamma", "", FALSE,
      "rbeta", "rand", "syntaxD", "Distributions.Beta", "", FALSE,
      "rcauchy", "rand", "syntaxD", "Distributions.Cauchy", "", FALSE,
      "rchisq", "rand", "syntaxD", "Distributions.Chisq", "", FALSE,
      "rgeom", "rand", "syntaxD", "Distributions.Geometric", "", FALSE,
      "rf", "rand", "syntaxD", "Distributions.FDist", "", FALSE,
      # "rhyper", "rand", "syntaxD", "Distributions.", "", FALSE,
      # "rlogis", "rand", "syntaxD", "Distributions.", "", FALSE,
      "rmultinom", "rand", "syntaxD", "Distributions.Multinomial", "", FALSE,
      # "rsignrank", "rand", "syntaxD", "Distributions.", "", FALSE,
      "rt", "rand", "syntaxD", "Distributions.TDist", "", FALSE,
      "rweibull", "rand", "syntaxD", "Distributions.Weibull", "", FALSE,
      # "rwilcox", "rand", "syntaxD", "Distributions.", "", FALSE,
      # "rbirthday", "rand", "syntaxD", "Distributions.", "", FALSE,
      # "rtukey", "rand", "syntaxD", "Distributions.", "", FALSE,
      "rdist", "rdist", "syntax1", "", "", FALSE,
      "set.seed", "Random.seed!", "syntax1", "", "", FALSE,
      # Statistical Distributions (20)
      "punif", "Distributions.cdf.", "syntaxD", "Distributions.Uniform", "", FALSE,
      "dunif", "Distributions.pdf.", "syntaxD", "Distributions.Uniform", "", FALSE,
      "qunif", "Distributions.quantile.", "syntaxD", "Distributions.Uniform", "", FALSE,
      "pnorm", "Distributions.cdf.", "syntaxD", "Distributions.Normal", "", FALSE,
      "dnorm", "Distributions.pdf.", "syntaxD", "Distributions.Normal", "", FALSE,
      "qnorm", "Distributions.quantile.", "syntaxD", "Distributions.Normal", "", FALSE,
      "plnorm", "Distributions.cdf.", "syntaxD", "Distributions.LogNormal", "", FALSE,
      "dlnorm", "Distributions.pdf.", "syntaxD", "Distributions.LogNormal", "", FALSE,
      "qlnorm", "Distributions.quantile.", "syntaxD", "Distributions.LogNormal", "", FALSE,
      "pbinom", "Distributions.cdf.", "syntaxD", "Distributions.Binomial", "", FALSE,
      "dbinom", "Distributions.pdf.", "syntaxD", "Distributions.Binomial", "", FALSE,
      "qbinom", "Distributions.quantile.", "syntaxD", "Distributions.Binomial", "", FALSE,
      "pnbinom", "Distributions.cdf.", "syntaxD", "Distributions.NegativeBinomial", "", FALSE,
      "dnbinom", "Distributions.pdf.", "syntaxD", "Distributions.NegativeBinomial", "", FALSE,
      "qnbinom", "Distributions.quantile.", "syntaxD", "Distributions.NegativeBinomial", "", FALSE,
      "pgamma", "Distributions.cdf.", "syntaxD", "Distributions.Gamma", "", FALSE,
      "dgamma", "Distributions.pdf.", "syntaxD", "Distributions.Gamma", "", FALSE,
      "qgamma", "Distributions.quantile.", "syntaxD", "Distributions.Gamma", "", FALSE,
      "pbeta", "Distributions.cdf.", "syntaxD", "Distributions.Beta", "", FALSE,
      "dbeta", "Distributions.pdf.", "syntaxD", "Distributions.Beta", "", FALSE,
      "qbeta", "Distributions.quantile.", "syntaxD", "Distributions.Beta", "", FALSE,
      "pcauchy", "Distributions.cdf.", "syntaxD", "Distributions.Cauchy", "", FALSE,
      "dcauchy", "Distributions.pdf.", "syntaxD", "Distributions.Cauchy", "", FALSE,
      "qcauchy", "Distributions.quantile.", "syntaxD", "Distributions.Cauchy", "", FALSE,
      "pgeom", "Distributions.cdf.", "syntaxD", "Distributions.Geometric", "", FALSE,
      "dgeom", "Distributions.pdf.", "syntaxD", "Distributions.Geometric", "", FALSE,
      "qgeom", "Distributions.quantile.", "syntaxD", "Distributions.Geometric", "", FALSE,
      "dmultinom", "Distributions.pdf.", "syntaxD", "Distributions.Multinomial", "", FALSE,
      "pweibull", "Distributions.cdf.", "syntaxD", "Distributions.Weibull", "", FALSE,
      "dweibull", "Distributions.pdf.", "syntaxD", "Distributions.Weibull", "", FALSE,
      "qweibull", "Distributions.quantile.", "syntaxD", "Distributions.Weibull", "", FALSE,
      "pt", "Distributions.cdf.", "syntaxD", "Distributions.TDist", "", FALSE,
      "dt", "Distributions.pdf.", "syntaxD", "Distributions.TDist", "", FALSE,
      "qt", "Distributions.quantile.", "syntaxD", "Distributions.TDist", "", FALSE,
      "pf", "Distributions.cdf.", "syntaxD", "Distributions.FDist", "", FALSE,
      "df", "Distributions.pdf.", "syntaxD", "Distributions.FDist", "", FALSE,
      "qf", "Distributions.quantile.", "syntaxD", "Distributions.FDist", "", FALSE,
      "pchisq", "Distributions.cdf.", "syntaxD", "Distributions.Chisq", "", FALSE,
      "dchisq", "Distributions.pdf.", "syntaxD", "Distributions.Chisq", "", FALSE,
      "qchisq", "Distributions.quantile.", "syntaxD", "Distributions.Chisq", "", FALSE,
      "pexp", "Distributions.cdf.", "syntaxD", "Distributions.Exponential", "", FALSE,
      "dexp", "Distributions.pdf.", "syntaxD", "Distributions.Exponential", "", FALSE,
      "qexp", "Distributions.quantile.", "syntaxD", "Distributions.Exponential", "", FALSE,
      "ppois", "Distributions.cdf.", "syntaxD", "Distributions.Poisson", "", FALSE,
      "dpois", "Distributions.pdf.", "syntaxD", "Distributions.Poisson", "", FALSE,
      "qpois", "Distributions.quantile.", "syntaxD", "Distributions.Poisson", "", FALSE,
      # Complete replacements (syntax0)
      "next", "continue", "syntax0", "", "", FALSE,
      "stop", "error", "syntax0", "", "", FALSE
    ),
    ncol = 6, byrow = TRUE,
    dimnames = list(NULL, c("R", "julia", "syntax", "add_first_arg", "add_second_arg", "add_broadcast"))
  )

  # Convert to data.frame
  conv_df <- as.data.frame(conv_df, stringsAsFactors = FALSE)

  # Create syntax_df by copying conv_df
  syntax_df <- conv_df

  # Add and modify columns
  syntax_df[["R_first_iter"]] <- syntax_df[["R"]]
  syntax_df[["R_regex_first_iter"]] <- ifelse(
    syntax_df[["syntax"]] == "syntax0",
    paste0("(?<!\\.)\\b", syntax_df[["R"]], "(?=(?:\\W|$))"),
    paste0("(?<!\\.)\\b", syntax_df[["R"]], "\\(")
  )
  syntax_df[["R"]] <- paste0(syntax_df[["R"]], "_replace")
  syntax_df[["R_regex"]] <- ifelse(
    syntax_df[["syntax"]] == "syntax0",
    paste0("(?<!\\.)\\b", syntax_df[["R"]], "(?=(?:\\W|$))"),
    paste0("(?<!\\.)\\b", syntax_df[["R"]], "\\(")
  )

  return(list(syntax_df = syntax_df, conv_df = conv_df))
}


#' Convert R built-in functions to Julia
#'
#' @returns List with transformed eqn and list with additional R code needed to make the eqn function
#' @inheritParams convert_equations_IM
#' @noRd
#' @importFrom rlang .data
#'
convert_builtin_functions_julia <- function(type, name, eqn, var_names) {
  # add_code <- list(func = list())

  # Check if equation contains letters and opening and closing brackets
  # (all translated R functions have brackets)
  contains_letters <- grepl("[[:alpha:]]", eqn) && grepl("\\(", eqn) && grepl("\\)", eqn)
  if (contains_letters) {
    # data.frame with regular expressions for each built-in R function
    syntax_df <- syntax_julia[["syntax_df"]]
    # conv_df <- syntax_julia[["conv_df"]]

    # Preparation for first iteration
    done <- FALSE
    i <- 1
    R_regex <- syntax_df[["R_regex_first_iter"]]

    while (!done) {
      # Remove those matches that are in quotation marks or names
      idxs_exclude <- get_seq_exclude(eqn, var_names)

      # Update location indices of functions in eqn
      idx_df <- lapply(seq_along(R_regex), function(i) {
        matches <- gregexpr(R_regex[i], eqn, perl = TRUE, ignore.case = FALSE)[[1]]

        if (matches[1] == -1) {
          return(NULL) # Return NULL instead of empty data.frame
        } else {
          # Use cbind instead of dplyr::bind_cols for speed
          cbind(
            syntax_df[rep(i, length(matches)), , drop = FALSE],
            data.frame(
              start = as.integer(matches),
              end = as.integer(matches + attr(matches, "match.length") - 1)
            )
          )
        }
      })

      # Remove NULL entries
      idx_keep <- !vapply(idx_df, is.null, logical(1))
      idx_df <- idx_df[idx_keep]

      if (length(idx_df) == 0) {
        done <- TRUE
        next
      }

      idx_df <- do.call(rbind, idx_df)

      if (nrow(idx_df) > 0) {
        idx_df <- idx_df[!(idx_df[["start"]] %in% idxs_exclude |
          idx_df[["end"]] %in% idxs_exclude), ]
      }

      if (nrow(idx_df) == 0) {
        done <- TRUE
        next
      }

      # For the first iteration, add _replace to all detected functions, so we don't end in an infinite loop (some Julia and R functions have the same name)
      if (i == 1 && nrow(idx_df) > 0) {
        idx_df <- idx_df[order(idx_df[["start"]]), ]
        idx_df[["R_regex"]] <- stringr::str_replace_all(
          idx_df[["R_regex"]],
          stringr::fixed(c("(?<!\\.)\\b" = "", "\\(" = "(", "\\)" = ")"))
        )

        for (j in rev(seq_len(nrow(idx_df)))) {
          stringr::str_sub(eqn, idx_df[j, "start"], idx_df[j, "end"]) <- idx_df[j, ][["R_regex"]]
        }
      }

      if (i == 1) {
        # Switch from R_regex_first_iter to R_regex
        # Also only keep those functions that were detected on the first iteration.
        # No new functions to be translated will be added.
        syntax_df <- syntax_df[idx_keep, , drop = FALSE]
        R_regex <- syntax_df[["R_regex"]]
        i <- i + 1
        # Stop first iteration
        next
      }

      if (nrow(idx_df) == 0) {
        done <- TRUE
      } else {
        # To find the arguments within round brackets, find all indices of matching '', (), [], c()
        paired_idxs <- get_range_all_pairs(eqn, var_names, add_custom = "paste0()")
        paired_idxs

        # If there are brackets in the eqn:
        if (nrow(paired_idxs) > 0) {
          # Match the opening bracket of each function to round brackets in paired_idxs
          idx_funcs <- merge(
            paired_idxs[paired_idxs[["type"]] == "round", ],
            idx_df,
            by.x = "start",
            by.y = "end"
          )
          idx_funcs[["start_bracket"]] <- idx_funcs[["start"]]
          idx_funcs[["start"]] <- idx_funcs[["start.y"]]


          df2 <- idx_df[idx_df[["syntax"]] == "syntax1b", ]
          # Add start_bracket column to prevent errors
          df2[["start_bracket"]] <- df2[["start"]]
          # Add back syntax1b which does not need brackets
          # idx_funcs <- dplyr::bind_rows(idx_funcs, df2)
          idx_funcs <- bind_rows_(idx_funcs, df2)
          idx_funcs <- idx_funcs[order(idx_funcs[["end"]]), ]
          idx_funcs
        } else {
          # If there are no brackets in the eqn:
          idx_funcs <- idx_df
          # Add start_bracket column to prevent errors
          idx_funcs[["start_bracket"]] <- idx_funcs[["start"]]
        }

        # Start with most nested function
        idx_funcs_ordered <- idx_funcs
        idx_funcs_ordered[["is_nested_around"]] <- any(idx_funcs_ordered[["start"]] < idx_funcs[["start"]] &
          idx_funcs_ordered[["end"]] > idx_funcs[["end"]])
        idx_funcs_ordered <- idx_funcs_ordered[order(idx_funcs_ordered[["is_nested_around"]]), ]
        idx_func <- idx_funcs_ordered[1, ] # Select first match

        if (P[["debug"]]) {
          cli::cli_inform(c("i" = "idx_func:"))
          cli::cli_inform(c("i" = toString(idx_func)))
        }

        # Extract argument between brackets (excluding brackets)
        bracket_arg <- stringr::str_sub(eqn, idx_func[["start_bracket"]] + 1, idx_func[["end"]] - 1)

        arg <- parse_args(bracket_arg)
        named_arg <- sort_args(arg, idx_func[["R_first_iter"]], var_names = var_names)
        arg <- unname(unlist(named_arg))

        # Indices of replacement in eqn
        start_idx <- idx_func[["start"]]
        end_idx <- idx_func[["end"]]

        if (idx_func[["syntax"]] == "syntax0") {
          replacement <- idx_func[["julia"]]
        } else if (idx_func[["syntax"]] == "syntax1") {
          arg <- paste0(arg, collapse = ", ")

          replacement <- sprintf(
            "%s%s(%s%s%s%s%s)",
            idx_func[["julia"]],
            ifelse(idx_func[["add_broadcast"]], ".", ""),
            idx_func[["add_first_arg"]],
            ifelse(nzchar(idx_func[["add_first_arg"]]) & nzchar(arg), ", ", ""),
            arg,
            idx_func[["add_second_arg"]],
            ifelse(nzchar(idx_func[["add_second_arg"]]) & nzchar(arg), ", ", "")
          )
        } else if (idx_func[["syntax"]] == "syntaxD") {
          # Convert random number generation
          replacement <- conv_distribution(
            arg,
            idx_func[["R_first_iter"]],
            idx_func[["julia"]],
            idx_func[["add_first_arg"]]
          )
        } else if (idx_func[["syntax"]] == "syntax_seq") {
          # Convert sequence
          replacement <- conv_seq(
            named_arg,
            idx_func[["R_first_iter"]],
            idx_func[["julia"]]
          )
        } else if (idx_func[["syntax"]] == "syntax_sample") {
          # Convert sequence
          replacement <- conv_sample(
            named_arg,
            idx_func[["R_first_iter"]],
            idx_func[["julia"]]
          )
        }

        if (P[["debug"]]) {
          cli::cli_inform(c("i" = stringr::str_sub(eqn, start_idx, end_idx)))
          cli::cli_inform(c("i" = replacement))
          cli::cli_inform(c(" " = ""))
        }

        # Replace eqn
        stringr::str_sub(eqn, start_idx, end_idx) <- replacement
      }
    }
  }

  # Flatten the add_code structure - extract all functions from add_code[["func"]]
  add_vars <- data.frame()
  # if (length(add_code[["func"]]) > 0) {
  #   # Flatten all functions from different syntax types
  #   for (syntax_type in names(add_code[["func"]])) {
  #     add_vars_aux <- append(add_vars_aux, add_code[["func"]][[syntax_type]])
  #   }
  # }

  return(list(eqn = eqn, add_vars = add_vars, doc = ""))
}


#' Convert random number generation in R to Julia
#'
#' @inheritParams sort_args
#' @param julia_func String with Julia function
#' @param R_func String with R function, e.g., "rnorm()".
#' @param distribution String with Julia distribution call
#'
#' @returns String with Julia code
#' @noRd
#' @keywords internal
#'
conv_distribution <- function(arg, R_func, julia_func, distribution) {
  conv_distribution_julia(arg, R_func, julia_func, distribution)
}


#' Convert sequence in R to Julia
#'
#' @inheritParams sort_args
#' @param R_func String with R function, e.g., "seq", "seq_along"
#' @param julia_func String with Julia function
#'
#' @returns String with Julia code
#' @noRd
#'
conv_seq <- function(arg, R_func, julia_func) {
  if (R_func == "seq_along") {
    julia_str <- paste0(julia_func, "(1.0, length(", arg[["along.with"]], "))")
  } else if (R_func == "seq_len") {
    julia_str <- paste0(julia_func, "(1.0, ", arg[["length.out"]], ")")
  } else {
    # If nothing is specified, specify by
    if (!is_defined(arg[["by"]]) && !is_defined(arg[["length.out"]]) &&
      !is_defined(arg[["along.with"]])) {
      arg[["by"]] <- "1.0" # Default value for by
    }

    if (is_defined(arg[["by"]])) {
      julia_str <- sprintf(
        "%s(%s, %s, step=%s)",
        julia_func, arg[["from"]], arg[["to"]], arg[["by"]]
      )
    } else if (is_defined(arg[["length.out"]])) {
      # Julia throws an error in this case
      if (as.numeric(arg[["length.out"]]) == 1 &&
        as.numeric(arg[["from"]]) != as.numeric(arg[["to"]])) {
        julia_str <- arg[["from"]]
      } else {
        # length.out should be an integer
        julia_str <- sprintf(
          "%s(%s, %s, round_(%s))",
          julia_func, arg[["from"]], arg[["to"]], arg[["length.out"]]
        )
      }
    } else if (is_defined(arg[["along.with"]])) {
      julia_str <- sprintf(
        "%s(%s, %s, length(%s))",
        julia_func, arg[["from"]], arg[["to"]], arg[["along.with"]]
      )
    }
  }

  return(julia_str)
}


#' Convert R sample() to Julia StatsBase.sample()
#'
#' @inheritParams conv_seq
#'
#' @returns String with Julia code
#' @noRd
conv_sample <- function(arg, R_func, julia_func) {
  # Order in StatsBase.sample() is different
  if (R_func == "sample.int") {
    arg[["x"]] <- paste0("seq(1.0, ", arg[["n"]], ")")
  }

  arg[["replace"]] <- ifelse(tolower(arg[["replace"]]) == "true", "true", "false")

  if (is_defined(arg[["prob"]])) {
    julia_str <- sprintf(
      "%s(%s, StatsBase.pweights(%s), round_(%s), replace=%s)",
      julia_func, arg[["x"]], arg[["prob"]], arg[["size"]], arg[["replace"]]
    )
  } else {
    julia_str <- sprintf(
      "%s(%s, round_(%s), replace=%s)",
      julia_func, arg[["x"]], arg[["size"]], arg[["replace"]]
    )
  }

  return(julia_str)
}


#' Translate vector bracket syntax from R to square brackets in Julia
#'
#' @inheritParams convert_equations_IM
#' @returns Updated eqn
#' @noRd
#'
vector_to_square_brackets <- function(eqn, var_names) {
  # Get indices of all enclosures
  paired_idxs <- get_range_all_pairs(eqn, var_names,
    type = "vector",
    names_with_brackets = FALSE
  )

  # Remove those that are preceded by a letter
  if (nrow(paired_idxs) > 0) paired_idxs <- paired_idxs[!stringr::str_detect(stringr::str_sub(eqn, paired_idxs[["start"]] - 1, paired_idxs[["start"]] - 1), "[[:alpha:]]"), ]

  if (nrow(paired_idxs) > 0) {
    # First replace all closing brackets with ]
    chars <- strsplit(eqn, "", fixed = TRUE)[[1]]
    chars[paired_idxs[["end"]]] <- "]"
    eqn <- paste0(chars, collapse = "")

    # Order paired_idxs by start position
    paired_idxs <- paired_idxs[order(paired_idxs[["start"]]), ]

    # Replace opening brackets c( with [
    for (j in rev(seq_len(nrow(paired_idxs)))) {
      # Replace c( with [
      stringr::str_sub(eqn, paired_idxs[j, "start"], paired_idxs[j, "start"] + 1) <- "["
    }
  }

  return(eqn)
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
