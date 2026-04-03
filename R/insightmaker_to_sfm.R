#' Import Insight Maker model
#'
#' Import a stock-and-flow model from [Insight Maker](https://insightmaker.com/). Models may be your own or another user's. Importing causal loop diagrams or agent-based models is not supported.
#'
#' Insight Maker models can be imported using a URL, Insight Maker file, or ModelJSON file. Ensure the URL refers to a public (not private) model. To download a model file from Insight Maker, first clone the model if it is not your own. Then, go to "Share" (top right), "Export", and "Download Insight Maker file" or "ModelJSON File".
#'
#' @param URL URL to Insight Maker model. Character.
#' @param file File path to Insight Maker model. Only used if URL is not specified. Needs to be a character with suffix .InsightMaker or .json.
#' @param keep_nonnegative_flow If TRUE, keeps original non-negativity setting of flows. Defaults to TRUE.
#' @param keep_nonnegative_stock If TRUE, keeps original non-negativity setting of stocks. Defaults to FALSE.
#'
#' @returns A stock-and-flow model object of class [`sdbuildR`][sdbuildR].
#' @export
#' @concept insightmaker
#' @seealso [update()], [sdbuildR()]
#'
#' @examplesIf has_internet() && Sys.getenv("NOT_CRAN") == "true"
#' # Load a model from Insight Maker
#' sfm <- insightmaker_to_sfm(
#'   URL =
#'     "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet"
#' )
#' plot(sfm)
#'
#' \dontshow{
#' sfm <- sim_specs(sfm, save_at = .5)
#' }
#'
#' # Simulate the model
#' sim <- simulate(sfm)
#' plot(sim)
#'
insightmaker_to_sfm <- function(URL,
                                file,
                                keep_nonnegative_flow = TRUE,
                                keep_nonnegative_stock = FALSE) {
  if (P[["debug"]]) {
    cli::cli_inform("URL: {URL}")
    cli::cli_inform("file: {file}")
  }

  # Get Insight Maker model
  out <- get_IM_model(URL, file)
  read_file <- out[["read_file"]]
  ext <- out[["ext"]]

  # Parse model into import context
  # file_to_sdbuildR() returns a context with:
  # - ctx$object: object with sim_specs, meta, and variables added (no Julia conversion yet)
  # - ctx$variables: original variable list (for reference)
  # - ctx$original_variables: data frame for import_metadata
  # - ctx$original_macros: data frame for import_metadata
  # - ctx$vendor_meta: meta info for import_metadata
  ctx <- tryCatch(
    {
      file_to_sdbuildR(read_file, ext)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to convert Insight Maker model structure to XMILE format.",
          "x" = "Check for unsupported Insight Maker syntax or model structure.",
          "i" = "Original error: {conditionMessage(e)}"
        ),
        call = NULL
      )
    }
  )

  # Store raw model and source info in context for import_metadata
  ctx$raw_model <- read_file
  ctx$file_path <- if (!missing(file) && !is.null(file)) file else NULL
  ctx$url <- if (!missing(URL) && !is.null(URL)) URL else NULL

  # Add URL to meta
  if (!missing(URL)) {
    ctx$object[["meta"]][["URL"]] <- URL
  }

  # Extract object for convenience (conversion functions work on object)
  object <- ctx$object

  # Clean up units
  if (P[["debug"]]) {
    cli::cli_inform("Cleaning units")
  }

  regex_units <- get_regex_units()

  object <- tryCatch(
    {
      clean_units_IM(object, regex_units)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to clean units in the model.",
          "x" = "Check for invalid unit syntax or unsupported unit types.",
          "i" = "Original error: {conditionMessage(e)}"
        ),
        call = NULL
      )
    }
  )

  # Check non-negativity for flows and stocks
  object <- tryCatch(
    {
      check_nonnegativity(object, keep_nonnegative_flow, keep_nonnegative_stock)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to check non-negativity constraints.",
          "x" = "Review your keep_nonnegative_flow and keep_nonnegative_stock settings.",
          "i" = "Original error: {conditionMessage(e)}"
        ),
        call = NULL
      )
    }
  )

  # Convert macros
  if (P[["debug"]]) {
    cli::cli_inform("Converting macros")
  }

  object <- tryCatch(
    {
      convert_macros_IM_wrapper(object, regex_units = regex_units)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to convert macros from Insight Maker format.",
          "x" = "Check for unsupported macro syntax or functions.",
          "i" = "Original error: {conditionMessage(e)}"
        ),
        call = NULL
      )
    }
  )

  # Convert equations in model variables (IM format -> R format)
  if (P[["debug"]]) {
    cli::cli_inform("Converting equations")
  }

  object <- tryCatch(
    {
      convert_equations_IM_wrapper(object, regex_units = regex_units)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to convert equations from Insight Maker format.",
          "x" = "Check for unsupported functions or syntax in your model equations.",
          "i" = "Original error: {conditionMessage(e)}"
        ),
        call = NULL
      )
    }
  )

  # Finalize equations by removing brackets from names
  object <- tryCatch(
    {
      remove_brackets_from_names(object)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to clean variable names.",
          "i" = "Original error: {conditionMessage(e)}"
        ),
        call = NULL
      )
    }
  )

  # Split auxiliaries into constants and auxiliaries
  object <- tryCatch(
    {
      split_aux_wrapper(object)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to split auxiliary variables into constants and auxiliaries.",
          "i" = "Original error: {conditionMessage(e)}"
        ),
        call = NULL
      )
    }
  )

  # Prepare equation strings for the target language
  # This must happen before sim_specs() to ensure eqn_str and sum_eqn are populated
  # Determine which language will be used (check for units first)
  unit_strings <- find_unit_strings(object)
  df_units <- as.data.frame(object, type = c("stock", "aux", "constant", "lookup"), properties = "units")

  will_use_julia <- length(unit_strings) > 0 ||
    nrow(object[["custom_unit"]]) > 0 ||
    any(df_units[["units"]] != "1")

  # Prepare equations (adapter handles R vs Julia based on sim_specs)
  if (will_use_julia) {
    object[["sim_specs"]][["language"]] <- "Julia"
  }
  object <- prep_equations_variables(object)
  object <- prep_stock_change(object)

  # Determine simulation language: if using units, set to Julia
  # Reuse variables computed above for prep functions
  if (will_use_julia) {
    cli::cli_inform("Units detected. Setting language to {.code Julia}")
    object <- sim_specs(object, language = "Julia")
  }
  object <- sim_specs(object, keep_nonnegative_flow = keep_nonnegative_flow, keep_nonnegative_stock = keep_nonnegative_stock)

  # Clean up temporary columns used during conversion
  # These columns are no longer needed and should not appear in the final sdbuildR object
  # temp_cols <- c("eqn_insightmaker", "units_insightmaker",
  #                "name_insightmaker", "id_insightmaker",
  #                "conveyor", "len")
  # for (col in temp_cols) {
  #   if (col %in% colnames(object[["variables"]])) {
  #     object[["variables"]][[col]] <- NULL
  #   }
  # }
  allowed_cols <- colnames(empty_variables())
  object[["variables"]] <- object[["variables"]][, colnames(object[["variables"]]) %in% allowed_cols]

  # Update context with converted object
  ctx$object <- object

  # Build import_metadata from context and attach to object
  object[["import_metadata"]] <- ctx_build_import_metadata(ctx)

  object <- sanitize_sdbuildR(object)
  validate_sdbuildR(object)

  object
}
