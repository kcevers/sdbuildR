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
#' @concept importExport
#' @seealso [update()], [sdbuildR()]
#'
#' @examplesIf has_internet() && Sys.getenv("NOT_CRAN") == "true"
#' # Load a model from Insight Maker
#' sfm <- import_insightmaker(
#'   URL =
#'     "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet"
#' )
#' plot(sfm)
#'
#' \dontshow{
#' sfm <- sim_settings(sfm, save_at = .5)
#' }
#'
#' # Simulate the model
#' sim <- simulate(sfm)
#' plot(sim)
#'
import_insightmaker <- function(URL,
                                file,
                                keep_nonnegative_flow = TRUE,
                                keep_nonnegative_stock = FALSE) {
  if (P[["debug"]]) {
    cli::cli_inform(c("i" = "URL: {URL}"))
    cli::cli_inform(c("i" = "file: {file}"))
  }

  # Get Insight Maker model
  out <- get_IM_model(URL, file)
  read_file <- out[["read_file"]]
  ext <- out[["ext"]]

  # Parse model into import context
  # file_to_sdbuildR() returns a context with:
  # - ctx$object: object with sim_settings, meta, and variables added (no Julia conversion yet)
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
        c(
          "x" = "Failed to convert Insight Maker model structure to XMILE format.",
          "i" = "Check for unsupported Insight Maker syntax or model structure.",
          ">" = "Original error: {conditionMessage(e)}"
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

  # Check non-negativity for flows and stocks
  object <- tryCatch(
    {
      check_nonnegativity(object, keep_nonnegative_flow, keep_nonnegative_stock)
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "x" = "Failed to check non-negativity constraints.",
          "i" = "Review your keep_nonnegative_flow and keep_nonnegative_stock settings.",
          ">" = "Original error: {conditionMessage(e)}"
        ),
        call = NULL
      )
    }
  )

  # Convert macros
  if (P[["debug"]]) {
    cli::cli_inform(c("i" = "Converting macros"))
  }

  object <- tryCatch(
    {
      convert_macros_IM_wrapper(object)
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "x" = "Failed to convert macros from Insight Maker format.",
          "i" = "Check for unsupported macro syntax or functions.",
          ">" = "Original error: {conditionMessage(e)}"
        ),
        call = NULL
      )
    }
  )

  # Convert equations in model variables (IM format -> R format)
  if (P[["debug"]]) {
    cli::cli_inform(c("i" = "Converting equations"))
  }

  object <- tryCatch(
    {
      convert_equations_IM_wrapper(object)
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "x" = "Failed to convert equations from Insight Maker format.",
          "i" = "Check for unsupported functions or syntax in your model equations.",
          ">" = "Original error: {conditionMessage(e)}"
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
        c(
          "x" = "Failed to clean variable names.",
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
        c(
          "x" = "Failed to split auxiliary variables into constants and auxiliaries.",
          "i" = "Original error: {conditionMessage(e)}"
        ),
        call = NULL
      )
    }
  )

  object <- prep_equations_variables(object)
  object <- prep_stock_change(object)
  object <- sim_settings(object, keep_nonnegative_flow = keep_nonnegative_flow, keep_nonnegative_stock = keep_nonnegative_stock)

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
