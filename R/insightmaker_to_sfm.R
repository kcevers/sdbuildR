#' Import Insight Maker model
#'
#' Import a stock-and-flow model from [Insight Maker](https://insightmaker.com/). Models may be your own or another user's. Importing causal loop diagrams or agent-based models is not supported.
#'
#' Insight Maker models can be imported using a URL, Insight Maker file, or ModelJSON file. Ensure the URL refers to a public (not private) model. To download a model file from Insight Maker, first clone the model if it is not your own. Then, go to "Share" (top right), "Export", and "Download Insight Maker file" or "ModelJSON File".
#'
#' @param URL URL to Insight Maker model. Character.
#' @param file File path to Insight Maker model. Only used if URL is not specified. Needs to be a character with suffix .InsightMaker or .json.
#' @param keep_nonnegative_flow If TRUE, keeps original non-negativity setting of flows. Defaults to TRUE.
#' @param keep_nonnegative_stock If TRUE, keeps original non-negativity setting of stocks Defaults to FALSE.
#' @param keep_solver If TRUE, keep the ODE solver as it is. If FALSE, switch to Euler integration in case of non-negative stocks to reproduce the Insight Maker data exactly. Defaults to FALSE.
#'
#' @returns A stock-and-flow model object of class [`sdbuildR_xmile`][xmile]
#' @export
#' @concept insightmaker
#' @seealso [build()], [xmile()]
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
                                keep_nonnegative_stock = FALSE,
                                keep_solver = FALSE) {
  if (P[["debug"]]) {
    cli::cli_inform("URL: {URL}")
    cli::cli_inform("file: {file}")
  }

  # Get Insight Maker model
  out <- get_IM_model(URL, file)
  read_file <- out[["read_file"]]
  ext <- out[["ext"]]

  # Create model structure
  sfm <- tryCatch(
    {
      # IM_to_xmile(xml_file)
      file_to_xmile(read_file, ext)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to convert Insight Maker model structure to XMILE format.",
          "x" = "Check for unsupported Insight Maker syntax or model structure.",
          "i" = "Original error: {conditionMessage(e)}"),
        call = NULL
      )
    }
  )

  # Add URL to header
  if (!missing(URL)) {
    sfm[["header"]][["URL"]] <- URL
  }

  # Clean up units
  if (P[["debug"]]) {
    cli::cli_inform("Cleaning units")
  }

  regex_units <- get_regex_units()

  sfm <- tryCatch(
    {
      clean_units_IM(sfm, regex_units)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to clean units in the model.",
          "x" = "Check for invalid unit syntax or unsupported unit types.",
          "i" = "Original error: {conditionMessage(e)}"),
        call = NULL
      )
    }
  )

  # Check non-negativity for flows and stocks
  sfm <- tryCatch(
    {
      check_nonnegativity(sfm, keep_nonnegative_flow, keep_nonnegative_stock, keep_solver)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to check non-negativity constraints.",
          "x" = "Review your keep_nonnegative_flow and keep_nonnegative_stock settings.",
          "i" = "Original error: {conditionMessage(e)}"),
        call = NULL
      )
    }
  )

  # Convert macros
  if (P[["debug"]]) {
    cli::cli_inform("Converting macros")
  }

  sfm <- tryCatch(
    {
      convert_macros_IM_wrapper(sfm, regex_units = regex_units)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to convert macros from Insight Maker format.",
          "x" = "Check for unsupported macro syntax or functions.",
          "i" = "Original error: {conditionMessage(e)}"),
        call = NULL
      )
    }
  )

  # Convert equations in model variables
  if (P[["debug"]]) {
    cli::cli_inform("Converting equations")
  }

  sfm <- tryCatch(
    {
      convert_equations_IM_wrapper(sfm, regex_units = regex_units)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to convert equations from Insight Maker format.",
          "x" = "Check for unsupported functions or syntax in your model equations.",
          "i" = "Original error: {conditionMessage(e)}"),
        call = NULL
      )
    }
  )

  # Finalize equations by removing brackets from names
  sfm <- tryCatch(
    {
      remove_brackets_from_names(sfm)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to clean variable names.",
          "i" = "Original error: {conditionMessage(e)}"),
        call = NULL
      )
    }
  )

  # Convert equations and macros to Julia
  sfm <- tryCatch(
    {
      convert_equations_julia_wrapper(sfm, regex_units = regex_units)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to convert equations to Julia format.",
          "i" = "Original error: {conditionMessage(e)}"),
        call = NULL
      )
    }
  )

  # Split auxiliaries into constants and auxiliaries
  sfm <- tryCatch(
    {
      split_aux_wrapper(sfm)
    },
    error = function(e) {
      cli::cli_abort(
        c("Failed to split auxiliary variables into constants and auxiliaries.",
          "i" = "Original error: {conditionMessage(e)}"),
        call = NULL
      )
    }
  )

  # Determine simulation language: if using units, set to Julia
  unit_strings <- find_unit_strings(sfm)
  df <- as.data.frame(sfm, type = c("stock", "aux", "constant", "gf"), properties = "units")

  if (length(unit_strings) > 0 || nrow(sfm[["model_units"]]) > 0 ||
    any(df[["units"]] != "1")) {
    cli::cli_inform("Units detected. Setting language to {.code Julia}")
    sfm <- sim_specs(sfm, language = "Julia", keep_nonnegative_flow = keep_nonnegative_flow, keep_nonnegative_stock = keep_nonnegative_stock)
  } else {
    sfm <- sim_specs(sfm, keep_nonnegative_flow = keep_nonnegative_flow, keep_nonnegative_stock = keep_nonnegative_stock)
  }

  sfm <- validate_xmile(sfm)
  return(sfm)
}
