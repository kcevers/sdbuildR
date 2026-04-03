# # S3 accessor methods for sdbuildR objects
# # Provides $, [[, and names() methods for convenient access to model components


# # Internal fields that should always fall through to standard list access
# .sdbuildR_internal_fields <- c(
#   "meta", "sim_specs", "variables", "custom_unit", "assemble", "import_metadata"
# )

# # User-friendly type names for display / names()
# .sdbuildR_type_names <- c(
#   "stocks", "flows", "constants", "auxiliaries", "lookups", "custom_functions",
#   "custom_units"
# )

# # Valid variable types in the variables data.frame (after clean_type)
# .sdbuildR_var_types <- c("stock", "flow", "constant", "aux", "lookup", "func")


# #' Resolve an accessor name to a model component
# #'
# #' Internal helper implementing the resolution logic for `$` and `[[`.
# #' Resolution order:
# #' 1. Internal fields (meta, sim_specs, variables, custom_unit, assemble, import_metadata)
# #' 2. Variable name match
# #' 3. Type match via clean_type()
# #' 4. Error
# #'
# #' @param x An sdbuildR object
# #' @param name Character string to resolve
# #' @returns The resolved component, or a signal to use NextMethod
# #' @noRd
# .resolve_accessor <- function(x, name) {
#   # Step 1: Internal fields → signal to use NextMethod

#   if (name %in% .sdbuildR_internal_fields) {
#     return(list(type = "internal"))
#   }

#   vars <- .subset2(x, "variables")

#   # Step 2: Variable name match (before clean_type to avoid name mangling)
#   var_match <- vars[["name"]] == name
#   if (any(var_match)) {
#     result <- vars[var_match, , drop = FALSE]
#     rownames(result) <- NULL
#     return(list(type = "result", value = result))
#   }

#   # Step 3: Type match via clean_type()
#   cleaned <- tryCatch(clean_type(name), error = function(e) NULL)

#   if (!is.null(cleaned) && length(cleaned) == 1) {
#     if (cleaned == "custom_unit") {
#       return(list(type = "result", value = .subset2(x, "custom_unit")))
#     }
#     if (cleaned %in% .sdbuildR_var_types) {
#       result <- vars[vars[["type"]] == cleaned, , drop = FALSE]
#       rownames(result) <- NULL
#       return(list(type = "result", value = result))
#     }
#   }

#   # Step 4: Error
#   var_names <- vars[["name"]]
#   valid <- c(.sdbuildR_type_names, if (length(var_names)) var_names)
#   cli::cli_abort(c(
#     "Cannot access {.val {name}} from a {.cls sdbuildR} object.",
#     "i" = "Valid accessors: {.val {valid}}."
#   ))
# }


# #' Access components of a stock-and-flow model
# #'
# #' Use `$` or `[[` to access model components by type or variable name.
# #'
# #' @details
# #' The resolution order for a name is:
# #' \enumerate{
# #'   \item **Internal fields**: `meta`, `sim_specs`, `variables`, `custom_unit`,
# #'     `assemble`, `import_metadata` — returns the raw list element.
# #'   \item **Variable name**: if the name matches a variable in the model,
# #'     returns a 1-row (or multi-row if duplicates) data.frame from `variables`.
# #'   \item **Type**: the name is cleaned and used to filter
# #'     `variables` by type. Accepted type names include `"stocks"`, `"flows"`,
# #'     `"constants"`, `"auxiliaries"`, `"lookups"`, `"custom_functions"`,
# #'     and their singular forms or aliases. `"custom_units"` returns the
# #'     `custom_unit` data.frame.
# #'   \item **Error**: if none of the above match, an informative error is raised.
# #' }
# #'
# #' @param x A [`sdbuildR`][sdbuildR] object.
# #' @param name Name of the component to access (unquoted for `$`, character for `[[`).
# #'
# #' @returns A data.frame (for variable names or types) or a list element (for internal fields).
# #'
# #' @export
# #' @concept build
# #'
# #' @examples
# #' sfm <- sdbuildR("SIR")
# #'
# #' # Access by type
# #' sfm$stocks
# #' sfm$flows
# #'
# #' # Access by variable name
# #' sfm$Susceptible
# #'
# #' # Access via [[
# #' sfm[["auxiliaries"]]
# #'
# #' # Internal fields still work
# #' sfm$meta
# `$.sdbuildR` <- function(x, name) {
#   resolved <- .resolve_accessor(x, name)
#   if (resolved$type == "internal") {
#     .subset2(x, name)
#   } else {
#     resolved$value
#   }
# }


# #' @param i Index: a character string (name) or numeric index.
# #'
# #' @export
# `[[.sdbuildR` <- function(x, i) {
#   if (is.numeric(i)) {
#     return(.subset2(x, i))
#   }
#   resolved <- .resolve_accessor(x, i)
#   if (resolved$type == "internal") {
#     .subset2(x, i)
#   } else {
#     resolved$value
#   }
# }


# #' @returns For `names()`: a character vector of valid accessor names, including
# #'   internal field names, type names, and variable names.
# #'
# #' @export
# names.sdbuildR <- function(x, ...) {
#   var_names <- .subset2(x, "variables")[["name"]]
#   c(.sdbuildR_internal_fields, .sdbuildR_type_names, var_names)
# }
