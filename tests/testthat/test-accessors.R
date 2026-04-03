# # Tests for $.sdbuildR, [[.sdbuildR, and names.sdbuildR


# # Helpers ------------------------------------------------------------------

# make_accessor_sfm <- function() {
#   sdbuildR() |>
#     update("S", type = "stock", eqn = "1") |>
#     update("I", type = "stock", eqn = "0") |>
#     update("infection", type = "flow", eqn = "S * beta", to = "I", from = "S") |>
#     update("beta", type = "constant", eqn = "0.3") |>
#     update("ratio", type = "aux", eqn = "I / (S + I)") |>
#     custom_unit("person", eqn = "1", doc = "Number of people")
# }


# # $ accessor ---------------------------------------------------------------

# test_that("$ returns filtered data.frame for stock type", {
#   sfm <- make_accessor_sfm()
#   result <- sfm$stocks
#   expect_s3_class(result, "data.frame")
#   expect_true(all(result[["type"]] == "stock"))
#   expect_equal(sort(result[["name"]]), c("I", "S"))
# })

# test_that("$ returns filtered data.frame for flow type", {
#   sfm <- make_accessor_sfm()
#   result <- sfm$flows
#   expect_s3_class(result, "data.frame")
#   expect_true(all(result[["type"]] == "flow"))
#   expect_equal(result[["name"]], "infection")
# })

# test_that("$ returns filtered data.frame for constant type", {
#   sfm <- make_accessor_sfm()
#   result <- sfm$constants
#   expect_s3_class(result, "data.frame")
#   expect_true(all(result[["type"]] == "constant"))
#   expect_equal(result[["name"]], "beta")
# })

# test_that("$ returns filtered data.frame for auxiliary type", {
#   sfm <- make_accessor_sfm()
#   result <- sfm$auxiliaries
#   expect_s3_class(result, "data.frame")
#   expect_true(all(result[["type"]] == "aux"))
#   expect_equal(result[["name"]], "ratio")
# })

# test_that("$ returns empty data.frame for type with no variables", {
#   sfm <- make_accessor_sfm()
#   result <- sfm$lookups
#   expect_s3_class(result, "data.frame")
#   expect_equal(nrow(result), 0)
#   # Should still have correct columns
#   expect_true("name" %in% names(result))
#   expect_true("type" %in% names(result))
# })

# test_that("$ returns custom_unit data.frame for custom_units", {
#   sfm <- make_accessor_sfm()
#   result <- sfm$custom_units
#   expect_s3_class(result, "data.frame")
#   expect_equal(result[["name"]], "person")
# })

# test_that("$ returns custom_functions (func type)", {
#   sfm <- make_accessor_sfm() |>
#     custom_func("double", eqn = "function(x) 2 * x")
#   result <- sfm$custom_functions
#   expect_s3_class(result, "data.frame")
#   expect_true(all(result[["type"]] == "func"))
#   expect_equal(result[["name"]], "double")
# })


# # $ alias normalization ----------------------------------------------------

# test_that("$ singular and plural type aliases return identical results", {
#   sfm <- make_accessor_sfm()

#   expect_identical(sfm$stock, sfm$stocks)
#   expect_identical(sfm$flow, sfm$flows)
#   expect_identical(sfm$constant, sfm$constants)
# })

# test_that("$ auxiliary aliases all work", {
#   sfm <- make_accessor_sfm()

#   expect_identical(sfm$aux, sfm$auxiliaries)
#   expect_identical(sfm$auxiliary, sfm$auxiliaries)
# })


# # $ variable name access ---------------------------------------------------

# test_that("$ returns 1-row data.frame for variable name", {
#   sfm <- make_accessor_sfm()
#   result <- sfm$S
#   expect_s3_class(result, "data.frame")
#   expect_equal(nrow(result), 1)
#   expect_equal(result[["name"]], "S")
#   expect_equal(result[["type"]], "stock")
# })

# test_that("$ returns correct variable by name", {
#   sfm <- make_accessor_sfm()
#   result <- sfm$beta
#   expect_equal(nrow(result), 1)
#   expect_equal(result[["name"]], "beta")
#   expect_equal(result[["type"]], "constant")
# })

# test_that("$ variable name takes priority over clean_type", {
#   # Edge case: a variable literally named "constant" should return

#   # the variable row, not all constants
#   sfm <- sdbuildR() |>
#     update("constant", type = "stock", eqn = "5") |>
#     update("other", type = "constant", eqn = "1")
#   result <- sfm$constant
#   expect_equal(nrow(result), 1)
#   expect_equal(result[["name"]], "constant")
#   expect_equal(result[["type"]], "stock")
# })


# # $ internal fields --------------------------------------------------------

# test_that("$ returns meta list for internal field", {
#   sfm <- make_accessor_sfm()
#   result <- sfm$meta
#   expect_type(result, "list")
#   expect_true("name" %in% names(result))
# })

# test_that("$ returns sim_specs list for internal field", {
#   sfm <- make_accessor_sfm()
#   result <- sfm$sim_specs
#   expect_type(result, "list")
#   expect_true("start" %in% names(result))
# })

# test_that("$ returns variables data.frame for internal field", {
#   sfm <- make_accessor_sfm()
#   result <- sfm$variables
#   expect_s3_class(result, "data.frame")
#   expect_true("name" %in% names(result))
#   expect_true("type" %in% names(result))
# })

# test_that("$ returns custom_unit data.frame for internal field", {
#   sfm <- make_accessor_sfm()
#   # Direct internal field name
#   result <- sfm$custom_unit
#   expect_s3_class(result, "data.frame")
# })


# # $ error on unknown name --------------------------------------------------

# test_that("$ errors on unknown name", {
#   sfm <- make_accessor_sfm()
#   expect_error(sfm$nonexistent, "Cannot access")
#   expect_error(sfm$foo, "Cannot access")
# })


# # [[ accessor --------------------------------------------------------------

# test_that("[[ character works identically to $", {
#   sfm <- make_accessor_sfm()
#   expect_identical(sfm[["stocks"]], sfm$stocks)
#   expect_identical(sfm[["flows"]], sfm$flows)
#   expect_identical(sfm[["S"]], sfm$S)
#   expect_identical(sfm[["meta"]], sfm$meta)
#   expect_identical(sfm[["custom_units"]], sfm$custom_units)
# })

# test_that("[[ numeric index works (standard list access)", {
#   sfm <- make_accessor_sfm()
#   # Index 1 should be meta
#   expect_identical(sfm[[1]], sfm$meta)
#   expect_identical(sfm[[2]], sfm$sim_specs)
# })

# test_that("[[ errors on unknown character name", {
#   sfm <- make_accessor_sfm()
#   expect_error(sfm[["nonexistent"]], "Cannot access")
# })


# # names() ------------------------------------------------------------------

# test_that("names() returns a character vector", {
#   sfm <- make_accessor_sfm()
#   result <- names(sfm)
#   expect_type(result, "character")
# })

# test_that("names() includes internal fields", {
#   sfm <- make_accessor_sfm()
#   result <- names(sfm)
#   for (field in c("meta", "sim_specs", "variables", "custom_unit", "assemble", "import_metadata")) {
#     expect_true(field %in% result, info = paste("Missing:", field))
#   }
# })

# test_that("names() includes type names", {
#   sfm <- make_accessor_sfm()
#   result <- names(sfm)
#   for (type_name in c("stocks", "flows", "constants", "auxiliaries", "lookups",
#                        "custom_functions", "custom_units")) {
#     expect_true(type_name %in% result, info = paste("Missing:", type_name))
#   }
# })

# test_that("names() includes variable names", {
#   sfm <- make_accessor_sfm()
#   result <- names(sfm)
#   for (var_name in c("S", "I", "infection", "beta", "ratio")) {
#     expect_true(var_name %in% result, info = paste("Missing:", var_name))
#   }
# })

# test_that("names() works on empty model", {
#   sfm <- sdbuildR()
#   result <- names(sfm)
#   expect_type(result, "character")
#   expect_true("meta" %in% result)
#   expect_true("stocks" %in% result)
#   # No variable names for empty model
#   expect_equal(
#     length(result),
#     length(c("meta", "sim_specs", "variables", "custom_unit", "assemble",
#              "import_metadata", "stocks", "flows", "constants", "auxiliaries",
#              "lookups", "custom_functions", "custom_units"))
#   )
# })


# # Row names are reset ------------------------------------------------------

# test_that("$ resets row names on filtered data.frame", {
#   sfm <- make_accessor_sfm()
#   result <- sfm$stocks
#   expect_equal(rownames(result), as.character(seq_len(nrow(result))))
# })


# # Snapshot tests for error messages ----------------------------------------

# cli::test_that_cli(configs = c("plain", "ansi"), "$ error message snapshot", {
#   sfm <- sdbuildR() |>
#     update("S", type = "stock", eqn = "1")
#   expect_snapshot(sfm$foo, error = TRUE)
# })
