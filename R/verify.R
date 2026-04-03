# ==============================================================================
# VERIFY GENERIC
# ==============================================================================

#' Verify model
#'
#' @param object A model object to verify.
#' @param ... Additional arguments passed to specific methods.
#' @export
verify <- function(object, ...) {
  UseMethod("verify")
}


#' Verify unit tests against simulation results
#'
#' Run all active unit tests defined on a stock-and-flow model. Use
#' [unit_test()] to define tests; use [unit_tests()] to display them.
#'
#' Calling `verify()` on a `sdbuildR` model (not yet simulated) will first
#' simulate the model, then run all tests — including those that require
#' re-simulation under alternative [conditions][unit_test()].
#'
#' Calling `verify()` on a `simulate_sdbuildR` result runs only tests whose
#' `conditions` are empty or already match the current model parameter values.
#' Tests that require re-simulation are skipped with an informative message;
#' use `verify(object)` (the unsimulated model) to run all tests.
#'
#' @param object An [`sdbuildR`][sdbuildR] or [`simulate_sdbuildR`][simulate.sdbuildR()] object.
#' @param verbose If `TRUE` (default), print results to the console.
#' @param return_sims If `TRUE`, return the simulations run during verification.
#'   Set to `FALSE` (default) to discard them.
#' @param ... Additional arguments passed to [simulate.sdbuildR()].
#'
#' @returns An object of class `verify_sdbuildR` with fields, returned invisibly:
#'   \describe{
#'     \item{results}{List of test result entries, each with `label`, `expr_str`, `conditions`, `status`, `message`, and `outcome`.}
#'     \item{object}{The `sdbuildR` model the tests were run against.}
#'     \item{sims}{Unnamed list of `simulate_sdbuildR` objects (one per unique condition
#'       set), or `NULL` if `return_sims = FALSE`.}
#'     \item{sim_index}{Named integer vector mapping each test label to its index in
#'       `sims`, or `NULL` if `return_sims = FALSE`. Use
#'       `result$sims[[result$sim_index[["my test label"]]]]` to retrieve the simulation
#'       used for a specific test.}
#'   }
#'
#' @export
#' @concept verify
#' @method verify sdbuildR
#' @seealso [unit_test()], [unit_tests()], [simulate.sdbuildR()]
#'
#' @examples
#' sfm <- sdbuildR("SIR") |>
#'   unit_test(expr = all(Susceptible >= 0)) |>
#'   unit_test(label = "Recovered increases over time", 
#'            expr = all(diff(Recovered) >= 0))
#'
#' verify(sfm)
#'
#' # Or verify against existing results
#' sim <- simulate(sfm)
#' verify(sim)
verify.sdbuildR <- function(object, verbose = TRUE, return_sims = FALSE, ...) {
  check_sdbuildR(object)
  rlang::check_installed("testthat", reason = "to run unit tests with {.fn verify}")

  if (!is.logical(return_sims) || length(return_sims) != 1L || is.na(return_sims)) {
    cli::cli_abort(c(
      "x" = "The {.arg return_sims} argument must be {.code TRUE} or {.code FALSE}.",
      "i" = "Received: {.val {return_sims}}"
    ))
  }

  tests <- object[["unit_tests"]]
  if (length(tests) == 0) {
    cli::cli_abort(c(
      "x" = "No unit tests defined. Add tests with {.fn unit_test}."
    ))
    return(invisible(new_verify_sdbuildR(results = list(), object = object)))
  }

  # Group tests by unique conditions to minimise re-simulations.
  # Use a sentinel for the no-conditions baseline.
  .BASELINE <- ".__baseline__."

  condition_keys <- vapply(tests, function(t) {
    if (length(t[["conditions"]]) == 0) {
      return(.BASELINE)
    }
    paste(names(t[["conditions"]]), unlist(t[["conditions"]]), sep = "=", collapse = ";")
  }, character(1))

  unique_keys <- unique(condition_keys)

  # Determine per condition whether the simulation requires `only_stocks` = FALSE`, which is necessary when unit test expressions contain flows or auxiliaries (not for stocks or constants).
  stock_or_constant_names <- object[["variables"]][
    object[["variables"]][["type"]] %in% c("stock", "constant"), "name"
  ]
  td <- get_test_deps(object)
  object <- td[["object"]]
  deps <- td[["deps"]]

  needs_non_stocks <- vapply(seq_along(unique_keys), function(i) {
    key <- unique_keys[[i]]
    test_indices <- which(condition_keys == key)
    all_refs <- unique(unlist(lapply(test_indices, function(j) deps[[j]][["expr_refs"]])))
    any(!all_refs %in% stock_or_constant_names)
  }, logical(1))

  # Remove only_stocks from dots if mistakenly passed (prevent duplicate arg error)
  dots <- list(...)
  dots[["only_stocks"]] <- NULL

  # Run one simulation per unique conditions set; use integer indexing for cache
  sim_cache <- vector("list", length(unique_keys))
  for (i in seq_along(unique_keys)) {
    key <- unique_keys[[i]]
    os <- !needs_non_stocks[[i]]
    if (key == .BASELINE) {
      sim_cache[[i]] <- rlang::inject(simulate(object, only_stocks = os, !!!dots))
    } else {

      # Modify the original model with the test conditions.
      obj_modified <- object
      first_test_idx <- which(condition_keys == key)[[1]]
      conds <- tests[[first_test_idx]][["conditions"]]
      for (nm in names(conds)) {
        eqn_val <- as.character(conds[[nm]])
        obj_modified <- rlang::inject(update(obj_modified, name = !!nm, eqn = !!eqn_val))
      }
      sim_cache[[i]] <- rlang::inject(simulate(obj_modified, only_stocks = os, !!!dots))
    }
  }

  # Evaluate each test against the appropriate simulation
  results <- lapply(tests, function(test) {
    key <- if (length(test[["conditions"]]) == 0) {
      .BASELINE
    } else {
      paste(names(test[["conditions"]]), unlist(test[["conditions"]]), sep = "=", collapse = ";")
    }
    cache_idx <- match(key, unique_keys)
    sim <- sim_cache[[cache_idx]]
    .run_one_unit_test(test, sim)
  })

  # If individual simulations are returned, name the list with test labels for easy retrieval; also create an index mapping test labels to their simulation in the cache.
  if (return_sims) {
    sims <- unname(sim_cache)
    sim_index <- stats::setNames(
      match(condition_keys, unique_keys),
      vapply(tests, function(t) t[["label"]], character(1))
    )
  } else {
    sims <- NULL
    sim_index <- NULL
  }

  result_obj <- new_verify_sdbuildR(results = results, object = object,
                                     sims = sims, sim_index = sim_index)
  if (verbose) print(result_obj)
  invisible(result_obj)
}


#' Verify unit tests against existing simulation results
#' 
#' Run all active unit tests defined on a stock-and-flow model against an existing
#' simulation result. Only tests whose `conditions` are empty or already match the current model parameter values are run; tests that require re-simulation are skipped with an informative message. Use `verify(object)` (the unsimulated model) to run all tests including those that require re-simulation.
#' 
#' @param object A [`simulate_sdbuildR`][simulate.sdbuildR()] object.
#' @param verbose If `TRUE` (default), print results to the console.
#' @param ... Additional arguments (not used).
#' @export
verify.simulate_sdbuildR <- function(object, verbose = TRUE, ...) {
  check_simulate_sdbuildR(object)
  rlang::check_installed("testthat", reason = "to run unit tests with {.fn verify}")

  if (!object[["success"]]) {
    cli::cli_abort(c(
      "Cannot verify tests on a failed simulation.",
      ">" = "Inspect {.code sim$error_message}."
    ))
  }

  sfm <- object[["object"]]
  tests <- sfm[["unit_tests"]]

  if (length(tests) == 0) {
    cli::cli_inform(c(
      "i" = "No unit tests defined. Add tests with {.fn unit_test}."
    ))
    return(invisible(new_verify_sdbuildR(results = list(), object = sfm)))
  }

  results <- lapply(tests, function(test) {
    expr_str <- test[["expr_str"]]
    conditions <- if (is.null(test[["conditions"]])) list() else test[["conditions"]]
    
    if (!isTRUE(test[["active"]])) {
      return(list(
        label   = test[["label"]],
        expr_str = expr_str,
        conditions = conditions,
        status  = "skipped",
        error_type = NA,
        message = "Test is inactive.",
        outcome = NULL
      ))
    }

    if (!.conditions_match(test, sfm)) {
      cond_str <- paste(names(test[["conditions"]]), unlist(test[["conditions"]]),
        sep = " = ", collapse = ", "
      )
      return(list(
        label = test[["label"]],
        expr_str = expr_str,
        conditions = conditions,
        status = "skipped",
        error_type = NA,
        message = paste0(
          "Conditions {", cond_str,
          "} require re-simulation. Call {.fn verify}({.arg object}) to run all tests."
        ),
        outcome = NULL
      ))
    }

    .run_one_unit_test(test, object)
  })

  result_obj <- new_verify_sdbuildR(results = results, object = sfm)
  if (verbose) print(result_obj)
  invisible(result_obj)
}


# ==============================================================================
# UNIT TEST BUILDER
# ==============================================================================

#' Add or modify unit tests
#'
#' Unit tests are assertions about model behavior that can be evaluated against
#' simulation results. For example, you might assert that a stock remains
#' non-negative, or that a certain variable reaches a threshold by the end of
#' the simulation. Unit tests can be added to a model such that they can be
#' evaluated with [verify()]. All unit tests can be displayed with
#' `unit_tests()`.
#'
#' The `expr` argument accepts either a plain logical expression or a
#' `testthat` expectation:
#' - **Logical**: `all(S >= 0)`, `cor(D, C) < -.5` — wrapped in
#'   `expect_true()` internally.
#' - **testthat expectation**: `expect_lt(max(D), 100)`, `expect_gt(tail(R, 1), 0)`
#'   — evaluated as-is for richer failure messages.
#'
#' When `label` is omitted, a human-readable label is generated automatically
#' by parsing the expression (e.g., `all(S >= 0)` →
#' `"S is at least 0 (for all values)"`).
#'
#' @section Adding vs. modifying:
#' - **Add** a new test: omit `nr` (and provide a `label` that does not match
#'   any existing test, or omit `label` to auto-generate one).
#' - **Modify** an existing test by number: supply `nr` (integer).
#' - **Modify** an existing test by label: supply a `label` that matches an
#'   existing test (without specifying `nr`).
#'
#' When modifying, only the arguments you explicitly supply are changed; all
#' other fields keep their current value.
#'
#' @section Uniqueness:
#' Labels must be unique across all unit tests. An error is thrown if a new
#' or modified label would create a duplicate. Expressions must also be unique;
#' an error is thrown if an identical `expr` already exists on another test.
#'
#' @inheritParams update.sdbuildR
#' @param nr Integer number of the test to modify. Must be a positive integer
#'   (a warning is issued and the value rounded when a non-integer is
#'   supplied). When `nr` exceeds the current number of tests a warning is
#'   issued and a new test is appended instead. Can be omitted when adding a
#'   new test.
#' @param expr An expression to evaluate against simulation results. Variable
#'   names in the expression refer to model variables; each resolves to a
#'   numeric vector of time-series values. Required when adding a new test;
#'   optional when modifying (keeps the current expression if omitted).
#' @param label A descriptive label for the test. If omitted when adding,
#'   auto-generated from `expr`. If omitted when modifying, the current label
#'   is kept. Labels must be unique.
#' @param conditions A named list of constant or initial stock overrides used
#'   when evaluating this test. If non-empty, [verify.sdbuildR()] will
#'   re-simulate the model with these parameter values before evaluating `expr`.
#' @param active If `FALSE`, the test is defined but skipped during
#'   [verify()]. Defaults to `TRUE`.
#'
#' @returns The model object with the unit test added or modified, invisibly.
#'
#' @export
#' @concept verify
#' @seealso [verify()], [unit_tests()], [discard_unit_test()]
#'
#' @examples
#' sfm <- sdbuildR("SIR") |>
#'   unit_test(expr = all(S >= 0)) |>
#'   unit_test(expr = all(diff(R) >= 0), label = "R increases over time") |>
#'   unit_test(
#'     expr = all(I == I[1]),
#'     label = "When beta is zero, no one gets infected",
#'     conditions = list(beta = 0)
#'   )
#'
#' unit_tests(sfm)
#'
#' # Modify test 1 by nr: deactivate it
#' sfm <- unit_test(sfm, nr = 1, active = FALSE)
#'
#' # Modify by label: change the expression
#' sfm <- unit_test(sfm,
#'   label = "R increases over time",
#'   expr = all(diff(R) > -1)
#' )
#'
#' verify(sfm)
unit_test <- function(object, nr, expr, label, conditions = list(), active = TRUE) {
  check_sdbuildR(object)

  tests <- object[["unit_tests"]]
  n_tests <- length(tests)

  # --- Capture arguments & missingness flags ---
  expr_captured <- substitute(expr)
  expr_missing <- missing(expr)
  label_missing <- missing(label)
  cond_missing <- missing(conditions)
  active_missing <- missing(active)

  .normalize_expr <- function(expr_obj) {
    if (is.character(expr_obj)) {
      if (length(expr_obj) != 1L || is.na(expr_obj) || !nzchar(trimws(expr_obj))) {
        cli::cli_abort(c(
          "Invalid {.arg expr} argument.",
          "x" = "Character {.arg expr} must be a single non-empty string."
        ))
      }
      parsed_all <- tryCatch(
        parse(text = expr_obj, keep.source = FALSE),
        error = function(e) {
          cli::cli_abort(c(
            "Expression has invalid R syntax.",
            "x" = "Failed to parse: {.code {expr_obj}}.",
            "i" = "{conditionMessage(e)}"
          ))
        }
      )
      if (length(parsed_all) != 1L) {
        cli::cli_abort(c(
          "Invalid {.arg expr} argument.",
          "x" = "Character {.arg expr} must contain exactly one expression."
        ))
      }
      expr_obj <- parsed_all[[1L]]
    }

    expr_str_local <- paste(deparse(expr_obj, width.cutoff = 500L), collapse = " ")
    parsed_roundtrip <- tryCatch(
      parse(text = expr_str_local, keep.source = FALSE),
      error = function(e) {
        cli::cli_abort(c(
          "Expression has invalid R syntax.",
          "x" = "Failed to parse: {.code {expr_str_local}}.",
          "i" = "{conditionMessage(e)}"
        ))
      }
    )
    if (length(parsed_roundtrip) != 1L) {
      cli::cli_abort(c(
        "Invalid {.arg expr} argument.",
        "x" = "Expression must contain exactly one expression."
      ))
    }

    list(expr_obj = parsed_roundtrip[[1L]], expr_str = expr_str_local)
  }

  # --- Validate nr ---
  nr_missing <- missing(nr)
  if (!nr_missing) {
    if (!is.numeric(nr) || length(nr) != 1L || is.na(nr)) {
      cli::cli_abort(c(
        "Invalid {.arg nr} argument.",
        "x" = "{.arg nr} must be a single integer."
      ))
    }
    if (nr != round(nr)) {
      nr_old <- nr
      nr <- as.integer(round(nr))
      cli::cli_warn(c(
        "{.arg nr} must be an integer.",
        "!" = "{.val {nr_old}} will be replaced by [{nr}]."
      ))
    }
    nr <- as.integer(nr)
    if (nr < 1L) {
      cli::cli_abort(c(
        "Invalid {.arg nr} argument.",
        "x" = "{.arg nr} must be a positive integer."
      ))
    }
  }

  # --- Determine mode: add vs. modify ---
  modifying <- FALSE
  modify_pos <- NULL

  if (!nr_missing) {
    if (nr <= n_tests) {
      modifying <- TRUE
      modify_pos <- nr
    } else {
      next_nr <- n_tests + 1L
      if (nr > next_nr) {
        cli::cli_warn(c(
          "!" = "Invalid {.arg nr} ({.val {nr}}).",
          "i" = "{.arg nr} does not need to be specified when adding a new test.",
          ">" = "{.arg nr} will be set to the existing number of tests + 1 ({.arg nr} = {.val {next_nr}})."
        ))
      }
    }
  } else if (!label_missing && is.character(label) && length(label) == 1L) {
    existing_labels <- if (n_tests > 0) {
      vapply(tests, function(t) t[["label"]], character(1))
    } else {
      character(0)
    }
    pos <- match(label, existing_labels)
    if (!is.na(pos)) {
      modifying <- TRUE
      modify_pos <- pos
    }
  }

  # --- Resolve fields (partial update when modifying) ---
  if (modifying) {
    current <- tests[[modify_pos]]

    if (!expr_missing) {
      normalized <- .normalize_expr(expr_captured)
      expr_captured <- normalized[["expr_obj"]]
      expr_str <- normalized[["expr_str"]]
    } else {
      expr_str <- current[["expr_str"]]
      expr_captured <- parse(text = expr_str, keep.source = FALSE)[[1]]
    }

    if (label_missing) {
      label <- current[["label"]]
    }

    if (cond_missing) {
      conditions <- current[["conditions"]]
    }

    if (active_missing) {
      active <- current[["active"]]
    }
  } else {
    # Adding: expr is required
    if (expr_missing) {
      cli::cli_abort(c(
        "{.arg expr} is required when adding a new unit test.",
        "i" = "To modify an existing test, specify its {.arg nr} or {.arg label}."
      ))
    }
    normalized <- .normalize_expr(expr_captured)
    expr_captured <- normalized[["expr_obj"]]
    expr_str <- normalized[["expr_str"]]
  }

  # --- Auto-generate label if needed (only when adding + label omitted) ---
  if (label_missing && !modifying) {
    label <- tryCatch(
      interpret(expr_captured),
      error = function(e) paste(deparse(expr_captured, width.cutoff = 500L), collapse = " ")
    )
    if (length(conditions) > 0) {
      cond_str <- paste(
        names(conditions), "=",
        vapply(conditions, format, character(1)),
        collapse = ", "
      )
      label <- paste0(label, " (", cond_str, ")")
    }
  }

  # --- Validate label ---
  if (!is.character(label) || length(label) != 1L || is.na(label) || !nzchar(trimws(label))) {
    cli::cli_abort(c(
      "Invalid {.arg label} argument.",
      "x" = "{.arg label} must be a single non-empty character string."
    ))
  }
  label <- trimws(label)

  # --- Check label uniqueness ---
  existing_labels <- if (n_tests > 0) {
    vapply(tests, function(t) t[["label"]], character(1))
  } else {
    character(0)
  }

  if (!modifying && label_missing && label %in% existing_labels) {
    dup_pos <- match(label, existing_labels)
    cli::cli_abort(c(
      "Auto-generated label {.val {label}} already exists ([{dup_pos}]).",
      "i" = "Provide a unique {.arg label} explicitly."
    ))
  }

  if (modifying) {
    other_labels <- existing_labels[-modify_pos]
    if (label %in% other_labels) {
      dup_pos <- match(label, existing_labels)
      cli::cli_abort(c(
        "Label {.val {label}} is already used by unit test [{dup_pos}].",
        "x" = "Labels must be unique."
      ))
    }
  } else {
    if (label %in% existing_labels) {
      dup_pos <- match(label, existing_labels)
      cli::cli_abort(c(
        "A unit test with label {.val {label}} already exists ([{dup_pos}]).",
        "i" = "Labels must be unique. Use {.code unit_test(object, nr = {dup_pos})} to modify the existing test."
      ))
    }
  }

  # --- Check for duplicate expr ---
  existing_exprs <- if (n_tests > 0) {
    vapply(tests, function(t) t[["expr_str"]], character(1))
  } else {
    character(0)
  }
  check_exprs <- if (modifying) existing_exprs[-modify_pos] else existing_exprs
  if (expr_str %in% check_exprs) {
    dup_idx <- which(existing_exprs == expr_str)
    if (modifying) dup_idx <- setdiff(dup_idx, modify_pos)
    dup_idx <- dup_idx[vapply(dup_idx, function(i) {
      identical(tests[[i]][["conditions"]], conditions)
    }, logical(1))]
    if (length(dup_idx) > 0) {
      cli::cli_abort(c(
        "An identical expression already exists in unit test number {dup_idx[1]}.",
        "i" = "Use {.code unit_test(object, nr = {dup_idx[1]})} to modify the existing test."
      ))
    }
  }

  # --- Validate expr variables ---
  model_names <- get_model_var(object)
  ut_vars <- .ut_expr_vars(expr_str, model_names)
  if (length(ut_vars[["unknown"]]) > 0) {
    cli::cli_abort(c(
      "Invalid {.arg expr}: variable{?s} not found in model: {.val {ut_vars[['unknown']]}}.",
      "i" = "Check for typos. Available variables: {.val {model_names}}."
    ))
  }

  # --- Validate conditions ---
  if (!is.list(conditions)) {
    cli::cli_abort(c(
      "Invalid {.arg conditions} argument.",
      "x" = "{.arg conditions} must be a named list of parameter overrides.",
      ">" = "Example: {.code conditions = list(beta = 0.1)}"
    ))
  }
  if (length(conditions) > 0) {
    if (is.null(names(conditions)) || any(!nzchar(names(conditions)))) {
      cli::cli_abort(c(
        "Invalid {.arg conditions} argument.",
        "x" = "All elements of {.arg conditions} must be named."
      ))
    }
    valid_names <- object[["variables"]][
      object[["variables"]][["type"]] %in% c("stock", "constant"), "name"
    ]
    bad_names <- setdiff(names(conditions), valid_names)
    if (length(bad_names) > 0) {
      cli::cli_abort(c(
        "Invalid {.arg conditions} argument.",
        "x" = "Names not found as stocks or constants: {.val {bad_names}}.",
        "i" = "Only stocks and constants can be specified as conditions. Available: {.val {valid_names}}."
      ))
    }
  }

  # --- Validate active ---
  if (!is.logical(active) || length(active) != 1L || is.na(active)) {
    cli::cli_abort(c(
      "Invalid {.arg active} argument.",
      "x" = "{.arg active} must be {.val TRUE} or {.val FALSE}."
    ))
  }

  # --- Build entry ---
  entry <- list(
    label      = label,
    expr_str   = expr_str,
    conditions = conditions,
    active     = active
  )

  # --- Insert or replace ---
  if (modifying) {
    object[["unit_tests"]][[modify_pos]] <- entry
  } else {
    object[["unit_tests"]] <- c(object[["unit_tests"]], list(entry))
  }

  # --- Eagerly recompute test deps cache ---
  object[["assemble"]][["unit_tests"]][["deps"]] <- .compute_test_deps(object)

  object
}


# ==============================================================================
# DISCARD UNIT TEST
# ==============================================================================

#' Remove a unit test from a stock-and-flow model
#'
#' Remove one or more unit tests by `nr` (integer position as shown by
#' [unit_tests()]) or by `label` (character). Warns if a label or index is
#' not found. Remaining tests are renumbered sequentially after removal.
#'
#' @inheritParams update.sdbuildR
#' @param nr Integer index/indices of the test(s) to remove. Corresponds to
#'   the order shown by [unit_tests()].
#' @param label Character label(s) of the test(s) to remove. Supports NSE
#'   (bare symbol or string). For backward compatibility, integer values
#'   passed via `label` are also accepted.
#'
#' @returns The model object with the specified test(s) removed.
#'
#' @export
#' @concept verify
#' @seealso [unit_test()], [unit_tests()]
#'
#' @examples
#' sfm <- sdbuildR("SIR") |>
#'   unit_test(label = "S is non-negative", expr = all(S >= 0)) |>
#'   unit_test(label = "R increases", expr = all(diff(R) >= 0))
#'
#' # Remove by nr
#' sfm <- discard_unit_test(sfm, nr = 1)
#'
#' # Remove by label
#' sfm <- discard_unit_test(sfm, label = "R increases")
discard_unit_test <- function(object, label, nr) {
  check_sdbuildR(object)

  n_tests <- length(object[["unit_tests"]])

  if (!missing(nr)) {
    # --- Explicit nr path ---
    if (!is.numeric(nr)) {
      cli::cli_abort(c(
        "Invalid {.arg nr} argument.",
        "x" = "{.arg nr} must be an integer vector."
      ))
    }
    idx <- as.integer(nr)

    bad_idx <- idx[idx < 1L | idx > n_tests]
    if (length(bad_idx) > 0) {
      cli::cli_abort(c(
        "x" = "{cli::qty(length(bad_idx))}Index{?es} out of range: {.val {bad_idx}}.",
        "i" = "Model has {n_tests} unit test{?s}."
      ))
    }
    idx_to_remove <- idx[idx >= 1L & idx <= n_tests]
  } else if (!missing(label)) {
    # --- label path (backward-compatible: handles int or char) ---
    label_expr <- rlang::enexpr(label)

    if (is.numeric(label_expr) || (rlang::is_call(label_expr) && identical(rlang::call_name(label_expr), "c"))) {
      idx <- eval(label_expr)
      if (!is.integer(idx)) idx <- as.integer(idx)

      bad_idx <- idx[idx < 1L | idx > n_tests]
      if (length(bad_idx) > 0) {
        cli::cli_abort(c(
          "x" = "{cli::qty(length(bad_idx))}Index{?es} out of range: {.val {bad_idx}}.",
          "i" = "Model has {n_tests} unit test{?s}."
        ))
      }
      idx_to_remove <- idx[idx >= 1L & idx <= n_tests]
    } else {
      all_labels <- vapply(object[["unit_tests"]], function(t) t[["label"]], character(1))
      labels_wanted <- .expr_to_char(label_expr)
      not_found <- setdiff(labels_wanted, all_labels)
      if (length(not_found) > 0) {
        cli::cli_abort(c(
          "x" = "{cli::qty(length(not_found))}Unit test{?s} not found: {.val {not_found}}."
        ))
      }
      idx_to_remove <- which(all_labels %in% labels_wanted)
    }
  } else {
    cli::cli_abort("Please specify {.arg nr} or {.arg label} to identify the test(s) to remove.")
  }

  # Batch remove (remaining tests are renumbered implicitly by position)
  if (length(idx_to_remove) > 0) {
    object[["unit_tests"]] <- object[["unit_tests"]][-idx_to_remove]
  }

  # Invalidate cached test deps (positions shifted after removal)
  object <- invalidate_assemble(object, "unit_tests")

  object
}


# ==============================================================================
# DISPLAY UNIT TESTS
# ==============================================================================

#' Display unit tests defined on a stock-and-flow model
#'
#' Returns an overview of all unit tests attached to the model. The result
#' has a `print()` method.
#'
#' @inheritParams update.sdbuildR
#'
#' @returns An object of class `unit_tests_sdbuildR`, printed automatically.
#'
#' @export
#' @concept verify
#' @seealso [unit_test()], [verify()]
#'
#' @examples
#' sfm <- sdbuildR("SIR") |>
#'   unit_test(expr = all(S >= 0)) |>
#'   unit_test(label = "R increases over time", expr = all(diff(R) >= 0))
#'
#' unit_tests(sfm)
unit_tests <- function(object) {
  check_sdbuildR(object)
  tests <- object[["unit_tests"]]
  result <- structure(
    list(tests = tests, n = length(tests)),
    class = "unit_tests_sdbuildR"
  )
  result
}


#' @export
print.unit_tests_sdbuildR <- function(x, ...) {
  if (x$n == 0) {
    cli::cli_inform(c("i" = "No unit tests defined. Add tests with {.fn unit_test}."))
    return(invisible(x))
  }

  # Number of tests, active tests, and tests with conditions
  n_active <- sum(vapply(x$tests, function(t) isTRUE(t[["active"]]), logical(1L)))
  n_cond <- sum(vapply(x$tests, function(t) length(t[["conditions"]]) > 0, logical(1L)))

  cli::cli_h1("Unit Tests in Stock-and-Flow Model")
  cli::cli_text(paste0(c("{x$n} tests", "{n_active}/{x$n} active", "{n_cond}/{x$n} include conditions"), collapse = " \u2022 "))


  for (i in seq_along(x$tests)) {
    t <- x$tests[[i]]
    icon <- if (isTRUE(t[["active"]])) "o" else "-"
    cli::cli_bullets(c("{icon}" = paste0("[", i, "] ", t[["label"]])))
    cli::cli_text("  {.code {t$expr_str}}")
    if (length(t[["conditions"]]) > 0) {
      cond_str <- paste(names(t[["conditions"]]), unlist(t[["conditions"]]),
        sep = " = ", collapse = ", "
      )
      cli::cli_text("  Conditions: {cond_str}")
    }
  }

  invisible(x)
}


# ==============================================================================
# RESULT CONSTRUCTOR + PRINT
# ==============================================================================

#' @noRd
new_verify_sdbuildR <- function(results, object, sims = NULL, sim_index = NULL) {
  structure(
    list(results = results, object = object, sims = sims, sim_index = sim_index),
    class = "verify_sdbuildR"
  )
}


#' @export
print.verify_sdbuildR <- function(x, ...) {
  results <- x[["results"]]

  if (length(results) == 0) {
    cli::cli_inform("No unit tests to report.")
    return(invisible(x))
  }

  n_pass <- sum(vapply(results, function(r) identical(r[["status"]], "pass"), logical(1L)))
  n_fail <- sum(vapply(results, function(r) identical(r[["status"]], "fail"), logical(1L)))
  n_skip <- sum(vapply(results, function(r) identical(r[["status"]], "skipped"), logical(1L)))
  n_error <- sum(vapply(results, function(r) identical(r[["status"]], "error"), logical(1L)))
  n_run <- n_pass + n_fail + n_error
  n_total <- length(results)

  cli::cli_h1("Unit Test Results")

  if (n_skip > 0 && n_run == 0) {
    cli::cli_text("{n_skip}/{n_total} test{?s} skipped.")
  } else {
    cli::cli_text("{n_pass}/{n_run} test{?s} passed.")
  }

  for (r in results) {
    switch(r[["status"]],
      pass = {
        cli::cli_bullets(c("v" = r[["label"]]))
      },
      fail = {
        cli::cli_bullets(c("x" = r[["label"]]))
        if (!is.null(r[["message"]]) && nzchar(r[["message"]])) {
          cli::cli_text("  {r$message}")
        }
      },
      error = {
        cli::cli_bullets(c("!" = r[["label"]]))
        if (!is.null(r[["message"]]) && nzchar(r[["message"]])) {
          cli::cli_text("  {r$message}")
        }
      },
      skipped = {
        cli::cli_bullets(c("-" = r[["label"]]))
        if (!is.null(r[["message"]]) && nzchar(r[["message"]])) {
          cli::cli_text("  {.emph {r$message}}")
        }
      }
    )
  }

  invisible(x)
}


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

#' Check whether a test's conditions already match the model's current values
#'
#' Returns TRUE when conditions is empty (no overrides needed) or when every
#' override value matches the corresponding variable's current `eqn`.
#'
#' @param test A single unit test entry (list with `conditions` field)
#' @param object An `sdbuildR` model object
#' @return Logical scalar
#' @noRd
.conditions_match <- function(test, object) {
  conds <- test[["conditions"]]
  if (length(conds) == 0L) {
    return(TRUE)
  }

  vars_df <- object[["variables"]]
  for (nm in names(conds)) {
    row <- vars_df[vars_df[["name"]] == nm, , drop = FALSE]
    if (nrow(row) == 0L) {
      return(FALSE)
    }
    current <- suppressWarnings(as.numeric(row[["eqn"]]))
    target <- suppressWarnings(as.numeric(conds[[nm]]))
    if (is.na(current) || is.na(target) || !isTRUE(all.equal(current, target))) {
      return(FALSE)
    }
  }
  TRUE
}


#' Build an evaluation environment from a simulation data frame
#'
#' Converts the long-format `df` (columns: time, variable, value) to a named
#' environment where each variable maps to its full time-series vector. The
#' parent is `asNamespace("testthat")` so `expect_*` functions are available
#' without qualification.
#'
#' @param df A data frame with columns `time`, `variable`, `value`
#' @return A new environment suitable for `eval()`
#' @noRd
.build_eval_env <- function(df) {
  vars <- unique(df[["variable"]])
  var_list <- lapply(
    stats::setNames(vars, vars),
    function(v) df[df[["variable"]] == v, "value"]
  )
  if (!"time" %in% vars) {
    var_list[["time"]] <- sort(unique(df[["time"]]))
  }
  list2env(var_list, parent = asNamespace("testthat"))
}


#' Evaluate a single unit test against a simulation result
#'
#' @param test A single unit test entry
#' @param sim A `simulate_sdbuildR` object
#' @return A named list with fields: `label`, `expr_str`, `conditions`, `status`, `error_type`, 
#'   `message`, `outcome`. The `error_type` field is `NA` for pass/fail/skipped results, 
#'   and one of `"expr_syntax"` (parse error), `"expr_result"` (type validation error), 
#'   `"expr_eval"` (runtime evaluation error), `"simulation"` (simulation failure), or 
#'   `"model_update"` (conditions application failure) for error results.
#' @noRd
.run_one_unit_test <- function(test, sim) {
  label <- test[["label"]]
  expr_str <- test[["expr_str"]]
  conditions <- if (is.null(test[["conditions"]])) list() else test[["conditions"]]

  if (!isTRUE(test[["active"]])) {
    return(list(label = label, expr_str = expr_str, conditions = conditions, status = "skipped", error_type = NA, message = "Test is inactive.", outcome = NULL))
  }

  if (!sim[["success"]]) {
    return(list(
      label = label, expr_str = expr_str, conditions = conditions, status = "error", error_type = "simulation",
      message = paste0("Simulation failed: ", sim[["error_message"]]), outcome = NULL
    ))
  }

  env <- .build_eval_env(sim[["df"]])

  # Inject constants into eval environment (they are not in sim$df)
  constants <- sim[["constants"]]
  if (!is.null(constants)) {
    for (nm in names(constants)) {
      env[[nm]] <- constants[[nm]]
    }
  }

  expr_parsed <- tryCatch(
    parse(text = test[["expr_str"]], keep.source = FALSE)[[1]],
    error = function(e) NULL
  )
  if (is.null(expr_parsed)) {
    return(list(
      label = label, expr_str = expr_str, conditions = conditions, status = "error", error_type = "expr_syntax",
      message = paste0("Could not parse expression: ", expr_str), outcome = NULL
    ))
  }

  # Heuristic: calls that look like assertions (e.g., expect_equal(...))
  # are allowed to return non-logical values when they complete without failure.
  is_assertion_call <- is.call(expr_parsed) && {
    head <- expr_parsed[[1]]
    if (is.symbol(head)) {
      grepl("^(expect|assert)", as.character(head))
    } else if (is.call(head) && as.character(head[[1]]) %in% c("::", ":::")) {
      fn <- head[[3]]
      is.symbol(fn) && grepl("^(expect|assert)", as.character(fn))
    } else {
      FALSE
    }
  }

  result <- tryCatch(
    {
      val <- eval(expr_parsed, envir = env)

      if (!is.logical(val) || length(val) != 1L || is.na(val)) {
        if (is_assertion_call) {
          return(list(label = label, expr_str = expr_str, conditions = conditions, status = "pass", error_type = NA, message = "", outcome = TRUE))
        }
        val_class <- paste(class(val), collapse = "/")
        val_len <- length(val)
        cond <- simpleError(
          sprintf(
            "Expression must return a non-missing logical scalar, got %s of length %d.",
            val_class, val_len
          )
        )
        class(cond) <- c("expr_type_error", class(cond))
        stop(cond)
      }

      if (!isTRUE(val)) {
        testthat::expect_true(val, label = label)
      }

      list(label = label, expr_str = expr_str, conditions = conditions, status = "pass", error_type = NA, message = "", outcome = val)
    },
    expectation_failure = function(e) {
      list(label = label, expr_str = expr_str, conditions = conditions, status = "fail", error_type = NA, message = conditionMessage(e), outcome = FALSE)
    },
    expr_type_error = function(e) {
      list(label = label, expr_str = expr_str, conditions = conditions, status = "error", error_type = "expr_result", message = conditionMessage(e), outcome = NULL)
    },
    error = function(e) {
      list(label = label, expr_str = expr_str, conditions = conditions, status = "error", error_type = "expr_eval", message = conditionMessage(e), outcome = NULL)
    }
  )

  result
}


#' Extract model-variable references from a unit-test expression string
#'
#' Returns a list with two character vectors:
#' - `model_refs`: symbols that appear in `expr_str` AND are model variables
#' - `unknown`:    symbols that appear in `expr_str` AND are neither model
#'                 variables nor objects exported by base R
#'
#' Uses `all.vars()` so function names (e.g. `all`, `diff`, `expect_equal`)
#' are never included — only value-position symbols are checked.
#'
#' @param expr_str    Deparsed expression string (as stored in `unit_tests`)
#' @param model_names Character vector of model variable names
#' @return Named list with `model_refs` and `unknown` character vectors
#' @noRd
.ut_expr_vars <- function(expr_str, model_names) {
  parsed <- tryCatch(
    parse(text = expr_str, keep.source = FALSE)[[1]],
    error = function(e) NULL
  )
  if (is.null(parsed)) {
    return(list(model_refs = character(0), unknown = character(0)))
  }

  ev <- all.vars(parsed)
  not_in_model <- setdiff(ev, model_names)
  unknown <- not_in_model[
    !vapply(not_in_model, exists, logical(1), envir = baseenv(), inherits = FALSE)
  ]

  list(
    model_refs = intersect(ev, model_names),
    unknown    = unknown
  )
}


#' Compute unit test dependencies for all tests
#'
#' For each unit test, extracts `expr_refs` (model variables referenced in the
#' expression) and `cond_refs` (variable names used as condition keys). Returns
#' a positionally-indexed list matching `object[["unit_tests"]]`.
#'
#' @inheritParams update.sdbuildR
#' @return List of `list(expr_refs = character(), cond_refs = character())`
#' @noRd
.compute_test_deps <- function(object) {
  tests <- object[["unit_tests"]]
  if (length(tests) == 0L) {
    return(list())
  }

  model_names <- get_model_var(object)

  lapply(tests, function(test) {
    expr_refs <- .ut_expr_vars(test[["expr_str"]], model_names)[["model_refs"]]
    cond_refs <- names(test[["conditions"]])
    if (is.null(cond_refs)) cond_refs <- character(0)
    list(expr_refs = expr_refs, cond_refs = cond_refs)
  })
}


#' Get (lazily computed) unit test dependencies
#'
#' Returns the cached test deps from `object[["assemble"]][["unit_tests"]][["deps"]]`
#' if available; otherwise computes via `.compute_test_deps()`, stores the result,
#' and returns it. The caller is responsible for assigning the returned object
#' back if caching is desired.
#'
#' @inheritParams update.sdbuildR
#' @return A list with two elements:
#'   - `object`: the (possibly updated) model object
#'   - `deps`: positionally-indexed list of `list(expr_refs, cond_refs)`
#' @noRd
get_test_deps <- function(object) {
  deps <- object[["assemble"]][["unit_tests"]][["deps"]]
  if (is.null(deps)) {
    deps <- .compute_test_deps(object)
    object[["assemble"]][["unit_tests"]][["deps"]] <- deps
  }
  list(object = object, deps = deps)
}
