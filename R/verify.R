# ==============================================================================
# VERIFY GENERIC
# ==============================================================================

#' Verify model
#'
#' @param object A model object to verify.
#' @param ... Additional arguments passed to specific methods.
#' @export
#' @concept unitTest
verify <- function(object, ...) {
  UseMethod("verify")
}


#' Verify unit tests against simulation results
#'
#' Run all active unit tests defined on a stock-and-flow model. Use
#' [unit_test()] to define tests; use [unit_tests()] to display them.
#'
#' Calling `verify()` on a `sdbuildR` model will first simulate the model, then
#' run all tests — including those that require re-simulation under alternative
#' [conditions][unit_test()].
#'
#' Setting `n > 1` runs an ensemble of `n` simulations per condition (robustness
#' testing). Each test is evaluated against all `n` runs. The test passes only
#' if all `n` runs pass.
#'
#' @param object An [`sdbuildR`][sdbuildR] object.
#' @param verbose If `TRUE` (default), print results to the console.
#' @param n Number of simulations to run per condition. Defaults to `1` (deterministic
#'   verification). Set to a larger value for robustness testing of stochastic models.
#' @param nr Integer vector of test number(s) to run (1-based, as shown by [unit_tests()]).
#'   Defaults to `NULL` (run all tests).
#' @param ... Additional arguments passed to [simulate.sdbuildR()].
#'
#' @returns An object of class `verify_sdbuildR` with fields, returned invisibly:
#'   \describe{
#'     \item{results}{List of test result entries, each with `label`, `expr_str`,
#'       `conditions`, `status`, `error_type`, `message`, `outcome`, `pass_rate`,
#'       `n_pass`, `n_fail`, and `n_error`.}
#'     \item{object}{The `sdbuildR` model the tests were run against.}
#'     \item{sims}{Nested list of `simulate_sdbuildR` objects structured as
#'       `sims[[j]][[i]]` (condition `j`, run `i`), or `NULL` if
#'       `return_sims = FALSE`.}
#'     \item{j}{Named integer vector mapping each test label to its condition
#'       index in `sims`. Always populated. Use
#'       `result$sims[[result$j[["my test label"]]]][[1]]` to retrieve the
#'       simulation used for a specific test.}
#'     \item{n}{Number of simulations run per condition.}
#'     \item{n_conditions}{Number of unique simulation conditions.}
#'     \item{test_indices}{Integer vector of the original 1-based test numbers that
#'       were run (as shown by [unit_tests()]). Equal to `seq_along(results)` when
#'       `nr = NULL` (all tests run).}
#'   }
#'
#' @export
#' @concept unitTest
#' @method verify sdbuildR
#' @seealso [unit_test()], [unit_tests()], [simulate.sdbuildR()]
#'
#' @examples
#' sfm <- sdbuildR("SIR") |>
#'   unit_test(expr = all(Susceptible >= 0)) |>
#'   unit_test(
#'     label = "Recovered increases over time",
#'     expr = all(diff(Recovered) >= 0)
#'   )
#'
#' verify(sfm)
verify.sdbuildR <- function(object, verbose = TRUE, n = 1L, nr = NULL, ...) {
  check_sdbuildR(object)
  rlang::check_installed("testthat", reason = "to run unit tests with {.fn verify}")

  # Override sim_settings with any arguments passed via ...
  varargs <- list(...)
  if (length(varargs) > 0) {
    object <- do.call(sim_settings, c(list(object), varargs))
  }

  # Persistent meta-setting: read return_sims from sim_settings AFTER applying varargs
  return_sims <- isTRUE(object[["sim_settings"]][["return_sims"]])

  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1L || n != as.integer(n)) {
    cli::cli_abort(c(
      "x" = "The {.arg n} argument must be a positive integer scalar.",
      "i" = "Received: {.val {n}}"
    ))
  }
  n <- as.integer(n)

  tests <- object[["unit_tests"]]
  if (length(tests) == 0) {
    cli::cli_abort(c(
      "x" = "No unit tests defined. Add tests with {.fn unit_test}."
    ))
    return(invisible(new_verify_sdbuildR(results = list(), object = object)))
  }

  # Subset tests if nr supplied
  test_indices <- seq_along(tests)
  if (!is.null(nr)) {
    .check_nr_index(nr, length(tests))
    tests        <- tests[nr]
    test_indices <- as.integer(nr)
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
  n_conditions <- length(unique_keys)

  # Determine per condition whether the simulation requires `only_stocks = FALSE`,
  # which is necessary when unit test expressions contain flows or auxiliaries.
  stock_or_constant_names <- object[["variables"]][
    object[["variables"]][["type"]] %in% c("stock", "constant"), "name"
  ]
  td <- get_test_deps(object)
  object <- td[["object"]]
  deps <- td[["deps"]]

  needs_non_stocks <- vapply(seq_along(unique_keys), function(i) {
    key <- unique_keys[[i]]
    test_indices <- which(condition_keys == key)
    all_refs <- unique(unlist(lapply(test_indices, function(k) deps[[k]][["expr_refs"]])))
    any(!all_refs %in% stock_or_constant_names)
  }, logical(1))

  # Remove only_stocks from dots if mistakenly passed (prevent duplicate arg error)
  dots <- list(...)
  dots[["only_stocks"]] <- NULL

  # Run n simulations per unique condition set.
  # sim_cache[[j_idx]] is a list of n simulate_sdbuildR objects.
  sim_cache <- vector("list", n_conditions)
  for (j_idx in seq_along(unique_keys)) {
    key <- unique_keys[[j_idx]]
    os <- !needs_non_stocks[[j_idx]]
    if (key == .BASELINE) {
      obj_for_cond <- object
    } else {
      obj_for_cond <- object
      first_test_idx <- which(condition_keys == key)[[1]]
      conds <- tests[[first_test_idx]][["conditions"]]
      for (nm in names(conds)) {
        eqn_val <- as.character(conds[[nm]])
        obj_for_cond <- rlang::inject(update(obj_for_cond, name = !!nm, eqn = !!eqn_val))
      }
    }
    sim_cache[[j_idx]] <- lapply(seq_len(n), function(dummy)
      rlang::inject(simulate(obj_for_cond, only_stocks = os, !!!dots))
    )
  }

  # Evaluate each test against the appropriate simulation(s).
  results <- lapply(tests, function(test) {
    key <- if (length(test[["conditions"]]) == 0) {
      .BASELINE
    } else {
      paste(names(test[["conditions"]]), unlist(test[["conditions"]]), sep = "=", collapse = ";")
    }
    j_idx <- match(key, unique_keys)
    run_results <- lapply(sim_cache[[j_idx]], function(sim) .run_one_unit_test(test, sim))
    if (n == 1L) {
      .enrich_single_result(run_results[[1L]])
    } else {
      .aggregate_run_results(run_results)
    }
  })

  # j always populated: named integer vector mapping test label -> condition index
  j <- stats::setNames(
    match(condition_keys, unique_keys),
    vapply(tests, function(t) t[["label"]], character(1))
  )

  sims <- if (return_sims) sim_cache else NULL

  result_obj <- new_verify_sdbuildR(
    results = results, object = object,
    sims = sims, j = j,
    n = n, n_conditions = n_conditions,
    test_indices = test_indices
  )
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
#' @concept unitTest
#' @seealso [verify()], [unit_tests()], [discard_unit_test()]
#'
#' @examples
#' sfm <- sdbuildR("SIR") |>
#'   unit_test(expr = all(Susceptible >= 0))
#'
#' # Run unit tests
#' verify(sfm)
#'
#' # Add test with label
#' sfm <- unit_test(sfm,
#'   label = "Recovered increases",
#'   expr = all(diff(Recovered) >= 0)
#' )
#' verify(sfm)
#'
#' # Add test with conditions
#' sfm <- unit_test(sfm,
#'   expr = all(Infected == Infected[1]),
#'   label = "When Beta is zero, no one gets infected",
#'   conditions = list(Beta = 0)
#' )
#' verify(sfm)
#'
#' # View all tests
#' unit_tests(sfm)
#'
#' # Deactivate test nr. 1
#' sfm <- unit_test(sfm, nr = 1, active = FALSE)
#' verify(sfm)
#'
#' # Modify test by label, e.g., to change the expression
#' sfm <- unit_test(sfm,
#'   label = "Recovered increases over time",
#'   expr = all(diff(Recovered) > -1)
#' )
#' verify(sfm)
#'
unit_test <- function(object, nr, expr, label, conditions = list(), active = TRUE) {
  check_sdbuildR(object)

  tests <- object[["unit_tests"]]
  n_tests <- length(tests)

  # --- Capture arguments & missingness flags ---
  expr_captured <- rlang::enexpr(expr)
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
#' @concept unitTest
#' @seealso [unit_test()], [unit_tests()]
#'
#' @examples
#' sfm <- sdbuildR("SIR") |>
#'   unit_test(label = "Susceptible is non-negative", expr = all(Susceptible >= 0)) |>
#'   unit_test(label = "Recovered increases", expr = all(diff(Recovered) >= 0))
#'
#' # Remove by nr
#' sfm <- discard_unit_test(sfm, nr = 1)
#'
#' # Remove by label
#' sfm <- discard_unit_test(sfm, label = "Recovered increases")
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
    cli::cli_abort(c("x" = "Please specify {.arg nr} or {.arg label} to identify the test(s) to remove."))
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
#' @param nr Integer vector of test number(s) to display (1-based). Defaults to
#'   `NULL` (show all tests). Can be combined with `label` (intersection).
#' @param label Character vector of regex patterns for partial, case-insensitive
#'   label matching. A test is included if its label matches *any* pattern.
#'   E.g., `c("non-neg", "beta")` returns tests matching either fragment.
#'   Can be combined with `nr` (intersection).
#' @param ignore_case Logical; whether `label` matching is case-insensitive.
#'   Default `TRUE`.
#'
#' @returns An object of class `unit_tests_sdbuildR`, printed automatically.
#'
#' @export
#' @concept unitTest
#' @seealso [unit_test()], [verify()]
#'
#' @examples
#' sfm <- sdbuildR("SIR") |>
#'   unit_test(expr = all(Susceptible >= 0)) |>
#'   unit_test(
#'     label = "Recovered increases over time",
#'     expr = all(diff(Recovered) >= 0)
#'   )
#'
#' unit_tests(sfm)
#' unit_tests(sfm, nr = 1L)
#' unit_tests(sfm, label = "increases")
unit_tests <- function(object, nr = NULL, label = NULL, ignore_case = TRUE) {
  check_sdbuildR(object)
  tests   <- object[["unit_tests"]]
  indices <- seq_along(tests)
  if (!is.null(nr)) {
    .check_nr_index(nr, length(tests))
    tests   <- tests[as.integer(nr)]
    indices <- as.integer(nr)
  }
  if (!is.null(label)) {
    if (!is.character(label) || length(label) == 0L || any(is.na(label)) || any(!nzchar(label)))
      cli::cli_abort(c(
        "Invalid {.arg label}.",
        "x" = "{.arg label} must be a character vector of non-empty regex pattern(s)."
      ))
    current_labels <- vapply(tests, function(t) t[["label"]], character(1L))
    hits    <- Reduce("|", lapply(label, grepl, x = current_labels, ignore.case = ignore_case))
    matched <- which(hits)
    if (length(matched) == 0) {
      cli::cli_warn(c(
        "No tests matched pattern{?s} {.val {label}}.",
        "i" = "Available labels: {.val {current_labels}}."
      ))
      tests   <- list()
      indices <- integer(0L)
    } else {
      tests   <- tests[matched]
      indices <- indices[matched]
    }
  }
  result <- structure(
    list(tests = tests, n = length(tests), indices = indices),
    class = "unit_tests_sdbuildR"
  )
  result
}


#' @export
#' @concept unitTest
print.unit_tests_sdbuildR <- function(x, ...) {
  if (x$n == 0) {
    cli::cli_inform(c("i" = "No unit tests defined. Add tests with {.fn unit_test}."))
    return(invisible(x))
  }

  # Number of tests, active tests, and tests with conditions
  n_active <- sum(vapply(x$tests, function(t) isTRUE(t[["active"]]), logical(1L)))
  n_cond <- sum(vapply(x$tests, function(t) length(t[["conditions"]]) > 0, logical(1L)))

  cli::cli_h1("Stock-and-Flow Unit Tests")
  cli::cli_text(paste0(c("{x$n} tests", "{n_active}/{x$n} active", "{n_cond}/{x$n} include conditions"), collapse = " \u2022 "))


  for (i in seq_along(x$tests)) {
    t <- x$tests[[i]]
    original_nr <- if (is.null(x[["indices"]])) i else x[["indices"]][[i]]
    icon <- if (isTRUE(t[["active"]])) cli::col_green(cli::symbol$bullet) else cli::symbol$line
    cli::cli_bullets(c(" " = "{original_nr}. {icon} {t[['label']]}"))
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
new_verify_sdbuildR <- function(results, object, sims = NULL, j = NULL,
                                 n = 1L, n_conditions = 1L, test_indices = NULL) {
  if (is.null(test_indices)) test_indices <- seq_along(results)
  structure(
    list(
      results = results, object = object, sims = sims, j = j,
      n = n, n_conditions = n_conditions,
      test_indices = as.integer(test_indices)
    ),
    class = "verify_sdbuildR"
  )
}


#' Add pass_rate/n_pass/n_fail/n_error fields to a single-run result
#' @noRd
.enrich_single_result <- function(r) {
  r[["pass_rate"]] <- if (r[["status"]] == "pass") 1.0 else if (r[["status"]] %in% c("fail", "error")) 0.0 else NA_real_
  r[["n_pass"]]  <- if (r[["status"]] == "pass") 1L else 0L
  r[["n_fail"]]  <- if (r[["status"]] == "fail") 1L else 0L
  r[["n_error"]] <- if (r[["status"]] == "error") 1L else 0L
  r
}


#' Aggregate n run results into a single summary result
#' @noRd
.aggregate_run_results <- function(run_results) {
  # If the test is skipped in every run, return the first skipped entry enriched
  if (all(vapply(run_results, function(r) r[["status"]] == "skip", logical(1)))) {
    return(.enrich_single_result(run_results[[1]]))
  }

  n_pass  <- sum(vapply(run_results, function(r) identical(r[["status"]], "pass"),  logical(1)))
  n_fail  <- sum(vapply(run_results, function(r) identical(r[["status"]], "fail"),  logical(1)))
  n_error <- sum(vapply(run_results, function(r) identical(r[["status"]], "error"), logical(1)))
  n_run   <- n_pass + n_fail + n_error

  pass_rate <- if (n_run == 0L) NA_real_ else n_pass / n_run

  # Determine aggregate status: strict — all runs must pass
  status <- if (is.na(pass_rate)) {
    "skip"
  } else if (n_error == n_run) {
    "error"
  } else if (n_fail == 0L && n_error == 0L) {
    "pass"
  } else {
    "fail"
  }

  # Build message from first non-passing run
  first_bad <- Find(function(r) r[["status"]] %in% c("fail", "error"), run_results)
  message <- if (!is.null(first_bad) && nzchar(first_bad[["message"]])) {
    paste0(n_pass, "/", n_run, " runs passed. First issue: ", first_bad[["message"]])
  } else if (status == "fail") {
    paste0(n_pass, "/", n_run, " runs passed.")
  } else {
    ""
  }

  # Most common error_type among non-NA entries
  error_types <- vapply(run_results, function(r) {
    if (is.na(r[["error_type"]])) "" else r[["error_type"]]
  }, character(1))
  error_type <- if (all(error_types == "")) NA_character_ else {
    tab <- table(error_types[error_types != ""])
    names(tab)[which.max(tab)]
  }

  # outcome: TRUE if passed threshold, FALSE if failed, NA for skip/error
  outcome <- if (status %in% c("skip", "error")) NULL else isTRUE(status == "pass")

  base <- run_results[[1]]
  list(
    label      = base[["label"]],
    expr_str   = base[["expr_str"]],
    conditions = base[["conditions"]],
    status     = status,
    error_type = error_type,
    message    = message,
    outcome    = outcome,
    pass_rate  = pass_rate,
    n_pass     = n_pass,
    n_fail     = n_fail,
    n_error    = n_error
  )
}


#' @export
#' @concept unitTest
print.verify_sdbuildR <- function(x, ...) {
  results <- x[["results"]]
  n_sims  <- if (is.null(x[["n"]])) 1L else x[["n"]]

  if (length(results) == 0) {
    cli::cli_inform(c("i" = "No unit tests to report."))
    return(invisible(x))
  }

  n_pass  <- sum(vapply(results, function(r) identical(r[["status"]], "pass"),    logical(1L)))
  n_fail  <- sum(vapply(results, function(r) identical(r[["status"]], "fail"),    logical(1L)))
  n_skip  <- sum(vapply(results, function(r) identical(r[["status"]], "skip"), logical(1L)))
  n_error <- sum(vapply(results, function(r) identical(r[["status"]], "error"),   logical(1L)))
  n_run   <- n_pass + n_fail + n_error
  n_total <- length(results)

  cli::cli_h1("Stock-and-Flow Unit Test Results")

  if (n_sims > 1L) {
    cli::cli_text("{n_pass}/{n_run} test{?s} passed ({n_sims} runs per condition).")
  } else if (n_skip > 0 && n_run == 0) {
    cli::cli_text("{n_skip}/{n_total} test{?s} skipped.")
  } else {
    cli::cli_text("{n_pass}/{n_run} test{?s} passed.")
  }

  test_indices <- x[["test_indices"]]
  for (i in seq_along(results)) {
    r <- results[[i]]
    original_nr <- if (!is.null(test_indices)) test_indices[[i]] else i

    # Build optional run-count prefix for n > 1
    run_prefix <- if (n_sims > 1L && !is.null(r[["pass_rate"]]) && !is.na(r[["pass_rate"]])) {
      n_pass_r  <- if (is.null(r[["n_pass"]]))  0L else r[["n_pass"]]
      n_fail_r  <- if (is.null(r[["n_fail"]]))  0L else r[["n_fail"]]
      n_error_r <- if (is.null(r[["n_error"]])) 0L else r[["n_error"]]
      n_r <- n_pass_r + n_fail_r + n_error_r
      paste0("(", n_pass_r, "/", n_r, ") ")
    } else {
      ""
    }

    switch(r[["status"]],
      pass = {
        cli::cli_bullets(c(" " = "{original_nr}. {cli::col_green(cli::symbol$tick)} {run_prefix}{r[['label']]}"))
      },
      fail = {
        cli::cli_bullets(c(" " = "{original_nr}. {cli::col_red(cli::symbol$cross)} {run_prefix}{r[['label']]}"))
        if (!is.null(r[["message"]]) && nzchar(r[["message"]])) {
          cli::cli_text("  {r$message}")
        }
      },
      error = {
        cli::cli_bullets(c(" " = "{original_nr}. {cli::col_yellow('!')} {run_prefix}{r[['label']]}"))
        if (!is.null(r[["message"]]) && nzchar(r[["message"]])) {
          cli::cli_text("  {r$message}")
        }
      },
      skip = {
        cli::cli_bullets(c(" " = "{original_nr}. {cli::symbol$line} {r[['label']]}"))
        if (!is.null(r[["message"]]) && nzchar(r[["message"]])) {
          cli::cli_text("  {.emph {r$message}}")
        }
      }
    )
  }

  invisible(x)
}


# ==============================================================================
# AS.DATA.FRAME / HEAD / TAIL
# ==============================================================================

#' Convert verify results to a data frame
#'
#' Returns one row per unit test with columns `nr`, `label`, `status`, `outcome`,
#' `pass_rate`, `n_pass`, `n_fail`, `n_error`, `expr_str`, `conditions`, and
#' `message`. Use the `nr` argument to filter to specific tests.
#'
#' @param x A `verify_sdbuildR` object (output of [verify()]).
#' @param row.names `NULL` or a character vector giving row names (optional).
#' @param optional Ignored; present for compatibility.
#' @param status Optional character vector of test statuses to include (e.g., `c("fail", "error")`). Defaults to `c("pass", "fail", "error", "skip")` (include all).
#' @inheritParams unit_tests
#' @param ... Additional arguments (unused).
#'
#' @returns A `data.frame`.
#' @export
#' @concept unitTest
#' @method as.data.frame verify_sdbuildR
#'
#' @examples
#' sfm <- sdbuildR("SIR") |>
#'   unit_test(expr = all(Susceptible >= 0))
#' res <- verify(sfm)
#' as.data.frame(res)
as.data.frame.verify_sdbuildR <- function(x, row.names = NULL, optional = FALSE,
                                           nr = NULL, label = NULL, ignore_case = TRUE,
                                           status = c("pass", "fail", "error", "skip"), ...) {
  results      <- x[["results"]]
  test_indices <- if (is.null(x[["test_indices"]])) seq_along(results) else x[["test_indices"]]

  if (!is.null(nr)) {
    if (!all(nr %in% test_indices)) {
      bad <- nr[!nr %in% test_indices]
      cli::cli_abort(c(
        "Test number{?s} not found in this result: {.val {bad}}.",
        "i" = "Available: {.val {test_indices}}."
      ))
    }
    keep         <- which(test_indices %in% nr)
    results      <- results[keep]
    test_indices <- test_indices[keep]
  }

  if (!is.null(label)) {
    if (!is.character(label) || length(label) == 0L || any(is.na(label)) || any(!nzchar(label)))
      cli::cli_abort(c(
        "Invalid {.arg label}.",
        "x" = "{.arg label} must be a character vector of non-empty regex pattern(s)."
      ))
    result_labels <- vapply(results, function(r) r[["label"]], character(1L))
    hits <- Reduce("|", lapply(label, grepl, x = result_labels, ignore.case = ignore_case))
    keep <- which(hits)
    if (length(keep) == 0) {
      cli::cli_warn(c(
        "No results matched pattern{?s} {.val {label}}.",
        "i" = "Available labels: {.val {result_labels}}."
      ))
    } else {
      results      <- results[keep]
      test_indices <- test_indices[keep]
    }
  }

  if (!is.null(status)) {
    status <- clean_status(status)
    keep <- which(vapply(results, function(r) r[["status"]] %in% status, logical(1L)))
    if (length(keep) == 0) {
      cli::cli_warn("No tests with status {.val {status}} found.")
    } else {
      results      <- results[keep]
      test_indices <- test_indices[keep]
    }
  }

  if (length(results) == 0) {
    df <- data.frame(
      nr = integer(0), label = character(0), status = character(0),
      outcome = logical(0), pass_rate = numeric(0),
      n_pass = integer(0), n_fail = integer(0), n_error = integer(0),
      expr_str = character(0), conditions = character(0), message = character(0),
      stringsAsFactors = FALSE
    )
    return(df)
  }

  .null_chr  <- function(v) if (is.null(v)) "" else v
  .null_real <- function(v) if (is.null(v)) NA_real_ else v
  .null_int  <- function(v) if (is.null(v)) NA_integer_ else v

  df <- data.frame(
    nr        = test_indices,
    label     = vapply(results, function(r) .null_chr(r[["label"]]),   character(1)),
    status    = vapply(results, function(r) .null_chr(r[["status"]]),  character(1)),
    outcome   = vapply(results, function(r) {
      v <- r[["outcome"]]
      if (is.null(v) || (length(v) == 1L && is.na(v))) NA else isTRUE(v)
    }, logical(1)),
    pass_rate = vapply(results, function(r) .null_real(r[["pass_rate"]]), numeric(1)),
    n_pass    = vapply(results, function(r) .null_int(r[["n_pass"]]),  integer(1)),
    n_fail    = vapply(results, function(r) .null_int(r[["n_fail"]]),  integer(1)),
    n_error   = vapply(results, function(r) .null_int(r[["n_error"]]), integer(1)),
    expr_str  = vapply(results, function(r) .null_chr(r[["expr_str"]]), character(1)),
    conditions = vapply(results, function(r) {
      conds <- r[["conditions"]]
      if (is.null(conds) || length(conds) == 0) "" else
        paste(names(conds), unlist(conds), sep = " = ", collapse = ", ")
    }, character(1)),
    message   = vapply(results, function(r) .null_chr(r[["message"]]), character(1)),
    stringsAsFactors = FALSE
  )

  if (!is.null(row.names)) {
    if (length(row.names) != nrow(df)) {
      cli::cli_abort(c(
        "Length mismatch in {.arg row.names}.",
        "x" = "Got {length(row.names)} name{?s} but {nrow(df)} row{?s}."
      ))
    }
    rownames(df) <- row.names
  }

  df
}


#' Print first rows of verify results
#'
#' Wrapper around [head()] that first converts the results to a data frame using
#' [as.data.frame.verify_sdbuildR()].
#'
#' @param x A `verify_sdbuildR` object.
#' @param n Number of rows. Defaults to 6.
#' @param ... Other arguments passed to [as.data.frame.verify_sdbuildR()].
#'
#' @returns A `data.frame`.
#' @export
#' @concept unitTest
#' @importFrom utils head
#' @method head verify_sdbuildR
#'
#' @examples
#' sfm <- sdbuildR("SIR") |>
#'   unit_test(expr = all(Susceptible >= 0))
#' res <- verify(sfm)
#' head(res)
head.verify_sdbuildR <- function(x, n = 6L, ...) {
  df <- as.data.frame(x, ...)
  head(df, n)
}


#' Print last rows of verify results
#'
#' Wrapper around [tail()] that first converts the results to a data frame using
#' [as.data.frame.verify_sdbuildR()].
#'
#' @param x A `verify_sdbuildR` object.
#' @param n Number of rows. Defaults to 6.
#' @param ... Other arguments passed to [as.data.frame.verify_sdbuildR()].
#'
#' @returns A `data.frame`.
#' @export
#' @concept unitTest
#' @importFrom utils tail
#' @method tail verify_sdbuildR
#'
#' @examples
#' sfm <- sdbuildR("SIR") |>
#'   unit_test(expr = all(Susceptible >= 0))
#' res <- verify(sfm)
#' tail(res)
tail.verify_sdbuildR <- function(x, n = 6L, ...) {
  df <- as.data.frame(x, ...)
  tail(df, n)
}


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================


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
    return(list(label = label, expr_str = expr_str, conditions = conditions, status = "skip", error_type = NA, message = "Test is inactive.", outcome = NULL))
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
