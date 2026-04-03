# ==============================================================================
# test-verify.R: Unit tests for the verify / unit_test system
# ==============================================================================

test_that("new_sdbuildR() initialises unit_tests as empty list", {
  sfm <- sdbuildR()
  expect_true("unit_tests" %in% names(sfm))
  expect_true(is.list(sfm[["unit_tests"]]))
  expect_equal(length(sfm[["unit_tests"]]), 0L)
})


# ==============================================================================
# unit_test() — building tests
# ==============================================================================

test_that("unit_test() adds a test to the list", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is non-negative", expr = all(S >= 0))

  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_equal(sfm[["unit_tests"]][[1]][["label"]], "S is non-negative")
  expect_equal(sfm[["unit_tests"]][[1]][["expr_str"]], "all(S >= 0)")
  expect_equal(sfm[["unit_tests"]][[1]][["conditions"]], list())
  expect_true(sfm[["unit_tests"]][[1]][["active"]])

  # Still returns the full sdbuildR object
  expect_s3_class(sfm, "sdbuildR")
  expect_equal(length(sfm[["unit_tests"]]), 1L)
})

test_that("unit_test() auto-generates label from expression", {
  sfm <- make_verifiable_sfm() |>
    unit_test(expr = all(S >= 0))

  # A human-readable label should be generated (not the raw deparse)
  labels <- vapply(sfm[["unit_tests"]], function(t) t[["label"]], character(1))
  expect_equal(length(labels), 1L)
  expect_match(labels[[1]], "S is at least 0")
})


test_that("unit_test() auto-generates unique labels for similar expressions", { 
  sfm <- make_verifiable_sfm() |>
    unit_test(expr = mean(S >= 0)) |>
    unit_test(expr = mean(S) >= 0)

  labels <- vapply(sfm[["unit_tests"]], function(t) t[["label"]], character(1))
  expect_equal(length(labels), 2L)
  expect_match(labels[[1]], "mean of \\(S is at least 0\\)")
  expect_match(labels[[2]], "mean of S is at least 0")
})

test_that("unit_test() upserts: same label replaces existing test", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test A", expr = all(S >= 0)) |>
    unit_test(label = "test A", expr = all(S > 0)) # overwrite

  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_equal(sfm[["unit_tests"]][[1]][["expr_str"]], "all(S > 0)")
})

test_that("unit_test() upserts: same nr replaces existing test", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test A", expr = all(S >= 0)) |>
    unit_test(nr = 1, expr = all(S > 0)) # overwrite

  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_equal(sfm[["unit_tests"]][[1]][["expr_str"]], "all(S > 0)")
})

test_that("unit_test() stores conditions correctly", {
  sfm <- make_verifiable_sfm() |>
    unit_test(
      label = "at zero rate, S is constant",
      expr = all(diff(S) == 0),
      conditions = list(rate = 0)
    )

  conds <- sfm[["unit_tests"]][[1]][["conditions"]]
  expect_equal(conds, list(rate = 0))
})

test_that("unit_test() respects active = FALSE", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "always skipped", expr = FALSE, active = FALSE)

  expect_false(sfm[["unit_tests"]][[1]][["active"]])
  v <- verify(sfm)
  expect_equal(length(v[["results"]]), 1L)
  expect_equal(v[["results"]][[1]][["status"]], "skipped")
})

test_that("unit_test() errors on bad conditions (unknown name)", {
  sfm <- make_verifiable_sfm()
  expect_error(
    unit_test(sfm, label = "x", expr = all(S >= 0), conditions = list(nonexistent = 1)),
    regexp = "not found as stocks or constants"
  )
})

test_that("unit_test() errors on unnamed conditions", {
  sfm <- make_verifiable_sfm()
  expect_error(
    unit_test(sfm, label = "x", expr = all(S >= 0), conditions = list(1)),
    regexp = "must be named"
  )
})

test_that("unit_test() errors when expr references undefined variable", {
  sfm <- make_verifiable_sfm()
  expect_error(
    unit_test(sfm, label = "x", expr = all(Nonexistent >= 0)),
    regexp = "not found in model"
  )
})

test_that("unit_test() allows base-R symbols in expr (e.g. Inf, pi)", {
  sfm <- make_verifiable_sfm()
  expect_no_error(unit_test(sfm, label = "x", expr = all(S < Inf)))
})

test_that("unit_test() errors on invalid syntax when expr is character", {
  sfm <- make_verifiable_sfm()
  expect_error(
    unit_test(sfm, label = "bad syntax", expr = "all(S >= 0 & )"),
    regexp = "invalid R syntax|Failed to parse"
  )
})

test_that("unit_test() errors when character expr contains multiple expressions", {
  sfm <- make_verifiable_sfm()
  expect_error(
    unit_test(sfm, label = "multi expr", expr = "all(S >= 0); all(S < 200)"),
    regexp = "exactly one expression"
  )
})

test_that("unit_test() requires explicit label when auto-generated label collides", {
  sfm <- make_verifiable_sfm() |>
    unit_test(expr = all(S >= 0))

  expect_error(
    unit_test(sfm, expr = all(S >=  0)),
    regexp = "Auto-generated label.*already exists|identical expression already exists"
  )
})

# ==============================================================================
# discard_unit_test()
# ==============================================================================

test_that("discard_unit_test() removes by label", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test A", expr = all(S >= 0)) |>
    unit_test(label = "test B", expr = all(S < 200)) |>
    discard_unit_test("test A")

  remaining_labels <- vapply(sfm[["unit_tests"]], function(t) t[["label"]], character(1))
  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_false("test A" %in% remaining_labels)
  expect_true("test B" %in% remaining_labels)
})

test_that("discard_unit_test() removes by integer index", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test A", expr = all(S >= 0)) |>
    unit_test(label = "test B", expr = all(S < 200)) |>
    discard_unit_test(1L)

  remaining_labels <- vapply(sfm[["unit_tests"]], function(t) t[["label"]], character(1))
  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_false("test A" %in% remaining_labels)
  expect_true("test B" %in% remaining_labels)
})

test_that("discard_unit_test() warns when label not found", {
  sfm <- make_verifiable_sfm()
  expect_error(
    discard_unit_test(sfm, "does not exist"),
    regexp = "not found"
  )
})

test_that("discard_unit_test() warns when index out of range", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "only test", expr = all(S >= 0))
  expect_error(
    discard_unit_test(sfm, 5L),
    regexp = "out of range"
  )
})


# ==============================================================================
# unit_tests() display
# ==============================================================================

test_that("unit_tests() returns unit_tests_sdbuildR with correct count", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "a", expr = all(S >= 0)) |>
    unit_test(label = "b", expr = all(S < 200))

  result <- unit_tests(sfm)
  expect_s3_class(result, "unit_tests_sdbuildR")
  expect_equal(result$n, 2L)
})

test_that("unit_tests() prints without error for empty model", {
  sfm <- make_verifiable_sfm()
  expect_no_error(print(unit_tests(sfm)))
})

test_that("unit_tests() snapshot for defined tests", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is non-negative", expr = all(S >= 0)) |>
    unit_test(label = "inactive test", expr = FALSE, active = FALSE)

  expect_snapshot(print(unit_tests(sfm)))
})


# ==============================================================================
# verify.sdbuildR()
# ==============================================================================

test_that("verify.sdbuildR() returns verify_sdbuildR class", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is non-negative", expr = all(S >= 0))

  result <- silence(verify(sfm))
  expect_s3_class(result, "verify_sdbuildR")
})

test_that("verify.sdbuildR() passes a correct test", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is non-negative", expr = all(S >= 0))

  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "pass")
})

test_that("verify result includes expr_str, conditions, and outcome for passing test", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is non-negative", expr = all(S >= 0))

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]
  
  expect_equal(res_entry[["expr_str"]], "all(S >= 0)")
  expect_equal(res_entry[["conditions"]], list())
  expect_equal(res_entry[["outcome"]], TRUE)
  expect_equal(res_entry[["status"]], "pass")
})

test_that("verify.sdbuildR() fails an incorrect test", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is always zero", expr = all(S == 0))

  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "fail")
})

test_that("verify result includes outcome = FALSE for failing test", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is always zero", expr = all(S == 0))

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]
  
  expect_equal(res_entry[["expr_str"]], "all(S == 0)")
  expect_equal(res_entry[["conditions"]], list())
  expect_equal(res_entry[["outcome"]], FALSE)
  expect_equal(res_entry[["status"]], "fail")
})

test_that("verify.sdbuildR() fails FALSE character expression", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "char false", expr = "all(S == 0)")

  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "fail")
})

test_that("verify.sdbuildR() errors if expression returns numeric scalar", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "numeric output", expr = mean(S > 0.2))

  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "error")
  expect_match(result[["results"]][[1]][["message"]], "logical scalar")
})

test_that("verify result includes outcome and conditions for error status", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "numeric output", expr = mean(S > 0.2))

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]
  
  expect_equal(res_entry[["expr_str"]], "mean(S > 0.2)")
  expect_equal(res_entry[["conditions"]], list())
  expect_null(res_entry[["outcome"]])
  expect_equal(res_entry[["status"]], "error")
})

test_that("verify.sdbuildR() errors if expression returns logical vector", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "vector output", expr = S > 0)

  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "error")
  expect_match(result[["results"]][[1]][["message"]], "logical scalar")
})


test_that("verify.sdbuildR() works with conditions", {
  sfm <- make_verifiable_sfm() |>
    unit_test(
      label      = "at zero rate, S does not decrease",
      expr       = all(diff(S) >= -1e-10), # essentially constant
      conditions = list(rate = 0)
    )

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]
  
  expect_equal(res_entry[["expr_str"]], "all(diff(S) >= -1e-10)")
  expect_equal(res_entry[["conditions"]], list(rate = 0))
  expect_equal(res_entry[["outcome"]], TRUE)
  expect_equal(res_entry[["status"]], "pass")

})

test_that("verify.sdbuildR() errors when no tests defined", {
  sfm <- make_verifiable_sfm()
  expect_error(verify(sfm), regexp = "No unit tests")
})


# --- return_sims parameter ---

test_that("verify() errors when return_sims is not logical", {
  sfm <- make_verifiable_sfm() |> unit_test(expr = all(S >= 0))
  expect_error(
    silence(verify(sfm, return_sims = "yes")),
    regexp = "return_sims"
  )
})

test_that("verify() returns NULL sims by default (return_sims = FALSE)", {
  sfm <- make_verifiable_sfm() |> unit_test(expr = all(S >= 0))
  result <- silence(verify(sfm))
  expect_null(result[["sims"]])
  expect_null(result[["sim_index"]])
})

test_that("verify() returns sims as unnamed list and sim_index as named int vector", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-negative", expr = all(S >= 0))
  result <- silence(verify(sfm, return_sims = TRUE))
  expect_true(is.list(result[["sims"]]))
  expect_null(names(result[["sims"]]))
  expect_equal(length(result[["sims"]]), 1L)
  expect_s3_class(result[["sims"]][[1]], "simulate_sdbuildR")
  expect_equal(result[["sim_index"]], c("S non-negative" = 1L))
})

test_that("verify() deduplicates sims: two tests sharing conditions map to the same index", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-negative", expr = all(S >= 0)) |>
    unit_test(label = "S ends positive", expr = tail(S, 1) > 0) |>
    unit_test(
      label = "S constant at zero rate",
      expr  = all(diff(S) == 0),
      conditions = list(rate = 0)
    )
  result <- silence(verify(sfm, return_sims = TRUE))
  # Two unique condition sets -> two sim objects
  expect_equal(length(result[["sims"]]), 2L)
  # Three tests, each mapped to a sim index
  expect_equal(length(result[["sim_index"]]), 3L)
  expect_setequal(
    names(result[["sim_index"]]),
    c("S non-negative", "S ends positive", "S constant at zero rate")
  )
  # The two baseline tests share the same index
  expect_equal(
    result[["sim_index"]][["S non-negative"]],
    result[["sim_index"]][["S ends positive"]]
  )
  # The condition test has a different index
  expect_false(
    result[["sim_index"]][["S non-negative"]] ==
      result[["sim_index"]][["S constant at zero rate"]]
  )
  # All sims are proper simulate_sdbuildR objects
  for (sim in result[["sims"]]) expect_s3_class(sim, "simulate_sdbuildR")
})


# ==============================================================================
# verify.simulate_sdbuildR()
# ==============================================================================

test_that("verify.simulate_sdbuildR() fails an incorrect test", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S always zero", expr = all(S == 0))

  sim <- simulate(sfm)
  result <- silence(verify(sim))

  expect_equal(result[["results"]][[1]][["status"]], "fail")
})

test_that("verify.simulate_sdbuildR() skips tests with non-matching conditions", {
  sfm <- make_verifiable_sfm() |>
    update(rate, eqn = 0.1) |>
    unit_test(
      label      = "rate=0 test",
      expr       = all(diff(S) >= 0),
      conditions = list(rate = 0) # rate is 0.1 in this model
    )

  sim <- simulate(sfm)
  result <- silence(verify(sim))

  # Should be skipped because the sim was run with rate = 0.1, not 0
  expect_equal(result[["results"]][[1]][["status"]], "skipped")
  expect_match(result[["results"]][[1]][["message"]], "re-simulation")
})

test_that("verify.simulate_sdbuildR() runs tests whose conditions match current model", {
  rate_value <- 0.1 
  sfm <- make_verifiable_sfm() |>
    update(rate, type = "constant", eqn = !!rate_value) |>
    unit_test(
      label      = "rate=0.1 test",
      expr       = all(S >= 0),
      conditions = list(rate = rate_value) # matches the model's actual rate
    )

  sim <- simulate(sfm)
  result <- silence(verify(sim))

  # Conditions match → test should run, not skip
  expect_equal(result[["results"]][[1]][["status"]], "pass")
})

test_that("verify.simulate_sdbuildR() skips inactive tests", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "inactive", expr = FALSE, active = FALSE)

  sim <- simulate(sfm)
  result <- silence(verify(sim))

  expect_equal(result[["results"]][[1]][["status"]], "skipped")
})

test_that("verify result for skipped test includes expr_str, conditions, and outcome = NULL", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "inactive test", expr = all(S >= 0), active = FALSE)

  sim <- simulate(sfm)
  result <- silence(verify(sim))
  res_entry <- result[["results"]][[1]]
  
  expect_equal(res_entry[["expr_str"]], "all(S >= 0)")
  expect_equal(res_entry[["conditions"]], list())
  expect_null(res_entry[["outcome"]])
  expect_equal(res_entry[["status"]], "skipped")
})

test_that("verify result for skipped test includes error_type = NA", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "inactive test", expr = all(S >= 0), active = FALSE)

  sim <- simulate(sfm)
  result <- silence(verify(sim))
  res_entry <- result[["results"]][[1]]
  
  expect_true(is.na(res_entry[["error_type"]]))
  expect_equal(res_entry[["status"]], "skipped")
})

test_that("verify result for passing test includes error_type = NA", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is non-negative", expr = all(S >= 0))

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]
  
  expect_true(is.na(res_entry[["error_type"]]))
  expect_equal(res_entry[["status"]], "pass")
})

test_that("verify result for failing test includes error_type = NA", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is always zero", expr = all(S == 0))

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]
  
  expect_true(is.na(res_entry[["error_type"]]))
  expect_equal(res_entry[["status"]], "fail")
})

test_that("error_type = 'expr_syntax' when expression has parse error", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "syntax error", expr = all(S >= 0))  # Will be modified after
  
  # Manually inject a broken expression that fails parsing
  sfm[["unit_tests"]][[1]][["expr_str"]] <- "all(S >= 0"  # missing closing paren

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]
  
  expect_equal(res_entry[["error_type"]], "expr_syntax")
  expect_equal(res_entry[["status"]], "error")
  expect_match(res_entry[["message"]], "Could not parse")
})

test_that("error_type = 'expr_result' when expression returns numeric instead of logical", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "numeric output", expr = mean(S > 0.2))

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]
  
  expect_equal(res_entry[["error_type"]], "expr_result")
  expect_equal(res_entry[["status"]], "error")
  expect_match(res_entry[["message"]], "logical scalar")
})

test_that("error_type = 'expr_result' when expression returns logical vector instead of scalar", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "vector output", expr = S > 0)

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]
  
  expect_equal(res_entry[["error_type"]], "expr_result")
  expect_equal(res_entry[["status"]], "error")
  expect_match(res_entry[["message"]], "logical scalar")
})

test_that("error_type = 'expr_eval' when expression references undefined variable", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "undefined var", expr = all(S >= 0))  # Will be modified
  
  # Inject an expression that references undefined variable
  sfm[["unit_tests"]][[1]][["expr_str"]] <- "all(undefined_var >= 0)"

  result <- silence(verify(sfm))
  res_entry <- result[["results"]][[1]]
  
  expect_equal(res_entry[["error_type"]], "expr_eval")
  expect_equal(res_entry[["status"]], "error")
  expect_match(res_entry[["message"]], "not found|undefined")
})


test_that("verify().results always includes error_type field", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "pass test", expr = all(S >= 0)) |>
    unit_test(label = "fail test", expr = all(S == 0)) |>
    unit_test(label = "type error", expr = mean(S > 0.2), active = FALSE)

  result <- silence(verify(sfm))
  
  for (res_entry in result[["results"]]) {
    expect_true("error_type" %in% names(res_entry), 
                info = paste("Missing error_type in result:", res_entry[["label"]]))
  }
})

test_that("verify.simulate_sdbuildR() handles testthat expectations", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S starts at 100", expr = expect_equal(S[[1]], 100))

  sim <- simulate(sfm)
  result <- silence(verify(sim))
  res_entry <- result[["results"]][[1]]

  expect_equal(res_entry[["status"]], "pass")
  expect_true(is.na(res_entry[["error_type"]]))
  expect_true(isTRUE(res_entry[["outcome"]]))
})


# ==============================================================================
# Snapshot tests
# ==============================================================================

test_that("print.verify_sdbuildR() snapshot for passing tests", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is non-negative", expr = all(S >= 0)) |>
    unit_test(label = "S starts at 100", expr = expect_equal(S[[1]], 100))

  result <- silence(verify(sfm))
  expect_snapshot(print(result))
})


# ==============================================================================
# discard() cascade — unit test clean-up
# ==============================================================================

test_that("discard() removes unit test that only references the discarded variable", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S only", expr = all(S >= 0))

  expect_warning(expect_warning(expect_warning(
    sfm <- discard(sfm, "S"), "Removed 1 unit test"), 
    "lingering reference"), "lingering reference")
  expect_equal(length(sfm[["unit_tests"]]), 0L)
})

test_that("discard() keeps unit test that references other variables too", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S and rate", expr = all(S >= 0) && rate > 0)

  # drain is not referenced by the test — test kept unchanged
  sfm <- suppressWarnings(discard(sfm, "drain"))
  expect_equal(length(sfm[["unit_tests"]]), 1L)
})

test_that("discard() warns when removing a unit test", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S only", expr = all(S >= 0))

  expect_warning(expect_warning(expect_warning(
    discard(sfm, "S"), "Removed 1 unit test"), "lingering reference"), "lingering reference")
})

test_that("discard() strips removed variable from conditions", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-neg", expr = all(S >= 0), conditions = list(rate = 0))

  expect_warning(expect_warning(sfm <- discard(sfm, "rate"), regexp = "Removed.*rate.*conditions"), "lingering reference")

  # rate removed from conditions; test itself kept (S still exists)
  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_equal(sfm[["unit_tests"]][[1]][["conditions"]], list())
})

test_that("discard() warns about lingering expr reference when multiple vars in expr", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "both vars", expr = all(S >= 0) && drain > 0)

  # Remove drain — test kept (S also referenced) but should warn about lingering ref to drain
  expect_warning(discard(sfm, "drain"), regexp = "\\[1\\].*still references")
})


# ==============================================================================
# test_deps cache
# ==============================================================================

test_that("unit_test() populates assemble$unit_tests$deps", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-neg", expr = all(S >= 0), conditions = list(rate = 0))

  deps <- sfm[["assemble"]][["unit_tests"]][["deps"]]
  expect_false(is.null(deps))
  expect_equal(length(deps), 1L)
  expect_equal(deps[[1]][["expr_refs"]], "S")
  expect_equal(deps[[1]][["cond_refs"]], "rate")
})

test_that("unit_test() updates deps when modifying a test", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test", expr = all(S >= 0)) |>
    unit_test(label = "test", expr = all(drain >= 0)) # overwrite

  deps <- sfm[["assemble"]][["unit_tests"]][["deps"]]
  expect_equal(deps[[1]][["expr_refs"]], "drain")
})

test_that("discard_unit_test() invalidates test deps cache", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test A", expr = all(S >= 0)) |>
    discard_unit_test("test A")

  # Cache should be invalidated (NULL) after removal
  expect_null(sfm[["assemble"]][["unit_tests"]][["deps"]])
})

test_that("get_test_deps() lazily recomputes when cache is NULL", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S non-neg", expr = all(S >= 0))

  # Manually invalidate
  sfm[["assemble"]][["unit_tests"]][["deps"]] <- NULL

  td <- get_test_deps(sfm)
  expect_false(is.null(td[["deps"]]))
  expect_equal(td[["deps"]][[1]][["expr_refs"]], "S")
  # Object should now have cached deps
  expect_false(is.null(td[["object"]][["assemble"]][["unit_tests"]][["deps"]]))
})


# ==============================================================================
# unit_test() — additional edge cases
# ==============================================================================

test_that("unit_test() errors on duplicate expression", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test A", expr = all(S >= 0))

  expect_error(
    unit_test(sfm, label = "test B", expr = all(S >= 0)),
    regexp = "identical expression already exists"
  )
})

test_that("unit_test() warns when nr is not sequential (gap)", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "test A", expr = all(S >= 0))

  # Nr 5 is far beyond next_nr = 2
  expect_warning(
    unit_test(sfm, nr = 5, label = "test B", expr = all(S > 0)),
    regexp = "will be set to the existing number of tests \\+ 1"
  )
})


# ==============================================================================
# discard() — additional edge cases
# ==============================================================================

test_that("discard() handles variable referenced in both expr and conditions", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "rate test", expr = all(rate > -1), conditions = list(rate = 0))

  # rate is in both expr_refs and cond_refs — test should be removed entirely
  expect_warning(expect_warning(sfm <- discard(sfm, "rate"), "Removed 1 unit test"), "lingering reference")
  expect_equal(length(sfm[["unit_tests"]]), 0L)
})

test_that("discard() with multiple variables removes affected tests and strips conditions", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S test", expr = all(S >= 0)) |>
    unit_test(label = "rate test", expr = all(drain > 0), conditions = list(rate = 0))

  # Discard S and rate — first test removed (S-only), second test loses rate condition
  sfm <- suppressWarnings(discard(sfm, c("S", "rate")))

  # "S test" removed, "rate test" kept (drain still exists) but conditions stripped
  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_equal(sfm[["unit_tests"]][[1]][["label"]], "rate test")
  expect_equal(sfm[["unit_tests"]][[1]][["conditions"]], list())
})


# ==============================================================================
# get_test_deps() — edge cases
# ==============================================================================

test_that("get_test_deps() returns empty list for model with no unit tests", {
  sfm <- make_verifiable_sfm()
  td <- get_test_deps(sfm)
  expect_equal(td[["deps"]], list())
})


# ==============================================================================
# discard_unit_test() — multiple indices/labels
# ==============================================================================

test_that("discard_unit_test() removes multiple tests by index", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "A", expr = all(S >= 0)) |>
    unit_test(label = "B", expr = all(S > 0)) |>
    unit_test(label = "C", expr = all(S < 200)) |>
    discard_unit_test(c(1L, 3L))

  expect_equal(length(sfm[["unit_tests"]]), 1L)
  expect_equal(sfm[["unit_tests"]][[1]][["label"]], "B")
})


# ==============================================================================
# verify — only_stocks auto-detection
# ==============================================================================

test_that("verify.sdbuildR() passes test referencing a flow (non-stock)", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "drain is positive", expr = all(drain > 0))

  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "pass")
})

test_that("verify.sdbuildR() passes test referencing a constant (non-stock)", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "rate is 0.1", expr = expect_equal(rate[[1]], 0.1))

  result <- silence(verify(sfm))
  expect_equal(result[["results"]][[1]][["status"]], "pass")
})

test_that("verify.sdbuildR() passes mixed stock and non-stock tests", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "S is non-negative", expr = all(S >= 0)) |>
    unit_test(label = "drain is positive", expr = all(drain > 0))

  result <- silence(verify(sfm))
  statuses <- vapply(result[["results"]], function(r) r[["status"]], character(1))
  expect_equal(statuses, c("pass", "pass"))
})
