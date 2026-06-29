# Tests for interpret_unit_test.R
# Covers: op_precedence(), needs_parens(), find_named_arg(), interpret()

# Helper: parse a string to a language object
expr_ <- function(text) parse(text = text)[[1]]


# ============================================================================
# op_precedence()
# ============================================================================

test_that("op_precedence: logical and comparison operator levels are correct", {
  expect_equal(op_precedence("||"), 1L)
  expect_equal(op_precedence("|"), 2L)
  expect_equal(op_precedence("&&"), 3L)
  expect_equal(op_precedence("&"), 4L)
  expect_equal(op_precedence("!"), 5L)
  expect_equal(op_precedence(">"), 6L)
  expect_equal(op_precedence(">="), 6L)
  expect_equal(op_precedence("<"), 6L)
  expect_equal(op_precedence("<="), 6L)
  expect_equal(op_precedence("=="), 6L)
  expect_equal(op_precedence("!="), 6L)
})

test_that("op_precedence: arithmetic operator levels are correct", {
  expect_equal(op_precedence("+"), 7L)
  expect_equal(op_precedence("-"), 7L)
  expect_equal(op_precedence("*"), 8L)
  expect_equal(op_precedence("/"), 8L)
  expect_equal(op_precedence("^"), 9L)
})

test_that("op_precedence: unknown operator returns -1", {
  expect_equal(op_precedence("@"), -1L)
  expect_equal(op_precedence("??"), -1L)
  expect_equal(op_precedence(NULL), -1L)
})

test_that("op_precedence: multiplication strictly outranks addition", {
  expect_gt(op_precedence("*"), op_precedence("+"))
})

test_that("op_precedence: power strictly outranks multiplication", {
  expect_gt(op_precedence("^"), op_precedence("*"))
})

test_that("op_precedence: comparisons strictly outrank addition", {
  expect_gt(op_precedence("+"), op_precedence("=="))
})


# ============================================================================
# needs_parens()
# ============================================================================

test_that("needs_parens: lower-prec inner inside higher-prec outer → TRUE", {
  expect_true(needs_parens("+", "*")) # (a+b) inside a*b
  expect_true(needs_parens("+", "^")) # (a+b) inside a^b
  expect_true(needs_parens("*", "^")) # (a*b) inside a^b
})

test_that("needs_parens: higher-prec inner inside lower-prec outer → FALSE", {
  expect_false(needs_parens("*", "+")) # a*b inside a+b → no wrapping
  expect_false(needs_parens("^", "*"))
  expect_false(needs_parens("^", "+"))
})

test_that("needs_parens: same operator, same precedence → FALSE", {
  expect_false(needs_parens("+", "+"))
  expect_false(needs_parens("*", "*"))
  expect_false(needs_parens("&&", "&&"))
})

test_that("needs_parens: same precedence, different operator → TRUE", {
  expect_true(needs_parens("+", "-")) # subtraction is not associative
  expect_true(needs_parens("-", "+"))
})

test_that("needs_parens: NULL outer → FALSE (no parent context)", {
  expect_false(needs_parens("+", NULL))
  expect_false(needs_parens("*", NULL))
})

test_that("needs_parens: unknown operator → FALSE (safe, not enough info)", {
  expect_false(needs_parens("@", "+"))
  expect_false(needs_parens("+", "@"))
})


# ============================================================================
# find_named_arg()
# ============================================================================

test_that("find_named_arg: returns correct value by name", {
  args <- list(a = 1, tolerance = 1e-6, b = 2)
  expect_equal(find_named_arg(args, "tolerance"), 1e-6)
  expect_equal(find_named_arg(args, "a"), 1)
  expect_equal(find_named_arg(args, "b"), 2)
})

test_that("find_named_arg: returns NULL for absent name", {
  args <- list(a = 1, b = 2)
  expect_null(find_named_arg(args, "missing"))
  expect_null(find_named_arg(args, "tolerance"))
})

test_that("find_named_arg: works when list is fully unnamed → returns NULL", {
  args <- list(1, 2, 3)
  expect_null(find_named_arg(args, "a"))
})


# ============================================================================
# interpret() — atomic values
# ============================================================================

test_that("interpret: numeric literals round-trip as strings", {
  expect_equal(interpret(expr_("42")), "42")
  expect_equal(interpret(expr_("3.14")), "3.14")
  expect_equal(interpret(expr_("0")), "0")
})

test_that("interpret: unary minus produces negative string", {
  expect_equal(interpret(expr_("-1")), "-1")
  expect_equal(interpret(expr_("-5.5")), "-5.5")
})

test_that("interpret: logical literals are lowercase", {
  expect_equal(interpret(expr_("TRUE")), "true")
  expect_equal(interpret(expr_("FALSE")), "false")
})

test_that("interpret: symbol returns variable name unchanged", {
  expect_equal(interpret(expr_("x")), "x")
  expect_equal(interpret(expr_("my_var")), "my_var")
})


# ============================================================================
# interpret() — arithmetic operators → English words
# ============================================================================

test_that("interpret: binary arithmetic operators use English words", {
  expect_equal(interpret(expr_("a + b")), "a plus b")
  expect_equal(interpret(expr_("a - b")), "a minus b")
  expect_equal(interpret(expr_("a * b")), "a times b")
  expect_equal(interpret(expr_("a / b")), "a divided by b")
})

test_that("interpret: a + b * c needs no extra parens (precedence correct)", {
  result <- interpret(expr_("a + b * c"))
  expect_equal(result, "a plus b times c")
  expect_false(grepl("\\(b", result)) # b should NOT be wrapped
})

test_that("interpret: (a + b) * c preserves parens around low-prec sub-expr", {
  result <- interpret(expr_("(a + b) * c"))
  expect_equal(result, "(a plus b) times c")
  expect_match(result, "\\(.*plus.*\\)")
})

test_that("interpret: exponentiation — squared / cubed shortcuts", {
  expect_equal(interpret(expr_("x^2")), "x squared")
  expect_equal(interpret(expr_("x^3")), "x cubed")
})

test_that("interpret: exponentiation with general power uses 'raised to the power of'", {
  result <- interpret(expr_("x^4"))
  expect_match(result, "raised to the power of")
  expect_match(result, "4")
})


# ============================================================================
# interpret() — comparison operators → readable English
# ============================================================================

test_that("interpret: comparison operators produce correct English phrases", {
  expect_equal(interpret(expr_("x > 0")), "x is greater than 0")
  expect_equal(interpret(expr_("x < 0")), "x is less than 0")
  expect_equal(interpret(expr_("x >= 0")), "x is at least 0")
  expect_equal(interpret(expr_("x <= 0")), "x is at most 0")
  expect_equal(interpret(expr_("x == y")), "x is equal to y")
  expect_equal(interpret(expr_("x != y")), "x is not equal to y")
})

test_that("interpret: >= says 'at least', not 'greater' + 'equal'", {
  result <- interpret(expr_("x >= 5"))
  expect_match(result, "at least")
  expect_false(grepl("greater", result))
})

test_that("interpret: <= says 'at most', not 'less' + 'equal'", {
  result <- interpret(expr_("y <= 3"))
  expect_match(result, "at most")
  expect_false(grepl("less", result))
})


# ============================================================================
# interpret() — quantifiers: all() and any()
# ============================================================================

test_that("interpret: all() appends '(for all values)' suffix", {
  result <- interpret(expr_("all(x > 0)"))
  expect_match(result, "for all values")
})

test_that("interpret: any() appends '(for at least one value)' suffix, NOT 'any'", {
  result <- interpret(expr_("any(x < 0)"))
  expect_match(result, "for at least one value")
  # The word "any" does NOT appear literally in the output
  expect_false(grepl("\\bany\\b", result))
})

test_that("interpret: all() and any() include the inner expression", {
  result_all <- interpret(expr_("all(x > 0)"))
  result_any <- interpret(expr_("any(x < 0)"))
  expect_match(result_all, "is greater than 0")
  expect_match(result_any, "is less than 0")
})


# ============================================================================
# interpret() — summary statistics
# ============================================================================

test_that("interpret: summary statistics use 'the <stat> of' form", {
  expect_equal(interpret(expr_("mean(x)")), "the mean of x")
  expect_equal(interpret(expr_("sum(x)")), "the sum of x")
  expect_equal(interpret(expr_("max(x)")), "the maximum of x")
  expect_equal(interpret(expr_("min(x)")), "the minimum of x")
})


# ============================================================================
# interpret() — testthat expectations (WRAPPING behaviour)
# ============================================================================

test_that("interpret: expect_equal(x, 1) → 'expect that x equals 1'", {
  result <- interpret(expr_("expect_equal(x, 1)"))
  expect_match(result, "^expect that")
  expect_match(result, "equals")
  expect_match(result, "1")
})

test_that("interpret: expect_equal WRAPS — output differs from interpret(x == 1)", {
  direct <- interpret(expr_("x == 1"))
  wrapped <- interpret(expr_("expect_equal(x, 1)"))
  expect_false(identical(direct, wrapped))
  expect_match(direct, "is equal to")
  expect_match(wrapped, "equals")
})

test_that("interpret: expect_true(x > 0) → 'expect that ... is true' (wraps, not strips)", {
  result <- interpret(expr_("expect_true(x > 0)"))
  expect_match(result, "^expect that")
  expect_match(result, "is true$")
  expect_match(result, "is greater than 0")
})

test_that("interpret: expect_equal with tolerance mentions the tolerance value", {
  result <- interpret(expr_("expect_equal(x, y, tolerance = 1e-3)"))
  expect_match(result, "^expect that")
  expect_match(result, "within tolerance")
  expect_match(result, "0\\.001")
})

test_that("interpret: expect_error(f()) → 'expect that f() throws an error'", {
  result <- interpret(expr_("expect_error(f())"))
  expect_match(result, "^expect that")
  expect_match(result, "throws an error")
  expect_match(result, "f\\(\\)")
})

test_that("interpret: expect_false(cond) → 'expect that ... is false'", {
  result <- interpret(expr_("expect_false(x > 0)"))
  expect_match(result, "^expect that")
  expect_match(result, "is false$")
})


# ============================================================================
# interpret() - additional user-facing expression descriptions
# ============================================================================

test_that("interpret: string literals and vectors are readable", {
  expect_equal(interpret(expr_("'stable'")), '"stable"')
  expect_equal(interpret(expr_('c("low", "high")')), '["low", "high"]')
})

test_that("interpret: logical negation keeps the inner comparison", {
  result <- interpret(expr_("!(x > 0)"))
  expect_equal(result, "it is not the case that x is greater than 0")
})

test_that("interpret: summary statistics wrap logical inputs", {
  expect_equal(interpret(expr_("mean(x > 0)")), "the mean of (x is greater than 0)")
  expect_equal(interpret(expr_("sum(x <= target)")), "the sum of (x is at most target)")
})

test_that("interpret: transformations, logs and rounding describe parameters", {
  expect_equal(interpret(expr_("sqrt(abs(x))")), "the square root of the absolute value of x")
  expect_equal(interpret(expr_("log(x)")), "the natural log of x")
  expect_equal(interpret(expr_("log(x, 10)")), "the log (base 10) of x")
  expect_equal(interpret(expr_("round(score)")), "score rounded")
  expect_equal(interpret(expr_("round(score, 2)")), "score rounded to 2 decimal places")
})

test_that("interpret: sequence, difference, head and tail helpers describe slices", {
  expect_equal(interpret(expr_("1:5")), "1 to 5")
  expect_equal(interpret(expr_("diff(x)")), "the successive differences of x")
  expect_equal(interpret(expr_("diff(x, lag = 2)")), "the successive differences (lag 2) of x")
  expect_equal(interpret(expr_("head(x, 1)")), "the first 1 value of x")
  expect_equal(interpret(expr_("tail(x)")), "the last 6 values of x")
})

test_that("interpret: indexing describes initial, final, range and filtered values", {
  expect_equal(interpret(expr_("x[1]")), "the initial value of x")
  expect_equal(interpret(expr_("x[3]")), "x at index 3")
  expect_equal(interpret(expr_("x[2:5]")), "x from index 2 to 5")
  expect_equal(interpret(expr_("x[length(x)]")), "the final value of x")
  expect_equal(interpret(expr_("x[x > 0]")), "x where x is greater than 0")
})

test_that("interpret: extraction, approximate equality and conditionals are explicit", {
  expect_equal(interpret(expr_("model$value")), "model's value")
  expect_equal(interpret(expr_("near(x, y)")), "x is approximately equal to y")
  expect_equal(interpret(expr_("near(x, y, 0.01)")), "x is approximately equal to y (within tolerance 0.01)")
  expect_equal(interpret(expr_("ifelse(x > 0, 'gain', 'loss')")), 'if x is greater than 0 then "gain" otherwise "loss"')
})

test_that("interpret: extrema and identical describe common checks", {
  expect_equal(interpret(expr_("which.max(x)")), "the index of the peak of x")
  expect_equal(interpret(expr_("which.min(x)")), "the index of the trough of x")
  expect_equal(interpret(expr_("identical(sort(x), x)")), "x is sorted in ascending order")
  expect_equal(interpret(expr_("identical(x, y)")), "x is identical to y")
})

test_that("interpret: additional testthat expectations keep their intent", {
  expect_equal(interpret(expr_("expect_named(x)")), "expect that x is named")
  expect_equal(interpret(expr_('expect_named(x, c("a", "b"))')), 'expect that x has names ["a", "b"]')
  expect_equal(interpret(expr_('expect_type(x, "double")')), 'expect that x is of type "double"')
  expect_equal(interpret(expr_('expect_s3_class(x, "stockflow")')), 'expect that x is an S3 object of class "stockflow"')
  expect_equal(interpret(expr_("expect_null(x)")), "expect that x is NULL")
  expect_equal(interpret(expr_('expect_match(x, "abc")')), 'expect that x matches the pattern "abc"')
  expect_equal(interpret(expr_("expect_silent(run_model())")), "expect that run_model() runs without messages, warnings, or errors")
})

test_that("interpret: vector expectations report optional type and size", {
  expect_equal(
    interpret(expr_("expect_vector(x, ptype = double(), size = 3)")),
    "expect that x is a vector of type double() with size 3"
  )
})
