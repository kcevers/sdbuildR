# Tests for interpret_unit_test.R
# Covers: op_precedence(), needs_parens(), find_named_arg(), interpret()

# Helper: parse a string to a language object
expr_ <- function(text) parse(text = text)[[1]]


# ============================================================================
# op_precedence()
# ============================================================================

test_that("op_precedence: logical and comparison operator levels are correct", {
  expect_equal(op_precedence("||"), 1L)
  expect_equal(op_precedence("|"),  2L)
  expect_equal(op_precedence("&&"), 3L)
  expect_equal(op_precedence("&"),  4L)
  expect_equal(op_precedence("!"),  5L)
  expect_equal(op_precedence(">"),  6L)
  expect_equal(op_precedence(">="), 6L)
  expect_equal(op_precedence("<"),  6L)
  expect_equal(op_precedence("<="), 6L)
  expect_equal(op_precedence("=="), 6L)
  expect_equal(op_precedence("!="), 6L)
})

test_that("op_precedence: arithmetic operator levels are correct", {
  expect_equal(op_precedence("+"),  7L)
  expect_equal(op_precedence("-"),  7L)
  expect_equal(op_precedence("*"),  8L)
  expect_equal(op_precedence("/"),  8L)
  expect_equal(op_precedence("^"),  9L)
})

test_that("op_precedence: unknown operator returns -1", {
  expect_equal(op_precedence("@"),   -1L)
  expect_equal(op_precedence("??"),  -1L)
  expect_equal(op_precedence(NULL),  -1L)
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
  expect_true(needs_parens("+", "*"))   # (a+b) inside a*b
  expect_true(needs_parens("+", "^"))   # (a+b) inside a^b
  expect_true(needs_parens("*", "^"))   # (a*b) inside a^b
})

test_that("needs_parens: higher-prec inner inside lower-prec outer → FALSE", {
  expect_false(needs_parens("*", "+"))   # a*b inside a+b → no wrapping
  expect_false(needs_parens("^", "*"))
  expect_false(needs_parens("^", "+"))
})

test_that("needs_parens: same operator, same precedence → FALSE", {
  expect_false(needs_parens("+", "+"))
  expect_false(needs_parens("*", "*"))
  expect_false(needs_parens("&&", "&&"))
})

test_that("needs_parens: same precedence, different operator → TRUE", {
  expect_true(needs_parens("+", "-"))   # subtraction is not associative
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
  expect_equal(find_named_arg(args, "a"),         1)
  expect_equal(find_named_arg(args, "b"),         2)
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
  expect_equal(interpret(expr_("42")),   "42")
  expect_equal(interpret(expr_("3.14")), "3.14")
  expect_equal(interpret(expr_("0")),    "0")
})

test_that("interpret: unary minus produces negative string", {
  expect_equal(interpret(expr_("-1")), "-1")
  expect_equal(interpret(expr_("-5.5")), "-5.5")
})

test_that("interpret: logical literals are lowercase", {
  expect_equal(interpret(expr_("TRUE")),  "true")
  expect_equal(interpret(expr_("FALSE")), "false")
})

test_that("interpret: symbol returns variable name unchanged", {
  expect_equal(interpret(expr_("x")),      "x")
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
  expect_false(grepl("\\(b", result))  # b should NOT be wrapped
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
  expect_equal(interpret(expr_("x > 0")),  "x is greater than 0")
  expect_equal(interpret(expr_("x < 0")),  "x is less than 0")
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
  expect_equal(interpret(expr_("sum(x)")),  "the sum of x")
  expect_equal(interpret(expr_("max(x)")),  "the maximum of x")
  expect_equal(interpret(expr_("min(x)")),  "the minimum of x")
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
  direct  <- interpret(expr_("x == 1"))
  wrapped <- interpret(expr_("expect_equal(x, 1)"))
  expect_false(identical(direct, wrapped))
  expect_match(direct,  "is equal to")
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
