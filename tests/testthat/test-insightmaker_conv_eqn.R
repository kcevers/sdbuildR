test_that("convert_builtin_functions_IM basic functions", {
  sfm <- stockflow("predator_prey")
  var_names <- get_model_var(sfm)
  name <- "test"
  type <- "stock"

  # Constant
  eqn <- "0.1"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expected <- "0.1"
  expect_equal(result, expected)

  # Case insensitivity (Insight Maker is case-insensitive)
  eqn <- "MIN(3,4,5)"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expected <- "min(c(3, 4, 5))"
  expect_equal(result, expected)

  # Lowercase function
  eqn <- "min(3,4,5)"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expected <- "min(c(3, 4, 5))"
  expect_equal(result, expected)

  # Max function
  eqn <- "max(10, 20, 30)"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expected <- "max(c(10, 20, 30))"
  expect_equal(result, expected)

  # Nesting
  eqn <- "MIN(max(3,4,5), median(7,8,9))"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expected <- "min(c(max(c(3, 4, 5)), median(c(7, 8, 9))))"
  expect_equal(result, expected)

  # Random number generation
  eqn <- "Rand + Rand"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expected <- "runif(1) + runif(1)"
  expect_equal(result, expected)
})


test_that("convert_builtin_functions_IM comments", {
  eqn <- "# A Test\nmin(3,4,5)\n# Another Test\nmax(3,4,5)"
  result <- remove_comments(eqn)
  expect_equal(result$eqn, "min(3,4,5)\n\nmax(3,4,5)")
  expect_equal(result$doc, "# A Test# Another Test")

  # Test single comment
  eqn <- "# Comment\nmin(1,2)"
  result <- remove_comments(eqn)
  expect_equal(result$eqn, "min(1,2)")
  expect_equal(result$doc, "# Comment")

  # Test no comments
  eqn <- "min(1,2)"
  result <- remove_comments(eqn)
  expect_equal(result$eqn, "min(1,2)")
  expect_equal(result$doc, "")
})


test_that("convert_equations_IM basic math functions", {
  sfm <- stockflow("predator_prey")
  var_names <- get_model_var(sfm)
  name <- var_names[1]
  type <- "aux"

  # Arithmetic
  result <- convert_equations_IM(type, name, "1 + 2", var_names)$eqn
  expect_true(grepl("\\+", result))

  # Multiplication
  result <- convert_equations_IM(type, name, "3 * 4", var_names)$eqn
  expect_true(grepl("\\*", result))

  # Division
  result <- convert_equations_IM(type, name, "10 / 2", var_names)$eqn
  expect_true(grepl("/", result))

  # Exponentiation
  result <- convert_equations_IM(type, name, "2^3", var_names)$eqn
  expect_true(grepl("\\^", result))
})


test_that("convert_equations_IM conditional statements", {
  sfm <- stockflow("predator_prey")
  var_names <- get_model_var(sfm)
  name <- var_names[1]
  type <- "aux"

  # Simple if statement
  eqn <- "if(x > 0) { 1 } else { 0 }"
  result <- convert_equations_IM(type, name, eqn, var_names)$eqn
  expect_true(grepl("ifelse", result) || grepl("if", result))

  # Nested if
  eqn <- "if(x > 0) { if(x > 10) { 2 } else { 1 } } else { 0 }"
  result <- convert_equations_IM(type, name, eqn, var_names)$eqn
  expect_true(nchar(result) > 0)
})


test_that("convert_equations_IM empty and NULL cases", {
  sfm <- stockflow("predator_prey")
  var_names <- get_model_var(sfm)
  name <- var_names[1]
  type <- "aux"

  # Empty string
  result <- convert_equations_IM(type, name, "", var_names)$eqn
  expect_equal(result, "")

  # Null
  result <- convert_equations_IM(type, name, NULL, var_names)$eqn
  expect_equal(result, "")

  # Zero
  result <- convert_equations_IM(type, name, "0", var_names)$eqn
  expect_equal(result, "0")
})


test_that("convert_all_statements() preserves strings and adds missing else branches", {
  var_names <- c("x", "y")
  eqn <- "If x > 0 Then\n  y <- \"comma, only\"\nEnd If"
  result <- convert_all_statements(eqn, var_names)
  expect_match(result, '"comma, only"', fixed = TRUE)
  expect_match(result, "else")
  expect_match(result, "0")
})


test_that("convert_builtin_functions_IM string functions", {
  sfm <- stockflow("predator_prey")
  var_names <- get_model_var(sfm)
  name <- "test"
  type <- "aux"

  # Length
  eqn <- "Length('hello')"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expect_equal(result, "length_IM('hello')")

  # Uppercase
  eqn <- "UpperCase('hello')"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expect_equal(result, "toupper('hello')")

  # Lowercase
  eqn <- "LowerCase('HELLO')"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expect_equal(result, "tolower('HELLO')")
})


test_that("convert_builtin_functions_IM statistical functions", {
  sfm <- stockflow("predator_prey")
  var_names <- get_model_var(sfm)
  name <- "test"
  type <- "aux"

  # Sum
  eqn <- "Sum(1, 2, 3)"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expect_equal(result, "sum(c(1, 2, 3))")

  # Mean
  eqn <- "Mean(1, 2, 3)"
  result <- convert_builtin_functions_IM(type, name, eqn, var_names)$eqn
  expect_equal(result, "mean(c(1, 2, 3))")
})

# Extra tests for insightmaker_conv_eqn.R
# Covers: replace_comments(), remove_comments(), replace_op_IM(),
#         replace_colon(), curly_to_vector_brackets(),
#         conv_step(), conv_pulse(), conv_ramp(), conv_lookup()


# ============================================================================
# replace_comments()  — converts IM comment syntax (// /*) to R comment syntax (#)
# ============================================================================

test_that("replace_comments: converts // to # (line comment)", {
  result <- replace_comments("x + y // this is a comment")
  expect_match(result, "#")
  expect_false(grepl("//", result, fixed = TRUE))
})

test_that("replace_comments: converts /* to # and */ to newline (block comment)", {
  result <- replace_comments("x /* block comment */ + y")
  expect_false(grepl("/*", result, fixed = TRUE))
  expect_false(grepl("*/", result, fixed = TRUE))
  # The comment chars have been replaced; equation portion should survive
  expect_match(result, "x")
  expect_match(result, "y")
})

test_that("replace_comments: equation without IM comment chars is returned unchanged", {
  original <- "x + y * z"
  result <- replace_comments(original)
  expect_equal(result, original)
})


# ============================================================================
# remove_comments()  — strips R-style # comments and returns list(eqn, doc)
# ============================================================================

test_that("remove_comments: strips # line comment, returns doc text", {
  result <- remove_comments("x + y # this is a comment")
  expect_false(grepl("#", result$eqn))
  expect_match(result$doc, "#")
})

test_that("remove_comments: equation portion is preserved after stripping", {
  result <- remove_comments("x + y # comment")
  expect_match(trimws(result$eqn), "x \\+ y")
})

test_that("remove_comments: equation without # returns unchanged eqn and empty doc", {
  result <- remove_comments("x + y * z")
  expect_match(trimws(result$eqn), "x \\+ y \\* z")
  expect_equal(nchar(trimws(result$doc)), 0)
})

test_that("remove_comments: pure comment line → eqn becomes '0'", {
  result <- remove_comments("# only a comment")
  expect_equal(result$eqn, "0")
})


# ============================================================================
# replace_op_IM()  — logical operators, booleans, mod, <>
# ============================================================================

test_that("replace_op_IM: AND (case-insensitive) → &", {
  expect_match(replace_op_IM("x AND y", character(0)), "&")
  expect_match(replace_op_IM("x and y", character(0)), "&")
  expect_match(replace_op_IM("x And y", character(0)), "&")
})

test_that("replace_op_IM: OR (case-insensitive) → |", {
  expect_match(replace_op_IM("x OR y", character(0)), "\\|")
  expect_match(replace_op_IM("x or y", character(0)), "\\|")
})

test_that("replace_op_IM: NOT (case-insensitive) → !", {
  expect_match(replace_op_IM("NOT x", character(0)), "!")
  expect_match(replace_op_IM("not x", character(0)), "!")
})

test_that("replace_op_IM: true/false (case-insensitive) → TRUE/FALSE", {
  expect_match(replace_op_IM("true", character(0)), "TRUE")
  expect_match(replace_op_IM("false", character(0)), "FALSE")
  expect_match(replace_op_IM("True", character(0)), "TRUE")
  expect_match(replace_op_IM("FALSE", character(0)), "FALSE")
})

test_that("replace_op_IM: <> → !=", {
  result <- replace_op_IM("x <> y", character(0))
  expect_match(result, "!=")
  expect_false(grepl("<>", result, fixed = TRUE))
})

test_that("replace_op_IM: mod → %REM%", {
  result <- replace_op_IM("x mod y", character(0))
  expect_match(result, "%REM%")
  expect_false(grepl("\\bmod\\b", result))
})

test_that("replace_op_IM: standalone = (assignment) → ==", {
  result <- replace_op_IM("x = y", character(0))
  expect_match(result, "==")
})


# ============================================================================
# replace_colon()  — three-part step syntax vs plain range
# ============================================================================

test_that("replace_colon: start:step:end → seq(start, end, by = step)", {
  result <- replace_colon("1:2:10", character(0))
  expect_match(result, "seq")
  expect_match(result, "by\\s*=\\s*2")
  expect_false(grepl("1:2:10", result, fixed = TRUE))
})

test_that("replace_colon: plain a:b range is left intact", {
  result <- replace_colon("1:5", character(0))
  expect_equal(trimws(result), "1:5")
})

test_that("replace_colon: negative step in three-part form converts correctly", {
  result <- replace_colon("10:-1:0", character(0))
  expect_match(result, "seq")
  expect_match(result, "by\\s*=\\s*-1")
})


# ============================================================================
# curly_to_vector_brackets()  — {} disambiguation
# ============================================================================

test_that("curly_to_vector_brackets: standalone {} becomes c()", {
  result <- curly_to_vector_brackets("{1, 2, 3}", character(0))
  expect_match(result, "^c\\(1,\\s*2,\\s*3\\)$")
})

test_that("curly_to_vector_brackets: {} immediately after identifier becomes []", {
  result <- curly_to_vector_brackets("x{1}", character(0))
  expect_equal(result, "x[1]")
})

test_that("curly_to_vector_brackets: no curly braces → string returned unchanged", {
  original <- "x + y"
  result <- curly_to_vector_brackets(original, character(0))
  expect_equal(result, original)
})


# ============================================================================
# conv_step()  — Step() → step() auxiliary variable
# ============================================================================

test_that("conv_step: replacement references the new auxiliary variable name", {
  result <- conv_step(func = "step", arg = c("5", "2"), match_idx = "", name = "myvar")
  expect_match(result$replacement, "myvar_step")
  # Replacement uses the time variable
  expect_match(result$replacement, "\\(t\\)")
})

test_that("conv_step: add_var has correct type, name, and equation", {
  result <- conv_step(func = "step", arg = c("5", "2"), match_idx = "", name = "myvar")
  expect_equal(result$add_var[["name"]], "myvar_step")
  expect_equal(result$add_var[["type"]], "aux")
  expect_match(result$add_var[["eqn"]], "step\\(times")
  expect_match(result$add_var[["eqn"]], "start\\s*=\\s*5")
  expect_match(result$add_var[["eqn"]], "height\\s*=\\s*2")
})

test_that("conv_step: second occurrence gets a numbered suffix", {
  r1 <- conv_step(func = "step", arg = c("1", "1"), match_idx = "", name = "v")
  r2 <- conv_step(func = "step", arg = c("2", "1"), match_idx = "2", name = "v")
  expect_false(grepl("2", r1$replacement, fixed = TRUE))
  expect_match(r2$replacement, "2")
})


# ============================================================================
# conv_pulse()  — Pulse() → pulse() auxiliary variable
# ============================================================================

test_that("conv_pulse: add_var contains pulse() call with correct start, height, width", {
  result <- conv_pulse(
    func      = "pulse",
    arg       = c("3", "1", "2"),
    match_idx = "",
    name      = "myvar"
  )
  expect_equal(result$add_var[["type"]], "aux")
  expect_match(result$add_var[["eqn"]], "pulse\\(times")
  expect_match(result$add_var[["eqn"]], "start\\s*=\\s*3")
  expect_match(result$add_var[["eqn"]], "height\\s*=\\s*1")
  expect_match(result$add_var[["eqn"]], "width\\s*=\\s*2")
})

test_that("conv_pulse: replacement uses square-bracket primitive syntax", {
  result <- conv_pulse(func = "pulse", arg = c("3", "1", "2"), match_idx = "", name = "myvar")
  expect_match(result$replacement, "^\\[myvar_pulse\\]\\(t\\)$")
})

test_that("conv_pulse: zero width is replaced by timestep variable", {
  result <- conv_pulse(func = "pulse", arg = c("0", "1", "0"), match_idx = "", name = "v")
  # width = 0 → replaced by P[["timestep_name"]] = "dt"
  expect_match(result$add_var[["eqn"]], "width\\s*=\\s*dt")
})


# ============================================================================
# conv_ramp()  — Ramp() → ramp() auxiliary variable
# ============================================================================

test_that("conv_ramp: add_var contains ramp() call with start, finish, height", {
  result <- conv_ramp(
    func      = "ramp",
    arg       = c("0", "10", "5"),
    match_idx = "",
    name      = "myvar"
  )
  expect_equal(result$add_var[["type"]], "aux")
  expect_match(result$add_var[["eqn"]], "ramp\\(times")
  expect_match(result$add_var[["eqn"]], "start\\s*=\\s*0")
  expect_match(result$add_var[["eqn"]], "finish\\s*=\\s*10")
  expect_match(result$add_var[["eqn"]], "height\\s*=\\s*5")
})

test_that("conv_ramp: replacement references the ramp variable", {
  result <- conv_ramp(func = "ramp", arg = c("0", "10", "5"), match_idx = "", name = "myvar")
  expect_match(result$replacement, "myvar_ramp")
  expect_match(result$replacement, "\\(t\\)")
})

test_that("conv_ramp: default height=1 applied when arg[3] is NA", {
  result <- conv_ramp(func = "ramp", arg = c("0", "10", NA), match_idx = "", name = "v")
  expect_match(result$add_var[["eqn"]], "height\\s*=\\s*1")
})


# ============================================================================
# conv_lookup()  — Lookup() → lookup type variable
# ============================================================================

test_that("conv_lookup: add_var has type='lookup' and correct name", {
  result <- conv_lookup(
    func = "lookup",
    arg  = c("[0,10]", "0,5,10", "0,50,100"),
    name = "myvar"
  )
  expect_equal(result$add_var[["type"]], "lookup")
  expect_equal(result$add_var[["name"]], "myvar_lookup")
})

test_that("conv_lookup: replacement uses square-bracket primitive syntax", {
  result <- conv_lookup(
    func = "lookup",
    arg  = c("[mydata]", "0,5", "0,10"),
    name = "myvar"
  )
  expect_match(result$replacement, "myvar_lookup")
  # The source (arg[1] with brackets stripped) appears in the replacement
  expect_match(result$replacement, "mydata")
})


# ============================================================================
# conversion guardrails and replacement helper branches
# ============================================================================

test_that("replace_comments leaves comment markers inside quotes untouched", {
  original <- "label <- 'http://example.test/*not-comment*/' // real comment"
  result <- replace_comments(original)

  expect_match(result, "http://example.test/*not-comment*/", fixed = TRUE)
  expect_match(result, "# real comment", fixed = TRUE)
  expect_false(grepl("// real comment", result, fixed = TRUE))
})

test_that("replace_op_IM preserves function defaults but converts comparisons and assignments", {
  result <- replace_op_IM("Function f(x = 1)\ny <- x\nx = y", character(0))

  expect_match(result, "x = 1", fixed = TRUE)
  expect_match(result, "y = x", fixed = TRUE)
  expect_match(result, "x == y", fixed = TRUE)
})

test_that("postprocess_equation_IM wraps multiline non-function equations", {
  result <- postprocess_equation_IM("x <- 1\ny <- 2", name = "aux_var")

  expect_equal(result, "{\nx <- 1\ny <- 2\n}")
  expect_equal(postprocess_equation_IM("x <- 1\ny <- 2", name = P[["func_name"]]), "x <- 1\ny <- 2")
})

test_that("build_IM_builtin_replacement handles syntax0, syntax1b and unsupported syntax", {
  syntax0 <- syntax_IM$syntax_df[
    syntax_IM$syntax_df[["syntax"]] == "syntax0" &
      syntax_IM$syntax_df[["insightmaker_first_iter"]] == "Time",
  ]
  syntax0$start <- 1L
  syntax0$end <- 4L
  result <- build_IM_builtin_replacement("Rand", syntax0[1, ], character(0), "x", character(0), character(0))
  expect_equal(result$replacement, P[["time_name"]])
  expect_equal(result$start_idx, 1L)
  expect_equal(result$end_idx, 4L)
  expect_equal(nrow(result$add_var), 0L)

  syntax1b <- syntax_IM$syntax_df[
    syntax_IM$syntax_df[["syntax"]] == "syntax1b" &
      syntax_IM$syntax_df[["insightmaker_first_iter"]] == "Rand",
  ]
  syntax1b$start <- 1L
  syntax1b$end <- 4L
  result <- build_IM_builtin_replacement("Constant", syntax1b[1, ], character(0), "x", character(0), character(0))
  expect_equal(result$replacement, "runif(1)")

  bad_syntax <- syntax0
  bad_syntax$syntax <- "unknown"
  expect_error(
    build_IM_builtin_replacement("Bad", bad_syntax[1, ], character(0), "x", character(0), character(0)),
    "Unsupported Insight Maker syntax class"
  )
})

test_that("report_unsupported_IM_functions reports unsupported function families", {
  expect_message(
    report_unsupported_IM_functions("Distance(x)", "stock", character(0), syntax_IM$syntax_df_unsupp),
    "Agent-Based Modelling functions"
  )
  expect_message(
    report_unsupported_IM_functions("Prompt(x)", "stock", character(0), syntax_IM$syntax_df_unsupp),
    "Unsupported Insight Maker functions"
  )
})
