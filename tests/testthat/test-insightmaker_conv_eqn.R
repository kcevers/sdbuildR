test_that("convert_builtin_functions_IM basic functions", {
  sfm <- sdbuildR("predator_prey")
  var_names <- get_model_var(sfm)
  regex_units <- get_regex_units()
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
  sfm <- sdbuildR("predator_prey")
  var_names <- get_model_var(sfm)
  regex_units <- get_regex_units()
  name <- var_names[1]
  type <- "aux"

  # Arithmetic
  result <- convert_equations_IM(type, name, "1 + 2", var_names, regex_units)$eqn
  expect_true(grepl("\\+", result))

  # Multiplication
  result <- convert_equations_IM(type, name, "3 * 4", var_names, regex_units)$eqn
  expect_true(grepl("\\*", result))

  # Division
  result <- convert_equations_IM(type, name, "10 / 2", var_names, regex_units)$eqn
  expect_true(grepl("/", result))

  # Exponentiation
  result <- convert_equations_IM(type, name, "2^3", var_names, regex_units)$eqn
  expect_true(grepl("\\^", result))
})


test_that("convert_equations_IM conditional statements", {
  sfm <- sdbuildR("predator_prey")
  var_names <- get_model_var(sfm)
  regex_units <- get_regex_units()
  name <- var_names[1]
  type <- "aux"

  # Simple if statement
  eqn <- "if(x > 0) { 1 } else { 0 }"
  result <- convert_equations_IM(type, name, eqn, var_names, regex_units)$eqn
  expect_true(grepl("ifelse", result) || grepl("if", result))

  # Nested if
  eqn <- "if(x > 0) { if(x > 10) { 2 } else { 1 } } else { 0 }"
  result <- convert_equations_IM(type, name, eqn, var_names, regex_units)$eqn
  expect_true(nchar(result) > 0)
})


test_that("convert_equations_IM empty and NULL cases", {
  sfm <- sdbuildR("predator_prey")
  var_names <- get_model_var(sfm)
  regex_units <- get_regex_units()
  name <- var_names[1]
  type <- "aux"

  # Empty string
  result <- convert_equations_IM(type, name, "", var_names, regex_units)$eqn
  expect_equal(result, "")

  # Null
  result <- convert_equations_IM(type, name, NULL, var_names, regex_units)$eqn
  expect_equal(result, "")

  # Zero
  result <- convert_equations_IM(type, name, "0", var_names, regex_units)$eqn
  expect_equal(result, "0")
})


test_that("convert_builtin_functions_IM string functions", {
  sfm <- sdbuildR("predator_prey")
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
  sfm <- sdbuildR("predator_prey")
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

