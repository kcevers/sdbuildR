# Test custom_unit() function for creating and managing custom units

test_that("custom_unit() creates custom units", {
  sfm <- sdbuildR()

  sfm1 <- custom_unit(sfm, "dBm", "10^(x/10) milliwatts")
  df <- as.data.frame(sfm1, type = "custom_unit", properties = c("name", "eqn"))
  expect_equal(nrow(df), 1)
  expect_equal(df[["name"]], "dBm")
  expect_equal(df[["eqn"]], "10^(x/10) milliwatts")
})

test_that("custom_unit() adds multiple custom units", {
  sfm <- sdbuildR()

  sfm1 <- custom_unit(sfm, "unit1", "eqn1")
  sfm2 <- custom_unit(sfm1, "unit2", "eqn2")
  sfm3 <- custom_unit(sfm2, "unit3", "eqn3")

  df <- as.data.frame(sfm3, type = "custom_unit")
  expect_equal(nrow(df), 3)
  expect_true("unit1" %in% df[["name"]])
  expect_true("unit2" %in% df[["name"]])
  expect_true("unit3" %in% df[["name"]])
})

test_that("custom_unit() modifies existing unit equations", {
  sfm <- sdbuildR()

  sfm1 <- custom_unit(sfm, "myunit", "definition1")
  sfm2 <- custom_unit(sfm1, "myunit", "definition2")

  df <- as.data.frame(sfm2, type = "custom_unit", properties = c("name", "eqn"))
  expect_equal(df[["eqn"]], "definition2")
})

test_that("custom_unit() validates unit equations are not empty", {
  sfm <- sdbuildR()

  expect_snapshot(
    custom_unit(sfm, "unit1", "")
  )
})

test_that("custom_unit() sets documentation", {
  sfm <- sdbuildR()

  sfm1 <- custom_unit(sfm, "dBm", "10^(x/10) milliwatts",
    doc = "Decibel-milliwatts, used for power measurement"
  )
  df <- as.data.frame(sfm1, type = "custom_unit", properties = c("name", "doc"))
  expect_equal(df[["doc"]], "Decibel-milliwatts, used for power measurement")
})

test_that("custom_unit() preserves variables and funcs", {
  sfm <- sdbuildR()

  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- custom_func(sfm1, "param1", eqn = "5")
  sfm3 <- custom_unit(sfm2, "unit1", "definition1")

  # Check all three exist
  var_df <- as.data.frame(sfm3, type = "stock")
  func_df <- as.data.frame(sfm3, type = "func")
  unit_df <- as.data.frame(sfm3, type = "custom_unit")

  expect_equal(nrow(var_df), 1)
  expect_equal(nrow(func_df), 1)
  expect_equal(nrow(unit_df), 1)
})

test_that("custom_unit() with empty model works", {
  sfm <- sdbuildR()

  sfm1 <- custom_unit(sfm, "unit1", "eqn1")
  df <- as.data.frame(sfm1, type = "custom_unit")
  expect_equal(nrow(df), 1)
})

test_that("custom_unit() returns the modified model", {
  sfm <- sdbuildR()

  result <- custom_unit(sfm, "unit1", "definition1")
  expect_s3_class(result, "sdbuildR")
  expect_true(is.list(result))
  expect_true("custom_unit" %in% names(result))
})

test_that("custom_unit() supports complex unit definitions", {
  sfm <- sdbuildR()

  # Test with complex mathematical expression
  sfm1 <- custom_unit(sfm, "complex_unit", "x^2 / (1 + x)")
  df <- as.data.frame(sfm1, type = "custom_unit", properties = c("name", "eqn"))
  expect_equal(df[["eqn"]], "x^2 / (1 + x)")
})

test_that("custom_unit() can use special characters in definitions", {
  sfm <- sdbuildR()

  sfm1 <- custom_unit(sfm, "special", "μg/mL")
  df <- as.data.frame(sfm1, type = "custom_unit", properties = "eqn")
  expect_equal(df[["eqn"]], "μg/mL")
})

test_that("custom_unit() rejects names that clash with variable names", {
  sfm <- sdbuildR()
  sfm <- build(sfm, "X", type = "stock")

  expect_error(
    custom_unit(sfm, "X"),
    "conflicts with existing variable"
  )
})

# --- change_name() for model units ---

test_that("change_name() renames model units", {
  sfm <- sdbuildR()

  sfm1 <- custom_unit(sfm, "oldname", "definition")
  sfm2 <- change_name(sfm1, "oldname", "newname")

  df <- as.data.frame(sfm2, type = "custom_unit", properties = "name")
  expect_equal(df[["name"]], "newname")
  expect_false("oldname" %in% df[["name"]])
})

test_that("change_name() for units updates variable unit references", {
  sfm <- sdbuildR()
  sfm <- custom_unit(sfm, "BMI", "kg/m^2")
  sfm <- build(sfm, "weight", type = "stock", units = "BMI")

  sfm2 <- change_name(sfm, "BMI", "bmi")

  # Unit name should be updated
  unit_df <- as.data.frame(sfm2, type = "custom_unit", properties = "name")
  expect_equal(unit_df[["name"]], "bmi")

  # Variable's units column should be updated
  var_df <- as.data.frame(sfm2, type = "stock", properties = c("name", "units"))
  expect_equal(var_df[["units"]], "bmi")
})

test_that("change_name() errors on nonexistent names", {
  sfm <- sdbuildR()

  expect_error(
    change_name(sfm, "nonexistent", "new"),
    "not exist"
  )
})

test_that("change_name() errors on mixed variable and unit names", {
  sfm <- sdbuildR()
  sfm <- build(sfm, "var1", type = "stock")
  sfm <- custom_unit(sfm, "unit1", "definition")

  expect_error(
    change_name(sfm, c("var1", "unit1"), c("new_var", "new_unit")),
    "Cannot rename variables and model units"
  )
})

# --- discard() for model units ---

test_that("discard() removes custom units", {
  sfm <- sdbuildR()

  sfm1 <- custom_unit(sfm, "unit1", "eqn1")
  sfm2 <- custom_unit(sfm1, "unit2", "eqn2")

  # Remove one unit
  sfm3 <- discard(sfm2, "unit1")
  df <- as.data.frame(sfm3, type = "custom_unit")
  expect_equal(nrow(df), 1)
  expect_equal(df[["name"]], "unit2")
})

test_that("discard() removes multiple units", {
  sfm <- sdbuildR()

  sfm1 <- custom_unit(sfm, "u1", "e1")
  sfm2 <- custom_unit(sfm1, "u2", "e2")
  sfm3 <- custom_unit(sfm2, "u3", "e3")

  # Remove multiple
  sfm4 <- discard(sfm3, c("u1", "u3"))
  df <- as.data.frame(sfm4, type = "custom_unit")
  expect_equal(nrow(df), 1)
  expect_equal(df[["name"]], "u2")
})

test_that("discard() validates unit existence", {
  sfm <- sdbuildR()

  sfm1 <- custom_unit(sfm, "unit1", "eqn1")

  expect_error(
    discard(sfm1, "nonexistent"),
    "not exist"
  )
})

test_that("discard() fails when some units don't exist", {
  sfm <- sdbuildR()

  sfm1 <- custom_unit(sfm, "u1", "e1")
  sfm2 <- custom_unit(sfm1, "u2", "e2")

  expect_error(
    discard(sfm2, c("u1", "nonexistent")),
    "not exist"
  )
})

test_that("discard() warns about lingering unit references", {
  sfm <- sdbuildR()
  sfm <- custom_unit(sfm, "BMI", "kg/m^2")
  sfm <- build(sfm, "weight", type = "stock", units = "BMI")

  expect_warning(
    discard(sfm, "BMI"),
    "lingering reference"
  )
})

test_that("discard() errors on mixed variable and unit names", {
  sfm <- sdbuildR()
  sfm <- build(sfm, "var1", type = "stock")
  sfm <- custom_unit(sfm, "unit1", "definition")

  expect_error(
    discard(sfm, c("var1", "unit1")),
    "Cannot discard variables and model units"
  )
})

test_that("custom_unit() errors when model object passed as name", {
  sfm <- sdbuildR()
  expect_error(custom_unit(sfm, sfm, eqn = "kg/m^2"), "passed where a variable name")
})

