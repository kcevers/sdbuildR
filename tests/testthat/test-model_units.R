# Test model_units() function for creating and managing custom units

test_that("model_units() creates custom units", {
  sfm <- xmile()
  
  sfm1 <- model_units(sfm, "dBm", "10^(x/10) milliwatts")
  df <- as.data.frame(sfm1, type = "model_units", properties = c("name", "eqn"))
  expect_equal(nrow(df), 1)
  expect_equal(df[["name"]], "dBm")
  expect_equal(df[["eqn"]], "10^(x/10) milliwatts")
})

test_that("model_units() adds multiple custom units", {
  sfm <- xmile()
  
  sfm1 <- model_units(sfm, "unit1", "eqn1")
  sfm2 <- model_units(sfm1, "unit2", "eqn2")
  sfm3 <- model_units(sfm2, "unit3", "eqn3")
  
  df <- as.data.frame(sfm3, type = "model_units")
  expect_equal(nrow(df), 3)
  expect_true("unit1" %in% df[["name"]])
  expect_true("unit2" %in% df[["name"]])
  expect_true("unit3" %in% df[["name"]])
})

test_that("model_units() modifies existing unit equations", {
  sfm <- xmile()
  
  sfm1 <- model_units(sfm, "myunit", "definition1")
  sfm2 <- model_units(sfm1, "myunit", "definition2")
  
  df <- as.data.frame(sfm2, type = "model_units", properties = c("name", "eqn"))
  expect_equal(df[["eqn"]], "definition2")
})

test_that("model_units() prevents duplicate unit names", {
  sfm <- xmile()
  
  sfm1 <- model_units(sfm, "unit1", "eqn1")
  sfm2 <- model_units(sfm1, "unit2", "eqn2")
  
  # Try to rename unit2 to unit1 (which already exists)
  expect_error(
    model_units(sfm2, "unit2", change_name = "unit1"),
    "A custom unit with this name already exists"
  )
})

test_that("model_units() renames units with change_name argument", {
  sfm <- xmile()
  
  sfm1 <- model_units(sfm, "oldname", "definition")
  sfm2 <- model_units(sfm1, "oldname", change_name = "newname")
  
  df <- as.data.frame(sfm2, type = "model_units", properties = "name")
  expect_equal(df[["name"]], "newname")
  expect_false("oldname" %in% df[["name"]])
})

test_that("model_units() validates unit equations are not empty", {
  sfm <- xmile()
  
  expect_error(
    model_units(sfm, "unit1", ""),
    "eqn cannot be an empty string"
  )
})

test_that("model_units() erase removes custom units", {
  sfm <- xmile()
  
  sfm1 <- model_units(sfm, "unit1", "eqn1")
  sfm2 <- model_units(sfm1, "unit2", "eqn2")
  
  # Remove one unit
  sfm3 <- model_units(sfm2, "unit1", erase = TRUE)
  df <- as.data.frame(sfm3, type = "model_units")
  expect_equal(nrow(df), 1)
  expect_equal(df[["name"]], "unit2")
})

test_that("model_units() erase multiple units", {
  sfm <- xmile()
  
  sfm1 <- model_units(sfm, "u1", "e1")
  sfm2 <- model_units(sfm1, "u2", "e2")
  sfm3 <- model_units(sfm2, "u3", "e3")
  
  # Remove multiple
  sfm4 <- model_units(sfm3, c("u1", "u3"), erase = TRUE)
  df <- as.data.frame(sfm4, type = "model_units")
  expect_equal(nrow(df), 1)
  expect_equal(df[["name"]], "u2")
})

test_that("model_units() erase validates unit existence", {
  sfm <- xmile()
  
  sfm1 <- model_units(sfm, "unit1", "eqn1")
  
  expect_error(
    model_units(sfm1, "nonexistent", erase = TRUE),
    "Cannot erase non-existent custom unit"
  )
})

test_that("model_units() erase fails when some units don't exist", {
  sfm <- xmile()
  
  sfm1 <- model_units(sfm, "u1", "e1")
  sfm2 <- model_units(sfm1, "u2", "e2")
  
  expect_error(
    model_units(sfm2, c("u1", "nonexistent"), erase = TRUE),
    "Cannot erase non-existent custom unit"
  )
})

test_that("model_units() sets documentation", {
  sfm <- xmile()
  
  sfm1 <- model_units(sfm, "dBm", "10^(x/10) milliwatts",
                      doc = "Decibel-milliwatts, used for power measurement")
  df <- as.data.frame(sfm1, type = "model_units", properties = c("name", "doc"))
  expect_equal(df[["doc"]], "Decibel-milliwatts, used for power measurement")
})

test_that("model_units() preserves variables and macros", {
  sfm <- xmile()
  
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- macro(sfm1, "param1", eqn = "5")
  sfm3 <- model_units(sfm2, "unit1", "definition1")
  
  # Check all three exist
  var_df <- as.data.frame(sfm3, type = "stock")
  macro_df <- as.data.frame(sfm3, type = "macro")
  unit_df <- as.data.frame(sfm3, type = "model_units")
  
  expect_equal(nrow(var_df), 1)
  expect_equal(nrow(macro_df), 1)
  expect_equal(nrow(unit_df), 1)
})

test_that("model_units() with empty model works", {
  sfm <- xmile()
  
  sfm1 <- model_units(sfm, "unit1", "eqn1")
  df <- as.data.frame(sfm1, type = "model_units")
  expect_equal(nrow(df), 1)
})

test_that("model_units() returns the modified model", {
  sfm <- xmile()
  
  result <- model_units(sfm, "unit1", "definition1")
  expect_s3_class(result, "sdbuildR_xmile")
  expect_true(is.list(result))
  expect_true("model_units" %in% names(result))
})

test_that("model_units() supports complex unit definitions", {
  sfm <- xmile()
  
  # Test with complex mathematical expression
  sfm1 <- model_units(sfm, "complex_unit", "x^2 / (1 + x)")
  df <- as.data.frame(sfm1, type = "model_units", properties = c("name", "eqn"))
  expect_equal(df[["eqn"]], "x^2 / (1 + x)")
})

test_that("model_units() can use special characters in definitions", {
  sfm <- xmile()
  
  sfm1 <- model_units(sfm, "special", "μg/mL")
  df <- as.data.frame(sfm1, type = "model_units", properties = "eqn")
  expect_equal(df[["eqn"]], "μg/mL")
})

