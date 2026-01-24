# Test macro() function for creating and modifying macros

test_that("macro() creates custom macros", {
  sfm <- xmile()
  
  sfm1 <- macro(sfm, "parameter1", eqn = "5")
  df <- as.data.frame(sfm1, type = "macro", properties = c("name", "eqn"))
  expect_equal(nrow(df), 1)
  expect_equal(df[["name"]], "parameter1")
  expect_equal(df[["eqn"]], "5")
})

test_that("macro() adds multiple macros", {
  sfm <- xmile()
  
  sfm1 <- macro(sfm, "param1", eqn = "5")
  sfm2 <- macro(sfm1, "param2", eqn = "10")
  sfm3 <- macro(sfm2, "param3", eqn = "param1 + param2")
  
  df <- as.data.frame(sfm3, type = "macro")
  expect_equal(nrow(df), 3)
  expect_true("param1" %in% df[["name"]])
  expect_true("param2" %in% df[["name"]])
  expect_true("param3" %in% df[["name"]])
})

test_that("macro() modifies existing macro equations", {
  sfm <- xmile()
  
  sfm1 <- macro(sfm, "param1", eqn = "5")
  sfm2 <- macro(sfm1, "param1", eqn = "10")
  
  df <- as.data.frame(sfm2, type = "macro", properties = c("name", "eqn"))
  expect_equal(df[["eqn"]], "10")
})

test_that("macro() prevents duplicate macro names", {
  sfm <- xmile()
  
  sfm1 <- macro(sfm, "param1", eqn = "5")
  sfm2 <- macro(sfm1, "param2", eqn = "10")
  
  # Try to rename param2 to param1 (which already exists)
  expect_error(
    macro(sfm2, "param2", change_name = "param1"),
    "A macro with this name already exists"
  )
})

test_that("macro() renames macros with change_name argument", {
  sfm <- xmile()
  
  sfm1 <- macro(sfm, "old_name", eqn = "5")
  sfm2 <- macro(sfm1, "old_name", change_name = "new_name")
  
  df <- as.data.frame(sfm2, type = "macro", properties = "name")
  expect_equal(df[["name"]], "new_name")
  expect_false("old_name" %in% df[["name"]])
})

test_that("macro() validates macro equations", {
  sfm <- xmile()
  
  # Invalid equation format
  expect_error(
    macro(sfm, "param1", eqn = ""),
    "eqn cannot be an empty string"
  )
})

test_that("macro() sets macro units", {
  sfm <- xmile()
  
  sfm1 <- macro(sfm, "rate", eqn = "0.05", units = "1/year")
  df <- as.data.frame(sfm1, type = "macro", properties = c("name", "units"))
  expect_equal(df[["units"]], "1/year")
})

test_that("macro() sets macro documentation", {
  sfm <- xmile()
  
  sfm1 <- macro(sfm, "birth_rate", eqn = "0.02", 
                doc = "Annual birth rate per individual")
  df <- as.data.frame(sfm1, type = "macro", properties = c("name", "doc"))
  expect_equal(df[["doc"]], "Annual birth rate per individual")
})

test_that("macro() erase removes macros", {
  sfm <- xmile()
  
  sfm1 <- macro(sfm, "param1", eqn = "5")
  sfm2 <- macro(sfm1, "param2", eqn = "10")
  
  # Remove one macro
  sfm3 <- macro(sfm2, "param1", erase = TRUE)
  df <- as.data.frame(sfm3, type = "macro")
  expect_equal(nrow(df), 1)
  expect_equal(df[["name"]], "param2")
})

test_that("macro() erase multiple macros", {
  sfm <- xmile()
  
  sfm1 <- macro(sfm, "p1", eqn = "1")
  sfm2 <- macro(sfm1, "p2", eqn = "2")
  sfm3 <- macro(sfm2, "p3", eqn = "3")
  
  # Remove multiple
  sfm4 <- macro(sfm3, c("p1", "p3"), erase = TRUE)
  df <- as.data.frame(sfm4, type = "macro")
  expect_equal(nrow(df), 1)
  expect_equal(df[["name"]], "p2")
})

test_that("macro() erase validates macro existence", {
  sfm <- xmile()
  
  sfm1 <- macro(sfm, "param1", eqn = "5")
  
  expect_error(
    macro(sfm1, "nonexistent", erase = TRUE),
    "Cannot erase non-existent macro"
  )
})

test_that("macro() erase fails when some macros don't exist", {
  sfm <- xmile()
  
  sfm1 <- macro(sfm, "p1", eqn = "1")
  sfm2 <- macro(sfm1, "p2", eqn = "2")
  
  expect_error(
    macro(sfm2, c("p1", "nonexistent"), erase = TRUE),
    "Cannot erase non-existent macro"
  )
})

test_that("macro() preserves variables", {
  sfm <- xmile()
  
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- macro(sfm1, "param1", eqn = "5")
  
  # Check both exist
  var_df <- as.data.frame(sfm2, type = "stock")
  macro_df <- as.data.frame(sfm2, type = "macro")
  
  expect_equal(nrow(var_df), 1)
  expect_equal(var_df[["name"]], "Stock1")
  expect_equal(nrow(macro_df), 1)
  expect_equal(macro_df[["name"]], "param1")
})

test_that("macro() can be used in variable equations", {
  sfm <- xmile()
  
  sfm1 <- macro(sfm, "rate", eqn = "0.05")
  sfm2 <- build(sfm1, "Stock1", type = "stock")
  sfm3 <- build(sfm2, "Flow1", type = "flow", 
                eqn = "Stock1 * rate", from = "Stock1")
  
  df <- as.data.frame(sfm3, type = "flow", properties = c("name", "eqn"))
  expect_match(df[["eqn"]], "rate")
})

test_that("macro() with empty model works", {
  sfm <- xmile()
  
  # Should be able to add macro to empty model
  sfm1 <- macro(sfm, "macro1", eqn = "10")
  df <- as.data.frame(sfm1, type = "macro")
  expect_equal(nrow(df), 1)
})

test_that("macro() returns the modified model", {
  sfm <- xmile()
  
  result <- macro(sfm, "param1", eqn = "5")
  expect_s3_class(result, "sdbuildR_xmile")
  expect_true(is.list(result))
  expect_true("macro" %in% names(result))
})
