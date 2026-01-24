#!/usr/bin/env Rscript
# Quick test to verify refactored code
tryCatch({
  library(sdbuildR)
  
  # Test basic build functionality
  sfm <- xmile()
  sfm <- build(sfm, "x", "stock", eqn = 10)
  sfm <- build(sfm, "inflow", "flow", eqn = "x * 0.1", to = "x")
  sfm <- build(sfm, "rate", "constant", eqn = 0.1)
  
  cat("✓ All tests passed!\n")
  quit(save = "no", status = 0)
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
  quit(save = "no", status = 1)
})
