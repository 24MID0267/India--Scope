# tests/run_tests.R
# Script to run all testthat unit tests from project root

library(testthat)

cat("Running IndiaScope Automated Test Suite...\n")
results <- test_dir("tests/testthat")
print(results)

if (any(as.data.frame(results)$failed > 0)) {
  cat("\nSOME UNIT TESTS FAILED!\n")
  quit(status = 1)
} else {
  cat("\nALL UNIT TESTS PASSED SUCCESSFULLY!\n")
}
