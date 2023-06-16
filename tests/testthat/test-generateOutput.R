library(TreatmentPatterns)
library(testthat)

test_that("void", {
  expect_error(
    TreatmentPatterns::generateOutput(),
    "argument .+ is missing")
})

test_that("minimal", {
  suppressWarnings(expect_message(
    TreatmentPatterns::generateOutput(
      saveSettings = saveSettings),
    "Zipping:.+"))
})
