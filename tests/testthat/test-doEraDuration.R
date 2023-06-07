library(TreatmentPatterns)
library(testthat)

test_that("void", {
  expect_error(TreatmentPatterns:::doEraDuration())
})

test_that("minimal", {
  treatmentHistoryFiltered <- TreatmentPatterns:::doEraDuration(
    treatmentHistory = treatmentHistory,
    minEraDuration = minEraDuration)

  expect_s3_class(treatmentHistoryFiltered, "data.frame")
  expect_true(nrow(treatmentHistoryFiltered) < nrow(treatmentHistory))
})

test_that("invalid_input", {
  expect_error(TreatmentPatterns:::doEraDuration(
    treatmentHistory = NULL))

  expect_error(TreatmentPatterns:::doEraDuration(
    treatmentHistory = treatmentHistory,
    minEraDuration = NULL))

  expect_error(TreatmentPatterns:::doEraDuration(
    treatmentHistory = treatmentHistory,
    minEraDuration = mtcars))
})
