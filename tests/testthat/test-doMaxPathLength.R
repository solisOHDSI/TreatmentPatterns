library(TreatmentPatterns)
library(testthat)

test_that("void", {
  expect_error(TreatmentPatterns:::doMaxPathLength())
})

test_that("minimal", {
  treatmentHistoryFiltered <- TreatmentPatterns:::doMaxPathLength(
    doFilterTreatmentsTHPP,
    maxPathLength)

  expect_s3_class(treatmentHistory, "data.frame")

  expect_s3_class(treatmentHistoryFiltered, "data.frame")
  expect_true(nrow(treatmentHistoryFiltered) < nrow(treatmentHistory))
})

test_that("invalid_input", {
  expect_error(TreatmentPatterns:::doMaxPathLength(
    treatment_history = NULL))

  expect_error(TreatmentPatterns:::doMaxPathLength(
    treatment_history = treatmentHistory,
    maxPathLength = NULL))

  expect_error(TreatmentPatterns:::doMaxPathLength(
    treatment_history = treatmentHistory,
    maxPathLength = mtcars))
})
