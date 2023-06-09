library(TreatmentPatterns)
library(testthat)

test_that("void", {
  expect_error(TreatmentPatterns:::doFilterTreatments())
})

test_that("minimal", {
  treatmentHistoryFiltered <- TreatmentPatterns:::doFilterTreatments(
    treatmentHistory = doCombinationWindowTH,
    filterTreatments = filterTreatments)

  expect_s3_class(treatmentHistoryFiltered, "data.frame")
  expect_true(nrow(treatmentHistoryFiltered) < nrow(treatmentHistory))
})

test_that("invalid_input", {
  expect_error(TreatmentPatterns:::doMaxPathLength(
    treatmentHistory = NULL))

  expect_error(TreatmentPatterns:::doMaxPathLength(
    treatmentHistory = treatmentHistory,
    filterTreatments = NULL))

  expect_error(TreatmentPatterns:::doMaxPathLength(
    treatmentHistory = treatmentHistory,
    filterTreatments = mtcars))
})
