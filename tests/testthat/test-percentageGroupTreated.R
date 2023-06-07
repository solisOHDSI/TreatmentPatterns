library(testthat)
library(TreatmentPatterns)

test_that("void", {
  expect_error(
    TreatmentPatterns:::percentageGroupTreated()
  )
})

test_that("minimal", {
  expect_s3_class(TreatmentPatterns:::percentageGroupTreated(
    data = treatmentPathways[[1]],
    eventCohortIds = eventCohortIds,
    groupCombinations = groupCombinations,
    outputFolder = saveSettings$outputFolder), "data.frame")
})
