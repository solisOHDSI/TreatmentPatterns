library(testthat)
library(TreatmentPatterns)

test_that("computePathways", {
  expect_message(
    expect_message(
      expect_message(
        TreatmentPatterns::computePathways(
          cohorts = cohorts,
          cohortTableName = "CohortTable",
          connectionDetails = connectionDetails,
          cdmSchema = "main",
          resultSchema = "main"
        ),
        "After maxPathLength: 553"
      ),
      "After combinationWindow: 554"
    ),
    "Original number of rows: 8352"
  )
})
