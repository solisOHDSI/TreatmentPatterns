library(testthat)
library(TreatmentPatterns)

test_that("void", {
  expect_error(TreatmentPatterns::executeTreatmentPatterns())
})

test_that("CohortGenerator", {
  testthat::skip_on_ci()
  
  global <- generateCohortTableCG()
  
  tempDir <- tempdir()

  expect_message(
    TreatmentPatterns::executeTreatmentPatterns(
      cohorts = global$cohorts,
      cohortTableName = global$cohortTableName,
      connectionDetails = global$connectionDetails,
      cdmSchema = global$cdmSchema,
      resultSchema = global$resultSchema,
      outputPath = tempDir
    )
  )

  expect_true(
    file.exists(file.path(tempDir, "treatmentPathways.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "summaryStatsTherapyDuraion.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "countsYear.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "countsAge.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "countsSex.csv"))
  )
})

test_that("CDMConnector", {
  tempDir <- tempdir()
  expect_message(
    TreatmentPatterns::executeTreatmentPatterns(
      cohorts = cohortsCDMC,
      cohortTableName = "cohort_table",
      cdm = cdm,
      outputPath = tempDir
    )
  )

  expect_true(
    file.exists(file.path(tempDir, "treatmentPathways.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "summaryStatsTherapyDuraion.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "countsYear.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "countsAge.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "countsSex.csv"))
  )
})
