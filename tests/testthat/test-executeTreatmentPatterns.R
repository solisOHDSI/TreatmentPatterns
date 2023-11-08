library(testthat)
library(TreatmentPatterns)
library(Eunomia)

test_that("void", {
  expect_error(TreatmentPatterns::executeTreatmentPatterns())
})

test_that("CohortGenerator", {
  tempDir <- tempdir()

  expect_message(
    TreatmentPatterns::executeTreatmentPatterns(
      cohorts = cohortsDBC,
      cohortTableName = "CohortTable",
      connectionDetails = connectionDetails,
      cdmSchema = "main",
      resultSchema = "main",
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
