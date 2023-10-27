library(testthat)
library(TreatmentPatterns)

cohortsGenerated <- setupCohorts(connectionDetails)
cohorts <- setupCohortTypes(cohortsGenerated, connectionDetails)

cdm <- setupCDM()
cdm <- setupCohortsCDM(cdm)

withr::defer({
  DBI::dbDisconnect(con)
  rm("cohortsGenerated", "cohorts", "cdm")
})

test_that("void", {
  expect_error(TreatmentPatterns::executeTreatmentPatterns())
})

test_that("CohortGenerator", {
  tempDir <- tempdir()
  expect_message(
    TreatmentPatterns::executeTreatmentPatterns(
      cohorts = cohorts,
      cohortTableName = "cohortTable",
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
  
  withr::defer({
    unlink(tempDir, recursive = TRUE)
  })
})

test_that("CDMConnector", {
  tempDir <- tempdir()
  expect_message(
    TreatmentPatterns::executeTreatmentPatterns(
      cohorts = cohorts,
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
  
  withr::defer({
    unlink(tempDir, recursive = TRUE)
  })
})
