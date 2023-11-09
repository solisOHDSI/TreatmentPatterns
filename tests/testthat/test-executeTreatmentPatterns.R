library(testthat)
library(TreatmentPatterns)

test_that("void", {
  expect_error(TreatmentPatterns::executeTreatmentPatterns())
})

test_that("CohortGenerator", {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  cohortTableName <- "CohortTable"
  resultSchema <- "main"
  cdmSchema <- "main"
  
  cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
  
  cohortJsonFiles <- list.files(
    system.file(
      package = "TreatmentPatterns",
      "exampleCohorts"),
    full.names = TRUE)
  
  for (i in seq_len(length(cohortJsonFiles))) {
    cohortJsonFileName <- cohortJsonFiles[i]
    cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
    cohortJson <- readChar(cohortJsonFileName, file.info(
      cohortJsonFileName)$size)
    
    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
    
    cohortSql <- CirceR::buildCohortQuery(
      cohortExpression,
      options = CirceR::createGenerateOptions(generateStats = FALSE))
    cohortsToCreate <- rbind(
      cohortsToCreate,
      data.frame(
        cohortId = i,
        cohortName = cohortName,
        sql = cohortSql,
        stringsAsFactors = FALSE))
  }
  
  cohortTableNames <- CohortGenerator::getCohortTableNames(
    cohortTable = cohortTableName)
  
  CohortGenerator::createCohortTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = resultSchema,
    cohortTableNames = cohortTableNames)
  
  # Generate the cohorts
  cohortsGenerated <- CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmSchema,
    cohortDatabaseSchema = resultSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = cohortsToCreate)
  
  # Select Viral Sinusitis Cohort
  targetCohorts <- cohortsGenerated %>%
    dplyr::filter(cohortName == "ViralSinusitis") %>%
    dplyr::select(cohortId, cohortName)
  
  # Select everything BUT Viral Sinusitis cohorts
  eventCohorts <- cohortsGenerated %>%
    dplyr::filter(cohortName != "ViralSinusitis" & cohortName != "Death") %>%
    dplyr::select(cohortId, cohortName)
  
  exitCohorts <- cohortsGenerated %>%
    dplyr::filter(cohortName == "Death") %>%
    dplyr::select(cohortId, cohortName)
  
  cohorts <- dplyr::bind_rows(
    targetCohorts %>% dplyr::mutate(type = "target"),
    eventCohorts %>% dplyr::mutate(type = "event"),
    exitCohorts %>% dplyr::mutate(type = "exit")
  )
  
  tempDir <- tempdir()

  expect_message(
    TreatmentPatterns::executeTreatmentPatterns(
      cohorts = cohorts,
      cohortTableName = cohortTableName,
      connectionDetails = connectionDetails,
      cdmSchema = cdmSchema,
      resultSchema = resultSchema,
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
