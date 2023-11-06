library(testthat)
library(TreatmentPatterns)
library(Eunomia)
library(withr)
library(CohortGenerator)
library(tools)
library(CirceR)
library(dplyr)

connectionDetails <- Eunomia::getEunomiaConnectionDetails()

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
  cohortTable = "CohortTable")

CohortGenerator::createCohortTables(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = "main",
  cohortTableNames = cohortTableNames)

# Generate the cohorts
cohortsGenerated <- CohortGenerator::generateCohortSet(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  cohortDatabaseSchema = "main",
  cohortTableNames = cohortTableNames,
  cohortDefinitionSet = cohortsToCreate)

# Select Viral Sinusitis Cohort
targetCohorts <- cohortsGenerated %>%
  filter(cohortName == "ViralSinusitis") %>%
  select(cohortId, cohortName)

# Select everything BUT Viral Sinusitis cohorts
eventCohorts <- cohortsGenerated %>%
  filter(cohortName != "ViralSinusitis" & cohortName != "Death") %>%
  select(cohortId, cohortName)

exitCohorts <- cohortsGenerated %>%
  filter(cohortName == "Death") %>%
  select(cohortId, cohortName)

cohorts <- dplyr::bind_rows(
  targetCohorts %>% mutate(type = "target"),
  eventCohorts %>% mutate(type = "event"),
  exitCohorts %>% mutate(type = "exit")
)


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

# test_that("CDMConnector", {
#   tempDir <- tempdir()
#   expect_message(
#     TreatmentPatterns::executeTreatmentPatterns(
#       cohorts = cohorts,
#       cohortTableName = "cohort_table",
#       cdm = cdm,
#       outputPath = tempDir
#     )
#   )
#   
#   expect_true(
#     file.exists(file.path(tempDir, "treatmentPathways.csv"))
#   )
#   
#   expect_true(
#     file.exists(file.path(tempDir, "summaryStatsTherapyDuraion.csv"))
#   )
#   
#   expect_true(
#     file.exists(file.path(tempDir, "countsYear.csv"))
#   )
#   
#   expect_true(
#     file.exists(file.path(tempDir, "countsAge.csv"))
#   )
#   
#   expect_true(
#     file.exists(file.path(tempDir, "countsSex.csv"))
#   )
#   
#   withr::defer({
#     unlink(tempDir, recursive = TRUE)
#   })
# })
