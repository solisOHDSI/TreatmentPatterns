# ableToRun ----
ableToRunCG <- function() {
  invisible(all(
    require("Eunomia", character.only = TRUE),
    require("CirceR", character.only = TRUE),
    require("CohortGenerator", character.only = TRUE)
  ))
}

library(testthat)
library(TreatmentPatterns)
library(dplyr)
library(R6)
library(Andromeda)

tempDirCG <- file.path(tempdir(), "CohortGenerator")
dir.create(tempDirCG)

andromedaCGPath <- file.path(tempDirCG, "andromeda")

# Setup CohortGenerator ----
if (ableToRunCG()) {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  cdmDatabaseSchema <- "main"
  resultSchema <- "main"
  cohortTable <- "CohortTable"
  
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
    cohortTable = cohortTable)
  
  CohortGenerator::createCohortTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = resultSchema,
    cohortTableNames = cohortTableNames)
  
  # Generate the cohorts
  cohortsGenerated <- CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = resultSchema,
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
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "CohortTable",
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )
  
  Andromeda::saveAndromeda(
    andromeda = andromeda,
    fileName = andromedaCGPath,
    maintainConnection = FALSE,
    overwrite = TRUE
  )
  
  withr::defer({
    # Clean-up tempdir
    unlink(x = tempDirCG, recursive = TRUE)
  })
}
