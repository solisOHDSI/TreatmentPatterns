# library(CohortGenerator)
# library(CirceR)
# library(Eunomia)
# library(dplyr)
# 
# connectionDetails <- Eunomia::getEunomiaConnectionDetails()
# 
# cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
#   
# cohortJsonFiles <- list.files(
#   system.file(
#     package = "TreatmentPatterns",
#     "exampleCohorts"),
#   full.names = TRUE)
#   
# for (i in seq_len(length(cohortJsonFiles))) {
#   cohortJsonFileName <- cohortJsonFiles[i]
#   cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
#   cohortJson <- readChar(cohortJsonFileName, file.info(
#     cohortJsonFileName)$size)
#   
#   cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
#   
#   cohortSql <- CirceR::buildCohortQuery(
#     cohortExpression,
#     options = CirceR::createGenerateOptions(generateStats = FALSE))
#   cohortsToCreate <- rbind(
#     cohortsToCreate,
#     data.frame(
#       cohortId = i,
#       cohortName = cohortName,
#       sql = cohortSql,
#       stringsAsFactors = FALSE))
# }
# 
# cohortTableNames <- CohortGenerator::getCohortTableNames(
#   cohortTable = "CohortTable")
# 
# CohortGenerator::createCohortTables(
#   connectionDetails = connectionDetails,
#   cohortDatabaseSchema = "main",
#   cohortTableNames = cohortTableNames)
# 
# # Generate the cohorts
# cohortsGenerated <- CohortGenerator::generateCohortSet(
#   connectionDetails = connectionDetails,
#   cdmDatabaseSchema = "main",
#   cohortDatabaseSchema = "main",
#   cohortTableNames = cohortTableNames,
#   cohortDefinitionSet = cohortsToCreate)
# 
# # Select Viral Sinusitis Cohort
# targetCohorts <- cohortsGenerated %>%
#   filter(cohortName == "ViralSinusitis") %>%
#   select(cohortId, cohortName)
# 
# # Select everything BUT Viral Sinusitis cohorts
# eventCohorts <- cohortsGenerated %>%
#   filter(cohortName != "ViralSinusitis" & cohortName != "Death") %>%
#   select(cohortId, cohortName)
# 
# exitCohorts <- cohortsGenerated %>%
#   filter(cohortName == "Death") %>%
#   select(cohortId, cohortName)
# 
# cohorts <- dplyr::bind_rows(
#   targetCohorts %>% mutate(type = "target"),
#   eventCohorts %>% mutate(type = "event"),
#   exitCohorts %>% mutate(type = "exit")
# )
# 
# withr::defer({
#   rm(
#     "cohorts", "exitCohorts", "eventCohorts", "targetCohorts",
#     "cohortsGenerated", "cohortTableNames", "cohortJsonFiles",
#     "cohortsToCreate", "connectionDetails"
#   )
# })
