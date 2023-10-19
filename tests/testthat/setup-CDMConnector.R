# ableToRunCDMC <- function() {
#   invisible(all(
#     require("CDMConnector", character.only = TRUE),
#     require("CirceR", character.only = TRUE),
#     require("DBI", character.only = TRUE),
#     require("duckdb", character.only = TRUE)
#   ))
# }
# 
# # Globals ----
# library(testthat)
# library(TreatmentPatterns)
# library(dplyr)
# library(R6)
# library(Andromeda)
# 
# tempDirCDMC <- file.path(tempdir(), "CDMConnector")
# dir.create(tempDirCDMC)
# 
# andromedaCDMCPath <- file.path(tempDirCDMC, "andromeda.sqlite")
# 
# # Setup CDM ----
# if (ableToRunCDMC()) {
#   withr::local_envvar(
#     EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
#   )
#   
#   CDMConnector::downloadEunomiaData(
#     overwrite = TRUE
#   )
#   
#   con <- DBI::dbConnect(
#     duckdb::duckdb(),
#     dbdir = CDMConnector::eunomia_dir()
#   )
#   
#   # Disconnect here works, but not in wihtr::defer() later.
#   # Probably weird garbage collection order.
#   withr::defer({
#     dbDisconnect(con, shutdown = TRUE)
#   })
#   
#   cdm <- CDMConnector::cdmFromCon(
#     con = con,
#     cdmSchema = "main",
#     writeSchema = "main"
#   )
#   
#   ## Read in cohort set ----
#   cohortSet <- CDMConnector::readCohortSet(
#     path = system.file(package = "TreatmentPatterns", "exampleCohorts")
#   )
#   
#   ## Generate cohot set ----
#   cdm <- CDMConnector::generateCohortSet(
#     cdm = cdm,
#     cohortSet = cohortSet,
#     name = "cohort_table",
#     computeAttrition = FALSE
#   )
#   
#   cohorts <- data.frame(
#     cohortId = cohortSet$cohort_definition_id,
#     cohortName = cohortSet$cohort_name,
#     type = c("event", "event", "event", "event", "exit", "event", "event", "target")
#   )
#   
#   # TreatmentPatterns ----
#   ## Run TreatmentPattenrs ----
#   andromeda <- TreatmentPatterns::computePathways(
#     cohorts = cohorts,
#     cdm = cdm,
#     cohortTableName = "cohort_table"
#   )
#   
#   Andromeda::saveAndromeda(
#     andromeda = andromeda,
#     fileName = andromedaCDMCPath,
#     maintainConnection = FALSE,
#     overwrite = TRUE
#   )
#   
#   # Clean-up ----
#   withr::defer({
#     # Clean-up tempdir
#     unlink(x = tempDirCDMC, recursive = TRUE)
#   })
# }
