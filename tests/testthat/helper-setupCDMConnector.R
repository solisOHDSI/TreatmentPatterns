library(CDMConnector)
library(CirceR)
library(DBI)
library(duckdb)

setupCDM <- function() {
  withr::local_envvar(
    EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
  )
  
  CDMConnector::downloadEunomiaData(
    overwrite = TRUE
  )
  
  con <<- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = CDMConnector::eunomia_dir()
  )
  
  cdm <- CDMConnector::cdmFromCon(
    con = con,
    cdmSchema = "main",
    writeSchema = "main"
  )
  return(cdm)
}

setupCohortsCDM <- function(cdm) {
  ## Read in cohort set ----
  cohortsSet <<- CDMConnector::readCohortSet(
    path = system.file(package = "TreatmentPatterns", "exampleCohorts")
  )
  
  ## Generate cohot set ----
  cdm <- CDMConnector::generateCohortSet(
    cdm = cdm,
    cohortSet = cohortsSet,
    name = "cohort_table",
    computeAttrition = FALSE
  )
  return(cdm)
}

setupCDMConnector <- function() {
  cdm <- setupCDM()
  cdm <- setupCohortsCDM(cdm)
  
  cohorts <- data.frame(
    cohortId = cohortsSet$cohort_definition_id,
    cohortName = cohortsSet$cohort_name,
    type = c("event", "event", "event", "event", "exit", "event", "event", "target")
  )
  
  # TreatmentPatterns ----
  ## Run TreatmentPattenrs ----
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cdm = cdm,
    cohortTableName = "cohort_table"
  )
  
  withr::defer({
    DBI::dbDisconnect(con, shutdown = TRUE)
  })
  return(andromeda)
}
