library(CDMConnector)
library(CirceR)
library(DBI)
library(duckdb)

withr::local_envvar(
  R_USER_CACHE_DIR = tempfile(),
  .local_envir = teardown_env(),
  EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
)


CDMConnector::downloadEunomiaData(
  overwrite = TRUE
)

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = CDMConnector::eunomia_dir()
)

cdm <- CDMConnector::cdmFromCon(
  con = con,
  cdmSchema = "main",
  writeSchema = "main"
)

## Read in cohort set ----
cohortsSet <- CDMConnector::readCohortSet(
  path = system.file(package = "TreatmentPatterns", "exampleCohorts")
)

## Generate cohot set ----
cdm <- CDMConnector::generateCohortSet(
  cdm = cdm,
  cohortSet = cohortsSet,
  name = "cohort_table",
  computeAttrition = FALSE
)

cohorts <- data.frame(
  cohortId = cohortsSet$cohort_definition_id,
  cohortName = cohortsSet$cohort_name,
  type = c("event", "event", "event", "event", "exit", "event", "event", "target")
)

andromedaCDMC <- TreatmentPatterns::computePathways(
  cohorts = cohorts,
  cohortTableName = "cohort_table",
  cdm = cdm
)
