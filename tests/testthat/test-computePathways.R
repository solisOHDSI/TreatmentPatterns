# Global ----
library(testthat)
library(TreatmentPatterns)

test_that("computePathways DatabaseConnector", {
  testthat::skip()
  expect_message(
    expect_message(
      expect_message(
        computePathways(
          cohorts = cohortsDBC,
          cohortTableName = "CohortTable",
          connectionDetails = globalConnectionDetails,
          cdmSchema = "main",
          resultSchema = "main"
        ),
        "After maxPathLength: 554"
      ),
      "After combinationWindow: 555"
    ),
    "Original number of rows: 8334"
  )
})

# CDMConnector ----
test_that("computePathways CDMConnector", {
  expect_message(
    computePathways(
      cdm = cdm,
      cohorts = cohortsCDMC,
      cohortTableName = "cohort_table"
    ),
    "After maxPathLength: 554"
  )
})

# Parameter sweep ----
test_that("includeTreatments", {
  expect_error(
    expect_error(
      computePathways(
        cohorts = cohortsDBC,
        cohortTableName = "CohortTable",
        connectionDetails = globalConnectionDetails,
        cdmSchema = "main",
        resultSchema = "main",
        includeTreatments = 0
      ),
      "Must be of type 'character'"
    ),
    "Must be a subset of.+'startDate','endDate'.+"
  )
})

test_that("periodPriorToIndex", {
  expect_error(
    computePathways(
      cohorts = cohortsDBC,
      cohortTableName = "CohortTable",
      connectionDetails = globalConnectionDetails,
      cdmSchema = "main",
      resultSchema = "main",
      periodPriorToIndex = "0"
    ),
    "Must be of type.+'numeric'"
  )
})

test_that("minEraDuration", {
  expect_error(
    computePathways(
      cohorts = cohortsDBC,
      cohortTableName = "CohortTable",
      connectionDetails = globalConnectionDetails,
      cdmSchema = "main",
      resultSchema = "main",
      minEraDuration = "0"
    ),
    "Must be of type.+'numeric'"
  )
})

test_that("splitEventCohorts", {
  expect_error(
    computePathways(
      cohorts = cohortsDBC,
      cohortTableName = "CohortTable",
      connectionDetails = globalConnectionDetails,
      cdmSchema = "main",
      resultSchema = "main",
      splitEventCohorts = 1
    ),
    "Must be of type.+'character'"
  )
})

test_that("splitTime", {
  expect_error(
    computePathways(
      cohorts = cohortsDBC,
      cohortTableName = "CohortTable",
      connectionDetails = globalConnectionDetails,
      cdmSchema = "main",
      resultSchema = "main",
      splitTime = "1"
    ),
    "Must be of type.+'numeric'"
  )
})

test_that("eraCollapseSize", {
  expect_error(
    computePathways(
      cohorts = cohortsDBC,
      cohortTableName = "CohortTable",
      connectionDetails = globalConnectionDetails,
      cdmSchema = "main",
      resultSchema = "main",
      eraCollapseSize = ""
    ),
    " Must be of type.+'numeric'"
  )
})

test_that("combinationWindow", {
  expect_error(
    computePathways(
      cohorts = cohortsDBC,
      cohortTableName = "CohortTable",
      connectionDetails = globalConnectionDetails,
      cdmSchema = "main",
      resultSchema = "main",
      combinationWindow = ""
    ),
    "Must be of type.+'numeric'"
  )
})

test_that("minPostCombinationDuration", {
  expect_error(
    computePathways(
      cohorts = cohortsDBC,
      cohortTableName = "CohortTable",
      connectionDetails = globalConnectionDetails,
      cdmSchema = "main",
      resultSchema = "main",
      minPostCombinationDuration = "Stuff"
    ),
    "Must be of.+type.+"
  )
})

test_that("filterTreatments", {
  expect_error(
    computePathways(
      cohorts = cohortsDBC,
      cohortTableName = "CohortTable",
      connectionDetails = globalConnectionDetails,
      cdmSchema = "main",
      resultSchema = "main",
      filterTreatments = ""
    ),
    "Must be a subset of"
  )
})

test_that("includeTreatments", {
  expect_error(
    computePathways(
      cohorts = cohortsDBC,
      cohortTableName = "CohortTable",
      connectionDetails = globalConnectionDetails,
      cdmSchema = "main",
      resultSchema = "main",
      maxPathLength = ""
    ),
    "Must be of type.+'numeric'"
  )
})

test_that("identical treatment timeframe", {
  # Setup connection
  localCon <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir())
  
  withr::defer({
    DBI::dbDisconnect(localCon, shutdown = TRUE)
  })

  # Setup local Cohorts
  localCohorts <- data.frame(
    cohortId = c(28, 6, 3,11),
    cohortName = c("Disease X", "Drug A", "Drug B","Drug C"),
    type = c("target", "event", "event", "event")
  )

  # Setup cohort table
  cohort_table <- tibble(
    cohort_definition_id = c(11, 6, 6, 3, 11, 6, 28),
    subject_id = rep(1, 7),
    cohort_start_date = as.Date(c(
      "2017-03-21", "2017-03-21", "2018-07-18", "2019-06-25",
      "2015-10-05", "2015-10-05", "2016-07-18"
    )),
    cohort_end_date = as.Date(c(
      "2017-06-04", "2017-06-04", "2018-09-11", "2019-07-22",
      "2015-11-29", "2016-10-23", "2020-10-23"
    ))
  )

  # Write cohort table to connection
  dplyr::copy_to(localCon, cohort_table, overwrite = TRUE)

  localCDM <- CDMConnector::cdmFromCon(
    con = localCon,
    cdmSchema = "main",
    writeSchema = "main",
    cohortTables = "cohort_table"
  )

  expect_message(
    andromeda <- TreatmentPatterns::computePathways(
      cohorts = localCohorts,
      cdm = localCDM,
      cohortTableName = "cohort_table"
    ),
    "LRFS Combinations: 1"
  )

  result <- andromeda$treatmentHistory %>%
    collect()

  expect_identical(result$eventCohortId, c("11+6", "6"))
})
