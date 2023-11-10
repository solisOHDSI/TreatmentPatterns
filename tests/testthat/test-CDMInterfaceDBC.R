library(testthat)
library(TreatmentPatterns)
library(dplyr)
library(Eunomia)
library(DatabaseConnector)

test_that("Method: new", {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )
  
  expect_true(R6::is.R6(
    TreatmentPatterns:::CDMInterface$new(
      connectionDetails = connectionDetails,
      cdmSchema = "main",
      resultSchema = "main"
    )
  ))
})

test_that("Method: validate", {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )
  
  expect_true(R6::is.R6(cdmInterface$validate()))
})

test_that("Method: fetchMetadata", {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )
  
  andromeda <- Andromeda::andromeda()

  cdmInterface$fetchMetadata(andromeda)

  metadata <- andromeda$metadata %>% collect()

  expect_in(
    c("cdmSourceName", "cdmSourceAbbreviation", "cdmReleaseDate", "vocabularyVersion"),
    names(metadata)
  )

  expect_identical(metadata$rVersion, base::version$version.string)
  expect_identical(metadata$platform, base::version$platform)
  expect_identical(nrow(metadata), 1L)
  expect_identical(ncol(metadata), 8L)
})

test_that("Method: fetchCohortTable", {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  
  connection <- DatabaseConnector::connect(connectionDetails)
  
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "
  DROP TABLE IF EXISTS cohort_table;

  CREATE TABLE cohort_table (
    cohort_definition_id INT,
    subject_id INT,
    cohort_start_date DATE,
    cohort_end_date DATE
  );

  INSERT INTO cohort_table (
    cohort_definition_id,
    subject_id,
    cohort_start_date,
    cohort_end_date
  ) VALUES
  (3, 1, 2014-10-10, 2015-08-01),
  (2, 1, 2014-11-17, 2014-12-04),
  (1, 1, 2014-10-10, 2015-08-01);
  "
  )
  
  DatabaseConnector::disconnect(connection)
  
  andromeda <- Andromeda::andromeda()
  
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("Disease X", "Drug A", "Drug B"),
    type = c("target", "event", "event")
  )

  # Viral Sinusitis
  cdmInterface$fetchCohortTable(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    andromeda = andromeda,
    andromedaTableName = "cohortTable",
    minEraDuration = 0
  )

  res <- andromeda$cohortTable %>% dplyr::collect()

  expect_identical(ncol(res), 6L)
  expect_identical(nrow(res), 3L)

  # Empty
  cdmInterface$fetchCohortTable(
    cohorts = data.frame(
      cohortId = numeric(),
      cohortName = character(),
      type = character()
    ),
    cohortTableName = "cohort_table",
    andromeda = andromeda,
    andromedaTableName = "cohortTable",
    minEraDuration = 5
  )

  res <- andromeda$cohortTable %>% dplyr::collect()

  expect_identical(ncol(res), 6L)
  expect_identical(nrow(res), 0L)
  
  cdmInterface$destroy()
})
