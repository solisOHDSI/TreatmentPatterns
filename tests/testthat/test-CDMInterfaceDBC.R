library(testthat)
library(TreatmentPatterns)
library(dplyr)
library(Eunomia)
library(DatabaseConnector)

localAndromeda <- Andromeda::andromeda()

cohorts <- data.frame(
  cohortId = c(1, 2, 3),
  cohortName = c("Disease X", "Drug A", "Drug B"),
  type = c("target", "event", "event")
)

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

cdmInterface <- TreatmentPatterns:::CDMInterface$new(
  connectionDetails = connectionDetails,
  cdmSchema = "main",
  resultSchema = "main"
)

withr::defer({
  Andromeda::close(localAndromeda)
  rm("localAndromeda", "cdmInterface", "connection", "connectionDetails", "cohorts")
})

test_that("Method: new", {
  expect_true(R6::is.R6(
    TreatmentPatterns:::CDMInterface$new(
      connectionDetails = connectionDetails,
      cdmSchema = "main",
      resultSchema = "main"
    )
  ))
})

test_that("Method: validate", {
  expect_true(R6::is.R6(cdmInterface$validate()))
})

test_that("Method: fetchMetadata", {
  cdmInterface$fetchMetadata(localAndromeda)

  metadata <- localAndromeda$metadata %>% collect()

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
  # Viral Sinusitis
  cdmInterface$fetchCohortTable(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    andromeda = localAndromeda,
    andromedaTableName = "cohortTable",
    minEraDuration = 5
  )

  res <- localAndromeda$cohortTable %>% dplyr::collect()

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
    andromeda = localAndromeda,
    andromedaTableName = "cohortTable",
    minEraDuration = 5
  )

  res <- localAndromeda$cohortTable %>% dplyr::collect()

  expect_identical(ncol(res), 6L)
  expect_identical(nrow(res), 0L)
})
