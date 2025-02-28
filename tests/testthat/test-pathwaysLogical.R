library(testthat)
library(TreatmentPatterns)
library(dplyr)
library(CDMConnector)

test_that("A", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     5,           as.Date("2020-01-10"), as.Date("2020-03-10")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A")
  
  DBI::dbDisconnect(con)
})

test_that("A-B", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     5,           as.Date("2020-01-10"), as.Date("2020-03-10"),
    3,                     5,           as.Date("2020-03-12"), as.Date("2020-05-12")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A-B")
  
  DBI::dbDisconnect(con)
})


test_that("A-B-C", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4),
    cohortName = c("X", "A", "B", "C"),
    type = c("target", "event", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     5,           as.Date("2020-01-10"), as.Date("2020-03-10"),
    3,                     5,           as.Date("2020-03-12"), as.Date("2020-05-12"),
    4,                     5,           as.Date("2020-05-14"), as.Date("2020-07-14")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A-B-C")
  
  DBI::dbDisconnect(con)
})

test_that("A-B+C", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4),
    cohortName = c("X", "A", "B", "C"),
    type = c("target", "event", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     1,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     1,           as.Date("2020-01-03"), as.Date("2020-03-02"),
    3,                     1,           as.Date("2020-03-10"), as.Date("2020-05-10"),
    4,                     1,           as.Date("2020-03-10"), as.Date("2020-05-10")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A-B+C")
  
  DBI::dbDisconnect(con)
})

test_that("A+B-C", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4),
    cohortName = c("X", "A", "B", "C"),
    type = c("target", "event", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     5,           as.Date("2020-01-10"), as.Date("2020-03-10"),
    3,                     5,           as.Date("2020-01-10"), as.Date("2020-03-10"),
    4,                     5,           as.Date("2020-03-12"), as.Date("2020-05-12")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A+B-C")
  
  DBI::dbDisconnect(con)
})

test_that("A-A+B", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     1,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     1,           as.Date("2020-01-03"), as.Date("2020-03-02"),
    2,                     1,           as.Date("2020-03-10"), as.Date("2020-05-10"),
    3,                     1,           as.Date("2020-03-10"), as.Date("2020-05-10")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
    
  expect_identical(path, "A-A+B")
  
  DBI::dbDisconnect(con)
})

test_that("A-B-A-B", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     2,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     2,           as.Date("2020-01-03"), as.Date("2020-03-02"),
    3,                     2,           as.Date("2020-03-10"), as.Date("2020-05-10"),
    2,                     2,           as.Date("2020-05-12"), as.Date("2020-07-12"),
    3,                     2,           as.Date("2020-07-14"), as.Date("2020-09-14")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A-B-A-B")
  
  DBI::dbDisconnect(con)
})

test_that("A-B-A", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     3,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     3,           as.Date("2020-01-03"), as.Date("2020-03-02"),
    3,                     3,           as.Date("2020-03-10"), as.Date("2020-05-10"),
    2,                     3,           as.Date("2020-05-12"), as.Date("2020-07-12")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A-B-A")
  
  DBI::dbDisconnect(con)
})

test_that("A-B-B, collapse to A-B", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     7,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     7,           as.Date("2020-01-03"), as.Date("2020-03-02"),
    3,                     7,           as.Date("2020-03-10"), as.Date("2020-05-10"),
    3,                     7,           as.Date("2020-05-12"), as.Date("2020-06-12")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A-B")
  
  DBI::dbDisconnect(con)
})

test_that("A-B-B", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     7,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     7,           as.Date("2020-01-03"), as.Date("2020-03-02"),
    3,                     7,           as.Date("2020-03-10"), as.Date("2020-05-10"),
    3,                     7,           as.Date("2020-06-12"), as.Date("2020-07-12")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A-B-B")
  
  DBI::dbDisconnect(con)
})

test_that("A+B-A", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     5,           as.Date("2020-01-10"), as.Date("2020-03-10"),
    3,                     5,           as.Date("2020-01-10"), as.Date("2020-03-10"),
    2,                     5,           as.Date("2020-03-12"), as.Date("2020-05-12")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A+B-A")
  
  DBI::dbDisconnect(con)
})

test_that("A-A-B", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     6,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     6,           as.Date("2020-01-10"), as.Date("2020-03-10"),
    2,                     6,           as.Date("2020-04-12"), as.Date("2020-06-12"),
    3,                     6,           as.Date("2020-06-14"), as.Date("2020-08-14")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A-A-B")
  
  DBI::dbDisconnect(con)
})

test_that("A-A-B, collapse to A-B", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     6,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     6,           as.Date("2020-01-10"), as.Date("2020-03-10"),
    2,                     6,           as.Date("2020-03-12"), as.Date("2020-05-12"),
    3,                     6,           as.Date("2020-05-14"), as.Date("2020-06-14")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A-B")
  
  DBI::dbDisconnect(con)
})

test_that("A+B-A+B, collapse to A+B", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     5,           as.Date("2020-01-10"), as.Date("2020-03-10"),
    3,                     5,           as.Date("2020-01-10"), as.Date("2020-03-10"),
    2,                     5,           as.Date("2020-03-12"), as.Date("2020-05-12"),
    3,                     5,           as.Date("2020-03-12"), as.Date("2020-05-12")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A+B")
  
  DBI::dbDisconnect(con)
})

test_that("A+B-A+B", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     5,           as.Date("2020-01-10"), as.Date("2020-03-10"),
    3,                     5,           as.Date("2020-01-10"), as.Date("2020-03-10"),
    2,                     5,           as.Date("2020-04-12"), as.Date("2020-06-12"),
    3,                     5,           as.Date("2020-04-12"), as.Date("2020-06-12")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A+B-A+B")
  
  DBI::dbDisconnect(con)
})

test_that("start event == start target", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2),
    cohortName = c("X", "A"),
    type = c("target", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     5,           as.Date("2014-10-10"), as.Date("2015-07-01")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A")
  
  DBI::dbDisconnect(con)
})

test_that("end event == end target", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2),
    cohortName = c("X", "A"),
    type = c("target", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     5,           as.Date("2014-10-15"), as.Date("2015-08-01")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A")
  
  DBI::dbDisconnect(con)
})

test_that("start-end event == start-end target", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2),
    cohortName = c("X", "A"),
    type = c("target", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A")
  
  DBI::dbDisconnect(con)
})

test_that("start event < start target", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())

  cohorts <- data.frame(
    cohortId = c(1, 2),
    cohortName = c("X", "A"),
    type = c("target", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     5,           as.Date("2014-09-10"), as.Date("2015-08-01")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )

  tempDir <- tempdir()
  expect_message(
    TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1),
    "Treatment History table is empty. Nothing to export.")

  DBI::dbDisconnect(con)
})

test_that("start event < start target, periodPrior = 60", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2),
    cohortName = c("X", "A"),
    type = c("target", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     5,           as.Date("2014-09-10"), as.Date("2015-08-01")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 60,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A")
  
  DBI::dbDisconnect(con)
})

test_that("start event > end target", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2),
    cohortName = c("X", "A"),
    type = c("target", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     5,           as.Date("2015-08-01"), as.Date("2015-10-01")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  expect_message(
    TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1),
    "Treatment History table is empty. Nothing to export.")
  
  DBI::dbDisconnect(con)
})
