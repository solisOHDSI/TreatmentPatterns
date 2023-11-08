library(testthat)
library(TreatmentPatterns)
library(dplyr)
library(CDMConnector)

localAndromeda <- Andromeda::andromeda()

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())

cohorts <- data.frame(
  cohortId = c(1, 2, 3),
  cohortName = c("Disease X", "Drug A", "Drug B"),
  type = c("target", "event", "event")
)

cohort_table <- tibble(
  cohort_definition_id = c(3, 2, 1),
  subject_id = c(1, 1, 1),
  cohort_start_date = as.Date(c("2014-10-10", "2014-11-07", "2014-10-10")),
  cohort_end_date = as.Date(c("2015-08-01", "2014-12-04", "2015-08-01"))
)

copy_to(con, cohort_table, overwrite = TRUE)

cdm <- cdmFromCon(
  con = con,
  cdmSchema = "main",
  writeSchema = "main",
  cohortTables = "cohort_table"
)

cdmInterface <- TreatmentPatterns:::CDMInterface$new(
  cdm = cdm
)

test_that("Method: new", {
  expect_true(R6::is.R6(
    TreatmentPatterns:::CDMInterface$new(
      cdm = cdm
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
  # Update CDM with new dummy data
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    cdm = cdm
  )
  
  # Viral Sinusitis
  cdmInterface$fetchCohortTable(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    andromeda = localAndromeda,
    andromedaTableName = "cohortTable",
    minEraDuration = 5
  )
  
  res <- localAndromeda$cohortTable

  expect_identical(ncol(res), 6L)
  expect_identical(res %>% collect() %>% nrow(), 3L)

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
  
  res <- localAndromeda$cohortTable
  
  expect_identical(ncol(res), 6L)
  expect_identical(res %>% collect() %>% nrow(), 0L)
})
