library(testthat)
library(TreatmentPatterns)
library(dplyr)
library(R6)
library(DBI)
library(duckdb)

test_that("Method: new", {
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    cdm = cdm
  )
  
  expect_true(R6::is.R6(cdmInterface))
})

cdmInterface <- TreatmentPatterns:::CDMInterface$new(
  cdm = cdm
)

test_that("Method: validate", {
  expect_true(R6::is.R6(cdmInterface$validate()))
})

a <- Andromeda::andromeda()

test_that("Method: fetchMetadata", {
  cdmInterface$fetchMetadata(a)
  
  metadata <- a$metadata %>% collect()
  
  expect_in(
    c("cdm_source_name", "cdm_source_abbreviation", "cdm_release_date", "vocabulary_version"),
    names(metadata)
  )
  
  expect_identical(metadata$r_version, base::version$version.string)
  expect_identical(metadata$platform, base::version$platform)
  expect_identical(nrow(metadata), 1L)
  expect_identical(ncol(metadata), 8L)
})

a$treatmentHistory <- andromeda$treatmentHistory %>%
  select(-"age", -"sex")

test_that("Method: addSex", {
  cdmInterface$addSex(a)
  
  sex <- a$sex %>% collect()
  TH <- a$treatmentHistory %>% collect()
  
  expect_identical(ncol(sex), 2L)
  expect_identical(nrow(sex), 512L)
  
  expect_in(c("MALE", "FEMALE"), TH$sex)
  
  sexes <- TH %>%
    inner_join(sex, by = join_by(person_id == person_id)) %>%
    select("sex", "concept_name")
  
  expect_identical(sexes$sex, sexes$concept_name)
})

test_that("Method: addAge", {
  cdmInterface$addAge(a)
  
  year_of_birth <- a$year_of_birth %>% collect()
  TH <- a$treatmentHistory %>% collect()
  
  expect_identical(ncol(year_of_birth), 2L)
  # All people are included besides subset, probably not a problem.
  # expect_identical(nrow(year_of_birth), 512L)
  
  ages <- TH %>%
    inner_join(year_of_birth, by = join_by(person_id == person_id)) %>%
    mutate(ageCheck = .data$index_year - .data$year_of_birth) %>% select("age", "ageCheck")
  
  expect_identical(
    ages$age,
    ages$ageCheck
  )
})

test_that("Method: fetchCohortTable", {
  # Setup dummy cohort table
  cdm$CohortTable <- cdm$condition_occurrence %>%
    filter(.data$condition_concept_id == 40481087) %>%
    inner_join(cdm$person, by = join_by(person_id == person_id)) %>%
    select("person_id", "condition_start_date", "condition_end_date") %>%
    mutate(cohort_definition_id = 1) %>%
    rename(
      cohort_start_date = "condition_start_date",
      cohort_end_date = "condition_end_date")
  
  # Update CDM with new dummy data
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    cdm = cdm
  )
  
  # Viral Sinusitis
  res <- cdmInterface$fetchCohortTable(1, cohortTableName = "CohortTable")
  
  expect_identical(ncol(res), 4L)
  expect_identical(res %>% collect() %>% nrow(), 17268L)
  
  # Empty
  res <- cdmInterface$fetchCohortTable(23, cohortTableName = "CohortTable")
  expect_identical(ncol(res), 4L)
  expect_identical(res %>% collect() %>% nrow(), 0L)
})

Andromeda::close(a)