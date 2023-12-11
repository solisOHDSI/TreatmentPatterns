library(testthat)
library(TreatmentPatterns)

globals <- generateCohortTableCDMC()

cdmInterface <- TreatmentPatterns:::CDMInterface$new(cdm = globals$cdm)

test_that("new", {
  pathwayConstructor <- TreatmentPatterns:::PathwayConstructor$new(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdmInterface = cdmInterface
  )
  
  expect_equal(class(pathwayConstructor), c("PathwayConstructor", "R6"))
})

test_that("getSettings", {
  pathwayConstructor <- TreatmentPatterns:::PathwayConstructor$new(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdmInterface = cdmInterface
  )
  
  res <- pathwayConstructor$getSettings()

  expect_equal(length(res), 13)
  expect_equal(class(res), "list")
})

test_that("editSettings", {
  pathwayConstructor <- TreatmentPatterns:::PathwayConstructor$new(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdmInterface = cdmInterface
  )
  
  before <- pathwayConstructor$getSettings()
  
  pathwayConstructor$editSettings(minEraDuration = 100)
  
  after <- pathwayConstructor$getSettings()
  
  expect_false(identical(before, after))
})

test_that("getAndromeda", {
  pathwayConstructor <- TreatmentPatterns:::PathwayConstructor$new(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdmInterface = cdmInterface
  )
  
  res <- pathwayConstructor$getAndromeda()
  
  expect_null(res)
})

test_that("construct", {
  pathwayConstructor <- TreatmentPatterns:::PathwayConstructor$new(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdmInterface = cdmInterface
  )
  
  pathwayConstructor$construct()
  
  res <- pathwayConstructor$getAndromeda()
  
  expect_true(Andromeda::isAndromeda(res))
})
