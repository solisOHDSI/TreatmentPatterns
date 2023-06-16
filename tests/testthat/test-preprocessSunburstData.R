library(testthat)
library(TreatmentPatterns)

test_that("void", {
  expect_error(TreatmentPatterns:::preprocessSunburstData())
})

test_that("minimal", {
  expect_message(TreatmentPatterns:::preprocessSunburstData(
    data = treatmentPathways[[1]],
    tempFolder = saveSettings$tempFolder,
    outputFolder = saveSettings$outputFolder,
    databaseName = saveSettings$databaseName,
    studyName = "Viral_Sinusitis",
    eventCohortIds = eventCohortIds,
    addNoPaths = FALSE
  ), "preprocessSunburstData done")
})
