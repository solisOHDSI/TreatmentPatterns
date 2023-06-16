library(TreatmentPatterns)
library(testthat)

test_that("Void", {
  expect_error(
    constructPathways()
  )
})

test_that("Minimal", {
  expect_message(
    constructPathways(
      dataSettings = dataSettings,
      pathwaySettings = pathwaySettings,
      saveSettings = saveSettings
    ), "constructPathways done.")
})

test_that("Wrong variables", {
  expect_error(
    constructPathways(
      dataSettings = saveSettings,
      pathwaySettings = dataSettings,
      saveSettings = pathwaySettings
    ))
})
