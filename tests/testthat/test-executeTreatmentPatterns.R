library(testthat)
library(TreatmentPatterns)

test_that("void", {
  expect_error(TreatmentPatterns::executeTreatmentPatterns())
})

# test_that("minimal", {
#   suppressWarnings(
#     TreatmentPatterns::executeTreatmentPatterns(
#       dataSettings = dataSettings,
#       pathwaySettings = pathwaySettings,
#       saveSettings = saveSettings,
#       cohortSettings = cohortSettings
#     )
#   )
# 
#   expect_true(
#     file.exists(normalizePath(file.path(saveSettings$rootFolder, "output.zip")))
#   )
# })
