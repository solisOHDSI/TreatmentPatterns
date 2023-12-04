library(testthat)
library(TreatmentPatterns)

test_that("launchResultsExplorer", {
  app <- launchResultsExplorer()
  testthat::expect_s3_class(app, "shiny.appobj")
  
  rm(app)
  gc()
})
