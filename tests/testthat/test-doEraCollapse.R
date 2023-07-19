library(TreatmentPatterns)
library(testthat)

test_that("void", {
  expect_error(TreatmentPatterns:::doEraCollapse())
})

# test_that("minimal", {
#   # update generic test variables
#   nrows <- doSplitEventCohortsTH %>% summarise(n = n()) %>% pull() %>% suppressWarnings()
#   eraCollapseSize <- 1.5
#   
#   treatmentHistoryFiltered <- doEraCollapse(
#     doSplitEventCohortsTH,
#     eraCollapseSize) %>% collect() %>% suppressWarnings()
# 
#   expect_s3_class(treatmentHistoryFiltered, "data.frame")
#   expect_true(
#     nrow(treatmentHistoryFiltered) == nrows)
# })
# 
# test_that("invalid_input", {
#   expect_error(
#     TreatmentPatterns:::doEraCollapse(treatmentHistory = NULL))
# 
#   expect_error(
#     TreatmentPatterns:::doEraCollapse(
#       treatmentHistory = treatment_history,
#       eraCollapseSize = NULL))
# 
#   expect_error(TreatmentPatterns:::doEraCollapse(
#     treatmentHistory = treatment_history,
#     eraCollapseSize = mtcars))
# })
