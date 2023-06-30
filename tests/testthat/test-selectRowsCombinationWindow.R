library(usethis)
library(TreatmentPatterns)
library(testthat)

test_that("void", {
  expect_error(
    TreatmentPatterns:::selectRowsCombinationWindow())
})

test_that("minimal", {
  suppressWarnings(expect_s3_class(
    TreatmentPatterns:::selectRowsCombinationWindow(doEraCollapseTH),
    "tbl_Andromeda"))
})

test_that("validate GAP_PREVIOUS", {
  treatmentHistoryCW <- TreatmentPatterns:::selectRowsCombinationWindow(
    doEraCollapseTH) %>% dplyr::collect() %>% suppressWarnings()

  x <- treatmentHistoryCW %>% dplyr::select("GAP_PREVIOUS") %>% dplyr::pull()
  # Old data.table implementation
  # y <- treatmentHistoryCW[, GAP_PREVIOUS := difftime(
  #   event_start_date,
  #   data.table::shift(event_end_date, type = "lag"), units = "days"),
  #   by = person_id]
  
  y <- treatmentHistoryCW %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::mutate(GAP_PREVIOUS = difftime(.data$event_start_date, dplyr::lag(.data$event_end_date), units = "days"))
  
  y <- y %>% dplyr::select("GAP_PREVIOUS") %>% dplyr::pull()
  y <- as.integer(y)

  expect_true(identical(x, y))
})

test_that("validate SELECTED_ROWS", {
  treatmentHistoryCW <- TreatmentPatterns:::selectRowsCombinationWindow(
    doEraCollapseTH) %>% dplyr::collect() %>% suppressWarnings()

  # 0 NULL / NA
  expect_equal(sum(is.na(treatmentHistoryCW$SELECTED_ROWS)), 0)
  # Min: 0
  expect_equal(min(treatmentHistoryCW$SELECTED_ROWS), 0)
  # Max: 1
  expect_equal(max(treatmentHistoryCW$SELECTED_ROWS), 1)
})
