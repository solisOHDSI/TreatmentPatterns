library(TreatmentPatterns)
library(testthat)

test_that("void", {
  expect_error(TreatmentPatterns:::doCreateTreatmentHistory())
})

test_that("minimal", {
  expect_s3_class(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    exitCohortIds = NULL,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments), "tbl_Andromeda")
})

test_that("death", {
  expect_s3_class(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    exitCohortIds = c(20),
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments), "tbl_Andromeda")
})

test_that("event cohorts", {
  eventCohorts <- TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    exitCohortIds = NULL,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments) %>%
    collect() %>%
    suppressWarnings()
  expect_gt(nrow(eventCohorts), 0)
  expect_equal(
    nrow(eventCohorts[!(eventCohorts$event_cohort_id %in% eventCohortIds), ]),
    0)
})

test_that("non-dataframe current_cohorts", {
  currentCohorts <- c(1, 2)
  expect_error(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments))
})

test_that("too few columns currentCohorts", {
  # remove end_date column
  currentCohorts <- currentCohorts[, c("cohort_id", "person_id", "start_date")]
  expect_error(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments))
})

test_that("wrong column names dataframe currentCohorts", {
  colnames(currentCohorts) <- c("cohort", "person", "startDate", "endDate")
  expect_error(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments))
})

test_that("includeTreatments startDate", {
  eventCohorts <- TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    exitCohortIds = NULL,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = "startDate") %>%
    collect() %>%
    suppressWarnings()
  
  expect_equal(
    nrow(eventCohorts[eventCohorts$event_end_date <
                        eventCohorts$event_start_date, ]),
    0)
})

test_that("includeTreatments endDate", {
  eventCohorts <- TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    exitCohortIds = NULL,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = "endDate") %>% collect() %>% suppressWarnings()
  
  extendedEventCohorts <- inner_join(
    eventCohorts,
    currentCohorts[currentCohorts$cohort_id == "7", ],
    by = c("person_id"))
  
  expect_equal(
    nrow(extendedEventCohorts[extendedEventCohorts$event_end_date <
                        extendedEventCohorts$start_date, ]),
    2101)
})

test_that("includeTreatments other", {
  expect_warning(TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    exitCohortIds = NULL,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = "other"))
})

test_that("periodPriorToIndex", {
  periodPriorToIndex <- 365
  eventCohorts <- TreatmentPatterns:::doCreateTreatmentHistory(
    currentCohorts = currentCohorts,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    exitCohortIds = NULL,
    periodPriorToIndex = periodPriorToIndex,
    includeTreatments = includeTreatments) %>%
    collect() %>%
    suppressWarnings()
  
  
  extendedEventCohorts <- inner_join(
    eventCohorts,
    currentCohorts[currentCohorts$cohort_id == targetCohortId, ],
    by = c("person_id")) %>% suppressWarnings()
  expect_equal(
    nrow(extendedEventCohorts[(extendedEventCohorts$event_start_date -
                         extendedEventCohorts$event_start_date) >
                        periodPriorToIndex, ]),
    0)
})
