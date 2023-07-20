library(TreatmentPatterns)
library(testthat)
library(dplyr)

test_that("void", {
  expect_error(TreatmentPatterns:::doCombinationWindow())
})

# test_that("minimal", {
#   treatmentHistory <- TreatmentPatterns:::doCombinationWindow(
#       doEraCollapseTH,
#       combinationWindow,
#       minPostCombinationDuration) %>%
#     suppressWarnings()
# 
#   expect_s3_class(treatmentHistory, "tbl_Andromeda")
# })


# Test cases: Allen's interval algebra
# https://en.wikipedia.org/wiki/Allen%27s_interval_algebra

# Assume there are two eras to be combined. They are different events.
# Without loss of generality, assume A always starts before or on the
# same day that B starts.

## Possible relations for two intervals A and B (* means overlap)
# A equals B              *************
# A precedes B            aaaaaaa   bbb
# A meets B               aaaaaaabbbbbb
# A overlaps with B       aaaa****bbbbb
# A starts B              ********bbbbb
# A contains B            aaa*****aaaaa
# A is finished by B      aaaaaaaa*****

# test_that("case: A is equal to B", {
#   treatmentHistory <- tibble::tribble(
#     ~event_cohort_id,     ~person_id,   ~event_start_date,  ~event_end_date,
#     101,                   1,           "2020-05-01",       "2020-06-01",
#     102,                   1,           "2020-05-01",       "2020-06-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   # treatmentHistory[,
#   #    `:=`(duration_era = difftime(
#   #      event_end_date,
#   #      event_start_date, units = "days"),
#   #      index_year = as.numeric(format(event_start_date, "%Y")))]
#   
#   treatmentHistory <- treatmentHistory %>%
#     mutate(
#       duration_era = difftime(.data$event_end_date, .data$event_start_date, units = "days"),
#       index_year = as.numeric(format(event_start_date, "%Y")))
#   
# 
#   result <- TreatmentPatterns:::doCombinationWindow(
#     treatmentHistory = treatmentHistory,
#     combinationWindow = 1,
#     minPostCombinationDuration = 1) %>%
#     dplyr::select(-"duration_era", -"index_year") %>%
#     dplyr::collect() %>%
#     suppressWarnings()
#   
#   result$event_start_date <- Andromeda::restoreDate(result$event_start_date)
#   result$event_end_date <- Andromeda::restoreDate(result$event_end_date)
#   
#     # [,
#      # c("person_id", "event_cohort_id", "event_start_date", "event_end_date")]
# 
#   expectedResults <- tibble::tribble(
#    ~person_id, ~event_cohort_id, ~event_start_date, ~event_end_date,
#    1,          "102+101",        "2020-05-01",      "2020-06-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date))
# 
#   expect_equal(result, expectedResults)
# 
#   # case when eras are too short. But which will be chosen? A or B?
#   # invisible(capture.output({
#   #   result2 <- TreatmentPatterns:::doCombinationWindow(
#   #     treatmentHistory = treatmentHistory,
#   #     combinationWindow = 1000,
#   #     minPostCombinationDuration = 1) %>%
#   #     select(-"duration_era", -"index_year")
#   # }))
# })


# test_that("case: A precedes B", {
#   treatmentHistory <- tibble::tibble(
#     event_cohort_id = c(101, 102),
#     person_id = c(1, 1),
#     event_start_date = c("2020-05-01", "2020-06-02"),
#     event_end_date = c("2020-06-01", "2020-07-01")) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date))
# 
#   # treatmentHistory[,
#   #  `:=`(duration_era = difftime(
#   #    event_end_date,
#   #    event_start_date, units = "days"),
#   #    index_year = as.numeric(format(event_start_date, "%Y")))]
# 
#   treatmentHistory <- treatmentHistory %>%
#     mutate(
#       duration_era = difftime(.data$event_end_date, .data$event_start_date, units = "days"),
#       index_year = as.numeric(format(.data$event_start_date, "%Y")))
#   
#   result <- TreatmentPatterns:::doCombinationWindow(
#     treatmentHistory = treatmentHistory,
#     combinationWindow = 1,
#     minPostCombinationDuration = 1) %>%
#     select(-"duration_era", -"index_year") %>%
#     dplyr::collect() %>%
#     suppressWarnings()
# 
#   result$event_start_date <- Andromeda::restoreDate(result$event_start_date)
#   result$event_end_date <- Andromeda::restoreDate(result$event_end_date)
#   
#   expectedResult <- tibble::tibble(
#     event_cohort_id = c("101", "102"),
#     person_id = c(1,1), 
#     event_start_date = c("2020-05-01", "2020-06-02"),
#     event_end_date = c("2020-06-01", "2020-07-01")) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date))
# 
#   expect_equal(result, expectedResult)
# 
#   # case when min combination lenght and min post combination duration are large
#   result2 <- TreatmentPatterns:::doCombinationWindow(
#       treatmentHistory = treatmentHistory,
#       combinationWindow = 1000,
#       minPostCombinationDuration = 1000) %>%
#       select(-"duration_era", -"index_year") %>%
#     collect() %>%
#     suppressWarnings()
#   
#   result2$event_start_date <- Andromeda::restoreDate(result2$event_start_date)
#   result2$event_end_date <- Andromeda::restoreDate(result2$event_end_date)
# 
#   expect_equal(result2, expectedResult)
# })

# 
# test_that("case: A meets B", {
#   skip("failing test")
# 
#   treatmentHistory <- tibble::tibble(
#     event_cohort_id = c(101, 102),
#     person_id = c(1, 1),
#     event_start_date = c("2020-05-01", "2020-06-01"),
#     event_end_date = c("2020-06-01", "2020-07-01")) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date))
# 
#   # treatmentHistory[, `:=`(duration_era = difftime(
#   #   event_end_date,
#   #   event_start_date,
#   #   units = "days"),
#   #   index_year = as.numeric(format(event_start_date, "%Y")))]
# 
#   treatmentHistory <- treatmentHistory %>%
#     mutate(
#       duration_era = difftime(event_end_date, event_start_date, units = "days"),
#       index_year = as.numeric(format(event_start_date, "%Y"))
#     )
#   
#   invisible(capture.output({
#     result <- TreatmentPatterns:::doCombinationWindow(
#       treatmentHistory = treatmentHistory,
#       combinationWindow = 0,
#       minPostCombinationDuration = 0) %>%
#       select(-"duration_era", -"index_year")
#   }))
# 
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "101",            1,          "2020-05-01",      "2020-05-31",
#     "102+101",        1,          "2020-06-01",      "2020-06-01",
#     "102",            1,          "2020-06-02",      "2020-07-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date))
# 
#   expect_equal(result, expectedResult)
# 
#   # case when overlap < combinationWindow
#   invisible(capture.output({
#     result2 <- doCombinationWindow(treatmentHistory,
#                                    combinationWindow = 2,
#                                    minPostCombinationDuration = 0)[,
#       c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
# 
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "101",            1,          "2020-05-01",      "2020-05-31",
#     "102",            1,          "2020-06-01",      "2020-07-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   expect_equal(result2, expectedResult)
# 
#   # case when min post combination duration are large
#   invisible(capture.output({
#     result3 <- doCombinationWindow(treatmentHistory,
#                                    combinationWindow = 0,
#                                    minPostCombinationDuration = 1000)[,
#     c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
# 
#   # Note that minPostCombinationDuration restrict pre and post duration
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "102+101",        1,          "2020-06-01",      "2020-06-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   expect_equal(result3, expectedResult)
# })
# 
# 
# 
# test_that("case: A overlaps B", {
# 
#   skip("failing test")
# 
#   treatmentHistory <- tibble::tribble(
#     ~event_cohort_id,     ~person_id,   ~event_start_date,  ~event_end_date,
#     101,                   1,           "2020-05-01",       "2020-06-20",
#     102,                   1,           "2020-06-15",       "2020-07-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   treatmentHistory[,
#      `:=`(duration_era = difftime(
#        event_end_date,
#        event_start_date,
#        units = "days"),
#        index_year = as.numeric(format(event_start_date, "%Y")))]
# 
#   invisible(capture.output({
#     result <- doCombinationWindow(treatmentHistory,
#                                   combinationWindow = 0,
#                                   minPostCombinationDuration = 0)[,
#      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
# 
#   # This person should not be in an event an a combo event on the same day
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "101",            1,          "2020-05-01",      "2020-06-14",
#     "102+101",        1,          "2020-06-15",      "2020-06-20",
#     "102",            1,          "2020-06-21",      "2020-07-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   expect_equal(result, expectedResult)
# 
#   # case when overlap < combinationWindow
#   invisible(capture.output({
#     result2 <- doCombinationWindow(treatmentHistory,
#                                    combinationWindow = 30,
#                                    minPostCombinationDuration = 0)[,
#       c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
# 
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "101",            1,          "2020-05-01",      "2020-06-14",
#     "102",            1,          "2020-06-15",      "2020-07-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   expect_equal(result2, expectedResult)
# 
#   # case when minPostCombinationDuration is large
#   invisible(capture.output({
#     result3 <- doCombinationWindow(treatmentHistory,
#                                    combinationWindow = 0,
#                                    minPostCombinationDuration = 1000)[,
#      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
# 
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "102+101",        1,          "2020-06-15",      "2020-06-20"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   expect_equal(result3, expectedResult)
# })
# 
# 
# test_that("case: A starts B", {
# 
#   skip("failing test")
# 
#   treatmentHistory <- tibble::tribble(
#     ~event_cohort_id,     ~person_id,   ~event_start_date,  ~event_end_date,
#     101,                   1,           "2020-05-01",       "2020-06-01",
#     102,                   1,           "2020-05-01",       "2020-07-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   treatmentHistory[,
#     `:=`(duration_era = difftime(
#       event_end_date,
#       event_start_date,
#       units = "days"),
#       index_year = as.numeric(format(event_start_date, "%Y")))]
# 
#   invisible(capture.output({
#     result <- doCombinationWindow(treatmentHistory,
#                                   combinationWindow = 0,
#                                   minPostCombinationDuration = 0)[,
#       c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
# 
#   # This person should not be in an event an a combo event on the same day
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "102+101",        1,          "2020-05-01",      "2020-06-01",
#     "102",            1,          "2020-06-02",      "2020-07-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   expect_equal(result, expectedResult)
# 
#   # case when overlap < combinationWindow (i.e. A is contained in B but
#   # the overlap is not long enough to count as a combination)
#   invisible(capture.output({
#     result2 <- doCombinationWindow(treatmentHistory,
#                                    combinationWindow = 100,
#                                    minPostCombinationDuration = 0)[,
#      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
# 
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "102",        1,          "2020-05-01",      "2020-07-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   expect_equal(result2, expectedResult)
# 
#   # case when minPostCombinationDuration is large
#   invisible(capture.output({
#     result3 <- doCombinationWindow(treatmentHistory,
#                                    combinationWindow = 0,
#                                    minPostCombinationDuration = 1000)[,
#       c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
# 
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "102+101",        1,          "2020-05-01",      "2020-06-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   expect_equal(result3, expectedResult)
# })
# 
# 
# 
# test_that("case: A contains B", {
# 
#   skip("failing test")
# 
#   treatmentHistory <- tibble::tribble(
#     ~event_cohort_id,     ~person_id,   ~event_start_date,  ~event_end_date,
#     101,                   1,           "2020-05-01",       "2020-06-01",
#     102,                   1,           "2020-05-11",       "2020-05-21"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   treatmentHistory[,
#     `:=`(duration_era = difftime(
#       event_end_date,
#       event_start_date,
#       units = "days"),
#       index_year = as.numeric(format(event_start_date, "%Y")))]
# 
#   invisible(capture.output({
#     result <- doCombinationWindow(treatmentHistory,
#                                   combinationWindow = 0,
#                                   minPostCombinationDuration = 0)[,
#       c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
# 
#   # This person should not be in an event an a combo event on the same day
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "101",            1,          "2020-05-01",      "2020-05-10",
#     "102+101",        1,          "2020-05-11",      "2020-05-21",
#     "101",            1,          "2020-05-22",      "2020-06-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   expect_equal(result, expectedResult)
# 
#   # TODO: what should the result be in this case?
#   # case when overlap < combinationWindow (i.e. A is contained in B but
#   # the overlap is not long enough to count as a combination)
#   invisible(capture.output({
#     result2 <- doCombinationWindow(treatmentHistory,
#                                    combinationWindow = 100,
#                                    minPostCombinationDuration = 0)[,
#       c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
# 
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "101",        1,          "2020-05-01",      "2020-05-10",
#     "102",        1,          "2020-05-11",      "2020-05-21",
#     "101",        1,          "2020-05-22",      "2020-06-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   expect_equal(result2, expectedResult)
# 
#   # case when minPostCombinationDuration is large
#   invisible(capture.output({
#     result3 <- doCombinationWindow(treatmentHistory,
#                                    combinationWindow = 0,
#                                    minPostCombinationDuration = 1000)[,
#       c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
# 
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "102+101",        1,          "2020-05-11",      "2020-05-21"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   expect_equal(result3, expectedResult)
# })
# 
# test_that("case: A is finished by B", {
# 
#   skip("failing test")
# 
#   treatmentHistory <- tibble::tribble(
#     ~event_cohort_id,     ~person_id,   ~event_start_date,  ~event_end_date,
#     101,                   1,           "2020-05-01",       "2020-06-01",
#     102,                   1,           "2020-05-11",       "2020-06-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   treatmentHistory[,
#     `:=`(duration_era = difftime(
#       event_end_date,
#       event_start_date,
#       units = "days"),
#       index_year = as.numeric(format(event_start_date, "%Y")))]
# 
#   invisible(capture.output({
#     result <- doCombinationWindow(treatmentHistory,
#                                   combinationWindow = 0,
#                                   minPostCombinationDuration = 0)[,
#       c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
# 
#   # This person should not be in an event an a combo event on the same day
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "101",            1,          "2020-05-01",      "2020-05-10",
#     "102+101",        1,          "2020-05-11",      "2020-06-01",
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   expect_equal(result, expectedResult)
# 
#   # TODO: what should the result be in this case?
#   # case when overlap < combinationWindow (i.e. A is contained in B but
#   # the overlap is not long enough to count as a combination)
#   invisible(capture.output({
#     result2 <- doCombinationWindow(treatmentHistory,
#                                    combinationWindow = 100,
#                                    minPostCombinationDuration = 0)[,
#       c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
# 
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "101",            1,          "2020-05-01",      "2020-06-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   expect_equal(result2, expectedResult)
# 
#   # case when minPostCombinationDuration is large
#   invisible(capture.output({
#     result3 <- doCombinationWindow(treatmentHistory,
#                                    combinationWindow = 0,
#                                    minPostCombinationDuration = 1000)[,
#      c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
# 
#   expectedResult <- tibble::tribble(
#     ~event_cohort_id, ~person_id, ~event_start_date, ~event_end_date,
#     "102+101",        1,          "2020-05-11",      "2020-06-01"
#   ) %>%
#     dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.Date)) %>%
#     data.table::data.table()
# 
#   expect_equal(result3, expectedResult)
# 
#   # TODO what should the output be in this case?
#   # case when both combinationWindow and minPostCombinationDuration are large
#   invisible(capture.output({
#     result4 <- doCombinationWindow(treatmentHistory,
#                                    combinationWindow = 1000,
#                                    minPostCombinationDuration = 1000)[,
#       c("event_cohort_id", "person_id", "event_start_date", "event_end_date")]
#   }))
#   expect_equal(nrow(result3), 0)
# })
