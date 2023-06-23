library(TreatmentPatterns)
library(testthat)

# === Compute prerequisites ====
doSetDurationTH <- doEraDurationTH

doSetDurationTH$event_cohort_id <- as.character(
  doSetDurationTH$event_cohort_id)

doSetDurationTH <- TreatmentPatterns:::selectRowsCombinationWindow(
  doSetDurationTH)

doSetDurationTH <- doSetDurationTH %>%
  mutate(switch = case_when(
    .data$SELECTED_ROWS == 1 &
      (-.data$GAP_PREVIOUS < combinationWindow &
         !(-.data$GAP_PREVIOUS == .data$duration_era |
             -GAP_PREVIOUS == dplyr::lag(.data$duration_era))) ~ 1,
    .default = 0
  ))

doSetDurationTH <- doSetDurationTH %>%
  mutate(combination_FRFS = case_when(
    SELECTED_ROWS == 1 & switch == 0 & dplyr::lag(event_end_date) <= event_end_date ~ 1,
    .default = 0
  ))

doSetDurationTH <- doSetDurationTH %>%
  dplyr::mutate(combination_LRFS = dplyr::case_when(
    .data$SELECTED_ROWS == 1 & .data$switch == 0 & dplyr::lag(.data$event_end_date) > .data$event_end_date ~ 1,
    .default = 0
  ))

sumSwitchComb <- sum(
  sum(doSetDurationTH$switch, na.rm = TRUE),
  sum(doSetDurationTH$combination_FRFS, na.rm = TRUE),
  sum(doSetDurationTH$combination_LRFS, na.rm = TRUE))

sumSelectedRows <- sum(doSetDurationTH$SELECTED_ROWS)

if (sumSwitchComb != sumSelectedRows) {
  warning(paste0(
    sum(doSetDurationTH$SELECTED_ROWS),
    " does not equal total sum ",
    sum(!is.na(doSetDurationTH$switch)) +
      sum(!is.na(doSetDurationTH$combination_FRFS)) +
      sum(!is.na(doSetDurationTH$combination_LRFS))))
}

# Do transformations for each of the three newly added columns
# Construct helpers

doSetDurationTH <- doSetDurationTH %>%
  dplyr::group_by(.data$person_id) %>%
  dplyr::mutate(event_start_date_next = dplyr::lead(.data$event_start_date))

doSetDurationTH <- doSetDurationTH %>%
  dplyr::group_by(.data$person_id) %>%
  dplyr::mutate(event_end_date_previous = dplyr::lag(.data$event_end_date))

doSetDurationTH <- doSetDurationTH %>%
  dplyr::group_by(.data$person_id) %>%
  dplyr::mutate(event_end_date_next = dplyr::lead(.data$event_end_date))

doSetDurationTH <- doSetDurationTH %>%
  dplyr::group_by(.data$person_id) %>%
  dplyr::mutate(event_cohort_id_previous = dplyr::lag(.data$event_cohort_id)) %>%
  dplyr::ungroup()

doSetDurationTH <- doSetDurationTH %>%
  dplyr::mutate(event_end_date = dplyr::case_when(
    dplyr::lead(.data$switch) == 1 ~ .data$event_start_date_next,
    .default = .data$event_end_date
  ))

addRowsFRFS <- doSetDurationTH %>%
  dplyr::filter(.data$combination_FRFS == 1)

addRowsFRFS <- addRowsFRFS %>%
  dplyr::mutate(event_end_date = .data$event_end_date_previous)

addRowsFRFS <- addRowsFRFS %>%
  dplyr::mutate(event_cohort_id = paste0(.data$event_cohort_id, "+", .data$event_cohort_id_previous))

doSetDurationTH <- doSetDurationTH %>%
  mutate(
    event_end_date = dplyr::case_when(
      dplyr::lead(.data$combination_FRFS) == 1 ~ event_start_date_next,
      .default = .data$event_end_date),
    check_duration = dplyr::case_when(dplyr::lead(.data$combination_FRFS) == 1 ~ 1))

doSetDurationTH <- doSetDurationTH %>%
  dplyr::mutate(
    event_start_date = dplyr::case_when(
      .data$combination_FRFS == 1 ~ .data$event_end_date_previous,
      .default = .data$event_start_date),
    check_duration = dplyr::case_when(
      .data$combination_FRFS == 1 ~ 1,
      .default = .data$check_duration))

doSetDurationTH <- doSetDurationTH %>%
  dplyr::mutate(event_cohort_id = dplyr::case_when(
    .data$combination_LRFS == 1 ~ paste0(.data$event_cohort_id, "+", .data$event_cohort_id_previous),
    .default = .data$event_cohort_id
  ))

addRowsLRFS <- doSetDurationTH %>%
  dplyr::filter(lead(.data$combination_LRFS) == 1)

addRowsLRFS <- addRowsLRFS %>%
  dplyr::mutate(
    event_start_date = .data$event_end_date_next,
    check_duration = 1)

doSetDurationTH <- doSetDurationTH %>%
  dplyr::mutate(
    event_end_date = dplyr::case_when(
      dplyr::lead(.data$combination_LRFS) == 1 ~ .data$event_start_date_next,
      .default = .data$event_end_date),
    check_duration = dplyr::case_when(
      dplyr::lead(.data$combination_LRFS) == 1 ~ 1,
      .default = .data$check_duration
    ))

doSetDurationTH <- doSetDurationTH %>%
  dplyr::bind_rows(addRowsFRFS, addRowsLRFS) %>%
  dplyr::mutate(
    duration_era = difftime(event_end_date, event_start_date, units = "days"))

# === Tests ====
test_that("void", {
  expect_error(TreatmentPatterns:::doStepDuration())
})

test_that("minimal", {
  expect_s3_class(TreatmentPatterns:::doStepDuration(
    treatmentHistory = doSetDurationTH,
    minPostCombinationDuration = 30), "data.frame")
})

doStepDurationOut <- TreatmentPatterns:::doStepDuration(
  treatmentHistory = doSetDurationTH,
  minPostCombinationDuration = 30)

test_that("Max TH == Out", {
  expect_true(
    max(doStepDurationOut$duration_era) == max(doSetDurationTH$duration_era))
})

test_that("Min Out == 30 & != TH", {
  expect_false(
    min(doStepDurationOut$duration_era) == min(doSetDurationTH$duration_era) &&
      min(doStepDurationOut$duration_era) == 30)
})
