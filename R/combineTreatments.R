determineSwitch <- function(andromeda, combinationWindow) {
  andromeda$treatmentHistory %>%
    dplyr::mutate(switch = case_when(
      .data$selectedRows == 1 &
        -.data$gapPrevious < combinationWindow &
        !(-.data$gapPrevious == .data$durationEra |
            -gapPrevious == dplyr::lag(.data$durationEra, order_by = .data$sortOrder)) ~ 1,
      .default = 0
    ))
}

determineFRFS <- function(treatmentHistory) {
  treatmentHistory %>%
    dplyr::mutate(combinationFRFS = case_when(
      .data$selectedRows == 1 &
        switch == 0 &
        dplyr::lag(eventEndDate, order_by = .data$sortOrder) < eventEndDate ~ 1,
      .default = 0
    ))
}

determineLRFS <- function(treatmentHistory) {
  treatmentHistory %>%
    dplyr::mutate(combinationLRFS = dplyr::case_when(
      .data$selectedRows == 1 &
        .data$switch == 0 &
        (dplyr::lag(.data$eventEndDate, order_by = .data$sortOrder) >= .data$eventEndDate |
           (dplyr::lead(.data$durationEra, order_by = .data$sortOrder) == .data$durationEra &
              dplyr::lead(.data$eventEndDate, order_by = .data$sortOrder) == .data$eventEndDate &
              dplyr::lead(.data$eventStartDate, order_by = .data$sortOrder) == .data$eventStartDate)) ~ 1,
      .default = 0
    ))
}

reportCounts <- function(andromeda, iteration) {
  nSelected <- andromeda$treatmentHistory %>%
    dplyr::summarise(sum = sum(.data$selectedRows, na.rm = TRUE)) %>%
    dplyr::pull()
  
  nTotal <- andromeda$treatmentHistory %>%
    dplyr::count() %>%
    dplyr::pull()
  
  nSwitches <- andromeda$treatmentHistory %>%
    dplyr::summarise(sum = sum(!is.na(.data$switch), na.rm = TRUE)) %>%
    dplyr::pull()
  
  nFRFS <- andromeda$treatmentHistory %>%
    dplyr::summarise(sum = sum(.data$combinationFRFS, na.rm = TRUE)) %>%
    dplyr::pull()
  
  nLRFS <- andromeda$treatmentHistory %>%
    dplyr::summarise(sum = sum(.data$combinationLRFS, na.rm = TRUE)) %>%
    dplyr::pull()
  
  message(
    sprintf("Iteration: %s\n", iteration),
    sprintf("Selected %s out of %s rows\n", nSelected, nTotal),
    sprintf("Switches: %s", nSwitches),
    sprintf("FRFS Combinations: %s\n", nFRFS),
    sprintf("LRFS Combinations: %s\n", nLRFS)
  )
}

sumSwitchesCombinations <- function(andromeda) {
  andromeda$treatmentHistory %>%
    filter(
      .data$switch == 1 |
        .data$combinationFRFS == 1 |
        .data$combinationLRFS == 1) %>%
    summarise(n()) %>%
    pull()
}

sumSelectedRows <- function(andromeda) {
  andromeda$treatmentHistory %>%
    dplyr::summarise(sum = sum(.data$selectedRows, na.rm = TRUE)) %>%
    dplyr::pull()
}

checkCombinationCounts <- function(andromeda) {
  if (sumSwitchesCombinations(andromeda) != sumSelectedRows(andromeda)) {
    stop(sprintf("Expected switches before combination (%s) to be equal to switches after combination (%s)", sumSelectedRows(andromeda), sumSwitchesCombinations(andromeda)))
  }
}

determineNext <- function(treatmentHistory) {
  treatmentHistory %>%
    dplyr::group_by(.data$personId) %>%
    dplyr::mutate(
      eventStartDateNext = dplyr::lead(
        .data$eventStartDate,
        order_by = .data$eventStartDate),
      eventEndDateNext = dplyr::lead(
        .data$eventEndDate,
        order_by = .data$eventStartDate)
    ) %>%
    dplyr::ungroup()
}

determinePrevious <- function(treatmentHistory) {
  treatmentHistory %>%
    dplyr::group_by(.data$personId) %>%
    dplyr::mutate(
      eventEndDatePrevious = dplyr::lag(
        .data$eventEndDate,
        order_by = .data$eventStartDate),
      eventCohortIdPrevious = dplyr::lag(
        .data$eventCohortId,
        order_by = .data$eventStartDate)
    ) %>%
    dplyr::ungroup()
}

setEventEndDate <- function(treatmentHistory) {
  treatmentHistory %>%
    dplyr::mutate(
      eventEndDate = dplyr::case_when(
        dplyr::lead(.data$switch) == 1 ~ .data$eventStartDateNext,
        .default = .data$eventEndDate)
    )
}

determineNextPreviousData <- function(andromeda) {
  andromeda$treatmentHistory %>%
    determineNext() %>%
    determinePrevious() %>%
    setEventEndDate()
}

combineFRFS <- function(andromeda, iteration) {
  andromeda[[sprintf("addRowsFRFS_%s", iteration)]] <- andromeda$treatmentHistory %>%
    dplyr::filter(.data$combinationFRFS == 1) %>%
    dplyr::mutate(
      eventEndDate = .data$eventEndDatePrevious,
      eventCohortId = paste0(.data$eventCohortId, "+", .data$eventCohortIdPrevious)
    )
}

shiftFRFSStartDate <- function(treatmentHistory) {
  treatmentHistory %>%
    dplyr::mutate(
      eventEndDate = dplyr::case_when(
        dplyr::lead(.data$combinationFRFS) == 1 ~ eventStartDateNext,
        .default = .data$eventEndDate
      ),
      checkDuration = dplyr::case_when(dplyr::lead(.data$combinationFRFS) == 1 ~ 1)
    )
}

shiftFRFSEndDate <- function(treatmentHistory) {
  treatmentHistory %>%
    dplyr::mutate(
      eventStartDate = dplyr::case_when(
        .data$combinationFRFS == 1 ~ .data$eventEndDatePrevious,
        .default = .data$eventStartDate
      ),
      checkDuration = dplyr::case_when(
        .data$combinationFRFS == 1 ~ 1,
        .default = .data$checkDuration
      )
    )
}

combineLRFS <- function(andromeda, iteration) {
  andromeda$treatmentHistory <- treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(eventCohortId = dplyr::case_when(
      .data$combinationLRFS == 1 ~ paste0(.data$eventCohortId, "+", .data$eventCohortIdPrevious),
      .default = .data$eventCohortId
    ))
  
  andromeda[[sprintf("addRowsLRFS_%s", iteration)]] <- andromeda$treatmentHistory %>%
    dplyr::filter(dplyr::lead(.data$combinationLRFS) == 1)
  
  andromeda[[sprintf("addRowsLRFS_%s", iteration)]] <- andromeda[[sprintf("addRowsLRFS_%s", iteration)]] %>%
    dplyr::mutate(
      eventStartDate = .data$eventEndDateNext,
      checkDuration = 1
    )
}

shiftLRFSStartDate <- function(treatmentHistory) {
  treatmentHistory %>%
    dplyr::mutate(
      eventEndDate = dplyr::case_when(
        dplyr::lead(.data$combinationLRFS) == 1 ~ .data$eventStartDateNext,
        .default = .data$eventEndDate
      ),
      checkDuration = dplyr::case_when(
        dplyr::lead(.data$combinationLRFS) == 1 ~ 1,
        .default = .data$checkDuration
      )
    )
}

combineCombinations <- function(andromeda, iteration) {
  andromeda$treatmentHistory %>%
    dplyr::union_all(andromeda[[sprintf("addRowsFRFS_%s", iteration)]]) %>%
    dplyr::union_all(andromeda[[sprintf("addRowsLRFS_%s", iteration)]]) %>%
    dplyr::mutate(durationEra = .data$eventEndDate - .data$eventStartDate)
}

filterMinPostCombinationDuration <- function(treatmentHistory, minPostCombinationDuration) {
  treatmentHistory %>%
    dplyr::filter(.data$durationEra >= minPostCombinationDuration | is.na(.data$durationEra))
}

selectRelevantColumns <- function(treatmentHistory) {
  treatmentHistory %>%
    dplyr::select(
      "personId", "indexYear", "eventCohortId", "eventStartDate", "age",
      "sex", "eventEndDate", "durationEra", "gapPrevious"
    )
}

combineTreatments <- function(andromeda, combinationWindow, minPostCombinationDuration, iterations) {
  treatmentHistory <- determineSwitch(andromeda, combinationWindow)
  treatmentHistory <- determineFRFS(treatmentHistory)
  andromeda$treatmentHistory <- determineLRFS(treatmentHistory)
  
  reportCounts(andromeda, iterations)
  
  checkCombinationCounts(andromeda)
  
  andromeda$treatmentHistory <- determineNextPreviousData(andromeda)
  
  combineFRFS(andromeda, iterations)
  treatmentHistory <- shiftFRFSStartDate(andromeda$treatmentHistory)
  treatmentHistory <- shiftFRFSEndDate(treatmentHistory)
  combineLRFS(andromeda, iterations)
  andromeda$treatmentHistory <- shiftLRFSStartDate(treatmentHistory)
  treatmentHistory <- combineCombinations(andromeda, iterations)
  
  treatmentHistory <- filterMinPostCombinationDuration(treatmentHistory, minPostCombinationDuration)
  
  andromeda$treatmentHistory <- selectRelevantColumns(treatmentHistory)
  
  selectRowsCombinationWindow(andromeda)
}

nSelectedRows <- function(andromeda) {
  andromeda$treatmentHistory %>%
    dplyr::filter(.data$selectedRows) %>%
    dplyr::count() %>%
    dplyr::pull()
}

#' Combine overlapping events into combinations
#'
#' doCombinationWindow is an internal function that combines overlapping events
#' into combination events. It accepts a treatmentHistory dataframe and returns
#' a modified treatmentHistory dataframe. The returned treatmentHistory
#' dataframe always has the property that a person is only in one event cohort,
#' which might be a combination event cohort, at any point time.
#'
#' @noRd
#'
#' @param combinationWindow (`integer(1)`)
#' @param minPostCombinationDuration (`integer(1)`)
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return (`invisible(NULL)`)
doCombinationWindow <- function(
    andromeda,
    combinationWindow,
    minPostCombinationDuration) {
  time1 <- Sys.time()
  
  selectRowsCombinationWindow(andromeda)
  
  iterations <- 1
  while (nSelectedRows(andromeda) != 0) {
    combineTreatments(andromeda, combinationWindow, minPostCombinationDuration, iterations)
    iterations <- iterations + 1
  }
  
  nRows <- andromeda$treatmentHistory %>%
    dplyr::count() %>%
    dplyr::pull()
  
  message(sprintf("After combinationWindow: %s", nRows))
  
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    select(-"gapPrevious", -"selectedRows")
  
  time2 <- Sys.time()
  message(sprintf(
    "Time needed to execute combination window %s",
    difftime(time2, time1, units = 'mins')
  ))
  return(invisible(NULL))
}
