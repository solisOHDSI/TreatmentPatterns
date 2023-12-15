determineCollapse <- function(andromeda, eraCollapseSize) {
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::group_by(.data$personId, .data$eventCohortId) %>%
    dplyr::mutate(
      lagVariable = dplyr::lag(.data$eventEndDate, order_by = .data$eventStartDate)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      needsMerge = .data$eventStartDate - .data$lagVariable < eraCollapseSize,
      rowNumber = dplyr::row_number()) %>%
    dplyr::select(-"lagVariable")
  return(invisible(NULL))
}

fetchRowsNeedCollapse <- function(andromeda) {
  andromeda$treatmentHistory %>%
    dplyr::filter(.data$needsMerge) %>%
    dplyr::select("rowNumber") %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$rowNumber)
}

#' doEraCollapse
#'
#' Updates the treatmentHistory data.frame where if gapSame is smaller than the
#' specified era collapse size (eraCollapseSize) are collapsed
#'
#' @noRd
#'
#' @param andromeda (`Andromeda::andromeda()`)
#' @param eraCollapseSize (`integer(1)`)
#'
#' @return (`invisible(NULL)`)
doEraCollapse <- function(andromeda, eraCollapseSize) {
  determineCollapse(andromeda, eraCollapseSize)
  
  needsMerge <- fetchRowsNeedCollapse(andromeda)
  
  n <- nrow(needsMerge)
  
  # Remove all rows with gap_same < eraCollapseSize
  if (n == 0) {
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::select(-"needsMerge", -"rowNumber") %>%
      dplyr::mutate(durationEra = .data$eventEndDate - .data$eventStartDate)
  } else {
    blockEnd <- needsMerge$rowNumber[seq_len(n - 1) + 1] != needsMerge$rowNumber[seq_len(n - 1) ] + 1
    needsMerge$blockId <- c(0, cumsum(blockEnd))
    needsMerge <- needsMerge %>%
      dplyr::group_by(.data$blockId) %>%
      dplyr::summarise(
        startRowNumber = min(rowNumber, na.rm = TRUE) - 1,
        endRowNumber = max(rowNumber, na.rm = TRUE),
        .groups = "drop")
    
    newEndDates <- andromeda$treatmentHistory %>%
      dplyr::inner_join(
        needsMerge,
        copy = TRUE,
        by = dplyr::join_by("rowNumber" == "endRowNumber")) %>% 
      dplyr::select("startRowNumber", newEndDate = "eventEndDate")
    
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::left_join(
        newEndDates,
        by = dplyr::join_by("rowNumber" == "startRowNumber")) %>%
      dplyr::mutate(
        eventEndDate = if_else(
          is.null(.data$newEndDate), 
          .data$eventEndDate, 
          .data$newEndDate)) %>%
      dplyr::filter(!.data$needsMerge) %>%
      dplyr::select(-"newEndDate", -"needsMerge", -"rowNumber") %>%
      dplyr::mutate(durationEra = .data$eventEndDate - .data$eventStartDate)
  }
  message(sprintf("After eraCollapseSize: %s", n))
  return(invisible(NULL))
}
