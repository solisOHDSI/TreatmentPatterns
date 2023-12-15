#' constructPathway
#'
#' Constructs the pathways.
#'
#' @noRd
#'
#' @param settings (`data.frame`)
#' @param cohorts (`data.frame`)
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return `invisible(NULL)`
constructPathways <- function(
    settings,
    cohorts,
    andromeda) {
  targetCohortIds <- cohorts %>%
    filter(.data$type == "target") %>%
    select("cohortId") %>%
    pull()
  eventCohortIds <- cohorts %>%
    filter(.data$type == "event") %>%
    select("cohortId") %>%
    pull()
  exitCohortIds <- cohorts %>%
    filter(.data$type == "exit") %>%
    select("cohortId") %>%
    pull()

  andromeda$cohorts <- cohorts %>%
    as.data.frame()

  message(sprintf("Constructing treatment pathways: %s", settings$studyName))

  selectPeople <- andromeda$cohortTable %>%
    dplyr::filter(.data$cohortId == targetCohortIds) %>%
    dplyr::distinct(.data$personId)

  andromeda$currentCohorts <- andromeda$cohortTable %>%
    dplyr::inner_join(selectPeople, by = dplyr::join_by("personId"))

  nRows <- andromeda$cohortTable %>%
    dplyr::count() %>%
    dplyr::pull()
  
  if (nRows > 0) {
    # Preprocess the target/event cohorts to create treatment history
    createTreatmentHistory(
      andromeda = andromeda,
      targetCohortIds = targetCohortIds,
      eventCohortIds = eventCohortIds,
      exitCohortIds = exitCohortIds,
      periodPriorToIndex = settings$periodPriorToIndex,
      includeTreatments = settings$includeTreatments
    )

    andromeda$exitHistory <- andromeda$treatmentHistory %>%
      dplyr::filter(.data$type == "exit") %>%
      dplyr::select(-"type")

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::filter(.data$type == "event")

    # Apply pathway settings to create treatment pathways
    message("Construct treatment pathways, this may take a while for larger datasets.")

    message(sprintf(
      "Original number of rows: %s",
      andromeda$treatmentHistory %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::pull()
      ))

    doSplitEventCohorts(
      andromeda = andromeda,
      splitEventCohorts = settings$splitEventCohorts,
      splitTime = settings$splitTime
    )

    doEraCollapse(
      andromeda = andromeda,
      eraCollapseSize = settings$eraCollapseSize
    )

    doCombinationWindow(
      andromeda = andromeda,
      combinationWindow = settings$combinationWindow,
      minPostCombinationDuration = settings$minPostCombinationDuration
    )

    doFilterTreatments(
      andromeda = andromeda,
      filterTreatments = settings$filterTreatments
    )

    andromeda$exitHistory <- andromeda$exitHistory %>%
      dplyr::mutate(eventCohortId = as.character(as.integer(.data$eventCohortId)))
    
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::union_all(andromeda$exitHistory)

    if (andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% pull() > 0) {
      # Add eventSeq number to determine order of treatments in pathway
      message("Adding drug sequence number.")

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::arrange(.data$personId, .data$eventStartDate, .data$eventEndDate)

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::group_by(.data$personId) %>%
        dplyr::mutate(eventSeq = dplyr::row_number())

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::filter(.data$eventSeq <= !!settings$maxPathLength)

      nRows <- andromeda$treatmentHistory %>%
        dplyr::count() %>%
        dplyr::pull()
      message(sprintf("After maxPathLength: %s", nRows))

      # Add event_cohort_name (instead of only event_cohort_id)
      message("Adding concept names.")

      addLabels(andromeda = andromeda)

      # Order the combinations
      message("Ordering the combinations.")

      eventCohortNames <- andromeda$treatmentHistory %>%
        dplyr::select("eventCohortName") %>%
        dplyr::pull() %>%
        stringr::str_split(pattern = "\\+") %>%
        lapply(FUN = function(x) {
          paste(sort(x), collapse = "+")
        }) %>%
        unlist()

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::collect() %>%
        dplyr::mutate(
          eventCohortName = eventCohortNames,
          indexYear = floor(.data$indexYear / 365.25) + 1970)
    }
  }
  
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::filter(!is.na(.data$personId))
  
  message("constructPathways done.")
  return(andromeda)
}

#' createTreatmentHistory
#'
#' @noRd
#'
#' @template param_andromeda
#' @param targetCohortIds (`numeric(n)`)
#' @param eventCohortIds (`numeric(n)`)
#' @param exitCohortIds (`numeric(n)`)
#' @template param_periodPriorToIndex
#' @template param_includeTreatments
#'
#' @return (`data.frame()`)\cr
#' \enumerate{
#'   \item (`numeric()`) personId
#'   \item (`numeric()`) indexYear
#'   \item (`numeric()`) eventCohortId
#'   \item (`as.Date()`) eventStartDate
#'   \item (`as.Date()`) eventEndDate
#'   \item (`character()`) type
#'   \item (`difftime()`) durationEra
#'   \item (`difftime()`) gapSame
#' }
createTreatmentHistory <- function(
    andromeda,
    targetCohortIds,
    eventCohortIds,
    exitCohortIds,
    periodPriorToIndex,
    includeTreatments) {
  andromeda$targetCohorts <- andromeda$cohortTable %>%
    dplyr::filter(.data$cohortId %in% targetCohortIds) %>%
    dplyr::mutate(type = "target") %>%
    dplyr::mutate(indexYear = as.numeric(format(.data$startDate, "%Y"))) %>%
    dplyr::mutate(indexDate = .data$startDate - periodPriorToIndex)
  
  # Select event cohorts for target cohort and merge with start/end date and
  # index year
  andromeda$eventCohorts <- andromeda$cohortTable %>%
    dplyr::filter(.data$cohortId %in% eventCohortIds) %>%
    dplyr::mutate(type = "event")
  
  andromeda$exitCohorts <- andromeda$cohortTable %>%
    dplyr::filter(.data$cohortId %in% exitCohortIds) %>%
    dplyr::mutate(type = "exit")
  
  nRows <- andromeda$exitCohorts %>%
    dplyr::count() %>%
    dplyr::pull()
  
  if (nRows > 0) {
    andromeda$eventCohorts <- andromeda$eventCohorts %>%
      dplyr::union_all(andromeda$exitCohorts)
  }
  
  Andromeda::createIndex(andromeda$eventCohorts, c("personId", "startDate", "endDate"))
  Andromeda::createIndex(andromeda$targetCohorts, c("personId", "indexDate", "endDate"))
  
  # Only keep event cohorts starting (startDate) or ending (endDate) after
  # target cohort start date
  if (includeTreatments == "startDate") {
    andromeda$cohortTable <- dplyr::full_join(
      x = andromeda$eventCohorts,
      y = andromeda$targetCohorts,
      by = dplyr::join_by("personId", y$indexDate <= x$startDate, x$startDate < y$endDate)
    )
  } else if (includeTreatments == "endDate") {
    andromeda$cohortTable <- dplyr::full_join(
      x = andromeda$eventCohorts,
      y = andromeda$targetCohorts,
      by = dplyr::join_by("personId", y$indexDate <= x$endDate, x$endDate < y$endDate)) %>%
      dplyr::mutate(
        startDate.x = pmax(.data$startDate.y - periodPriorToIndex, .data$startDate.x, na.rm = TRUE)
      )
  }
  
  andromeda$treatmentHistory <- andromeda$cohortTable %>%
    dplyr::select(
      "personId",
      "indexYear",
      eventCohortId = "cohortId.x",
      eventStartDate = "startDate.x",
      eventEndDate = "endDate.x",
      type = "type.x",
      age = "age.x",
      sex = "sex.x") %>%
    dplyr::mutate(
      durationEra = .data$eventEndDate - .data$eventStartDate,
      eventCohortId = as.character(floor(.data$eventCohortId))) %>%
    dplyr::filter(!is.na(.data$indexYear))
  return(invisible(NULL))
}

#' doSplitEventCohorts
#'
#' Splits the treatmentHistory data.frame based on event cohorts into ‘acute’
#' and ‘therapy’ cohorts.
#'
#' @noRd
#'
#' @param splitEventCohorts (`character(n)`)
#'
#' @param splitTime (`integer(1)`)
#'
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return (`invisible(NULL)`)
doSplitEventCohorts <- function(
    andromeda,
    splitEventCohorts,
    splitTime) {
  
  if (!is.null(splitEventCohorts) & !is.null(splitTime)) {
    # Load in labels cohorts
    labels <- andromeda$cohorts %>%
      dplyr::collect()
    
    # Check if splitEventCohorts == splitTime
    checkmate::assertTRUE(length(splitEventCohorts) == length(splitTime))
    
    for (c in seq_len(length(splitEventCohorts))) {
      cohort <- as.character(as.integer(splitEventCohorts[c]))
      cutoff <- splitTime[c]
      
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::mutate(eventCohortId = dplyr::case_when(
          .data$eventCohortId == cohort & .data$durationEra < cutoff ~ paste0(cohort, 1L),
          .data$eventCohortId == cohort & .data$durationEra >= cutoff ~ paste0(cohort, 2L),
          .default = .data$eventCohortId
        ))
      
      # Add new labels
      original <- labels %>%
        dplyr::filter(.data$cohortId == as.integer(cohort))
      
      acute <- original
      acute$cohortId <- as.integer(paste0(cohort, 1))
      acute$cohortName <- sprintf("%s (acute)", acute$cohortName)
      
      therapy <- original
      therapy$cohortId <- as.integer(paste0(cohort, 2))
      therapy$cohortName <- sprintf("%ss (therapy)", therapy$cohortName)
      
      labels <- labels %>%
        dplyr::filter(.data$cohortId != as.integer(cohort))
      
      andromeda$labels <- rbind(labels, acute, therapy)
    }
  }
  return(invisible(NULL))
}

#' selectRowsCombinationWindow
#'
#' Help function for doCombinationWindow that selects one overlapping drug era
#' per person to modify in next iteration of the combination window.
#'
#' @noRd
#'
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return (`invisible(NULL)`)
selectRowsCombinationWindow <- function(andromeda) {
  # Order treatmentHistory by person_id, event_start_date, event_end_date
  # andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
  #   arrange(.data$personId, .data$eventStartDate, .data$eventEndDate)
  
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(sortOrder = as.numeric(.data$eventStartDate) + as.numeric(.data$eventEndDate) * row_number() / n() * 10^-6) %>%
    dplyr::group_by(.data$personId) %>%
    dplyr::mutate(gapPrevious = .data$eventStartDate - dplyr::lag(.data$eventEndDate, order_by = .data$sortOrder)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(allRows = ifelse(.data$gapPrevious < 0, dplyr::row_number(), NA)) %>%
    dplyr::mutate(gapPrevious = case_when(
      is.na(.data$gapPrevious) & eventEndDate == lead(eventEndDate) ~ lead(gapPrevious),
      .default = .data$gapPrevious
    ))
  
  #   dplyr::mutate(gapPrevious = .data$eventStartDate - dplyr::lag(.data$eventEndDate)) %>%
  #   ungroup()
  # 
  # # Find all rows with gap_previous < 0
  # andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
  #   dplyr::mutate(allRows = ifelse(.data$gapPrevious < 0, dplyr::row_number(), NA))
  
  # Select one row per iteration for each person
  rows <- andromeda$treatmentHistory %>%
    dplyr::filter(!is.na(.data$allRows)) %>%
    dplyr::group_by(.data$personId) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select("allRows") %>%
    dplyr::pull()
  
  treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(selectedRows = dplyr::case_when(
      dplyr::row_number() %in% rows ~ 1,
      .default = 0
    ))
  
  # treatmentHistory[, ALL_ROWS := NULL]
  andromeda$treatmentHistory <- treatmentHistory %>%
    dplyr::select(-"allRows") %>%
    dplyr::arrange(.data$personId, .data$eventStartDate, .data$eventEndDate, .data$eventCohortId)
  return(invisible(NULL))
}


#' doFilterTreatments
#'
#' Updates the treatmentHistory data.frame where the desired event cohorts are
#' maintained for the visualizations
#'
#' @noRd
#'
#' @param andromeda (`Andromeda::andromeda()`)
#' @param filterTreatments (`character(1)`)
#'
#' @return (`invisible(NULL)`)
doFilterTreatments <- function(andromeda, filterTreatments) {
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::arrange(.data$personId, .data$eventStartDate, .data$eventEndDate)
  
  if (filterTreatments != "All") {
    message("Order the combinations.")
    
    combi <- grep(
      pattern = "+",
      x = andromeda$treatmentHistory %>%
        dplyr::select("eventCohortId") %>%
        dplyr::pull(), fixed = TRUE
    )
    
    if (length(combi) > 0) {
      mem <- andromeda$treatmentHistory %>%
        dplyr::collect()
      
      conceptIds <- strsplit(
        x = mem$eventCohortId[combi],
        split = "+",
        fixed = TRUE
      )
      
      mem$eventCohortId[combi] <- sapply(
        X = conceptIds,
        FUN = function(x) {
          paste(sort(x), collapse = "+")
        }
      )
      
      andromeda$treatmentHistory <- mem
      rm("mem")
    }
  }
  
  if (filterTreatments == "First") {
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId, .data$eventCohortId) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup()
  } else if (filterTreatments == "Changes") {
    # Group all rows per person for which previous treatment is same
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::collect() %>%
      dplyr::mutate(group = dplyr::consecutive_id(.data$personId, .data$eventCohortId))
    
    # Remove all rows with same sequential treatments
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId, .data$indexYear, .data$eventCohortId, .data$group) %>%
      dplyr::summarise(
        eventStartDate = min(.data$eventStartDate, na.rm = TRUE),
        eventEndDate = max(.data$eventEndDate, na.rm = TRUE),
        durationEra = sum(.data$durationEra, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data$personId, .data$indexYear, .data$group) %>%
      dplyr::select(-"group")
  }
  nRows <- andromeda$treatmentHistory %>%
    dplyr::count() %>%
    dplyr::pull()
  
  message(sprintf("After filterTreatments: %s", nRows))
  return(invisible(NULL))
}

#' addLabels
#'
#' Adds back cohort names to concept ids.
#'
#' @noRd
#'
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return (invisible(NULL))
addLabels <- function(andromeda) {
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(eventCohortId = as.character(.data$eventCohortId))
  
  labels <- if (is.null(andromeda$labels)) {
    andromeda$cohorts %>%
      dplyr::collect() %>%
      dplyr::filter(.data$type != "target") %>%
      dplyr::select("cohortId", "cohortName")
  } else {
    andromeda$labels %>%
      dplyr::collect() %>%
      dplyr::filter(.data$type != "target") %>%
      select("cohortId", "cohortName")
  }
  
  # labels <- labels[labels$cohortType == "event" | labels$cohortType == "exit", c("cohortId", "cohortName")]
  colnames(labels) <- c("eventCohortId", "eventCohortName")
  
  andromeda$labels <- labels %>%
    dplyr::mutate(eventCohortId = as.character(.data$eventCohortId))
  
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    merge(andromeda$labels, all.x = TRUE, by = "eventCohortId")
  
  mem <- andromeda$treatmentHistory %>%
    dplyr::collect()
  
  mem$eventCohortName[is.na(mem$eventCohortName)] <- sapply(
    X = mem$eventCohortId[is.na(mem$eventCohortName)],
    FUN = function(x) {
      # Reverse search to look for longest concept_ids first
      for (l in rev(seq_len(nrow(labels)))) {
        # If treatment occurs twice in a combination (as monotherapy and as part
        # of fixed-combination) -> remove monotherapy occurrence
        if (any(grep(labels$eventCohortName[l], x))) {
          x <- gsub(labels$eventCohortId[l], "+", x)
        } else {
          x <- gsub(labels$eventCohortId[l], labels$eventCohortName[l], x)
        }
      }
      return(x)
    }
  )
  
  # Filter out + at beginning/end or repetitions
  mem$eventCohortName <- gsub(
    pattern = "(^\\++|\\++$)",
    replacement = "+",
    x = mem$eventCohortName
  )
  
  andromeda$treatmentHistory <- mem
  rm("mem")
  return(invisible(NULL))
}
