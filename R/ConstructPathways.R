constructPathway <- function(
    dataParams,
    settings,
    saveParams,
    cohortParams,
    andromeda,
    localAndromeda) {
  
  targetCohortIds <- unlist(settings$targetCohortIds)
  eventCohortIds <- unlist(settings$eventCohortIds)
  exitCohortIds <- unlist(settings$exitCohortIds)
  
  # Check if directories exist and create if necessary
  tempFolders <- file.path(saveParams$tempFolder, settings$studyName)
  if (!file.exists(tempFolders)) {
    dir.create(tempFolders, recursive = TRUE)
  }
  
  message(glue::glue("Constructing treatment pathways: {settings$studyName}"))
  
  selectPeople <- andromeda$fullCohorts %>%
    dplyr::filter(.data$cohort_id == targetCohortIds) %>%
    dplyr::select("person_id") %>%
    dplyr::pull()
  
  localAndromeda$currentCohorts <- andromeda$fullCohorts %>%
    dplyr::filter(.data$person_id %in% selectPeople)
  
  if (localAndromeda$currentCohorts %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull() > 0) {
    # Preprocess the target/event cohorts to create treatment history
    doCreateTreatmentHistory(
      localAndromeda,
      targetCohortIds,
      eventCohortIds,
      exitCohortIds,
      settings$periodPriorToIndex,
      settings$includeTreatments
    )

    localAndromeda$exitHistory <- localAndromeda$treatmentHistory %>%
      dplyr::filter(.data$type == "exit") %>%
      dplyr::select(-"type")

    localAndromeda$treatmentHistory <- localAndromeda$treatmentHistory %>%
      dplyr::filter(.data$type == "event")
    
    # Apply pathway settings to create treatment pathways
    message("Construct treatment pathways, this may take a while for larger datasets.")
    
    message(glue::glue("Original number of rows: {localAndromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
    
    # TODO: check what happens if treatmentHistory zero or few rows
    # (throw errors)

    doEraDuration(
      localAndromeda,
      settings$minEraDuration)
    
    doSplitEventCohorts(
      localAndromeda,
      settings$splitEventCohorts,
      settings$splitTime,
      saveParams$outputFolder)
    
    doEraCollapse(
      localAndromeda,
      settings$eraCollapseSize)

    doCombinationWindow(
      localAndromeda,
      settings$combinationWindow,
      settings$minPostCombinationDuration)
    
    doFilterTreatments(
      localAndromeda,
      settings$filterTreatments)
    
    localAndromeda$exitHistory <- localAndromeda$exitHistory %>%
      dplyr::mutate(event_cohort_id = as.character(as.integer(.data$event_cohort_id)))
    
    localAndromeda$treatmentHistory <- localAndromeda$treatmentHistory %>% 
      dplyr::union(localAndromeda$exitHistory)

    if (localAndromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% pull() > 0) {
      # Add event_seq number to determine order of treatments in pathway
      message("Adding drug sequence number.")
      
      localAndromeda$treatmentHistory <- localAndromeda$treatmentHistory %>%
        dplyr::arrange(.data$person_id, .data$event_start_date, .data$event_end_date)
      
      localAndromeda$treatmentHistory <- localAndromeda$treatmentHistory %>%
        dplyr::group_by(.data$person_id) %>%
        dplyr::mutate(event_seq = dplyr::row_number())
      
      localAndromeda$treatmentHistory <- localAndromeda$treatmentHistory %>%
        dplyr::filter(.data$event_seq <= !!settings$maxPathLength)
      
      message(glue::glue("After maxPathLength: {localAndromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
      
      # Add event_cohort_name (instead of only event_cohort_id)
      message("Adding concept names.")
      
      addLabels(
        localAndromeda,
        saveParams$outputFolder)
      
      # Order the combinations
      message("Ordering the combinations.")
      
      eventCohortNames <- localAndromeda$treatmentHistory %>%
        dplyr::select("event_cohort_name") %>%
        dplyr::pull() %>%
        stringr::str_split(pattern = "\\+") %>%
        lapply(FUN = function(x) {
          paste(sort(x), collapse = "+")
        }) %>%
        unlist()
      
      localAndromeda$treatmentHistory <- localAndromeda$treatmentHistory %>%
        collect() %>%
        mutate(event_cohort_name = eventCohortNames)
    }
    
    # Save the processed treatment history
    write.csv(localAndromeda$treatmentHistory, file.path(
      tempFolders,
      paste0(
        saveParams$databaseName,
        "_",
        settings$studyName,
        "_event_seq_processed.csv")),
      row.names = FALSE)
    
    # Save the treatment pathways
    if (localAndromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull() > 0) {
      localAndromeda$treatmentPathways <- localAndromeda$treatmentHistory %>%
        tidyr::pivot_wider(
          names_from = event_seq,
          values_from = c(event_cohort_name),
          id_cols = c("person_id", "index_year"),
          names_prefix = "event_cohort_name") %>%
        dplyr::arrange(.data$person_id)
      
      # TODO: treatmentPathway stored as list
      # treatmentPathways2 <- treatmentHistory %>%
      #   group_by(person_id, index_year) %>%
      #   summarise(
      #     pathway = list(event_cohort_name[.data$event_seq]),
      #     .groups = "drop")
      
      # layers2 <- treatmentPathways2 %>%
      #   rowwise() %>%
      #   mutate(l = length(pathway)) %>%
      #   select("l") %>%
      #   max()
      
      layers <- c(colnames(localAndromeda$treatmentPathways))[
        3:min(7, ncol(localAndromeda$treatmentPathways))] # max first 5
      
      localAndromeda$treatmentPathways <- localAndromeda$treatmentPathways %>%
        dplyr::mutate(index_year = floor(.data$index_year / 365.25 + 1970)) %>%
        dplyr::summarise(freq = length(.data$person_id), across(c(layers, "index_year")))
      
      # TODO: pathways as list
      # treatmentPathways2 <- treatmentPathways2 %>%
      #   group_by(.data$index_year, .data$pathway) %>%
      #   summarise(freq = length(person_id), .groups = "drop")
      
      write.csv(
        x = localAndromeda$treatmentPathways,
        file = file.path(
          tempFolders,
          glue::glue("{saveParams$databaseName}_{settings$studyName}_paths.csv")),
        row.names = FALSE)
      
      # Calculate counts of the number of persons in target cohort / with
      # pathways, in total / per year
      # targetCohort <- currentCohorts[
      #   currentCohorts$cohort_id %in% targetCohortId, ]
      
      localAndromeda$targetCohorts <- andromeda$fullCohorts %>%
        dplyr::filter(.data$cohort_id %in% targetCohortIds)
      
      localAndromeda$targetCohorts <- localAndromeda$targetCohorts %>%
        dplyr::mutate(index_year = floor(.data$start_date / 365.25 + 1970))
      
      countsTargetCohort <- localAndromeda$targetCohorts %>%
        dplyr::count(.data$index_year, name = "freq")
      
      countsTargetCohort <- countsTargetCohort %>% 
        dplyr::mutate(index_year = paste("# persons in target cohort", .data$index_year))
      
      countsPathway <- localAndromeda$treatmentPathways %>%
        dplyr::group_by(.data$index_year) %>%
        dplyr::summarise(freq = sum(.data$freq), .groups = "drop")
      
      countsPathway <- countsPathway %>%
        dplyr::union(
          countsPathway %>% dplyr::summarise(index_year = NA, freq = sum(.data$freq))
        )
      
      countsPathway <- countsPathway %>% 
        dplyr::mutate(index_year = paste("# pathways before minCellCount in", .data$index_year))
      
      counts <- dplyr::union(countsTargetCohort, countsPathway)
      
      write.csv(
        counts,
        file.path(tempFolders, glue::glue(
          "{saveParams$databaseName}_{settings$studyName}_summary_cnt.csv")),
        row.names = FALSE)
    }
  }
}


#' constructPathways
#'
#' Construct treatment pathways. Also generates output in csv format.
#'
#' @param dataSettings
#' Settings object as created by \link[TreatmentPatterns]{createDataSettings}.
#' @param pathwaySettings
#' Settings object as created by \link[TreatmentPatterns]{createPathwaySettings}.
#' @param saveSettings
#' Settings object as created by \link[TreatmentPatterns]{createSaveSettings}.
#' @param cohortSettings
#' Settings object created by \link[TreatmentPatterns]{createCohortSettings}.
#'
#' @export
#'
#' @examples \dontrun{
#'   constructPathways(
#'     dataSettings = dataSettings,
#'     pathwaySettings = pathwaySettings,
#'     saveSettings = saveSettings,
#'     cohortSettings = cohortSettings
#'   )
#' }
constructPathways <- function(
    dataSettings,
    pathwaySettings,
    saveSettings,
    cohortSettings,
    andromeda) {
  
  dataParams <- dataSettings$get()
  pathwayParams <- pathwaySettings$get()
  saveParams <- saveSettings$get()
  cohortParams <- cohortSettings$get()
  
  cohortIds <- cohortParams$cohortId
  
  # Get cohorts from database
  andromeda$fullCohorts <- extractCohortTable(
    connection = dataParams$connectionDetails,
    resultsSchema = dataParams$resultSchema,
    cohortTableName = dataParams$cohortTable,
    cohortIds = cohortIds
  ) %>%
    dplyr::rename(
      cohort_id = "COHORT_DEFINITION_ID",
      person_id = "SUBJECT_ID",
      start_date = "COHORT_START_DATE",
      end_date = "COHORT_END_DATE")
  
  if (!dir.exists(saveParams$outputFolder)) {
    dir.create(saveParams$outputFolder)
  }
  
  write.csv(
    x = cohortParams,
    file = file.path(saveParams$outputFolder, "cohortsToCreate.csv")
  )

  write.csv(
    x = andromeda$fullCohorts,
    file = file.path(saveParams$outputFolder, "cohortTable.csv")
  )

  # Create output and temp folders
  fs::dir_create(saveParams$tempFolder)
  
  # write.csv(
  #   pathwayParams,
  #   file.path(
  #     saveParams$outputFolder,
  #     "pathwaySettings.csv"),
  #   row.names = FALSE)
  
  andromedas <- lapply(seq_len(nrow(pathwayParams)), function(i) {
    localAndromeda <- Andromeda::andromeda()
    settings <- pathwayParams[i, ]
    constructPathway(dataParams, settings, saveParams, cohortParams, andromeda, localAndromeda)
    
    localAndromeda
  })
  
  # for (i in seq_len(nrow(pathwayParams))) {
  #   localAndromeda <- Andromeda::andromeda()
  #   settings <- pathwayParams[i, ]
  #   constructPathway(dataParams, settings, saveParams, cohortParams, andromeda, localAndromeda)
  #   
  #   andromedas[pathwayParams[1, ]$studyName] <- localAndromeda
  # }
  
  names(andromedas) <- pathwayParams$studyName
  suppressWarnings(andromedas["global"] <- andromeda)
  message("constructPathways done.")
  return(andromedas)
}


#' doCreateTreatmentHistory
#'
#' @param currentCohorts (\link[base]{data.frame})\cr
#' \enumerate{
#'   \item (\link[base]{numeric}) cohort_id
#'   \item (\link[base]{numeric}) person_id
#'   \item (\link[base]{date}: `\%Y-\%m-\%d`) start_date
#'   \item (\link[base]{date}: `\%Y-\%m-\%d`) end_date
#' }
#' 
#' @param targetCohortId (\link[base]{c}) of (\link[base]{numeric})\cr
#' targetCohortId from \link[TreatmentPatterns]{addPathwaySettings}.
#' 
#' @param eventCohortIds (\link[base]{c}) of (\link[base]{numeric})\cr
#' eventCohortIds from \link[TreatmentPatterns]{addPathwaySettings}.
#' 
#' @param exitCohortIds (\link[base]{c}) of (\link[base]{numeric})\cr
#' exitCohortIds from \link[TreatmentPatterns]{addPathwaySettings}.
#' 
#' @param periodPriorToIndex (\link[base]{numeric})\cr
#' periodPriorToIndex from \link[TreatmentPatterns]{addPathwaySettings}.
#' 
#' @param includeTreatments (\link[base]{character})\cr
#' includeTreatments from \link[TreatmentPatterns]{addPathwaySettings}.
#'
#' @return (\link[base]{data.frame})\cr
#' \enumerate{
#'   \item (\link[base]{numeric}) person_id
#'   \item (\link[base]{numeric}) index_year
#'   \item (\link[base]{numeric}) event_cohort_id
#'   \item (\link[base]{date}) event_start_date
#'   \item (\link[base]{date}) event_end_date
#'   \item (\link[base]{character}) type
#'   \item (\link[base]{difftime}) duration_era
#'   \item (\link[base]{difftime}) gap_same
#' }
doCreateTreatmentHistory <- function(
    andromeda,
    targetCohortIds,
    eventCohortIds,
    exitCohortIds,
    periodPriorToIndex,
    includeTreatments) {
  
  andromeda$targetCohorts <- andromeda$currentCohorts %>%
    dplyr::filter(.data$cohort_id %in% targetCohortIds) %>%
    dplyr::mutate(type = "target") %>%
    dplyr::mutate(index_year = as.numeric(format(.data$start_date, "%Y")))
  
  # Select event cohorts for target cohort and merge with start/end date and
  # index year
  andromeda$eventCohorts <- andromeda$currentCohorts %>%
    dplyr::filter(.data$cohort_id %in% eventCohortIds) %>%
    dplyr::mutate(type = "event")
  
  andromeda$exitCohorts <- andromeda$currentCohorts %>%
    dplyr::filter(.data$cohort_id %in% exitCohortIds) %>%
    dplyr::mutate(type = "exit")
  
  if (andromeda$exitCohorts %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull() > 0) {
    andromeda$eventCohorts <- andromeda$eventCohorts %>% 
      dplyr::union(andromeda$exitCohorts)
  }
  
  andromeda$currentCohorts <- dplyr::full_join(
    x = andromeda$eventCohorts,
    y = andromeda$targetCohorts,
    by = "person_id"
  )
  
  # Only keep event cohorts starting (startDate) or ending (endDate) after
  # target cohort start date
  if (includeTreatments == "startDate") {
    andromeda$currentCohorts <- andromeda$currentCohorts %>%
      dplyr::filter(.data$start_date.y - periodPriorToIndex <= .data$start_date.x) %>%
      dplyr::filter(.data$start_date.x < .data$end_date.y)
    
  } else if (includeTreatments == "endDate") {
    andromeda$currentCohorts <- andromeda$currentCohorts %>%
      dplyr::filter(.data$start_date.y - periodPriorToIndex <= .data$end_date.x) %>%
      dplyr::filter(.data$start_date.x < .data$end_date.y) %>%
      dplyr::mutate(
        start_date.x = pmax(.data$start_date.y - periodPriorToIndex, .data$start_date.x))
  } else {
    warning(paste(
      "includeTreatments input incorrect, ",
      "return all event cohorts ('includeTreatments')"))
    
    andromeda$currentCohorts <- andromeda$currentCohorts %>%
      dplyr::filter(.data$start_date.y - periodPriorToIndex <= .data$start_date.x) %>%
      dplyr::filter(.data$start_date.x < .data$end_date.y)
  }
  
  andromeda$currentCohorts <- andromeda$currentCohorts %>%
    dplyr::select("person_id", "index_year", "cohort_id.x", "start_date.x", "end_date.x", "type.x")
  
  andromeda$currentCohorts <- andromeda$currentCohorts %>%
    dplyr::rename(
      event_cohort_id = "cohort_id.x",
      event_start_date = "start_date.x",
      event_end_date = "end_date.x",
      type = "type.x")
  
  # Calculate duration and gap same
  andromeda$currentCohorts <- andromeda$currentCohorts %>%
    dplyr::mutate(duration_era = event_end_date - event_start_date)
  
  andromeda$currentCohorts <- andromeda$currentCohorts %>%
    dplyr::arrange(.data$event_start_date, .data$event_end_date)
  
  andromeda$currentCohorts <- andromeda$currentCohorts %>%
    dplyr::group_by(.data$person_id, .data$event_cohort_id) %>%
    dplyr::mutate(lag_variable = dplyr::lag(.data$event_end_date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(gap_same = .data$event_start_date - .data$lag_variable) %>%
    dplyr::select(-"lag_variable")

  andromeda$treatmentHistory <- andromeda$currentCohorts
  return(invisible(NULL))
}

#' doEraDuration
#'
#' Filters the treatmentHistory based on the specified minimum era duration
#'
#' @param treatmentHistory (\link[base]{data.frame})\cr
#' See \link[TreatmentPatterns]{doCreateTreatmentHistory}.
#' 
#' @param minEraDuration (\link[base]{numeric})\cr
#' minEraDuration from \link[TreatmentPatterns]{addPathwaySettings}.
#'
#' @return (\link[base]{data.frame})
#' Updated treatmentHistory dataframe, rows with duration < minEraDuration
#' filtered out.
#' @examples
#' \dontrun{
#' th <- doCreateTreatmentHistory(current_cohorts = currentCohorts,
#'                                targetCohortId = targetCohortId,
#'                                eventCohortIds = eventCohortIds,
#'                                periodPriorToIndex = periodPriorToIndex,
#'                                includeTreatments = includeTreatments)
#' doEraDuration(treatmentHistory = th, minEraDuration = 1)
#' }
doEraDuration <- function(andromeda, minEraDuration) {
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::filter(duration_era >= minEraDuration)
  message(glue::glue("After minEraDuration: {andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
  return(invisible(NULL))
}


#' doStepDuration
#'
#' Filters treatmentHistory based on if durationEra is smaller than the
#' specified minimum post combination duration (minPostCombinationDuration).
#'
#' @param treatmentHistory
#'     Dataframe with event cohorts of the target cohort in different rows.
#' @param minPostCombinationDuration
#'     Minimum time an event era should last before or after a generated
#'     combination treatment for it to be included in analysis.
#'
#' @return treatmentHistory
#'     Updated dataframe, rows with duration_era <
#'     minPostCombinationDuration filtered out.
doStepDuration <- function(treatmentHistory, minPostCombinationDuration) {
  # Assertions
  # checkmate::assertDataFrame(x = treatmentHistory)
  # checkmate::assertNumeric(
  #   x = minPostCombinationDuration,
  #   lower = 0,
  #   finite = TRUE,
  #   len = 1,
  #   null.ok = FALSE
  # )
  
  treatmentHistory <- treatmentHistory %>% 
    dplyr::filter(.data$duration_era >= minPostCombinationDuration | is.na(.data$duration_era))
  
  # print(
  #   treatmentHistory %>%
  #     dplyr::ungroup() %>%
  #     dplyr::summarise(n = dplyr::n())
  # )
  
  message(
    glue::glue("After minPostCombinationDuration: ??"))
  return(treatmentHistory)
}


#' doSplitEventCohorts
#'
#' Splits the treatmentHistory data.frame based on event cohorts into ‘acute’
#' and ‘therapy’ cohorts.
#'
#' @param treatmentHistory
#'     Dataframe with event cohorts of the target cohort in different rows.
#'
#' @param splitEventCohorts
#'     Specify event cohort to split in acute (< X days) and therapy
#'     (>= X days).
#'
#' @param splitTime
#'     Specify number of days (X) at which each of the split event cohorts
#'     should be split in acute and therapy
#'
#' @param outputFolder
#'     Name of local folder to place results; make sure to use forward
#'     slashes (/).
#'
#' @return treatmentHistory
#'     Updated dataframe, with specified event cohorts now
#'     split in two different event cohorts (acute and therapy).
#'
#' @examples \dontrun{
#' source(system.file(
#'   package = "TreatmentPatterns",
#'   "examples", "SettingObjects", "createDummySettings.R"))
#'
#' source(system.file(
#'   package = "TreatmentPatterns",
#'   "testing",
#'   "testParams.R"))
#'
#' doSplitEventCohorts(
#'   treatmentHistory = doEraDurationTH,
#'   splitEventCohorts = c(1,2,3),
#'   splitTime = c("30", "20", "10"),
#'   outputFolder = saveSettings$outputFolder)}
doSplitEventCohorts <- function(
    andromeda,
    splitEventCohorts,
    splitTime,
    outputFolder) {
  
  if (all(!splitEventCohorts == "")) {
    # Load in labels cohorts
    labels <- dplyr::tibble(read.csv(
      file = file.path(outputFolder, "cohortsToCreate.csv"))
    )[, -1]

    # Check if splitEventCohorts == splitTime
    checkmate::assertTRUE(length(splitEventCohorts) == length(splitTime))

    for (c in seq_len(length(splitEventCohorts))) {
      cohort <- splitEventCohorts[c]
      cutoff <- splitTime[c]

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::union(
          andromeda$treatmentHistory %>%
            dplyr::filter(.data$event_cohort_id == cohort) %>%
            dplyr::filter(.data$duration_era < cutoff) %>%
            dplyr::mutate(event_cohort_id = as.integer(paste0(cohort, 1)))
      )

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::union(
          andromeda$treatmentHistory %>%
            dplyr::filter(.data$event_cohort_id == cohort) %>%
            dplyr::filter(.data$duration_era >= cutoff) %>%
            dplyr::mutate(event_cohort_id = as.integer(paste0(cohort, 2)))
        )
      
      # Add new labels
      original <- labels %>%
        filter(cohortId == as.integer(cohort))
      
      acute <- original
      acute$cohortId <- as.integer(paste0(cohort, 1))
      acute$cohortName <- paste0(acute$cohortName, " (acute)")

      therapy <- original
      therapy$cohortId <- as.integer(paste0(cohort, 2))
      therapy$cohortName <- paste0(therapy$cohortName, " (therapy)")
      
      labels <- labels %>%
        filter(.data$cohortId != as.integer(cohort))
      
      labels <- rbind(labels, acute, therapy)
    }
  }
  return(invisible(NULL))
}


#' doEraCollapse
#'
#' Updates the treatmentHistory data.frame where if gapSame is smaller than the
#' specified era collapse size (eraCollapseSize) are collapsed
#'
#' @param treatmentHistory
#'     Dataframe with event cohorts of the target cohort in different rows.
#' @param eraCollapseSize
#'     Window of time between which two eras of the same event cohort are
#'     collapsed into one era.
#'
#' @return treatmentHistory
#'     Updated dataframe, where event cohorts with
#'     gap_same < eraCollapseSize are collapsed.
#' @examples
#' \dontrun{
#' th <- doCreateTreatmentHistory(current_cohorts = currentCohorts,
#'                                targetCohortId = targetCohortId,
#'                                eventCohortIds = eventCohortIds,
#'                                periodPriorToIndex = periodPriorToIndex,
#'                                includeTreatments = includeTreatments)
#' doEraCollapse(treatmentHistory = th, eraCollapseSize = 1)
#' }
doEraCollapse <- function(andromeda, eraCollapseSize) {
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    arrange(.data$person_id, .data$event_cohort_id, .data$event_start_date, .data$event_end_date)

  # rows <- which(treatmentHistory_OLD$gap_same < eraCollapseSize)
  rows <- which(andromeda$treatmentHistory %>% select("gap_same") %>% pull() < eraCollapseSize)
  
  for (r in rev(rows)) {
    # treatmentHistory[r - 1, "event_end_date"] <- treatmentHistory[
    #   r,
    #   event_end_date]
    
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(event_end_date = dplyr::case_when(dplyr::row_number() == r - 1 ~ event_end_date))
    
  }
  
  # Remove all rows with gap_same < eraCollapseSize
  if (length(rows) > 0) {
    # treatmentHistory <- treatmentHistory[-rows, ]
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::filter(!dplyr::row_number() %in% rows)
  }
  
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::select(-"gap_same") %>%
    dplyr::mutate(duration_era = .data$event_end_date - .data$event_start_date)
  
  message(glue::glue("After eraCollapseSize: {andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
  return(invisible(NULL))
}


#' Combine overlapping events into combinations
#'
#' doCombinationWindow is an internal function that combines overlapping events
#' into combination events. It accepts a treatmentHistory dataframe and returns
#' a modified treatmentHistory dataframe. The returned treatmentHistory
#' dataframe always has the property that a person is only in one event cohort,
#' which might be a combination event cohort, at any point time.
#'
#' @param treatmentHistory
#'     A dataframe of 'event cohorts' with the following columns:
#'     event_cohort_id, person_id, event_start_date, event_end_date.
#'
#' @param combinationWindow
#'     Minimum number of days two event cohorts need to overlap to be
#'     considered a combination event.
#'
#' @param minPostCombinationDuration
#'     Minimum number of days an event era starting after a combination event
#'     or ending before a combination event must last to be counted a separate
#'     event. Events occuring before or after a combination that are less than
#'     `minPostCombinationDuration` days long will be dropped from the analysis.
#'
#' @return A treatmentHistory dataframe with the columns event_cohort_id,
#'     person_id, event_start_date, event_end_date. event_cohort_id will be
#'     of character type and combination events will have a new event_cohort_id
#'     made up of the concatenated event_cohort_ids of each combined
#'     event_cohort_id. When two events overlap for more than
#'     `combinationWindow` days they will be collapsed into a single combination
#'     event. Events are collapsed iteratively starting with the first two
#'     overlapping events per person and continuing until no more overlapping
#'     events exist in the treatmentHistory.
doCombinationWindow <- function(
    andromeda,
    combinationWindow,
    minPostCombinationDuration) {
  
  time1 <- Sys.time()

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(event_cohort_id = as.character(floor(.data$event_cohort_id)))
  
  # Find which rows contain some overlap
  selectRowsCombinationWindow(andromeda)
  
  # While rows that need modification exist:
  iterations <- 1
  
  while (andromeda$treatmentHistory %>% summarise(sum = sum(SELECTED_ROWS)) %>% dplyr::pull() != 0) {
    # Which rows have gap previous shorter than combination window OR
    # min(current duration era, previous duration era) -> add column switch
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      mutate(switch = case_when(
        .data$SELECTED_ROWS == 1 &
          (-.data$GAP_PREVIOUS < combinationWindow &
             !(-.data$GAP_PREVIOUS == .data$duration_era |
                 -GAP_PREVIOUS == dplyr::lag(.data$duration_era))) ~ 1,
        .default = 0
      ))
    
    # For rows selected not in column switch ->
    # if treatmentHistory[r - 1, event_end_date] <=
    # treatmentHistory[r, event_end_date] ->
    # add column combination first received, first stopped
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      mutate(combination_FRFS = case_when(
        SELECTED_ROWS == 1 & switch == 0 & dplyr::lag(event_end_date) <= event_end_date ~ 1,
        .default = 0
      ))
    
    # For rows selected not in column switch ->
    # if treatmentHistory[r - 1, event_end_date] >
    # treatmentHistory[r, event_end_date] ->
    # add column combination last received, first stopped
    
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(combination_LRFS = dplyr::case_when(
        .data$SELECTED_ROWS == 1 & .data$switch == 0 & dplyr::lag(.data$event_end_date) > .data$event_end_date ~ 1,
        .default = 0
      ))
    
    message(glue::glue(
      "Selected {andromeda$treatmentHistory %>% dplyr::summarise(sum = sum(.data$SELECTED_ROWS)) %>% dplyr::pull()} ",
      "out of {andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()} rows\n",
      "Iteration: {iterations}\n",
      "Switches: {andromeda$treatmentHistory %>% dplyr::summarise(sum = sum(!is.na(.data$switch))) %>% dplyr::pull()}\n",
      "FRFS Combinations: {andromeda$treatmentHistory %>% dplyr::summarise(sum = sum(.data$combination_FRFS)) %>% dplyr::pull()}\n",
      "LRFS Combinations: {andromeda$treatmentHistory %>% dplyr::summarise(sum = sum(.data$combination_LRFS)) %>% dplyr::pull()}"))
    
    sumSwitchComb <- sum(
      andromeda$treatmentHistory %>% dplyr::summarise(sum = sum(.data$switch, na.rm = TRUE)) %>% dplyr::pull(),
      andromeda$treatmentHistory %>% dplyr::summarise(sum = sum(.data$combination_FRFS, na.rm = TRUE)) %>% dplyr::pull(),
      andromeda$treatmentHistory %>% dplyr::summarise(sum = sum(.data$combination_LRFS, na.rm = TRUE)) %>% dplyr::pull()
    )
    
    # sumSwitchComb <- sum(
    #   sum(treatmentHistory$switch, na.rm = TRUE),
    #   sum(treatmentHistory$combination_FRFS, na.rm = TRUE),
    #   sum(treatmentHistory$combination_LRFS, na.rm = TRUE))
    
    sumSelectedRows <- andromeda$treatmentHistory %>% dplyr::summarise(sum = sum(.data$SELECTED_ROWS)) %>% dplyr::pull()
    
    if (sumSwitchComb != sumSelectedRows) {
      warning(glue::glue(
        "{sumSelectedRows} does not equal total sum {sumSwitchComb}"))
    }

    # Do transformations for each of the three newly added columns
    # Construct helpers
    
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(event_start_date_next = dplyr::lead(.data$event_start_date))
    
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(event_end_date_previous = dplyr::lag(.data$event_end_date))
    
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(event_end_date_next = dplyr::lead(.data$event_end_date))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(event_cohort_id_previous = dplyr::lag(.data$event_cohort_id)) %>%
      dplyr::ungroup()
    
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(event_end_date = dplyr::case_when(
        dplyr::lead(.data$switch) == 1 ~ .data$event_start_date_next,
        .default = .data$event_end_date
      ))

    andromeda$addRowsFRFS <- andromeda$treatmentHistory %>%
      dplyr::filter(.data$combination_FRFS == 1)

    andromeda$addRowsFRFS <- andromeda$addRowsFRFS %>%
      dplyr::mutate(event_end_date = .data$event_end_date_previous)

    andromeda$addRowsFRFS <- andromeda$addRowsFRFS %>%
      dplyr::mutate(event_cohort_id = paste0(.data$event_cohort_id, "+", .data$event_cohort_id_previous))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        event_end_date = dplyr::case_when(
          dplyr::lead(.data$combination_FRFS) == 1 ~ event_start_date_next,
          .default = .data$event_end_date),
        check_duration = dplyr::case_when(dplyr::lead(.data$combination_FRFS) == 1 ~ 1))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        event_start_date = dplyr::case_when(
          .data$combination_FRFS == 1 ~ .data$event_end_date_previous,
          .default = .data$event_start_date),
        check_duration = dplyr::case_when(
          .data$combination_FRFS == 1 ~ 1,
          .default = .data$check_duration))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(event_cohort_id = dplyr::case_when(
        .data$combination_LRFS == 1 ~ paste0(.data$event_cohort_id, "+", .data$event_cohort_id_previous),
        .default = .data$event_cohort_id
      ))

    andromeda$addRowsLRFS <- andromeda$treatmentHistory %>%
      dplyr::filter(lead(.data$combination_LRFS) == 1)

    andromeda$addRowsLRFS <- andromeda$addRowsLRFS %>%
      dplyr::mutate(
        event_start_date = .data$event_end_date_next,
        check_duration = 1)

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        event_end_date = dplyr::case_when(
          dplyr::lead(.data$combination_LRFS) == 1 ~ .data$event_start_date_next,
          .default = .data$event_end_date),
        check_duration = dplyr::case_when(
          dplyr::lead(.data$combination_LRFS) == 1 ~ 1,
          .default = .data$check_duration
        ))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::union(andromeda$addRowsFRFS, andromeda$addRowsLRFS) %>%
      dplyr::mutate(duration_era = .data$event_end_date - .data$event_start_date)

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>% 
      dplyr::filter(.data$duration_era >= minPostCombinationDuration | is.na(.data$duration_era))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      select("person_id", "index_year", "event_cohort_id",
             "event_start_date", "event_end_date", "duration_era", "GAP_PREVIOUS")

    selectRowsCombinationWindow(andromeda)

    iterations <- iterations + 1
  }

  message(glue::glue("After combinationWindow: {andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
  
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    select(-"GAP_PREVIOUS", -"SELECTED_ROWS")
  
  time2 <- Sys.time()
  message(glue::glue(
    "Time needed to execute combination window {difftime(time2, time1, units = 'mins')}"))

  return(invisible(NULL))
}


#' selectRowsCombinationWindow
#'
#' Help function for doCombinationWindow that selects one overlapping drug era
#' per person to modify in next iteration of the combination window.
#'
#' @param treatmentHistory
#'   Dataframe with event cohorts of the target cohort in different rows.
#'
#' @return Updated treatmentHistory data.frame
#'
#' @examples \dontrun{
#' source(system.file(
#'   package = "TreatmentPatterns",
#'  "testing", "testParams.R"))
#'
#' selectRowsCombinationWindow(doEraCollapseTH)
#' }
selectRowsCombinationWindow <- function(andromeda) {
  # Order treatmentHistory by person_id, event_start_date, event_end_date
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    arrange(.data$person_id, .data$event_start_date, .data$event_end_date)
  
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::mutate(GAP_PREVIOUS = .data$event_start_date - dplyr::lag(.data$event_end_date)) %>%
    ungroup()

  # Find all rows with gap_previous < 0
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(ALL_ROWS = ifelse(.data$GAP_PREVIOUS < 0, dplyr::row_number(), NA))

  # Select one row per iteration for each person
  rows <- andromeda$treatmentHistory %>%
    dplyr::filter(!is.na(.data$ALL_ROWS)) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select("ALL_ROWS") %>%
    dplyr::pull()

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(SELECTED_ROWS = dplyr::case_when(
      dplyr::row_number() %in% rows ~ 1,
      .default = 0
    ))

  # treatmentHistory[, ALL_ROWS := NULL]
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::select(-"ALL_ROWS") %>%
    dplyr::arrange(.data$person_id, .data$event_start_date, .data$event_end_date, .data$event_cohort_id)
  return(invisible(NULL))
}


#' doFilterTreatments
#'
#' Updates the treatmentHistory data.frame where the desired event cohorts are
#' maintained for the visualizations
#'
#' @param treatmentHistory
#'     Dataframe with event cohorts of the target cohort in different rows.
#' @param filterTreatments
#'     Select first occurrence of ('First') / changes between ('Changes') / all
#'     event cohorts ('All').
#'
#' @return treatmentHistory
#'     Updated dataframe, where the desired event cohorts are maintained for
#'     the visualizations.
#' @examples
#' \dontrun{
#' th <- doCreateTreatmentHistory(current_cohorts = currentCohorts,
#'                                targetCohortId = targetCohortId,
#'                                eventCohortIds = eventCohortIds,
#'                                periodPriorToIndex = periodPriorToIndex,
#'                                includeTreatments = includeTreatments)
#'
#' doFilterTreatments(treatmentHistory = th, filterTreatments = "All")}
doFilterTreatments <- function(andromeda, filterTreatments) {
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::arrange(.data$person_id, .data$event_start_date, .data$event_end_date)
  
  if (filterTreatments != "All") {
    message("Order the combinations.")

    combi <- grep("+", andromeda$treatmentHistory %>% select("event_cohort_id") %>% dplyr::pull(), fixed = TRUE)

    if (length(combi) > 0) {
      mem <- andromeda$treatmentHistory %>% dplyr::collect()
      
      conceptIds <- strsplit(
        x = mem$event_cohort_id[combi],
        split = "+",
        fixed = TRUE)

      mem$event_cohort_id[combi] <- sapply(
        X = conceptIds,
        FUN = function(x) {
          paste(sort(x), collapse = "+")})
      
      andromeda$treatmentHistory <- mem
      rm("mem")
    }
  }
  
  if (filterTreatments == "First") {
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$person_id, .data$event_cohort_id) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup()
  } else if (filterTreatments == "Changes") {
    # Group all rows per person for which previous treatment is same
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::collect() %>%
      dplyr::mutate(group = dplyr::consecutive_id(.data$person_id, .data$event_cohort_id))

      # Remove all rows with same sequential treatments
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::group_by(.data$person_id, .data$index_year, .data$event_cohort_id, .data$group) %>%
        dplyr::summarise(
          event_start_date = min(.data$event_start_date),
          event_end_date = max(.data$event_end_date),
          duration_era = sum(.data$duration_era),
          .groups = "drop") %>%
        dplyr::arrange(.data$person_id, .data$index_year, .data$group) %>%
        dplyr::select(-"group")
    }
  message(glue::glue("After filterTreatments: {andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
  return(invisible(NULL))
}


#' doMaxPathLength
#'
#' Filters the treatmentHistory data.frame where eventSeq is smaller or equal
#' than maxPathLength
#'
#' @param treatmentHistory
#' Dataframe with event cohorts of the target cohort in different rows.
#' @param maxPathLength
#' Maximum number of steps included in treatment pathway.
#'
#' @return treatmentHistory
#' Updated dataframe, where the desired event cohorts all have a seq value of <=
#' maxPathLength
#' @examples
#' \dontrun{
#' th <- doCreateTreatmentHistory(current_cohorts = currentCohorts,
#'                                targetCohortId = targetCohortId,
#'                                eventCohortIds = eventCohortIds,
#'                                periodPriorToIndex = periodPriorToIndex,
#'                                includeTreatments = includeTreatments)
#' doMaxPathLength(treatmentHistory = th, maxPathLength = 1)}
doMaxPathLength <- function(andromeda, maxPathLength) {
  # Apply maxPathLength
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::filter(.data$event_seq <= maxPathLength)
  
  message(glue::glue("After maxPathLength: {treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
  return(treatmentHistory)
}

#' addLabels
#'
#' Adds back cohort names to concept ids.
#'
#' @param treatmentHistory treatmentHistory object
#' @param outputFolder Folder of output
#'
#' @return treatmentHistory
addLabels <- function(andromeda, outputFolder) {
  # TH <- treatmentHistory
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    mutate(event_cohort_id = as.character(as.integer(.data$event_cohort_id)))

  labels <- read.csv(
      file = file.path(outputFolder, "cohortsToCreate.csv"))
  # convenrt event_cohort_id to character
  labels["cohortId"] <- as.character(labels[, "cohortId"])

  labels <- labels[labels$cohortType == "event" | labels$cohortType == "exit", c("cohortId", "cohortName")]
  colnames(labels) <- c("event_cohort_id", "event_cohort_name")
  
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::inner_join(labels, by = "event_cohort_id", copy = TRUE)
  
  mem <- andromeda$treatmentHistory %>% collect()
  
  mem$event_cohort_name[is.na(mem$event_cohort_name)] <- sapply(
    X = mem$event_cohort_id[is.na(mem$event_cohort_name)],
    FUN = function(x) {
      # Revert search to look for longest concept_ids first
      
      for (l in seq_len(nrow(labels))) {
        # If treatment occurs twice in a combination (as monotherapy and as part
        # of fixed-combination) -> remove monotherapy occurrence
        if (any(grep(labels$event_cohort_name[l], x))) {
          x <- gsub(labels$event_cohort_id[l], "", x)
        } else {
          x <- gsub(labels$event_cohort_id[l], labels$event_cohort_name[l], x)
        }
      }
      return(x)
    })
  
  mem$event_cohort_name
  
  # Filter out + at beginning/end or repetitions
  mem$event_cohort_name <- gsub(
    pattern = "(^\\++|\\++$)",
    replacement = "+",
    x = mem$event_cohort_name)
  
  
  andromeda$treatmentHistory <- mem
  rm("mem")
}
