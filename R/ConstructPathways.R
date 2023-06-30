#' checkConstructPathways
#'
#' Checks parameters for constructPathways.
#'
#' @param env Environment containging all the function environment variables.
#'
#' @return TRUE if all assertions pass
checkConstructPathways <- function(env) {
  # dataSettings
  checkmate::assert(
    checkmate::checkClass(env$dataSettings, "dataSettings"),
    checkmate::checkClass(
      env$dataSettings$connectionDetails,
      "connectionDetails"),
    checkmate::checkCharacter(env$dataSettings$connectionDetails$dbms, len = 1),
    checkmate::checkCharacter(env$dataSettings$cdmDatabaseSchema, len = 1),
    checkmate::checkCharacter(env$dataSettings$resultSchema, len = 1),
    checkmate::checkCharacter(env$dataSettings$cohortTable, len = 1))

  # pathwaySettings
  checkmate::assert(
    checkmate::checkClass(env$pathwaySettings, "pathwaySettings"),
    checkmate::checkDataFrame(env$pathwaySettings$all_settings, nrows = 17))

  # saveSettings
  checkmate::assert(
    checkmate::checkClass(env$saveSettings, "saveSettings"),
    checkmate::checkCharacter(env$saveSettings$databaseName, len = 1),
    checkmate::checkDirectory(env$saveSettings$rootFolder),
    checkmate::checkDirectory(env$saveSettings$outputFolder),
    checkmate::checkDirectory(env$saveSettings$tempFolder)
  )
  return(TRUE)
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
constructPathways <- function(dataSettings,
                              pathwaySettings,
                              saveSettings,
                              cohortSettings) {
  # Check if inputs correct
  check <- checkConstructPathways(environment())

  if (check) {
    # do stuff
    message("check passed")
  }

  # Load already created cohorts
  # Connect to database
  connection <- DatabaseConnector::connect(dataSettings$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  # Get cohort ids from pathwaySettings
  cohortIds <- pathwaySettings$all_settings[c(2,3,4), 2:length(pathwaySettings$all_settings)] %>%
    unlist() %>%
    stringr::str_split(pattern = ",") %>%
    unlist() %>%
    unique() %>%
    as.integer()
  
  cohortIds <- cohortIds[!is.na(cohortIds)]
  
  andromeda <- Andromeda::andromeda()
  
  # Get cohorts from database
  andromeda$fullCohorts <- dplyr::tibble(TreatmentPatterns::extractCohortTable(
    connection = connection,
    resultsSchema = dataSettings$resultSchema,
    cohortTableName = dataSettings$cohortTable,
    cohortIds = cohortIds
  ))
  
  andromeda$fullCohorts <- andromeda$fullCohorts %>%
    dplyr::rename(
      cohort_id = "COHORT_DEFINITION_ID",
      person_id = "SUBJECT_ID",
      start_date = "COHORT_START_DATE",
      end_date = "COHORT_END_DATE")
  
  
  if (!dir.exists(saveSettings$outputFolder)) {
    dir.create(saveSettings$outputFolder)
  }
  
  write.csv(
    x = cohortSettings$cohortsToCreate,
    file = file.path(saveSettings$outputFolder, "cohortsToCreate.csv")
  )
  
  write.csv(
    x = andromeda$fullCohorts,
    file = file.path(saveSettings$outputFolder, "cohortTable.csv")
  )

  # Save pathway settings
  pathwaySettings <- pathwaySettings$all_settings

  # Create output and temp folders
  fs::dir_create(saveSettings$tempFolder)

  write.csv(
    pathwaySettings,
    file.path(
      saveSettings$outputFolder,
      "pathwaySettings.csv"),
    row.names = FALSE)

  # For all different pathway settings
  settings <- colnames(pathwaySettings)[
    grepl("analysis", colnames(pathwaySettings))
  ]

  for (s in settings) {
    studyName <- pathwaySettings[pathwaySettings$param == "studyName", s]

    # Check if directories exist and create if necessary
    tempFolders <- file.path(saveSettings$tempFolder, studyName)
    if (!file.exists(tempFolders)) {
      dir.create(tempFolders, recursive = TRUE)
    }
    
    message(glue::glue("Constructing treatment pathways: {studyName}"))

    # Select cohorts included
    targetCohortId <- pathwaySettings[
      pathwaySettings$param == "targetCohortId", s
    ]

    eventCohortIds <- pathwaySettings[
      pathwaySettings$param == "eventCohortIds", s
    ]
    
    eventCohortIds <- unlist(strsplit(eventCohortIds, split = c(";|,")))

    exitCohortIds <- pathwaySettings[
      pathwaySettings$param == "exitCohortIds", s
    ]
    
    exitCohortIds <- unlist(strsplit(exitCohortIds, split = c(";|,")))
    
    # Analysis settings
    includeTreatments <- pathwaySettings[
      pathwaySettings$param == "includeTreatments", s
    ]

    periodPriorToIndex <- as.integer(pathwaySettings[
      pathwaySettings$param == "periodPriorToIndex", s
    ])

    minEraDuration <- as.integer(pathwaySettings[
      pathwaySettings$param == "minEraDuration", s
    ])

    splitEventCohorts <- pathwaySettings[
      pathwaySettings$param == "splitEventCohorts", s
    ]

    splitTime <- pathwaySettings[
      pathwaySettings$param == "splitTime", s
    ]

    eraCollapseSize <- as.integer(pathwaySettings[
      pathwaySettings$param == "eraCollapseSize", s
    ])

    combinationWindow <- as.integer(pathwaySettings[
      pathwaySettings$param == "combinationWindow", s
    ])

    minPostCombinationDuration <- as.integer(pathwaySettings[
      pathwaySettings$param == "minPostCombinationDuration", s
    ])

    filterTreatments <-  pathwaySettings[
      pathwaySettings$param == "filterTreatments", s
    ]

    maxPathLength <- as.integer(pathwaySettings[
      pathwaySettings$param == "maxPathLength", s
    ])

    # Select subset of full cohort including only data for the current target
    # cohort
    # andromeda$selectPeople <- andromeda$fullCohorts$person_id[
    #   andromeda$fullCohorts$cohort_id == targetCohortId]
    
    selectPeople <- andromeda$fullCohorts %>%
      dplyr::filter(.data$cohort_id == targetCohortId) %>%
      dplyr::select("person_id") %>%
      dplyr::pull()
    
    andromeda$currentCohorts <- andromeda$fullCohorts %>%
      dplyr::filter(.data$person_id %in% selectPeople)
    
    if (andromeda$currentCohorts %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull() > 0) {
      # Preprocess the target/event cohorts to create treatment history
      andromeda$treatmentHistory <- doCreateTreatmentHistory(
        andromeda$currentCohorts,
        targetCohortId,
        eventCohortIds,
        exitCohortIds,
        periodPriorToIndex,
        includeTreatments
      )
      
      andromeda$exitHistory <- andromeda$treatmentHistory %>%
        dplyr::filter(.data$type == "exit") %>%
        dplyr::select(-"type")
      
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::filter(.data$type == "event")

      # Apply pathway settings to create treatment pathways
      message("Construct treatment pathways, this may take a while for larger datasets.")

      message(glue::glue("Original number of rows: {andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))

      # TODO: check what happens if treatmentHistory zero or few rows
      # (throw errors)

      andromeda$treatmentHistory <- doEraDuration(
        andromeda$treatmentHistory,
        minEraDuration)

      andromeda$treatmentHistory <- doSplitEventCohorts(
        andromeda$treatmentHistory,
        splitEventCohorts,
        splitTime,
        saveSettings$outputFolder)

      andromeda$treatmentHistory <- doEraCollapse(
        andromeda$treatmentHistory,
        eraCollapseSize)

      andromeda$treatmentHistory <- doCombinationWindow(
        andromeda$treatmentHistory,
        combinationWindow,
        minPostCombinationDuration)

      andromeda$treatmentHistory <- doFilterTreatments(
        andromeda$treatmentHistory,
        filterTreatments)
      
      # andromeda$exitHistory$event_cohort_id <- as.character(exitHistory$event_cohort_id)
      
      andromeda$exitHistory <- andromeda$exitHistory %>%
        dplyr::mutate(event_cohort_id = as.character(as.integer(.data$event_cohort_id)))
      
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>% 
        dplyr::union(andromeda$exitHistory)
      
      if (andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% pull() > 0) {
        # Add event_seq number to determine order of treatments in pathway
        message("Adding drug sequence number.")
        
        andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
          dplyr::arrange(.data$person_id, .data$event_start_date, .data$event_end_date)
  
        andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
          dplyr::group_by(.data$person_id) %>%
          dplyr::mutate(event_seq = dplyr::row_number())
        
        andromeda$treatmentHistory <- doMaxPathLength(
          andromeda$treatmentHistory,
          maxPathLength)

        # Add event_cohort_name (instead of only event_cohort_id)
        message("Adding concept names.")

        andromeda$treatmentHistory <- addLabels(
          andromeda$treatmentHistory,
          saveSettings$outputFolder)

        # Order the combinations
        message("Ordering the combinations.")

        treatmentHistoryMem <- andromeda$treatmentHistory %>% collect()

        print(treatmentHistoryMem)
        eventCohortNames <- treatmentHistoryMem %>%
          dplyr::select("event_cohort_name") %>%
          dplyr::pull() %>%
          stringr::str_split(pattern = "\\+") %>%
          lapply(FUN = function(x) {
            paste(sort(x), collapse = "+")
          }) %>%
          unlist()

        treatmentHistoryMem %>% mutate(event_cohort_name = eventCohortNames)

        andromeda$treatmentHistory <- treatmentHistoryMem
        rm("treatmentHistoryMem")
      }
      
      # Save the processed treatment history
      write.csv(andromeda$treatmentHistory, file.path(
        tempFolders,
        paste0(
          saveSettings$databaseName,
          "_",
          studyName,
          "_event_seq_processed.csv")),
        row.names = FALSE)

      # Save the treatment pathways
      if (andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull() > 0) {
        andromeda$treatmentPathways <- andromeda$treatmentHistory %>%
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
        
        layers <- c(colnames(andromeda$treatmentPathways))[
          3:min(7, ncol(andromeda$treatmentPathways))] # max first 5
        
        andromeda$treatmentPathways <- andromeda$treatmentPathways %>%
          dplyr::mutate(index_year = floor(.data$index_year / 365.25 + 1970)) %>%
          dplyr::summarise(freq = length(.data$person_id), across(c(layers, "index_year")))

        # TODO: pathways as list
        # treatmentPathways2 <- treatmentPathways2 %>%
        #   group_by(.data$index_year, .data$pathway) %>%
        #   summarise(freq = length(person_id), .groups = "drop")
        
        write.csv(
          x = andromeda$treatmentPathways,
          file = file.path(
            tempFolders,
            glue::glue("{saveSettings$databaseName}_{studyName}_paths.csv")),
          row.names = FALSE)

        # Calculate counts of the number of persons in target cohort / with
        # pathways, in total / per year
        # targetCohort <- currentCohorts[
        #   currentCohorts$cohort_id %in% targetCohortId, ]
        
        andromeda$targetCohort <- andromeda$currentCohorts %>%
          dplyr::filter(.data$cohort_id %in% targetCohortId)
        
        andromeda$targetCohort <- andromeda$targetCohort %>%
          dplyr::mutate(index_year = floor(.data$start_date / 365.25 + 1970))
        
        countsTargetCohort <- andromeda$targetCohort %>%
          dplyr::count(.data$index_year)

        countsTargetCohort <- countsTargetCohort %>% 
          dplyr::mutate(index_year = paste("# persons in target cohort", .data$index_year))
        
        countsPathway <- andromeda$treatmentPathways %>%
          dplyr::group_by(.data$index_year) %>%
          dplyr::summarise(n = sum(.data$freq), .groups = "drop")

        countsPathway <- countsPathway %>%
          dplyr::union(
            countsPathway %>% dplyr::summarise(index_year = NA, n = sum(.data$n))
          )
        
        countsPathway <- countsPathway %>% 
          dplyr::mutate(index_year = paste("# pathways before minCellCount in", .data$index_year))
        
        counts <- dplyr::union(countsTargetCohort, countsPathway) %>%
          dplyr::rename(freq = "n")
        
        write.csv(
          counts,
          file.path(tempFolders, glue::glue(
            "{saveSettings$databaseName}_{studyName}_summary_cnt.csv")),
          row.names = FALSE)
      }
    }
  }
  message("constructPathways done.")
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
    currentCohorts,
    targetCohortId,
    eventCohortIds,
    exitCohortIds = NULL,
    periodPriorToIndex,
    includeTreatments) {
  
  # checkmate::assert(checkmate::check_data_frame(
  #   currentCohorts,
  #   min.cols = 4,
  #   col.names = "named"))
  # checkmate::assert(checkmate::checkNames(
  #   names(currentCohorts),
  #   permutation.of = c("cohort_id", "person_id", "start_date", "end_date")))
  # 
  # checkmate::assert(checkmate::checkCharacter(targetCohortId, len = 1))
  # checkmate::assert(checkmate::checkCharacter(eventCohortIds))
  # checkmate::assert(checkmate::checkInt(periodPriorToIndex))
  
  # Add index year column based on start date target cohort
  # TODO: UNCOMMENT THIS
  andromeda <- Andromeda::andromeda()
  # currentCohorts <- andromeda$currentCohorts
  
  andromeda$targetCohorts <- currentCohorts %>%
    dplyr::filter(.data$cohort_id %in% targetCohortId) %>%
    dplyr::mutate(type = "target") %>%
    dplyr::mutate(index_year = as.numeric(format(.data$start_date, "%Y")))
  
  # Select event cohorts for target cohort and merge with start/end date and
  # index year
  andromeda$eventCohorts <- currentCohorts %>%
    dplyr::filter(.data$cohort_id %in% eventCohortIds) %>%
    dplyr::mutate(type = "event")
  
  andromeda$exitCohorts <- currentCohorts %>%
    dplyr::filter(.data$cohort_id %in% exitCohortIds) %>%
    dplyr::mutate(type = "exit")
  
  if (andromeda$exitCohorts %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull() > 0) {
    andromeda$eventCohorts <- andromeda$eventCohorts %>% 
      dplyr::bind_rows(andromeda$exitCohorts)
  }
  
  currentCohorts <- dplyr::full_join(
    x = andromeda$eventCohorts,
    y = andromeda$targetCohorts,
    by = "person_id"
  )
  
  # Only keep event cohorts starting (startDate) or ending (endDate) after
  # target cohort start date
  if (includeTreatments == "startDate") {
    currentCohorts <- currentCohorts %>%
      dplyr::filter(.data$start_date.y - periodPriorToIndex <= .data$start_date.x) %>%
      dplyr::filter(.data$start_date.x < .data$end_date.y)
    
  } else if (includeTreatments == "endDate") {
    currentCohorts <- currentCohorts %>%
      dplyr::filter(.data$start_date.y - periodPriorToIndex <= .data$end_date.x) %>%
      dplyr::filter(.data$start_date.x < .data$end_date.y) %>%
      dplyr::mutate(
        start_date.x = pmax(.data$start_date.y - periodPriorToIndex, .data$start_date.x))
  } else {
    warning(paste(
      "includeTreatments input incorrect, ",
      "return all event cohorts ('includeTreatments')"))
    
    currentCohorts <- currentCohorts %>%
      dplyr::filter(.data$start_date.y - periodPriorToIndex <= .data$start_date.x) %>%
      dplyr::filter(.data$start_date.x < .data$end_date.y)
  }
  
  currentCohorts <- currentCohorts %>%
    dplyr::select("person_id", "index_year", "cohort_id.x", "start_date.x", "end_date.x", "type.x")
  
  currentCohorts <- currentCohorts %>%
    dplyr::rename(
      event_cohort_id = "cohort_id.x",
      event_start_date = "start_date.x",
      event_end_date = "end_date.x",
      type = "type.x")
  
  # Calculate duration and gap same
  currentCohorts <- currentCohorts %>%
    dplyr::mutate(duration_era = event_end_date - event_start_date)
  
  currentCohorts <- currentCohorts %>%
    dplyr::arrange(.data$event_start_date, .data$event_end_date)

  # Andromeda::groupApply(andromeda$currentCohorts, groupVariable = "person_id", fun = unique)
  
  currentCohorts <- currentCohorts %>%
    dplyr::group_by(.data$person_id, .data$event_cohort_id) %>%
    dplyr::mutate(lag_variable = dplyr::lag(.data$event_end_date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(gap_same = .data$event_start_date - .data$lag_variable) %>%
    dplyr::select(-"lag_variable")

  return(currentCohorts)
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
doEraDuration <- function(treatmentHistory, minEraDuration) {
  # Assertions
  # checkmate::assertDataFrame(x = treatmentHistory)
  # checkmate::assertNumeric(
  #   x = minEraDuration,
  #   lower = 0,
  #   finite = TRUE,
  #   len = 1,
  #   null.ok = FALSE
  # )

  treatmentHistory <- treatmentHistory %>%
    dplyr::filter(duration_era >= minEraDuration)
  message(glue::glue("After minEraDuration: {treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
  return(treatmentHistory)
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
    treatmentHistory,
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

      treatmentHistory <- dplyr::bind_rows(
        treatmentHistory, 
        treatmentHistory %>%
          dplyr::filter(.data$event_cohort_id == cohort) %>%
          dplyr::filter(.data$duration_era < cutoff) %>%
          dplyr::mutate(event_cohort_id = as.integer(paste0(cohort, 1)))
      )

      treatmentHistory <- dplyr::bind_rows(
        treatmentHistory,
        treatmentHistory %>%
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
  return(treatmentHistory)
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
doEraCollapse <- function(treatmentHistory, eraCollapseSize) {
  # Assertions
  # checkmate::assertDataFrame(x = treatmentHistory)
  # checkmate::assertNumeric(
  #   x = eraCollapseSize,
  #   lower = 0,
  #   finite = TRUE,
  #   len = 1,
  #   null.ok = FALSE
  # )
  # Order treatmentHistory by person_id, event_cohort_id, start_date, end_date
  # treatmentHistory <- andromeda$treatmentHistory
  
  treatmentHistory <- treatmentHistory %>%
    arrange(.data$person_id, .data$event_cohort_id, .data$event_start_date, .data$event_end_date)

  # rows <- which(treatmentHistory_OLD$gap_same < eraCollapseSize)
  rows <- which(treatmentHistory %>% select("gap_same") %>% pull() < eraCollapseSize)
  
  for (r in rev(rows)) {
    # treatmentHistory[r - 1, "event_end_date"] <- treatmentHistory[
    #   r,
    #   event_end_date]
    
    treatmentHistory <- treatmentHistory %>%
      dplyr::mutate(event_end_date = dplyr::case_when(dplyr::row_number() == r - 1 ~ event_end_date))
    
  }
  
  # Remove all rows with gap_same < eraCollapseSize
  if (length(rows) > 0) {
    # treatmentHistory <- treatmentHistory[-rows, ]
    treatmentHistory <- treatmentHistory %>%
      dplyr::filter(!dplyr::row_number() %in% rows)
  }
  
  treatmentHistory <- treatmentHistory %>%
    dplyr::select(-"gap_same") %>%
    dplyr::mutate(duration_era = .data$event_end_date - .data$event_start_date)
  
  message(glue::glue("After eraCollapseSize: {treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
  return(treatmentHistory)
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
    treatmentHistory,
    combinationWindow,
    minPostCombinationDuration) {
  
  combWinAndrom <- Andromeda::andromeda()
  
  time1 <- Sys.time()

  combWinAndrom$treatmentHistory <- treatmentHistory %>%
    dplyr::mutate(event_cohort_id = as.character(.data$event_cohort_id))
  
  # Find which rows contain some overlap
  combWinAndrom$treatmentHistory <- selectRowsCombinationWindow(combWinAndrom$treatmentHistory) %>%
    dplyr::arrange(.data$person_id, .data$event_start_date, .data$event_end_date, .data$event_cohort_id)
  
  # While rows that need modification exist:
  iterations <- 1
  
  while (combWinAndrom$treatmentHistory %>% summarise(sum = sum(SELECTED_ROWS)) %>% dplyr::pull() != 0) {
    # Which rows have gap previous shorter than combination window OR
    # min(current duration era, previous duration era) -> add column switch
    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
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
    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
      mutate(combination_FRFS = case_when(
        SELECTED_ROWS == 1 & switch == 0 & dplyr::lag(event_end_date) <= event_end_date ~ 1,
        .default = 0
      ))
    
    # For rows selected not in column switch ->
    # if treatmentHistory[r - 1, event_end_date] >
    # treatmentHistory[r, event_end_date] ->
    # add column combination last received, first stopped
    
    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
      dplyr::mutate(combination_LRFS = dplyr::case_when(
        .data$SELECTED_ROWS == 1 & .data$switch == 0 & dplyr::lag(.data$event_end_date) > .data$event_end_date ~ 1,
        .default = 0
      ))
    
    message(glue::glue(
      "Selected {combWinAndrom$treatmentHistory %>% dplyr::summarise(sum = sum(.data$SELECTED_ROWS)) %>% dplyr::pull()} ",
      "out of {combWinAndrom$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()} rows\n",
      "Iteration: {iterations}\n",
      "Switches: {combWinAndrom$treatmentHistory %>% dplyr::summarise(sum = sum(!is.na(.data$switch))) %>% dplyr::pull()}\n",
      "FRFS Combinations: {combWinAndrom$treatmentHistory %>% dplyr::summarise(sum = sum(.data$combination_FRFS)) %>% dplyr::pull()}\n",
      "LRFS Combinations: {combWinAndrom$treatmentHistory %>% dplyr::summarise(sum = sum(.data$combination_LRFS)) %>% dplyr::pull()}"))
    
    sumSwitchComb <- sum(
      combWinAndrom$treatmentHistory %>% dplyr::summarise(sum = sum(.data$switch, na.rm = TRUE)) %>% dplyr::pull(),
      combWinAndrom$treatmentHistory %>% dplyr::summarise(sum = sum(.data$combination_FRFS, na.rm = TRUE)) %>% dplyr::pull(),
      combWinAndrom$treatmentHistory %>% dplyr::summarise(sum = sum(.data$combination_LRFS, na.rm = TRUE)) %>% dplyr::pull()
    )
    
    # sumSwitchComb <- sum(
    #   sum(treatmentHistory$switch, na.rm = TRUE),
    #   sum(treatmentHistory$combination_FRFS, na.rm = TRUE),
    #   sum(treatmentHistory$combination_LRFS, na.rm = TRUE))
    
    sumSelectedRows <- combWinAndrom$treatmentHistory %>% dplyr::summarise(sum = sum(.data$SELECTED_ROWS)) %>% dplyr::pull()
    
    if (sumSwitchComb != sumSelectedRows) {
      warning(glue::glue(
        "{sumSelectedRows} does not equal total sum {sumSwitchComb}"))
    }

    # Do transformations for each of the three newly added columns
    # Construct helpers
    
    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(event_start_date_next = dplyr::lead(.data$event_start_date))
    
    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(event_end_date_previous = dplyr::lag(.data$event_end_date))
    
    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(event_end_date_next = dplyr::lead(.data$event_end_date))

    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(event_cohort_id_previous = dplyr::lag(.data$event_cohort_id)) %>%
      dplyr::ungroup()
    
    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
      dplyr::mutate(event_end_date = dplyr::case_when(
        dplyr::lead(.data$switch) == 1 ~ .data$event_start_date_next,
        .default = .data$event_end_date
      ))

    combWinAndrom$addRowsFRFS <- combWinAndrom$treatmentHistory %>%
      dplyr::filter(.data$combination_FRFS == 1)

    combWinAndrom$addRowsFRFS <- combWinAndrom$addRowsFRFS %>%
      dplyr::mutate(event_end_date = .data$event_end_date_previous)

    combWinAndrom$addRowsFRFS <- combWinAndrom$addRowsFRFS %>%
      dplyr::mutate(event_cohort_id = paste0(.data$event_cohort_id, "+", .data$event_cohort_id_previous))

    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
      dplyr::mutate(
        event_end_date = dplyr::case_when(
          dplyr::lead(.data$combination_FRFS) == 1 ~ event_start_date_next,
          .default = .data$event_end_date),
        check_duration = dplyr::case_when(dplyr::lead(.data$combination_FRFS) == 1 ~ 1))

    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
      dplyr::mutate(
        event_start_date = dplyr::case_when(
          .data$combination_FRFS == 1 ~ .data$event_end_date_previous,
          .default = .data$event_start_date),
        check_duration = dplyr::case_when(
          .data$combination_FRFS == 1 ~ 1,
          .default = .data$check_duration))

    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
      dplyr::mutate(event_cohort_id = dplyr::case_when(
        .data$combination_LRFS == 1 ~ paste0(.data$event_cohort_id, "+", .data$event_cohort_id_previous),
        .default = .data$event_cohort_id
      ))

    combWinAndrom$addRowsLRFS <- combWinAndrom$treatmentHistory %>%
      dplyr::filter(lead(.data$combination_LRFS) == 1)

    combWinAndrom$addRowsLRFS <- combWinAndrom$addRowsLRFS %>%
      dplyr::mutate(
        event_start_date = .data$event_end_date_next,
        check_duration = 1)

    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
      dplyr::mutate(
        event_end_date = dplyr::case_when(
          dplyr::lead(.data$combination_LRFS) == 1 ~ .data$event_start_date_next,
          .default = .data$event_end_date),
        check_duration = dplyr::case_when(
          dplyr::lead(.data$combination_LRFS) == 1 ~ 1,
          .default = .data$check_duration
        ))

    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
      dplyr::union(combWinAndrom$addRowsFRFS, combWinAndrom$addRowsLRFS) %>%
      dplyr::mutate(duration_era = .data$event_end_date - .data$event_start_date)
    
    # treatmentHistory <- doStepDuration(
    #   treatmentHistory, minPostCombinationDuration)

    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>% 
      dplyr::filter(.data$duration_era >= minPostCombinationDuration | is.na(.data$duration_era))

    combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
      select("person_id", "index_year", "event_cohort_id",
             "event_start_date", "event_end_date", "duration_era", "GAP_PREVIOUS")

    combWinAndrom$treatmentHistory <- selectRowsCombinationWindow(combWinAndrom$treatmentHistory)

    iterations <- iterations + 1

    invisible(gc())
  }

  message(glue::glue("After combinationWindow: {combWinAndrom$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
  
  combWinAndrom$treatmentHistory <- combWinAndrom$treatmentHistory %>%
    select(-"GAP_PREVIOUS", -"SELECTED_ROWS")
  
  time2 <- Sys.time()
  message(glue::glue(
    "Time needed to execute combination window {difftime(time2, time1, units = 'mins')}"))

  return(combWinAndrom$treatmentHistory)
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
selectRowsCombinationWindow <- function(treatmentHistory) {
  # Order treatmentHistory by person_id, event_start_date, event_end_date
  treatmentHistory <- treatmentHistory %>%
    arrange(.data$person_id, .data$event_start_date, .data$event_end_date)
  
  treatmentHistory <- treatmentHistory %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::mutate(GAP_PREVIOUS = .data$event_start_date - dplyr::lag(.data$event_end_date)) %>%
    ungroup()

  # Find all rows with gap_previous < 0
  treatmentHistory <- treatmentHistory %>%
    dplyr::mutate(ALL_ROWS = ifelse(.data$GAP_PREVIOUS < 0, dplyr::row_number(), NA))

  # Select one row per iteration for each person
  rows <- treatmentHistory %>%
    dplyr::filter(!is.na(.data$ALL_ROWS)) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::select("ALL_ROWS") %>%
    dplyr::collect() %>%
    dplyr::pull()

  treatmentHistory <- treatmentHistory %>%
    dplyr::mutate(SELECTED_ROWS = ifelse(row_number() %in% rows, 1, 0))

  # treatmentHistory[, ALL_ROWS := NULL]
  treatmentHistory <- treatmentHistory %>%
    dplyr::select(-"ALL_ROWS")

  return(treatmentHistory)
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
doFilterTreatments <- function(treatmentHistory, filterTreatments) {
  treatmentHistory <- treatmentHistory %>%
    dplyr::arrange(.data$person_id, .data$event_start_date, .data$event_end_date)
  
  if (filterTreatments != "All") {
    message("Order the combinations.")
    
    combi <- grep("+", treatmentHistory$event_cohort_id, fixed = TRUE)
    
    if (length(combi) > 0) {
      conceptIds <- strsplit(
        x = treatmentHistory$event_cohort_id[combi],
        split = "+",
        fixed = TRUE)
      
      treatmentHistory$event_cohort_id[combi] <- sapply(
        X = conceptIds,
        FUN = function(x) {
          paste(sort(x), collapse = "+")})
    }
  }
  
  if (filterTreatments == "First") {
    treatmentHistory <- treatmentHistory %>%
      dplyr::group_by(.data$person_id, .data$event_cohort_id) %>%
      dplyr::filter(dplyr::row_number() == 1)
  } else if (filterTreatments == "Changes") {
    # Group all rows per person for which previous treatment is same
    tryCatch({
      treatmentHistory <- treatmentHistory %>%
        dplyr::mutate(group = dplyr::consecutive_id(.data$person_id, .data$event_cohort_id))
      }, error = function(e) {
        message(glue::glue(
          "Error encountered:\n{e}\n\n",
          "Using filterTreatments method: 'All'."
        ))
        message(glue::glue("After filterTreatments: {treatmentHistory %>% ungroup() %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
        return(treatmentHistory)
      })

      # Remove all rows with same sequential treatments
      treatmentHistory <- treatmentHistory %>%
        dplyr::group_by(.data$person_id, .data$index_year, .data$event_cohort_id, .data$group) %>%
        dplyr::summarise(
          event_start_date = min(.data$event_start_date),
          event_end_date = max(.data$event_end_date),
          duration_era = sum(.data$duration_era),
          .groups = "drop") %>%
        dplyr::arrange(.data$person_id, .data$index_year, .data$group) %>%
        dplyr::select(-"group")
    }
  message(glue::glue("After filterTreatments: {treatmentHistory %>% ungroup() %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
  return(treatmentHistory)
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
doMaxPathLength <- function(treatmentHistory, maxPathLength) {
  # Assertions
  # checkmate::assertDataFrame(x = treatmentHistory)
  # checkmate::assertNumeric(
  #   x = maxPathLength,
  #   lower = 0,
  #   finite = TRUE,
  #   len = 1,
  #   null.ok = FALSE
  # )

  # Apply maxPathLength
  treatmentHistory <- treatmentHistory %>%
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
addLabels <- function(treatmentHistory, outputFolder) {
  # TH <- treatmentHistory
  treatmentHistory <- treatmentHistory %>%
    mutate(event_cohort_id = as.character(as.integer(.data$event_cohort_id)))

  labels <- read.csv(
      file = file.path(outputFolder, "cohortsToCreate.csv"))
  # convenrt event_cohort_id to character
  labels["cohortId"] <- as.character(labels[, "cohortId"])

  labels <- labels[labels$cohortType == "event" | labels$cohortType == "exit", c("cohortId", "cohortName")]
  colnames(labels) <- c("event_cohort_id", "event_cohort_name")
  
  treatmentHistory <- treatmentHistory %>%
    dplyr::inner_join(labels, by = "event_cohort_id", copy = TRUE)
  
  # TH <- merge(
  #   x = TH,
  #   y = labels,
  #   all.x = TRUE,
  #   by = "event_cohort_id") %>%
  #   dplyr::tibble()
  
  mem <- treatmentHistory %>% collect()
  
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
  
  treatmentHistory <- mem
  rm("mem")
  
  # Filter out + at beginning/end or repetitions
  treatmentHistory$event_cohort_name <- gsub(
    pattern = "(^\\++|\\++$)",
    replacement = "+",
    x = treatmentHistory$event_cohort_name)
  
  return(treatmentHistory)
}
