#' executeTreatmentPatterns
#'
#' @template param_cohorts
#' @template param_cohortTableName
#' @template param_cdm
#' @template param_connectionDetails
#' @template param_cdmSchema
#' @template param_resultSchema
#' @template param_studyName
#' @template param_includeTreatments
#' @template param_periodPriorToIndex
#' @template param_minEraDuration
#' @template param_splitEventCohorts
#' @template param_splitTime
#' @template param_eraCollapseSize
#' @template param_combinationWindow
#' @template param_minPostCombinationDuration
#' @template param_filterTreatments
#' @template param_maxPathLength
#' @template param_minCellCount
#' @template param_minCellMethod
#' @template param_groupCombinations
#' @template param_addNoPaths
#' @template param_outputPath
#'
#' @return (`invisible(NULL)`)
#' @export
executeTreatmentPatterns <- function(
    cohorts,
    cohortTableName,
    cdm = NULL,
    connectionDetails = NULL,
    cdmSchema = NULL,
    resultSchema = NULL,
    studyName = "default",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5,
    minCellCount = 5,
    minCellMethod = "Remove",
    groupCombinations = 10,
    addNoPaths = TRUE,
    outputPath) {

  # Compute pathways on patient level
  andromeda <- runPatientLevel(
    cohorts,
    cohortTableName,
    cdm,
    connectionDetails,
    cdmSchema,
    resultSchema,
    studyName,
    includeTreatments,
    periodPriorToIndex,
    minEraDuration,
    splitEventCohorts,
    splitTime,
    eraCollapseSize,
    combinationWindow,
    minPostCombinationDuration,
    filterTreatments,
    maxPathLength,
    minCellCount,
    minCellMethod,
    groupCombinations,
    addNoPaths
  )
  
  # Export csv-files
  TreatmentPatterns::export(andromeda, outputPath = outputpath)
  return(invisible(NULL))
}
