#' executeTreatmentPatterns
#'
#' @template param_cohorts
#' @template param_cohortTableName
#' @template param_outputPath
#' @template param_cdm
#' @template param_connectionDetails
#' @template param_cdmSchema
#' @template param_resultSchema
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
#' @template param_minFreq
#' @template param_addNoPaths
#'
#' @return (`invisible(NULL)`)
#' @export
executeTreatmentPatterns <- function(
    cohorts,
    cohortTableName,
    outputPath,
    cdm = NULL,
    connectionDetails = NULL,
    cdmSchema = NULL,
    resultSchema = NULL,
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
    minFreq = 5,
    addNoPaths = TRUE) {

  # Compute pathways on patient level
  andromeda <- computePathways(
    cohorts,
    cohortTableName,
    cdm,
    connectionDetails,
    cdmSchema,
    resultSchema,
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
    minFreq,
    addNoPaths
  )
  
  # Export csv-files
  TreatmentPatterns::export(andromeda, outputPath = outputPath, minFreq = minFreq)
  
  Andromeda::close(andromeda)
  return(invisible(NULL))
}
