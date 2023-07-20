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
  
  checkmate::assert_character(outputPath, len = 1, null.ok = FALSE)
  checkmate::assert_integerish(minFreq, len = 1, null.ok = FALSE, lower = 0)

  # Compute pathways on patient level
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = cohortTableName,
    cdm = cdm,
    connectionDetails = connectionDetails,
    cdmSchema = cdmSchema,
    resultSchema = resultSchema,
    includeTreatments = includeTreatments,
    periodPriorToIndex = periodPriorToIndex,
    minEraDuration = minEraDuration,
    splitEventCohorts = splitEventCohorts,
    splitTime = splitTime,
    eraCollapseSize = eraCollapseSize,
    combinationWindow = combinationWindow,
    minPostCombinationDuration = minPostCombinationDuration,
    filterTreatments = filterTreatments,
    maxPathLength = maxPathLength,
    addNoPaths = addNoPaths
  )
  
  # Export csv-files
  TreatmentPatterns::export(andromeda, outputPath = outputPath, minFreq = minFreq)
  
  Andromeda::close(andromeda)
  return(invisible(NULL))
}
