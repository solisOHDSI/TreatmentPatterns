#' checkAddPathwaySettings
#'
#' Asserts that parameters fall within specified bounds.
#'
#' @param env
#' (\link[base]{environment}) containing all the function environment variables.
#'
#' @return TRUE if all assertions pass
#'
#' @examples
#' \dontrun{
#' TreatmentPatterns:::checkAddPathwaySettings(env)
#'}
checkAddPathwaySettings <- function(env) {

  # studyName
  checkmate::assertCharacter(x = env$studyName,
                             len = 1,
                             null.ok = FALSE)

  # targetCohortId
  checkmate::assertNumeric(x = env$targetCohortId,
                           min.len = 1,
                           unique = TRUE,
                           null.ok = FALSE)

  # eventCohortIds
  checkmate::assertNumeric(x = env$eventCohortIds,
                           min.len = 1,
                           unique = TRUE,
                           null.ok = FALSE)

  # eventCohortIds
  checkmate::assertNumeric(x = env$exitCohortIds,
                           min.len = 0,
                           unique = TRUE,
                           null.ok = TRUE)
  
  # includeTreatments
  checkmate::assertCharacter(x = env$includeTreatments,
                             len = 1)

  checkmate::assertSubset(x = env$includeTreatments,
                          choices = c("startDate", "endDate"))

  # periodPriorToIndex
  checkmate::assertNumeric(x = env$periodPriorToIndex,
                           # lower = 0, # Can it be negative?
                           len = 1,
                           finite = TRUE,
                           null.ok = FALSE)

  # minEraDuration
  checkmate::assertNumeric(x = env$minEraDuration,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # splitEventCohorts
  checkmate::assertCharacter(x = env$splitEventCohorts,
                             len = 1)

  # splitTime
  checkmate::assertNumeric(x = env$splitTime,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # eraCollapseSize
  checkmate::assertNumeric(x = env$eraCollapseSize,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # combinationWindow
  checkmate::assertNumeric(x = env$combinationWindow,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # minPostCombinationDuration
  checkmate::assertNumeric(x = env$minPostCombinationDuration,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # filterTreatments
  checkmate::assertCharacter(x = env$filterTreatments,
                             len = 1)

  checkmate::assertSubset(x = env$filterTreatments,
                          choices = c("First", "Changes", "All"))

  # maxPathLength
  checkmate::assertNumeric(x = env$maxPathLength,
                           lower = 0,
                           upper = 5,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # minCellCount
  checkmate::assertNumeric(x = env$minCellCount,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # minCellMethod
  # Not used in ConstructPathways.R
  checkmate::assertCharacter(x = env$minCellMethod,
                             len = 1)

  # groupCombinations
  checkmate::assertNumeric(x = env$groupCombinations,
                           lower = 0,
                           finite = TRUE,
                           len = 1,
                           null.ok = FALSE)

  # addNoPaths
  checkmate::assertLogical(x = env$addNoPaths,
                           any.missing = FALSE,
                           len = 1)

  return(TRUE)
}

#' addPathwaySettings
#'
#' Defines and returns a data.frame specifying different parameters how to
#' compute the existing pathways in the function call.
#'
#' @param studyName (\link[base]{character}: "nameUnknown")\cr
#' Name identifying the set of study parameters.
#'
#' @param targetCohortId (\link[base]{c}) of (\link[base]{numeric})\cr
#' Vector of target cohort ID of current study settings.
#'
#' @param eventCohortIds (\link[base]{c}) of (\link[base]{numeric})\cr
#' Vector event cohort IDs of current study settings.
#' 
#' @param exitCohortIds (\link[base]{c}) of (\link[base]{numeric})\cr
#' Vector exit cohort IDs of current study settings.
#'
#' @param includeTreatments (\link[base]{character}: "startDate") ["startDate", "endDate"]\cr
#' Include treatments starting "startDate" or ending "endDate" after target
#' cohort start date.
#'
#' @param periodPriorToIndex (\link[base]{numeric}: 0)\cr
#' Number of days prior to the index date of the
#' target cohort that event cohorts are allowed to start.
#'
#' @param minEraDuration (\link[base]{numeric}: 0)\cr
#' Minimum time an event era should last to be
#' included in analysis.
#'
#' @param splitEventCohorts (\link[base]{character}: "")\cr
#' Specify event cohort ID's to split in acute
#' (< X days) and therapy (>= X days).
#'
#' @param splitTime (\link[base]{numeric}: 30)\cr
#' Specify number of days (X) at which each of the split event cohorts should
#' be split in acute and therapy.
#'
#' @param eraCollapseSize (\link[base]{numeric}: 30)\cr
#' Window of time between which two eras of the same event cohort are collapsed
#' into one era.
#'
#' @param combinationWindow (\link[base]{numeric}: 30)\cr
#' Window of time two event cohorts need to overlap to be considered a
#' combination treatment.
#'
#' @param minPostCombinationDuration (\link[base]{numeric}: 30)\cr
#' Minimum time an event era before or after a generated combination treatment
#' should last to be included in analysis.
#'
#' @param filterTreatments (\link[base]{character}: "First") ["First", "Changes", "All"]\cr
#' Select first occurrence of ("First"); changes between ("Changes'); all event
#' cohorts ("All").
#'
#' @param maxPathLength (\link[base]{numeric}: 5)\cr
#' Maximum number of steps included in treatment pathway. Up to 5 is supported.
#'
#' @param minCellCount (\link[base]{numeric}: 5)\cr
#' Minimum number of persons with a specific treatment pathway for the pathway
#' to be included in analysis.
#'
#' @param minCellMethod (\link[base]{character}: "Remove") ["Remove"]\cr
#' Select to completely remove / sequentially adjust (by removing last step
#' as often as necessary) treatment pathways below minCellCount.
#'
#' @param groupCombinations (\link[base]{numeric}: 10)\cr
#' Select to group all non-fixed combinations in one category 'other’ in the
#' sunburst plot.
#'
#' @param addNoPaths (\link[base]{logical}: TRUE)\cr
#' Select to include untreated persons without treatment pathway in the
#' sunburst plot. Default value is FALSE. When set to TRUE the sunburst plot
#' will show non-treated individuals as blank space.
#'
#' @return (\link[base]{data.frame})\cr
#' A data frame containing the pathway settings.
#'
#' @export
#' @examples
#' \dontrun{
#' pathwaySettings <- addPathwaySettings(
#'   studyName = "myCoolStudy",
#'   targetCohortId = 1,
#'   eventCohortIds = c(1,2,3))
#' }
addPathwaySettings <- function(
    studyName = "nameUnknown",
    targetCohortId,
    eventCohortIds,
    exitCohortIds,
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
    addNoPaths = TRUE) {

  check <- checkAddPathwaySettings(environment())

  if (check) {
    settings <- data.frame(
      studyName = studyName,
      targetCohortId = targetCohortId,
      eventCohortIds = paste(eventCohortIds, collapse = ","),
      exitCohortIds = paste(exitCohortIds, collapse = ","),
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
      minCellCount = minCellCount,
      minCellMethod = minCellMethod,
      groupCombinations = groupCombinations,
      addNoPaths = addNoPaths
    )

    return(settings)
  }
}
