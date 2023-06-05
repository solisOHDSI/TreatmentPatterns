#' createPathwaySettings
#'
#' Create pathway settings.
#'
#' @param cohortSettings (\link[TreatmentPatterns]{addPathwaySettings})\cr
#' cohortSettings object
#' @param ...
#'   \enumerate{
#'   \item studyName
#'   \item includeTreatments
#'   \item periodPriorToIndex
#'   \item minEraDuration
#'   \item splitEventCohorts
#'   \item splitTime
#'   \item eraCollapseSize
#'   \item combinationWindow
#'   \item minPostCombinationDuration
#'   \item filterTreatments
#'   \item maxPathLength
#'   \item minCellCount
#'   \item minCellMethod
#'   \item groupCombinations
#'   \item addNoPaths
#'   }
#'
#' @return (\link[TreatmentPatterns]{createPathwaySettings})
#' S3 pathwaySettings object.
#'
#' @export
#' @examples
#' targetCohorts <- data.frame(
#'   cohortName = c("targetCohort"),
#'   cohortId = c(1))
#'
#' eventCohorts <- data.frame(
#'   cohortName = c("eventCohort1", "eventCohort2"),
#'   cohortId = c(2, 3))
#'
#' cohortSettings <- TreatmentPatterns::createCohortSettings(
#'   targetCohorts = targetCohorts,
#'   eventCohorts = eventCohorts)
#'
#' createPathwaySettings(
#'   cohortSettings = cohortSettings,
#'   studyName = "MyStudyName")
createPathwaySettings <- function(cohortSettings, ...) {
  # Assertions
  errorMessages <- checkmate::makeAssertCollection()
  
  checkmate::assertClass(
    cohortSettings,
    classes = "cohortSettings",
    add = errorMessages
  )
  
  checkmate::assertDataFrame(
    cohortSettings$cohortsToCreate,
    types = c("integer", "character", "character"),
    ncols = 3,
    any.missing = FALSE,
    add = errorMessages
  )
  
  checkmate::assertSubset(
    names(cohortSettings$cohortsToCreate),
    choices = c("cohortId", "cohortName", "cohortType"),
    add = errorMessages
  )
  
  checkmate::reportAssertions(collection = errorMessages)

  if (exists("studyName")) {
    studyName <- studyName
  } else {
    studyName <- "default"
  }

  
  targetCohorts <- cohortSettings$cohortsToCreate %>%
    dplyr::filter(cohortType == "target")

  eventCohorts <- cohortSettings$cohortsToCreate %>%
    dplyr::filter(cohortType == "event")
  
  exitCohorts <- cohortSettings$cohortsToCreate %>%
    dplyr::filter(cohortType == "exit")

  # Create default pathwaySettings template
  pathwaySettingsDefault <- addPathwaySettings(
    targetCohortId = targetCohorts$cohortId,
    eventCohortIds = eventCohorts$cohortId,
    exitCohortIds = exitCohorts$cohortId,
    ...)

  # Transpose
  pathwaySettings <- data.table::transpose(pathwaySettingsDefault)

  # Add colnames analysis1, analysis2, ...
  colnames(pathwaySettings) <- paste0(
    "analysis", seq_len(ncol(pathwaySettings)))

  # Add param names to pathwaySettings
  pathwaySettings <- cbind(
    param = colnames(pathwaySettingsDefault),
    pathwaySettings)

  pathwaySettings <- list(all_settings = pathwaySettings)
  class(pathwaySettings) <- "pathwaySettings"

  return(pathwaySettings)
}

utils::globalVariables("cohortType")
