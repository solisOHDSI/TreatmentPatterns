#' Create cohort settings.
#'
#' Create a cohortsSettings object, containing information about the target
#' and event cohorts. A cohort ID and name are required to specify the target
#' and event cohorts. The cohortId and cohortName are the ID and Name specified
#' while creating cohorts with i.e. CohortGenerator.
#'
#' @param targetCohorts (\link[base]{data.frame})\cr
#' containing the study population of interest cohortId = "Unique ID number",
#' cohortName = "Descriptive name cohort".
#' @param eventCohorts (\link[base]{data.frame})\cr
#' containing the events of interest cohortId = "Unique ID number",
#' cohortName = "Descriptive name cohort".
#' @param exitCohorts (\link[base]{data.frame})\cr
#' containing the exit events of interest cohortId = "Unique ID number",
#' cohortName = "Descriptive name cohort".
#'
#' @return (\link[TreatmentPatterns]{createCohortSettings})\cr
#' S3 cohortSettings object.
#'
#' @export
#'
#' @examples
#' cohortSettings <- createCohortSettings(
#'   targetCohorts = data.frame(
#'     cohortId = c(1),
#'     cohortName = c("a")),
#'   eventCohorts = data.frame(
#'     cohortId = c(2, 3),
#'     cohortName = c("b", "c")),
#'   exitCohorts = data.frame(
#'     cohortId = c(4),
#'     cohortName = c("d"))
#'   )
createCohortSettings <- function(targetCohorts, eventCohorts, exitCohorts = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  
  checkmate::assertSubset(
    names(targetCohorts), choices = c("cohortId", "cohortName"), add = errorMessages)
  checkmate::assertDataFrame(
    targetCohorts, any.missing = FALSE, types = c("numeric", "character"), add = errorMessages)
  
  checkmate::assertSubset(
    names(eventCohorts), choices = c("cohortId", "cohortName"), add = errorMessages)
  checkmate::assertDataFrame(
    eventCohorts, any.missing = FALSE, types = c("numeric", "character"), add = errorMessages)
  
  checkmate::assertSubset(
    names(exitCohorts), choices = c("cohortId", "cohortName"), add = errorMessages)
  checkmate::assertDataFrame(
    exitCohorts, any.missing = FALSE, null.ok = TRUE, types = c("numeric", "character"), add = errorMessages)
  
  checkmate::reportAssertions(collection = errorMessages)
  
  targetCohorts$cohortType <- "target"
  eventCohorts$cohortType <- "event"
  if (!is.null(exitCohorts)) {
    exitCohorts$cohortType <- "exit"
  }
  
  cohortsToCreate <- rbind(targetCohorts, eventCohorts, exitCohorts)

  cohortsToCreate$cohortId <- as.integer(cohortsToCreate$cohortId)

  cohortSettings <- list(
    cohortsToCreate = cohortsToCreate)
  class(cohortSettings) <- "cohortSettings"

  return(cohortSettings)
}
