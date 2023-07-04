#' @title
#' AnalysisSettings
#' 
#' @export
AnalysisSettings <- R6::R6Class(
  inherit = Settings,
  classname = "AnalysisSettings",
  public = list(
    initialize = function(cohortSettings, studyName) {
      private$settings$targetCohortIds <- list(cohortSettings$getTarget()$cohortId)
      private$settings$eventCohortIds <- list(cohortSettings$getEvent()$cohortId)
      private$settings$exitCohortIds <- list(cohortSettings$getExit()$cohortId)
      private$settings$studyName <- studyName
    },
    editSettings = function(
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
      settings <- mget(
        x = names(formals()),
        envir = sys.frame(
          which = sys.nframe()
        )
      )
      private$settings <- utils::modifyList(private$settings, settings)
      self$validate()
    },
    get = function() {
      private$settings
    },
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(
        x = private$settings$includeTreatments, len = 1, add = errorMessages)
      checkmate::assertSubset(
        x = private$settings$includeTreatments, choices = c("startDate", "endDate"), add = errorMessages)
      checkmate::assertNumeric(
        x = private$settings$periodPriorToIndex, len = 1, finite = TRUE, null.ok = FALSE, add = errorMessages)
      checkmate::assertNumeric(
        x = private$settings$minEraDuration, lower = 0, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertCharacter(
        x = private$settings$splitEventCohorts, len = 1, add = errorMessages)
      checkmate::assertNumeric(
        x = private$settings$splitTime, lower = 0, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertNumeric(
        x = private$settings$eraCollapseSize, lower = 0, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertNumeric(
        x = private$settings$combinationWindow, lower = 0, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertNumeric(
        x = private$settings$minPostCombinationDuration, lower = 0, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertCharacter(
        x = private$settings$filterTreatments, len = 1, add = errorMessages)
      checkmate::assertSubset(
        x = private$settings$filterTreatments, choices = c("First", "Changes", "All"), add = errorMessages)
      checkmate::assertNumeric(
        x = private$settings$maxPathLength, lower = 0, upper = 5, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertNumeric(
        x = private$settings$minCellCount, lower = 0, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      # Not used in ConstructPathways.R
      checkmate::assertCharacter(
        x = private$settings$minCellMethod, len = 1, add = errorMessages)
      checkmate::assertNumeric(
        x = private$settings$groupCombinations, lower = 0, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertLogical(
        x = private$settings$addNoPaths, any.missing = FALSE, len = 1, add = errorMessages)

      checkmate::reportAssertions(collection = errorMessages)
      return(invisible(self))
    }
  ),
  private = list(
    settings = list(
      studyName = NULL,
      targetCohortIds = NULL,
      eventCohortIds = NULL,
      exitCohortIds = NULL,
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
      addNoPaths = TRUE
    )
  )
)
