#' @title
#' AnalysisSettings
#' 
#' @description
#' R6 AnalysisSettings object.
#'
#' @export
#' 
#' @family
#' Settings
#' 
#' @examples
#' cohortSettings <- CohortSettings$new(
#'   targetCohorts = data.frame(cohortId = c(1), cohortName = c("disease")),
#'   eventCohorts = data.frame(cohortId = c(2, 3), cohortName = c("drugA", "drugB"))
#' )
#' 
#' AnalysisA <- AnalysisSettings$new(
#'   cohortSettings = cohortSettings,
#'   studyName = "Analysis 1"
#' )
AnalysisSettings <- R6::R6Class(
  inherit = Settings, 
  classname = "AnalysisSettings",
  public = list(
    #' @description
    #' Initializer
    #' 
    #' @param cohortSettings (`CohortSettings`)
    #' An instance of CohortSettings R6 object.
    #' @param studyName (\link[base]{character})
    #' Name of the study.
    #' 
    #' @return
    #' `self`
    initialize = function(cohortSettings, studyName) {
      private$settings$targetCohortIds <- list(cohortSettings$getTarget()$cohortId)
      private$settings$eventCohortIds <- list(cohortSettings$getEvent()$cohortId)
      private$settings$exitCohortIds <- list(cohortSettings$getExit()$cohortId)
      private$settings$studyName <- studyName
      return(invisible(self))
    },
    
    #' @description
    #' Method to edit the analysis settings
    #'
    #' @param includeTreatments (\link[base]{character})
    #' @param periodPriorToIndex (\link[base]{numeric})
    #' @param minEraDuration (\link[base]{numeric})
    #' @param splitEventCohorts (\link[base]{character})
    #' @param splitTime (\link[base]{numeric})
    #' @param eraCollapseSize (\link[base]{numeric})
    #' @param combinationWindow (\link[base]{numeric})
    #' @param minPostCombinationDuration (\link[base]{numeric})
    #' @param filterTreatments (\link[base]{character})
    #' @param maxPathLength (\link[base]{numeric})
    #' @param minCellCount (\link[base]{numeric})
    #' @param minCellMethod (\link[base]{character})
    #' @param groupCombinations (\link[base]{numeric})
    #' @param addNoPaths (\link[base]{logical})
    #' 
    #' @return
    #' `self`
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
      self$validate(settings)
      private$settings <- utils::modifyList(private$settings, settings)
      return(invisible(self))
    },
    
    #' @description
    #' Gettter method
    #' 
    #' @return (\link[base]{list})
    get = function() {
      private$settings
    },
    
    #' @description
    #' validate
    #' 
    #' @param settings (\link[base]{list})
    #' List of settings
    #' 
    #' @return
    #' `self`
    validate = function(settings) {
      # Assertions
      errorMessages <- checkmate::makeAssertCollection()
      
      checkmate::assertCharacter(
        studyName, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertNumeric(
        targetCohortId, min.len = 1, unique = TRUE, null.ok = FALSE, add = errorMessages)
      checkmate::assertNumeric(
        eventCohortIds, min.len = 1, unique = TRUE, null.ok = FALSE, add = errorMessages)
      checkmate::assertNumeric(
        exitCohortIds, min.len = 0, unique = TRUE, null.ok = TRUE, add = errorMessages)
      checkmate::assertCharacter(
        includeTreatments, len = 1, add = errorMessages)
      checkmate::assertSubset(
        includeTreatments, choices = c("startDate", "endDate"), add = errorMessages)
      checkmate::assertNumeric(
        periodPriorToIndex, len = 1, finite = TRUE, null.ok = FALSE, add = errorMessages)
      checkmate::assertNumeric(
        minEraDuration, lower = 0, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertCharacter(
        splitEventCohorts, len = 1, add = errorMessages)
      checkmate::assertNumeric(
        splitTime, lower = 0, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertNumeric(
        eraCollapseSize, lower = 0, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertNumeric(
        combinationWindow, lower = 0, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertNumeric(
        minPostCombinationDuration, lower = 0, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertCharacter(
        filterTreatments, len = 1, add = errorMessages)
      checkmate::assertSubset(
        filterTreatments, choices = c("First", "Changes", "All"), add = errorMessages)
      checkmate::assertNumeric(
        maxPathLength, lower = 0, upper = 5, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertNumeric(
        minCellCount, lower = 0, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertCharacter(
        minCellMethod, len = 1, add = errorMessages)
      checkmate::assertNumeric(
        groupCombinations, lower = 0, finite = TRUE, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertLogical(
        addNoPaths, any.missing = FALSE, len = 1, add = errorMessages)
      
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
