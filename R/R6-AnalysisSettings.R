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
#' 
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
      checkmate::assertCharacter(x = settings$includeTreatments, len = 1)
      checkmate::assertSubset(x = settings$includeTreatments, choices = c("startDate", "endDate"))
      
      checkmate::assertNumeric(x = settings$periodPriorToIndex, len = 1,
        finite = TRUE,
        null.ok = FALSE
      )
      
      checkmate::assertNumeric(
        x = settings$minEraDuration,
        lower = 0,
        finite = TRUE,
        len = 1,
        null.ok = FALSE
      )
      
      checkmate::assertCharacter(
        x = settings$splitEventCohorts,
        len = 1
      )
      
      checkmate::assertNumeric(
        x = settings$splitTime,
        lower = 0,
        finite = TRUE,
        len = 1,
        null.ok = FALSE
      )
      
      checkmate::assertNumeric(
        x = settings$eraCollapseSize,
        lower = 0,
        finite = TRUE,
        len = 1,
        null.ok = FALSE
      )
      
      checkmate::assertNumeric(
        x = settings$combinationWindow,
        lower = 0,
        finite = TRUE,
        len = 1,
        null.ok = FALSE
      )
      
      checkmate::assertNumeric(
        x = settings$minPostCombinationDuration,
        lower = 0,
        finite = TRUE,
        len = 1,
        null.ok = FALSE
      )
      
      checkmate::assertCharacter(
        x = settings$filterTreatments,
        len = 1
      )
      
      checkmate::assertSubset(
        x = settings$filterTreatments,
        choices = c("First", "Changes", "All")
      )
      
      checkmate::assertNumeric(
        x = settings$maxPathLength,
        lower = 0,
        upper = 5,
        finite = TRUE,
        len = 1,
        null.ok = FALSE
      )
      
      checkmate::assertNumeric(
        x = settings$minCellCount,
        lower = 0,
        finite = TRUE,
        len = 1,
        null.ok = FALSE
      )
      
      # Not used in ConstructPathways.R
      checkmate::assertCharacter(
        x = settings$minCellMethod,
        len = 1
      )
      
      checkmate::assertNumeric(
        x = settings$groupCombinations,
        lower = 0,
        finite = TRUE,
        len = 1,
        null.ok = FALSE
      )
      
      checkmate::assertLogical(
        x = settings$addNoPaths,
        any.missing = FALSE,
        len = 1
      )
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
