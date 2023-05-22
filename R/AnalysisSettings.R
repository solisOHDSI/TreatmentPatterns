#' AnalysisSettings
#' 
#' AnalysisSettings R6 object. Inherits from Settings
#'
#' @export
AnalysisSettings <- R6::R6Class(
  inherit = Settings, 
  classname = "AnalysisSettings",
  public = list(
    #' @description
    #'   Initializer
    #' 
    #' @param cohortSettings
    #'   <CohortSettings> An instance of CohortSettings R6 object.
    #' @param studyName
    #'   <character> Name of the study.
    initialize = function(cohortSettings, studyName) {
      private$settings$targetCohortIds <- list(cohortSettings$getTarget()$cohortId)
      private$settings$eventCohortIds <- list(cohortSettings$getEvent()$cohortId)
      private$settings$exitCohortIds <- list(cohortSettings$getExit()$cohortId)
      private$settings$studyName <- studyName
      return(invisible(self))
    },
    #' @description
    #'   Function to edit the analysis settings
    #'
    #' @param includeTreatments
    #'   <character>
    #' @param periodPriorToIndex
    #'   <numeric> 
    #' @param minEraDuration
    #'   <numeric>
    #' @param splitEventCohorts
    #'   <character>
    #' @param splitTime
    #'   <numeric>
    #' @param eraCollapseSize
    #'   <numeric>
    #' @param combinationWindow
    #'   <numeric> 
    #' @param minPostCombinationDuration
    #'   <numeric> 
    #' @param filterTreatments
    #'   <character>
    #' @param maxPathLength
    #'   <numeric>
    #' @param minCellCount
    #'   <numeric> 
    #' @param minCellMethod
    #'   <character> 
    #' @param groupCombinations
    #'   <numeric>
    #' @param addNoPaths
    #'   <logical>
    #' 
    #' @return
    #'   invisible(self)
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
    #'   get
    #' 
    #' @return
    #'   <list>
    get = function() {
      private$settings
    },
    
    #' @description
    #'   validate
    #' 
    #' @param settings
    #'   <list> settings
    #' 
    #' @return
    #'   invisible(self)
    validate = function(settings) {
      checkmate::assertCharacter(
        x = settings$includeTreatments,
        len = 1
      )
      
      checkmate::assertSubset(
        x = settings$includeTreatments,
        choices = c("startDate", "endDate")
      )
      
      checkmate::assertNumeric(
        x = settings$periodPriorToIndex,
        # lower = 0, # Can it be negative?
        len = 1,
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
