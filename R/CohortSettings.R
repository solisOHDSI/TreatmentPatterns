#' CohortSettings
#' 
#' CohortSettings object, inherits from Settings.
#' 
#' @export
CohortSettings <- R6::R6Class(
  classname = "CohortSettings",
  inherit = Settings,
  
  public = list(
    #' @description
    #'   initialize
    #'
    #' @param eventCohorts
    #'   <data.frame> Data frame containing event cohorts. Should have two
    #'   columns: 1) cohortId, 2) cohortName.
    #' @param targetCohorts
    #'   <data.frame> Data frame containing target cohorts. Should have two
    #'   columns: 1) cohortId, 2) cohortName.
    #' @param exitCohorts
    #'   <data.frame> Data frame containing exit cohorts. Should have two
    #'   columns: 1) cohortId, 2) cohortName.
    #' 
    #' @return
    #'   invisible(self)
    initialize = function(
    eventCohorts = NULL,
    targetCohorts = NULL,
    exitCohorts = NULL) {
      
      if (all(is.null(c(eventCohorts, targetCohorts, exitCohorts)))) {
        warning("No cohorts specified")
      }
      
      self$validate(list(eventCohorts, targetCohorts, exitCohorts))
      
      if (!is.null(eventCohorts)) {
        private$eventCohorts <- eventCohorts
      }
      
      if (!is.null(targetCohorts)) {
        private$targetCohorts <- targetCohorts
      }
      
      if (!is.null(exitCohorts)) {
        private$exitCohorts <- exitCohorts
      }
      
      return(invisible(self))
    },
    
    #' @description
    #'   get
    #' 
    #' @return
    #'   <data.frame>
    get = function() {
      dplyr::bind_rows(
        private$targetCohorts %>%
          dplyr::mutate(cohortType = "target"),
        private$eventCohorts %>%
          dplyr::mutate(cohortType = "event"),
        private$exitCohorts %>%
          dplyr::mutate(cohortType = "exit")
      )
    },
    
    #' @description
    #'   getTarget
    #' 
    #' @return
    #'   <data.frame>
    getTarget = function() {
      private$targetCohorts
    },
    
    #' @description
    #'   getEvent
    #' 
    #' @return
    #'   <data.frame>
    getEvent = function() {
      private$eventCohorts
    },
    
    #' @description
    #'   getExit
    #' 
    #' @return
    #'   <data.frame>
    getExit = function() {
      private$exitCohorts
    },
    
    #' @description
    #'   validate
    #'
    #' @param cohortsList
    #'   <list> List of target, event, and exit cohort data frames.
    #'
    #' @return
    #'   invisible(self)
    validate = function(cohortsList) {
      lapply(cohortsList, function(cohorts) {
        checkmate::assert(
          checkmate::checkSubset(
            x = names(cohorts),
            choices = c("cohortId", "cohortName")),
          
          checkmate::checkDataFrame(
            cohorts,
            any.missing = FALSE,
            types = c("numeric", "character"),
            null.ok = TRUE),
          combine = "and"
        )
      })
      return(invisible(self))
    }
  ),
  
  private = list(
    targetCohorts = data.frame(
      cohortId = c(),
      cohortName = c()
    ),
    eventCohorts = data.frame(
      cohortId = c(),
      cohortName = c()
    ),
    exitCohorts = data.frame(
      cohortId = c(),
      cohortName = c()
    )
  )
)
