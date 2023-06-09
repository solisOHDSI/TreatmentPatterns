#' @title
#' CohortSettings
#' 
#' @description
#' CohortSettings object, inherits from Settings.
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
CohortSettings <- R6::R6Class(
  classname = "CohortSettings",
  inherit = Settings,
  
  public = list(
    #' @description
    #' Initializer method. event, target, and exit cohort data frames should
    #' have the following structure:
    #' \describe{
    #'   \item{cohortId}{(\link[base]{integer}) ID of the cohort.}
    #'   \item{cohortName}{(\link[base]{character}) Name of the cohort.}
    #' }
    #'
    #' @param eventCohorts (\link[base]{data.frame})
    #' Data frame containing event cohorts.
    #' @param targetCohorts (\link[base]{data.frame})
    #' Data frame containing target cohorts.
    #' @param exitCohorts (\link[base]{data.frame} \["target", "event", "exit"\])
    #' Data frame containing exit cohorts.
    #' 
    #' @return
    #' `self`
    initialize = function(
    targetCohorts,
    eventCohorts,
    exitCohorts = NULL) {
      
      if (all(is.null(c(eventCohorts, targetCohorts, exitCohorts)))) {
        warning("No cohorts specified")
      }
      
      self$validate()
      
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
    #' Getter method.
    #' 
    #' @return
    #' \link[base]{data.frame}
    #' \describe{
    #'   \item{cohortId}{(\link[base]{character}) ID of the cohort.}
    #'   \item{cohortname}{(\link[base]{character}) Name of the cohort.}
    #'   \item{cohortType}{(\link[base]{character}) Type of the cohort}
    #' }
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
    #' getTarget
    #' 
    #' @return \link[base]{data.frame}
    getTarget = function() {
      return(private$targetCohorts)
    },
    
    #' @description
    #' getEvent
    #' 
    #' @return \link[base]{data.frame}
    getEvent = function() {
      return(private$eventCohorts)
    },
    
    #' @description
    #' getExit
    #' 
    #' @return \link[base]{data.frame}
    getExit = function() {
      return(private$exitCohorts)
    },
    
    #' @description
    #' validate
    #'
    #' @return
    #' `self`
    validate = function() {
      cohortList <- list(
        private$targetCohorts,
        private$eventCohorts,
        private$exitCohorts
      )
      
      lapply(cohortList, function(cohorts) {
        errorMessages <- checkmate::makeAssertCollection()
        checkmate::assertSubset(
          names(cohorts), choices = c("cohortId", "cohortName"), add = errorMessages)
        checkmate::assertDataFrame(
          cohorts, any.missing = FALSE, types = c("numeric", "character"), null.ok = TRUE, add = errorMessages)
        checkmate::reportAssertions(collection = errorMessages)
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
