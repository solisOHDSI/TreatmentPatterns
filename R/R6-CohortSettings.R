# ==== CohortSettings =========================================================
CohortSettings <- R6::R6Class(
  classname = "CohortSettings",
  inherit = Settings,
  public = list(
    initialize = function(eventCohorts = NULL, targetCohorts = NULL, exitCohorts = NULL) {
      if (!is.null(eventCohorts)) {
        private$eventCohorts <- eventCohorts
      }
      if (!is.null(targetCohorts)) {
        private$targetCohorts <- targetCohorts
      }
      if (!is.null(exitCohorts)) {
        private$exitCohorts <- exitCohorts
      }
      
      self$validate()
    },
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
    getTarget = function() {
      private$targetCohorts
    },
    getEvent = function() {
      private$eventCohorts
    },
    getExit = function() {
      private$exitCohorts
    },
    validate = function() {
      lapply(list(private$targetCohorts, private$eventCohorts, private$exitCohorts), function(cohorts) {
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
