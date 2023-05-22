#' TPSettings
#' 
#' TPSettings object, inherits from Settings.
#' 
#' @export
TPSettings <- R6::R6Class(
  classname = "TPSettings",
  inherit = Settings,
  public = list(
    #' @description
    #'   initialize
    #'
    #' @param dataSettings
    #'   <DataSettings> DataSettings R6 object.
    #' @param cohortSettings
    #'   <CohortSettings> CohortSettings R6 object.
    #' @param pathwaySettings
    #'   <PathwaySettings> PathwaySettings R6 object.
    #' @param saveSettings
    #'   <SaveSettings> SaveSettings R6 object.
    #'
    #' @return
    #'   invisible(self)
    initialize = function(
    dataSettings,
    cohortSettings,
    pathwaySettings,
    saveSettings) {
      private$validate(dataSettings, cohortSettings, pathwaySettings, saveSettings)
      private$dataSettings <- dataSettings
      private$cohortSettings <- cohortSettings
      private$pathwaySettings <- pathwaySettings
      private$saveSettings <- saveSettings
      invisible(self)
    },
    
    #' @description
    #'   get
    #'
    #' @return
    #'   <list>
    get = function() {
      list(
        dataSettings = private$dataSettings$get(),
        cohortSettings = private$cohortSettings$get(),
        pathwaySettings = private$pathwaySettings$get(),
        saveSettings = private$saveSettings$get()
      )
    }
  ),
  
  private = list(
    dataSettings = NULL,
    cohortSettings = NULL,
    pathwaySettings = NULL,
    saveSettings = NULL,
    
    # @description
    #   validate
    # 
    # @param dataSettings
    #   <DataSettings> DataSettings R6 object.
    # @param cohortSettings
    #   <CohortSettings> CohortSettings R6 object.
    # @param pathwaySettings
    #   <PathwaySettings> PathwaySettings R6 object.
    # @param saveSettings
    #   <SaveSettings> SaveSettings R6 object.
    # 
    # @return
    #   invisible(self)
    validate = function(dataSettings, cohortSettings, pathwaySettings, saveSettings) {
      checkmate::assert(
        checkmate::checkR6(dataSettings, classes = c("R6", "Settings", "DataSettings")),
        checkmate::checkR6(cohortSettings, classes = c("R6", "Settings", "CohortSettings")),
        checkmate::checkR6(pathwaySettings, classes = c("R6", "Settings", "PathwaySettings")),
        checkmate::checkR6(saveSettings, classes = c("R6", "Settings", "SaveSettings")),
        combine = "and")
      invisible(self)
    }
  )
)
