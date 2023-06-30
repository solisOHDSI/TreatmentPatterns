# ==== TPSettings =============================================================
TPSettings <- R6::R6Class(
  classname = "TPSettings",
  inherit = Settings,
  public = list(
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
    },
    
    get = function() {
      list(
        DataSettings = private$dataSettings,
        CohortSettings = private$cohortSettings,
        PathwaySettings = private$pathwaySettings,
        SaveSettings = private$saveSettings
      )
    }
  ),
  private = list(
    dataSettings = NULL,
    cohortSettings = NULL,
    pathwaySettings = NULL,
    saveSettings = NULL,
    validate = function(dataSettings, cohortSettings, pathwaySettings, saveSettings) {
      checkmate::assert(
        checkmate::checkR6(dataSettings, classes = c("R6", "Settings", "DataSettings")),
        checkmate::checkR6(cohortSettings, classes = c("R6", "Settings", "CohortSettings")),
        checkmate::checkR6(pathwaySettings, classes = c("R6", "Settings", "PathwaySettings")),
        checkmate::checkR6(saveSettings, classes = c("R6", "Settings", "SaveSettings")),
        combine = "and")
    }
  )
)
