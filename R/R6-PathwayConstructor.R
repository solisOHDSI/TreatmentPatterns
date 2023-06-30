PathwayConstructor <- R6::R6Class(
  classname = "PathwayConstructor",
  public = list(
    # Public ----
    initialize = function(tpSettings) {
      private$dataSettings <- tpSettings$get()$DataSettings
      private$cohortSettings <- tpSettings$get()$CohortSettings
      private$pathwaySettings <- tpSettings$get()$PathwaySettings
      private$saveSettings <- tpSettings$get()$SaveSettings
      
      # Set up Andromeda sqlite environment
      private$andromeda <- Andromeda::andromeda()
      
      self$validate()
    },
    
    validate = function() {
      dataSettings$validate()
      cohortSettings$validate()
      pathwaySettings$validate()
      saveSettings$validate()
      
      checkmate::assertTRUE(Andromeda::isAndromeda(private$andromeda))
    },
    
    construct = function() {
      constructPathways(
        private$dataSettings,
        private$pathwaySettings,
        private$saveSettings,
        private$cohortSettings,
        private$andromeda
      )
    },
    
    getAndromeda = function() {
      return(private$andromeda)
    }
  ),
  private = list(
    # Private ----
    ## Fields ----
    dataSettings = NULL,
    pathwaySettings = NULL,
    saveSettings = NULL,
    cohortSettings = NULL,
    andromeda = NULL
  )
)
