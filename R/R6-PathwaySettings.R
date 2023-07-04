#' @title PathwaySettings
#' 
#' @export
PathwaySettings <- R6::R6Class(
  classname = "PathwaySettings",
  inherit = Settings,
  public = list(
    # Public ----
    addAnalysis = function(analysisSettings) {
      private$analyses <- append(private$analyses, analysisSettings)
      return(invisible(self))
    },
    
    
    exportAnalyses = function(filePath) {
      path <- file.path(filePath)
      saveRDS(object = private$analyses, file = path)
      message(
        glue::glue("Exported AnalysisSettings objects to: {path}"))
      return(invisible(self))
    },
    
    
    importAnalyses = function(filePath, validate = TRUE) {
      path <- file.path(filePath)
      analyses <- readRDS(file = path)
      if (validate) {
        self$validate()
        message(
          glue::glue("Imported AnalysisSettings objects from: {path}"))
      }
      private$analyses <- analyses
      return(invisible(self))
    },
    
    
    get = function() {
      dplyr::bind_rows(lapply(private$analyses, function(analysis) {
        analysis$get()
      }))
    },
    
    
    validate = function() {
      lapply(private$analyses, function(analysis) {
        analysis$validate()
        return(invisible(NULL))
      })
    }
  ),
  private = list(
    analyses = list()
  )
)
