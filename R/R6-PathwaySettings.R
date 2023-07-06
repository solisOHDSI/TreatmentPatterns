#' @title PathwaySettings
#' 
#' @export
PathwaySettings <- R6::R6Class(
  classname = "PathwaySettings",
  inherit = Settings,
  public = list(
    # Public ----
    
    #' @description
    #' addAnalysis method
    #'
    #' @param analysisSettings (`TreatmentPatterns::AnalysisSettings`)
    #'
    #' @return (`invisible(self)`)
    addAnalysis = function(analysisSettings) {
      private$analyses <- append(private$analyses, analysisSettings)
      return(invisible(self))
    },
    
    #' @description
    #' exportAnalyses method
    #'
    #' @param filePath (`character(1)`)
    #'
    #' @return (`invisible(self)`)
    exportAnalyses = function(filePath) {
      path <- file.path(filePath)
      saveRDS(object = private$analyses, file = path)
      message(
        glue::glue("Exported AnalysisSettings objects to: {path}"))
      return(invisible(self))
    },
    
    #' @description
    #' importAnalyses method
    #'
    #' @param filePath (`character(1)`)
    #' @param validate (`logical(1)`)
    #'
    #' @return (`invisible(self)`)
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
    
    #' @description
    #' Overload get from super
    #' 
    #' @return (`data.frame()`)
    get = function() {
      dplyr::bind_rows(lapply(private$analyses, function(analysis) {
        analysis$get()
      }))
    },
    
    #' @description
    #' Validation method
    #'
    #' @return (`return(invisible(self)`)
    validate = function() {
      lapply(private$analyses, function(analysis) {
        analysis$validate()
        return(invisible(self))
      })
    }
  ),
  private = list(
    analyses = list()
  )
)
