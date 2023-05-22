#' PathwaySettings
#' 
#' PathwaySettings object, inherits from Settings.
#' 
#' @export
PathwaySettings <- R6::R6Class(
  classname = "PathwaySettings",
  inherit = Settings,
  public = list(
    #' @description
    #'   Adds an `AnalysisSettings` R6 object to analysis.
    #' 
    #' @param analysisSettings
    #'   <AnalysisSettings> An instance of AnalysisSettings R6 object.
    #' 
    #' @return
    #'   invisible(self) 
    addAnalysis = function(analysisSettings) {
      private$analyses <- append(private$analyses, analysisSettings)
      return(invisible(self))
    },
    
    #' @description
    #'   Exports the analyses list as an RDS-file.
    #' 
    #' @param filePath
    #'   <character> Path to RDS-file.
    #' 
    #' @return
    #'   invisible(self)
    exportAnalyses = function(filePath) {
      path <- file.path(filePath)
      saveRDS(object = private$analyses, file = path)
      message(
        glue::glue("Exported AnalysisSettings objects to: {path}"))
      return(invisible(self))
    },
    
    #' @description
    #'   Imports the analyses list from an RDS-file.
    #' 
    #' @param filePath
    #'   <character> Path to RDS-file.
    #' @param validate
    #'   <logical> (default: TRUE)
    #'
    #' @return
    #'   invisible(self)
    importAnalyses = function(filePath, validate = TRUE) {
      path <- file.path(filePath)
      analyses <- readRDS(file = path)
      if (validate) {
        self$validate(analyses)
        message(
          glue::glue("Imported AnalysisSettings objects from: {path}"))
      }
      private$analyses <- analyses
      return(invisible(self))
    },
    
    #' @description
    #'   get
    #'
    #' @return
    #'   <data.frame>
    get = function() {
      dplyr::bind_rows(lapply(private$analyses, function(analysis) {
        analysis$get()
      }))
    },
    
    #' @description
    #'   validate
    #'
    #' @param analyses
    #'   <list> List of AnalysisSettings R6 objects.
    #'
    #' @return
    #'   invisible(self)
    validate = function(analyses) {
      lapply(analyses, function(analysis) {
        analysis$validate(analysis$get())
        return(invisible(NULL))
      })
      return(invisible(self))
    }
  ),
  
  private = list(
    analyses = list()
  )
)
