#' SaveSettings
#' 
#' SaveSettings R6 object. Inherits from Settings.
#'
#' @export
SaveSettings <- R6::R6Class(
  classname = "SaveSettings",
  inherit = Settings,
  public = list(
    #' @description
    #'   Initialize
    #'
    #' @param databaseName
    #'   <character> Database name.
    #' @param outputFolder
    #'   <character> Path to output folder.
    #'
    #' @return
    #'   invisible(self)
    initialize = function(databaseName, outputFolder) {
      self$validate(databaseName, outputFolder)
      private$databaseName <- databaseName
      private$outputFolder <- normalizePath(outputFolder, mustWork = FALSE)
      invisible(self)
    },
    
    #' @description
    #'   get
    #'
    #' @return
    #'   <list>
    get = function() {
      list(
        databaseName = private$databaseName,
        outputFolder = private$outputFolder,
        tempFolder = private$tempFolder
      )
    },
    
    #' @description
    #'   validate
    #' 
    #' @param databaseName
    #'   <character> Database name.
    #' @param outputFolder
    #'   <character> Path to output folder.
    #' 
    #' @return
    #'   invisible(self)
    validate = function(databaseName, outputFolder) {
      checkmate::assertCharacter(databaseName, min.chars = 1, len = 1)
      checkmate::assertPathForOutput(outputFolder, overwrite = FALSE)
      invisible(self)
    }
  ),
  private = list(
    databaseName = "",
    outputFolder = "",
    tempFolder = tempdir()
  )
)
