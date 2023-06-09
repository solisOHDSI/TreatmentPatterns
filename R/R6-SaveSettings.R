#' @title
#' SaveSettings
#' 
#' @description
#' R6 SaveSettings object.
#'
#' @export
#' 
#' @family
#' Settings
#' 
#' @examples
#' saveSettings <- SaveSettings$new(
#'   databaseName = "Eunomia",
#'   outputFolder = "output"
#' )
SaveSettings <- R6::R6Class(
  classname = "SaveSettings",
  inherit = Settings,
  public = list(
    #' @description
    #' Initializer
    #'
    #' @param databaseName (\link[base]{character})
    #' Database name.
    #' @param outputFolder (\link[base]{character})
    #' Path to output folder.
    #'
    #' @return
    #' `self`
    initialize = function(databaseName, outputFolder) {
      private$databaseName <- databaseName
      private$outputFolder <- normalizePath(outputFolder, mustWork = FALSE)
      self$validate()
      return(invisible(self))
    },
    
    #' @description
    #' Getter
    #'
    #' @return (\link[base]{list})
    #' \describe{
    #'   \item{databaseName}{(\link[base]{character}) Name of database.}
    #'   \item{outputFolder}{(\link[base]{character}) Path to output folder.}
    #'   \item{tempFolder}{(\link[base]{character}) Path to temp folder.}
    #' }
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
    #' @param databaseName (\link[base]{character})
    #' Database name.
    #' @param outputFolder (\link[base]{character})
    #' Path to output folder.
    #' 
    #' @return
    #' `self`
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(private$databaseName, min.chars = 1, len = 1, add = errorMessages)
      checkmate::assertPathForOutput(private$outputFolder, overwrite = FALSE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      return(invisible(self))
    }
  ),
  private = list(
    databaseName = "",
    outputFolder = "",
    tempFolder = tempdir()
  )
)
