# ==== SaveSettings ===========================================================
SaveSettings <- R6::R6Class(
  classname = "SaveSettings",
  inherit = Settings,
  public = list(
    initialize = function(databaseName, outputFolder) {
      private$databaseName <- databaseName
      private$outputFolder <- normalizePath(outputFolder, mustWork = FALSE)
      
      self$validate()
    },
    get = function() {
      list(
        databaseName = private$databaseName,
        outputFolder = private$outputFolder,
        tempFolder = private$tempFolder
      )
    },
    validate = function() {
      checkmate::assertCharacter(private$databaseName, min.chars = 1, len = 1)
      checkmate::assertPathForOutput(private$outputFolder, overwrite = TRUE)
    }
  ),
  private = list(
    databaseName = "",
    outputFolder = "",
    tempFolder = tempdir()
  )
)
