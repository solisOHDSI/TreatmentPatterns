#' @title
#' R6 DataSettings class
#' 
#' @description
#' DataSettings object. Inherits from Settings
#' 
#' @export
#' 
#' @family
#' Settings
#' 
#' @examples
#' connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#' 
#' DataSettings$new(
#'   connectionDetails = connectionDetails,
#'   cdmDatabaseSchema = "main",
#'   resultSchema = "main",
#'   cohortTable = "cohortTable"
#' )
DataSettings <- R6::R6Class(
  classname = "DataSettings",
  inherit = Settings,
  public = list(
    # Public ----
    ## Methods ----
    #' @description
    #' \[Overload\] Initializer method
    #' 
    #' @param connectionDetails (\link[DatabaseConnector]{createConnectionDetails})
    #' ConnectionDetails object.
    #' @param cdmDatabaseSchema (\link[base]{character})
    #' CDM Schema in database.
    #' @param resultSchema (\link[base]{character})
    #' Result Schema in database.
    #' @param cohortTable (\link[base]{character})
    #' Name of the cohort table.
    #'
    #' @return `self`
    initialize = function(
    connectionDetails,
    cdmDatabaseSchema,
    resultSchema,
    cohortTable) {
      private$connectionDetails <- connectionDetails
      private$cdmDatabaseSchema <- cdmDatabaseSchema
      private$resultSchema <- resultSchema
      private$cohortTable <- cohortTable
      
      self$validate()
      
      return(invisible(self))
    },
    
    #' @description
    #' Overload print
    #' 
    #' @param x
    #' DataSettings object.
    #' @param ...
    #' Other parameters for \link[base]{print}.
    #' 
    #' @return
    #' Message from \link[base]{cat}.
    print = function(x, ...) {
      cat(
        glue::glue(
          "cdmDatabaseSchema: {private$cdmDatabaseSchema}\n",
          "resultSchema: {private$resultSchema}\n",
          "cohortTable: {private$cohortTable}\n",
          "ConnectionDetails:\n",
          "\tDBMS: {private$connectionDetails$dbms}\n",
          "\tOracle Driver: {private$connectionDetails$oracleDriver}\n",
          "\tPath to Driver: {private$connectionDetails$pathToDriver}"
        )
      )
    },
    
    #' @description
    #' !overload validation
    #'
    #' @return
    #' `self`
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertClass(
        private$connectionDetails, "ConnectionDetails", add = errorMessages)
      checkmate::assertCharacter(
        private$connectionDetails$dbms, len = 1, null.ok = FALSE, add = errorMessages)
      checkmate::assertCharacter(
        private$cdmDatabaseSchema, null.ok = FALSE, len = 1, add = errorMessages)
      checkmate::assertCharacter(
        private$resultSchema, null.ok = FALSE, len = 1, add = errorMessages)
      checkmate::assertCharacter(
        private$cohortTable, null.ok = FALSE, len = 1, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      return(invisible(self))
    },
    
    #' @description
    #' Gets the specified items packged in a named list.
    #' 
    #' @return
    #' (\link[base]{list})
    #' \describe{
    #'   \item{connectionDetails}{(`ConnectionDetails`) ConnectionDetails object. See \link[DatabaseConnector]{createConnectionDetails}.}
    #'   \item{cdmDatabaseSchema}{(\link[base]{character}) Name of the CDM Schema in database.}
    #'   \item{resultSchema}{(\link[base]{character}) Name of the Result Schema in the database.}
    #'   \item{cohortTable}{(\link[base]{character}) Name of the cohort table}
    #' }
    get = function() {
      list(
        connectionDetails = private$connectionDetails,
        cdmDatabaseSchema = private$cdmDatabaseSchema,
        resultSchema = private$resultSchema,
        cohortTable = private$cohortTable
      )
    }
  ),
  private = list(
    # private ----
    connectionDetails = NULL,
    cdmDatabaseSchema = "",
    resultSchema = "",
    cohortTable = "")
)
