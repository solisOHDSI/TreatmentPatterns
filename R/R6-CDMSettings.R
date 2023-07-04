#' @title
#' R6 CDMSettings class
#' 
#' @description
#' CDMSettings object. Inherits from Settings
#' 
#' @export
#' 
#' @family
#' Settings
#' 
#' @examples
#' # Using a ConnectionDetails
#' connectionDetails <- Eunomia::getConnectionDetails()
#' 
#' CDMSettings$new(
#'   connectionDetails = connectionDetails,
#'   cdmDatabaseSchema = "main",
#'   resultSchema = "main",
#'   cohortTable = "cohortTable"
#' )
#'
#' # Using a CDMConnector cdm object.
#' con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#' cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main")
#'
#' CDMSettings$new(
#'   cdm = cdm,
#'   cohortTable = "cohortTable"
#' )
CDMSettings <- R6::R6Class(
  classname = "CDMSettings",
  inherit = Settings,
  public = list(
    # Public ----
    ## Methods ----
    #' @description
    #' !Overload initialize new
    #' Initializer method
    #'
    #' @return `self`
    initialize = function(cohortTable, connectionDetails = NULL, cdmDatabaseSchema = "", resultSchema = "", cdm = NULL) {
      private$connectionDetails <- connectionDetails
      private$cdmDatabaseSchema <- cdmDatabaseSchema
      private$resultSchema <- resultSchema
      private$cohortTable <- cohortTable
      private$cdm <- cdm
      private$cohortTable <- cohortTable
      
      self$validate()
      return(invisible(self))
    },
    
    #' @description
    #' !Overload print
    #' 
    #' @param x
    #' CDMSettings object.
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
    #' @param connectionDetails \link[DatabaseConnector]{createConnectionDetails}\cr
    #' ConnectionDetails object.
    #' @param cdmDatabaseSchema \link[base]{character}\cr
    #' CDM Schema in database.
    #' @param resultSchema \link[base]{character}\cr
    #' Result Schema in database.
    #' @param cohortTable \link[base]{character}\cr
    #' Name of the cohort table.
    #' 
    #' @return
    #' `self`
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      
      checkmate::assertClass(private$connectionDetails, "ConnectionDetails", null.ok = TRUE, add = errorMessages)
      checkmate::assertCharacter(x = private$connectionDetails$dbms, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::assertCharacter(private$cdmDatabaseSchema, null.ok = TRUE, len = 1, add = errorMessages)
      checkmate::assertCharacter(private$resultSchema, null.ok = TRUE, len = 1, add = errorMessages)
      checkmate::assertCharacter(private$cohortTable, null.ok = FALSE, len = 1, add = errorMessages)
      checkmate::assertClass(private$cdm, classes = "cdm_reference", null.ok = TRUE)
      checkmate::reportAssertions(collection = errorMessages)
      
      return(invisible(self))
    },
    
    #' @description
    #' Gets the specified items packged in a named list.
    #' 
    #' @return
    #' \link[base]{list}
    #' \describe{
    #'   \item{connectionDetails}{(`ConnectionDetails`)ConnectionDetails object. See \link[DatabaseConnector]{createConnectionDetails}.}
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
    },
    
    fetchCohortTable = function(...) {
      if (is.null(private$cdm)) {
        extractCohortTable(...)
      } else if (is.null(private$connectionDetails)) {
          return(cdm[[private$cohortTable]])
      }
    }
  ),
  private = list(
    # private ----
    connectionDetails = NULL,
    cdmDatabaseSchema = "",
    resultSchema = "",
    cohortTable = "",
    cdm = NULL
  )
)
