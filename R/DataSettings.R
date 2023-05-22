# ==== DataSettings ===========================================================
#' DataSettings
#' 
#' DataSettings R6 object. Inherits from Settings
#'
#' @export
DataSettings <- R6::R6Class(
  classname = "DataSettings",
  inherit = Settings,
  public = list(
    #' @description
    #'   initialize method
    #' 
    #' @param connectionDetails
    #'   <ConnectionDetails> object >= 6.0.0.
    #' @param cdmDatabaseSchema
    #'   <character> CDM schema name in the database.
    #' @param resultSchema
    #'   <character> Name of results schema.
    #' @param cohortTable
    #'   <character> Name of the cohort table in the results schema.
    #' 
    #' @return
    #'   invisible(self)
    initialize = function(
    connectionDetails,
    cdmDatabaseSchema = "",
    resultSchema = "",
    cohortTable = "") {
      self$validate(connectionDetails, cdmDatabaseSchema, resultSchema, cohortTable)
      private$connectionDetails <- connectionDetails
      private$cdmDatabaseSchema <- cdmDatabaseSchema
      private$resultSchema <- resultSchema
      private$cohortTable <- cohortTable
      return(invisible(self))
    },
    
    #' @description
    #'   print method
    #' 
    #' @return
    #'   invisible(self)
    print = function() {
      print(
        glue::glue(
          cli::style_bold("cdmDatabaseSchema: "), "{private$cdmDatabaseSchema}\n",
          cli::style_bold("resultSchema: "), "{private$resultSchema}\n",
          cli::style_bold("cohortTable: "), "{private$cohortTable}\n",
          cli::style_bold("ConnectionDetails:\n"),
          cli::style_italic("\tDBMS: "), "{private$connectionDetails$dbms}\n",
          cli::style_italic("\tOracle Driver: "), "{private$connectionDetails$oracleDriver}\n",
          cli::style_italic("\tPath to Driver: "), "{private$connectionDetails$pathToDriver}\n"
        )
      )
      return(invisible(self))
    },
    
    #' @description
    #'   get method
    #' 
    #' @return
    #'   <list>
    get = function() {
      list(
        connectionDetails = private$connectionDetails,
        cdmDatabaseSchema = private$cdmDatabaseSchema,
        resultSchema = private$resultSchema,
        cohortTable = private$cohortTable
      )
    },
    
    #' @description
    #'   validate method
    #' 
    #' @param connectionDetails
    #'   <ConnectionDetails> object >= 6.0.0.
    #' @param cdmDatabaseSchema
    #'   <character> CDM schema name in the database.
    #' @param resultSchema
    #'   <character> Name of results schema.
    #' @param cohortTable
    #'   <character> Name of the cohort table in the results schema.
    #' 
    #' @return
    #'   invisible(self)
    validate = function(
    connectionDetails,
    cdmDatabaseSchema,
    resultSchema,
    cohortTable) {
      # ConnectionDetails
      checkmate::assert(
        checkmate::checkClass(
          connectionDetails,
          "ConnectionDetails"),
        checkmate::checkCharacter(
          x = connectionDetails$dbms,
          len = 1,
          null.ok = FALSE),
        combine = "and"
      )
      
      # cdmDatabaseSchema
      checkmate::assert(
        checkmate::checkCharacter(
          cdmDatabaseSchema,
          null.ok = FALSE,
          len = 1)
      )
      
      # resultSchema
      checkmate::assert(
        checkmate::checkCharacter(
          resultSchema,
          null.ok = FALSE,
          len = 1)
      )
      
      # cohortTable
      checkmate::assert(
        checkmate::checkCharacter(
          cohortTable,
          null.ok = FALSE,
          len = 1)
      )
      return(invisible(self))
    }),
  
  private = list(
    connectionDetails = NULL,
    cdmDatabaseSchema = "",
    resultSchema = "",
    cohortTable = "")
)
