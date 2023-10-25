#' DBCInterfade
#' 
#' @inherit CDMInterface
DBCInterface <- R6::R6Class(
  classname = "DBCInterfade",
  inherit = "CDMInterface",
  
  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #' 
    #' @template param_connectionDetails
    #' @template param_cdmSchema
    #' @template param_resultSchema
    #' @param tempEmulationSchema (`character(1)`: `NULL`)\cr
    #' Schema to emulate temp tables.
    #'
    #' @return (`self`)
    initialize = function(connectionDetails, cdmSchema, resultSchema, tempEmulationSchema = NULL) {
      private$.connectionDetails <- connectionDetails
      private$.cdmSchema <- cdmSchema
      private$.resultSchema <- resultSchema
      private$.tempEmulationSchema <- tempEmulationSchema
      
      self$validate()
      
      private$.connection <- DatabaseConnector::connect(private$.connectionDetails)
      
      return(invisible(self))
    },
    
    #' @description
    #' Validator method
    #'
    #' @return (`self`)
    validate = function() {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertClass(private$.connectionDetails, classes = "ConnectionDetails", null.ok = FALSE, add = assertions)
      checkmate::assertCharacter(private$.cdmSchema, min.chars = 1, len = 1, null.ok = FALSE, add = assertions)
      checkmate::assertCharacter(private$.resultSchema, min.chars = 1, len = 1, null.ok = FALSE, add = assertions)
      checkmate::assertCharacter(private$.tempEmulationSchema, n.chars = 1, len = 1, null.ok = TRUE, add = assertions)
      checkmate::reportAssertions(assertions)
      return(invisible(self))
    },
    
    #' @description
    #' fetchCohortTable
    #' 
    #' @template param_cohorts
    #' @template param_cohortTableName
    #' @template param_andromeda
    #' @param andromedaTableName (`character(1)`)\cr
    #' Name of the table to be used in the Andromeda environment.
    #' @template param_minEraDuration
    fetchCohortTable = function(cohorts, cohortTableName, minEraDuration) {
      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "selectData.sql",
        packageName = "TreatmentPatterns",
        dbms = private$connection@dbms,
        tempEmulationSchema = private$tempEmulationSchema,
        resultSchema = private$resultSchema,
        cdmSchema = private$cdmSchema,
        cohortTable = cohortTableName,
        cohortIds = cohorts$cohortId,
        minEraDuration = minEraDuration,
        targetCohortId = targetCohortId
      )
      
      DatabaseConnector::querySqlToAndromeda(
        connection = private$connection,
        sql = sql,
        andromeda = andromeda,
        andromedaTableName = andromedaTableName
      )
      
      return(invisible(self))
    },
    
    #' @description
    #' fetchMetaData
    fetchMetaData = function(andromeda) {
      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "fetchCdmSource.sql",
        packageName = "TreatmentPatterns",
        dbms = private$.connection@dbms,
        tempEmulationSchema = private$.tempEmulationSchema, 
        cdmSchema = private$cdmSchema
      )
      
      DatabaseConnector::querySqlToAndromeda(
        connection = private$.connection,
        sql = sql,
        andromeda = private$.andromeda,
        andromedaTableName = "metadata",
        snakeCaseToCamelCase = TRUE
      )
      
      private$.andromeda$metadata <- private$.andromeda$metadata %>% 
        dplyr::mutate(
          executionStartDate = as.character(Sys.Date()),
          packageVersion = as.character(utils::packageVersion("TreatmentPatterns")),
          rVersion = base::version$version.string,
          platform = base::version$platform
        )
      
      return(invisible(self))
    }
  ),
  
  # Private ----
  private = list(
    ## Fields ----
    .connectionDetails = NULL,
    .cdmSchema = "",
    .resultSchema = "",
    .tempEmulationSchema = NULL,
    .connection = NULL,
    .andromeda = Andromeda::andromeda(),
    
    ## Methods ----
    finalize = function() {
      DatabaseConnector::disconnect(private$.connection)
    },
    
    getTargetCohortId = function() {
      cohorts %>%
        dplyr::filter(.data$type == "target") %>%
        dplyr::select("cohortId") %>%
        dplyr::pull() %>%
        return()
    }
  ),
  
  # Active ----
  active = list(
    connectionDetails = function() {
      return(private$.connectionDetails)
    },
    
    cdmSchema = function() {
      return(private$.cdmSchema)
    },
    
    resultSchema = function() {
      return(private$.resultSchema)
    },
    
    tempEmulationSchema = function() {
      return(private$.tempEmulationSchema)
    },
    
    connection = function() {
      return(private$.connection)
    },
    
    andromeda = function() {
      return(private$.andromeda)
    }
  )
)