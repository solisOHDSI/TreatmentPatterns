#' PathwayConstructor
#' 
#' @export
PathwayConstructor <- R6::R6Class(
  classname = "PathwayConstructor",
  public = list(
    # Public ----
    initialize = function(cohortSettings, pathwaySettings, cohortTable, connectionDetails = NULL, cdmSchema = NULL, resultSchema = NULL, cdm = NULL) {
      private$cohortSettings <- cohortSettings$get()
      private$pathwaySettings <- pathwaySettings$get()
      private$cohortTable <- cohortTable
      private$connectionDetails <- connectionDetails
      private$cdmSchema <- cdmSchema
      private$resultSchema <- resultSchema
      private$cdm <- cdm
      
      # Set up Andromeda sqlite environment
      private$andromeda <- Andromeda::andromeda()
      
      self$validate()
    },
    
    validate = function() {
      cohortSettings$validate()
      pathwaySettings$validate()
      
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertTRUE(Andromeda::isAndromeda(private$andromeda), add = errorMessages)
      checkmate::assertClass(private$connectionDetails, "ConnectionDetails", null.ok = TRUE, add = errorMessages)
      checkmate::assertCharacter(x = private$connectionDetails$dbms, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::assertCharacter(private$cdmDatabaseSchema, null.ok = TRUE, len = 1, add = errorMessages)
      checkmate::assertCharacter(private$resultSchema, null.ok = TRUE, len = 1, add = errorMessages)
      checkmate::assertCharacter(private$cohortTable, null.ok = FALSE, len = 1, add = errorMessages)
      checkmate::assertClass(private$cdm, classes = "cdm_reference", null.ok = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
    },
    
    construct = function() {
      private$andromeda$fullCohorts <- private$fetchCohortTable()
      
      private$andromeda$fullCohorts <- private$andromeda$fullCohorts %>%
        dplyr::rename(
          cohort_id = "COHORT_DEFINITION_ID",
          person_id = "SUBJECT_ID",
          start_date = "COHORT_START_DATE",
          end_date = "COHORT_END_DATE")
      
      constructPathways(
        pathwaySettings = private$pathwaySettings,
        cohortSettings = private$cohortSettings,
        andromeda = private$andromeda
      )
    },
    
    getAndromeda = function() {
      return(private$andromeda)
    }
  ),
  private = list(
    # Private ----
    ## Fields ----
    pathwaySettings = NULL,
    cohortSettings = NULL,
    andromeda = NULL,
    connectionDetails = NULL,
    cdmSchema = NULL,
    resultSchema = NULL,
    cdm = NULL,
    cohortTable = NULL,
    
    fetchCohortTable = function() {
      if (is.null(private$cdm)) {
        if (all(is.null(cohortSettings$cohortId)) | all(cohortSettings$cohortId)) {
          renderedSql <- SqlRender::render(
            "SELECT * FROM @resultSchema.@cohortTable",
            resultSchema = private$resultSchema,
            cohortTable = private$cohortTable
          )
        } else {
          renderedSql <- SqlRender::render(
            "SELECT * FROM @resultSchema.@cohortTable WHERE cohort_definition_id IN (@cohortIds)",
            resultSchema = private$resultSchema,
            cohortTable = private$cohortTable,
            cohortIds = cohortSettings$cohortId
          )
        }
        
        connection <- DatabaseConnector::connect(private$connectionDetails)
        on.exit(DatabaseConnector::disconnect(connection))
        
        translatedSql <- SqlRender::translate(
          renderedSql,
          targetDialect = connectionDetails$dbms
        )
        DatabaseConnector::querySql(connection, translatedSql)
      } else {
        return(cdm[[private$cohortTable]])
      }
    }
  )
)
