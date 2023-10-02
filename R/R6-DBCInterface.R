DBCInterface <- R6::R6Class(
  classname = "DBCInterface",
  inherit = CDMInterface,
  ## Public ----
  public = list(
    ### Methods ----
    initialize = function(connectionDetails = NULL, cdmSchema = NULL, resultSchema = NULL) {
      private$.connectionDetails <- connectionDetails
      private$.cdmSchema <- cdmSchema
      private$.resultSchema <- resultSchema
      private$.connection <- DatabaseConnector::connect(connectionDetails)
      
      self$validate()
    },
    
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      
      checkmate::assertClass(
        x = private$.connectionDetails,
        "ConnectionDetails",
        null.ok = TRUE,
        add = errorMessages
      )
      
      checkmate::assertCharacter(
        x = private$.connectionDetails$dbms,
        len = 1,
        null.ok = TRUE,
        add = errorMessages
      )
      
      checkmate::assertCharacter(
        private$.cdmDatabaseSchema,
        null.ok = TRUE,
        len = 1,
        add = errorMessages
      )
      
      checkmate::assertCharacter(
        private$.resultSchema,
        null.ok = TRUE,
        len = 1,
        add = errorMessages
      )
      
      checkmate::reportAssertions(collection = errorMessages)
      
      return(invisible(self))
    },
    
    fetchCohortTable = function(cohortIds, cohortTableName, andromeda, andromedaTableName) {
      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "fetchCohortTable.sql",
        packageName = "TreatmentPatterns",
        dbms = private$.connection@dbms,
        # Variables
        resultSchema = private$.resultSchema,
        cohortTableName = cohortTableName,
        cohortIds = cohortIds
      )

      DatabaseConnector::querySqlToAndromeda(
        connection = private$.connection,
        sql = sql,
        andromeda = andromeda,
        andromedaTableName = andromedaTableName
      )

      return(invisible(self))
    },
    
    addAge = function(andromeda) {
      personIds <- super$fetchPersonIds(andromeda)
      
      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "fetchYearOfBirthPerPerson.sql",
        packageName = "TreatmentPatterns",
        dbms = private$.connection@dbms,
        # Variables
        cdmSchema = private$.cdmSchema,
        personIds = personIds
      )
      
      DatabaseConnector::querySqlToAndromeda(
        connection = private$.connection,
        sql = sql,
        andromeda = andromeda,
        andromedaTableName = "yearOfBirth",
        snakeCaseToCamelCase = TRUE
      )
      
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::inner_join(andromeda$yearOfBirth, by = dplyr::join_by(personId == personId)) %>%
        dplyr::mutate(age = .data$indexYear - .data$yearOfBirth) %>%
        dplyr::select(-"yearOfBirth")
      
      return(invisible(self))
    },
    
    addSex = function(andromeda) {
      personIds <- super$fetchPersonIds(andromeda)

      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "fetchSexPerPerson.sql",
        packageName = "TreatmentPatterns",
        dbms = private$.connection@dbms,
        # Variables
        cdmSchema = private$.cdmSchema,
        personIds = personIds
      )

      DatabaseConnector::querySqlToAndromeda(
        connection = private$.connection,
        andromeda = andromeda,
        sql = sql,
        andromedaTableName = "sex",
        snakeCaseToCamelCase = TRUE
      )

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::inner_join(andromeda$sex, by = dplyr::join_by(personId == personId))

      return(invisible(self))
    },

    fetchMetadata = function(andromeda) {
      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "fetchMetaData.sql",
        packageName = "TreatmentPatterns",
        dbms = private$.connection@dbms,
        # Variables
        cdmSchema = private$.cdmSchema
      )

      DatabaseConnector::querySqlToAndromeda(
        connection = private$.connection,
        sql = sql,
        andromeda = andromeda,
        andromedaTableName = "metadata",
        snakeCaseToCamelCase = TRUE
      )
      
      andromeda$metadata <- andromeda$metadata %>%
        dplyr::collect() %>%
        dplyr::bind_cols(super$fetchSessionInfo())
      
      return(invisible(self))
    }
  ),
  
  ## Private ----
  private = list(
    ### Fields ----
    .connectionDetails = NULL,
    .connection = NULL,
    .cdmSchema = NULL,
    .resultSchema = NULL,
    
    ### Methods ----
    finalize = function() {
      DatabaseConnector::disconnect(private$.connection)
    }
  ),
  ## Active ----
  active = list(
    ### Bindings ----
    connectionDeatils = function() {
      return(private$.connectionDetails)
    },
    
    connection = function() {
      return(private$.connection)
    },
    
    cdmSchema = function() {
      return(private$.cdmSchema)
    },
    
    resultSchema = function() {
      return(private$.resultSchema)
    }
  )
)
