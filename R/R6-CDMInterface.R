#' @title CDMInterface
#'
#' @description
#' Abstract interface to the CDM, using CDMConnector or DatabaseConnector.
CDMInterface <- R6::R6Class(
  classname = "CDMInterface",
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param connectionDetails (`DatabaseConnector::createConnectionDetails()`)
    #' @param cdmSchema (`character(1)`)
    #' @param resultSchema (`character(1)`)
    #' @param cdm (`CDMConnector::cdmFromCon()`)
    #'
    #' @return (`invisible(self)`)
    initialize = function(connectionDetails = NULL, cdmSchema = NULL, resultSchema = NULL, cdm = NULL) {
      private$connectionDetails <- connectionDetails
      private$cdmSchema <- cdmSchema
      private$resultSchema <- resultSchema
      private$cdm <- cdm
      
      if (!is.null(cdm)) {
        private$type <- "CDMConnector"
      } else if (!is.null(connectionDetails)) {
        private$type <- "DatabaseConnector"
      } else {
        stop("Could not assert if CDMConnector or DatabaseConnector is being used.")
      }
      self$validate()
      return(invisible(self))
    },
    
    #' @description
    #' Validation method
    #' 
    #' @return (`invisible(self)`)
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      
      checkmate::assertClass(private$connectionDetails, "ConnectionDetails", null.ok = TRUE, add = errorMessages)
      checkmate::assertCharacter(x = private$connectionDetails$dbms, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::assertCharacter(private$cdmDatabaseSchema, null.ok = TRUE, len = 1, add = errorMessages)
      checkmate::assertCharacter(private$resultSchema, null.ok = TRUE, len = 1, add = errorMessages)
      checkmate::assertClass(private$cdm, classes = "cdm_reference", null.ok = TRUE, add = errorMessages)
      
      checkmate::reportAssertions(collection = errorMessages)
      return(invisible(self))
    },
    
    #' @description
    #' Fetch specified cohort IDs from a specified cohort table
    #'
    #' @return (`data.frame`)
    fetchChortTable = function(cohortIds, cohortTableName) {
      switch(
        private$type,
        CDMConnector = private$cdmconFetchChortTable(cohortIds, cohortTableName),
        DatabaseConnector = private$dbconFetchCohortTable(cohortIds, cohortTableName)
      )
    },
    
    #' @description
    #' Stratisfy the treatmentHistory data frame by age.
    #'
    #' @return (`data.frame()`)
    addAge = function(andromeda) {
      switch(
        private$type,
        CDMConnector = private$cdmconAddAge(andromeda),
        DatabaseConnector = private$dbconAddAge(andromeda)
      )
    },
    
    #' @description
    #' Stratisfy the treatmentHistory data frame by sex.
    #'
    #' @return (`data.frame()`)
    addSex = function(andromeda) {
      switch(
        private$type,
        CDMConnector = private$cdmconAddSex(andromeda),
        DatabaseConnector = private$dbconAddSex(andromeda)
      )
    }
  ),
  private = list(
    connectionDetails = NULL,
    cdmSchema = NULL,
    resultSchema = NULL,
    cdm = NULL,
    type = "",
    
    dbconFetchCohortTable = function(cohortIds, cohortTableName) {
      renderedSql <- SqlRender::render(
        sql = "SELECT * FROM @resultSchema.@cohortTableName WHERE cohort_definition_id IN (@cohortIds)",
        resultSchema = private$resultSchema,
        cohortTableName = cohortTableName,
        cohortIds = cohortIds
      )
      
      connection <- DatabaseConnector::connect(private$connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
      
      translatedSql <- SqlRender::translate(
        sql = renderedSql,
        targetDialect = connection@dbms
      )
      return(DatabaseConnector::querySql(connection, translatedSql))
    },
    
    dbconAddAge = function(andromeda) {
      message("Not yet implemented")
      
      personIds <- andromeda$treatmentHistory %>% select("person_id") %>% pull()
      
      renderedSql <- SqlRender::render(
        sql = "SELECT
                 person_id,
                 year_of_birth
               FROM @resultSchema.person
               WHERE person_id IN (@personIds)",
        resultSchema = private$resultSchema,
        personIds = personIds
      )
      
      connection <- DatabaseConnector::connect(private$connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
      
      translatedSql <- SqlRender::translate(
        sql = renderedSql,
        targetDialect = connection@dbms)
      
      andromeda$year_of_birth <- DatabaseConnector::querySql(connection, translatedSql)
      
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::inner_join(andromeda$year_of_birth, by = dplyr::join_by(person_id == PERSON_ID)) %>%
        dplyr::mutate(age = .data$index_year - .data$YEAR_OF_BIRTH) %>%
        select(-"YEAR_OF_BIRTH")
    },
    
    dbconAddSex = function(andromeda) {
      # message("Not yet implemented")
      
      personIds <- andromeda$treatmentHistory %>% select("person_id") %>% pull()
      
      renderedSql <- SqlRender::render(
        sql = "SELECT
                 person_id,
                 concept_name AS sex
               FROM @resultSchema.person
               INNER JOIN @resultSchema.concept
               ON person.gender_concept_id = concept.concept_id
               WHERE person_id IN (@personIds)",
        resultSchema = private$resultSchema,
        personIds = personIds
      )
      
      connection <- DatabaseConnector::connect(private$connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
      
      translatedSql <- SqlRender::translate(
        sql = renderedSql,
        targetDialect = connection@dbms)
      
      andromeda$sex <- DatabaseConnector::querySql(connection, translatedSql)
      
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::inner_join(andromeda$sex, by = dplyr::join_by(person_id == PERSON_ID)) %>%
        dplyr::rename(sex = "SEX")
      
      return(invisible(NULL))
    },
    
    cdmconFetchChortTable = function(cohortIds, cohortTableName) {
      return(
        private$cdm[[cohortTableName]] %>%
          filter(.data$cohort_definition_id %in% cohortIds))
    },
    
    cdmconAddAge = function() {
      message("Not yet implemented")
    },
    
    cdmconAddSex = function() {
      message("Not yet implemented")
    }
  )
)