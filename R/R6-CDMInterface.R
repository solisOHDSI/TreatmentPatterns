#' @title CDMInterface
#'
#' @description
#' Abstract interface to the CDM, using CDMConnector or DatabaseConnector.
CDMInterface <- R6::R6Class(
  classname = "CDMInterface",
  public = list(
    ## Public ----
    ### Methods ----
    #' @description
    #' Initializer method
    #'
    #' @template param_connectionDetails
    #' @template param_cdmSchema
    #' @template param_resultSchema
    #' @template param_cdm
    #'
    #' @return (`invisible(self)`)
    initialize = function(connectionDetails = NULL, cdmSchema = NULL, resultSchema = NULL, cdm = NULL) {
      private$connectionDetails <- connectionDetails
      if (!is.null(private$connectionDetails)) {
        private$connection <- DatabaseConnector::connect(private$connectionDetails)
      }
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

      checkmate::assertClass(
        x = private$connectionDetails,
        "ConnectionDetails",
        null.ok = TRUE,
        add = errorMessages
      )
      
      checkmate::assertCharacter(
        x = private$connectionDetails$dbms,
        len = 1,
        null.ok = TRUE,
        add = errorMessages
      )
      
      checkmate::assertCharacter(
        private$cdmDatabaseSchema,
        null.ok = TRUE,
        len = 1,
        add = errorMessages
      )
      
      checkmate::assertCharacter(
        private$resultSchema,
        null.ok = TRUE,
        len = 1,
        add = errorMessages
      )
      
      checkmate::assertClass(
        private$cdm,
        classes = "cdm_reference",
        null.ok = TRUE,
        add = errorMessages
      )

      checkmate::reportAssertions(collection = errorMessages)
      return(invisible(self))
    },

    #' @description
    #' Fetch specified cohort IDs from a specified cohort table
    #'
    #' @template param_cohortIds
    #' @template param_cohortTableName
    #' @template param_andromeda
    #' @param andromedaTableName (`character(1)`)\cr
    #' Name of the table in the Andromeda object where the data will be loaded.
    #' @template param_minEraDuration
    #'
    #' @return (`data.frame`)
    fetchCohortTable = function(cohortIds, cohortTableName, andromeda, andromedaTableName, minEraDuration) {
      switch(private$type,
        CDMConnector = private$cdmconFetchCohortTable(cohortIds, cohortTableName, andromeda, andromedaTableName, minEraDuration),
        DatabaseConnector = private$dbconFetchCohortTable(cohortIds, cohortTableName, andromeda, andromedaTableName, minEraDuration)
      )
    },

    #' @description
    #' Stratisfy the treatmentHistory data frame by age.
    #'
    #' @template param_andromeda
    #'
    #' @return (`data.frame()`)
    addAge = function(andromeda) {
      switch(private$type,
        CDMConnector = private$cdmconAddAge(andromeda),
        DatabaseConnector = private$dbconAddAge(andromeda)
      )
    },

    #' @description
    #' Stratisfy the treatmentHistory data frame by sex.
    #'
    #' @template param_andromeda
    #'
    #' @return (`data.frame()`)
    addSex = function(andromeda) {
      switch(private$type,
        CDMConnector = private$cdmconAddSex(andromeda),
        DatabaseConnector = private$dbconAddSex(andromeda)
      )
    },

    #' @description
    #' Fetch metadata from CDM
    #'
    #' @template param_andromeda
    #'
    #' @return (`invisible(NULL)`)
    fetchMetadata = function(andromeda) {
      switch(private$type,
        CDMConnector = private$cdmconFetchMetadata(andromeda),
        DatabaseConnector = private$dbconFetchMetadata(andromeda)
      )
      return(invisible(self))
    },
    
    #' @description
    #' Destroys instance
    #' 
    #' @return (NULL)
    destroy = function() {
      private$finalize()
    }
  ),
  private = list(
    ## Private ----
    ### Fields ----
    connectionDetails = NULL,
    connection = NULL,
    cdmSchema = NULL,
    resultSchema = NULL,
    cdm = NULL,
    type = "",

    ### Methods ----
    finalize = function() {
      if (!is.null(private$connection)) {
        DatabaseConnector::disconnect(private$connection)
      }
    },
    
    #### DatabaseConnector ----
    # cohortIds (`integer(n)`)
    # cohortTableName (`character(1)`)
    dbconFetchCohortTable = function(cohortIds, cohortTableName, andromeda, andromedaTableName, minEraDuration) {
      renderedSql <- SqlRender::render(
        sql = "
        SELECT
          *
        FROM
          @resultSchema.@cohortTableName
        WHERE
          cohort_definition_id
        IN
          (@cohortIds)
        AND
          cohort_end_date - cohort_start_date > @minEraDuration
        ;",
        resultSchema = private$resultSchema,
        cohortTableName = cohortTableName,
        cohortIds = cohortIds,
        minEraDuration = minEraDuration
      )

      translatedSql <- SqlRender::translate(
        sql = renderedSql,
        targetDialect = private$connection@dbms
      )
      
      DatabaseConnector::querySqlToAndromeda(
        connection = private$connection,
        sql = translatedSql,
        andromeda = andromeda,
        andromedaTableName = andromedaTableName
      )
      
      return(invisible(self))
    },

    # andromeda (`Andromeda::andromeda()`)
    dbconAddAge = function(andromeda) {
      personIds <- andromeda$treatmentHistory %>%
        select("personId") %>%
        pull()

      renderedSql <- SqlRender::render(
        sql = "
        SELECT
          person_id,
          year_of_birth
        FROM
          @cdmSchema.person
        WHERE
          person_id
        IN
          (@personIds)
        ;",
        cdmSchema = private$cdmSchema,
        personIds = personIds
      )

      translatedSql <- SqlRender::translate(
        sql = renderedSql,
        targetDialect = private$connection@dbms
      )

      andromeda$yearOfBirth <- DatabaseConnector::querySql(
        connection = private$connection,
        sql = translatedSql,
        snakeCaseToCamelCase = TRUE
      )

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::inner_join(andromeda$yearOfBirth, by = dplyr::join_by(personId == personId)) %>%
        dplyr::mutate(age = .data$indexYear - .data$yearOfBirth) %>%
        dplyr::select(-"yearOfBirth")

      return(invisible(self))
    },

    # andromeda (`Andromeda::andromeda()`)
    dbconAddSex = function(andromeda) {
      personIds <- andromeda$treatmentHistory %>%
        select("personId") %>%
        pull()

      renderedSql <- SqlRender::render(
        sql = "
        SELECT
          person_id,
          concept_name AS sex
        FROM
          @cdmSchema.person
        INNER JOIN
          @cdmSchema.concept
        ON
          person.gender_concept_id = concept.concept_id
        WHERE
          person_id
        IN
          (@personIds)
        ;",
        cdmSchema = private$cdmSchema,
        personIds = personIds
      )

      translatedSql <- SqlRender::translate(
        sql = renderedSql,
        targetDialect = private$connection@dbms
      )

      andromeda$sex <- DatabaseConnector::querySql(
        connection = private$connection,
        sql = translatedSql,
        snakeCaseToCamelCase = TRUE
      )

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::inner_join(andromeda$sex, by = dplyr::join_by(personId == personId))

      return(invisible(self))
    },
    dbconFetchMetadata = function(andromeda) {
      renderedSql <- SqlRender::render(
        sql = "
        SELECT
          cdm_source_name,
          cdm_source_abbreviation,
          cdm_release_date,
          vocabulary_version
        FROM
          @cdmSchema.cdm_source
      ;",
        cdmSchema = private$cdmSchema
      )

      translatedSql <- SqlRender::translate(
        sql = renderedSql,
        targetDialect = private$connection@dbms
      )

      andromeda$metadata <- DatabaseConnector::querySql(
        connection = private$connection,
        sql = translatedSql,
        snakeCaseToCamelCase = TRUE) %>%
        dplyr::mutate(
          executionStartDate = as.character(Sys.Date()),
          packageVersion = as.character(utils::packageVersion("TreatmentPatterns")),
          rVersion = base::version$version.string,
          platform = base::version$platform
        )
      
      return(invisible(self))
    },
    #### CDMConnector ----
    # cohortIds (`integer(n)`)
    # cohortTableName (`character(1)`)
    # andromeda (`Andromeda::andromeda()`)
    # andromedaTableName (`character(1)`)
    cdmconFetchCohortTable = function(cohortIds, cohortTableName, andromeda, andromedaTableName, minEraDuration) {
      andromeda[[andromedaTableName]] <- private$cdm[[cohortTableName]] %>%
        dplyr::filter(.data$cohort_definition_id %in% cohortIds) %>%
        dplyr::filter(.data$cohort_end_date - .data$cohort_start_date >= minEraDuration)
      return(invisible(self))
    },

    # andromeda (`Andromeda::andromeda()`)
    cdmconAddAge = function(andromeda) {
      personIds <- andromeda$treatmentHistory %>%
        dplyr::select("personId") %>%
        dplyr::pull()

      andromeda$yearOfBirth <- private$cdm$person %>%
        dplyr::select("person_id", "year_of_birth")
      
      andromeda$yearOfBirth <- andromeda$yearOfBirth %>%
        SqlRender::snakeCaseToCamelCaseNames()
      
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::inner_join(andromeda$yearOfBirth, by = dplyr::join_by(personId == personId)) %>%
        dplyr::mutate(age = .data$indexYear - .data$yearOfBirth) %>%
        dplyr::select(-"yearOfBirth")

      return(invisible(self))
    },

    # andromeda (`Andromeda::andromeda()`)
    cdmconAddSex = function(andromeda) {
      # message("Not yet implemented")
      personIds <- andromeda$treatmentHistory %>%
        select("personId") %>%
        pull()
      # personIds <- c(1,2,3,4)

      andromeda$sex <- private$cdm$person %>%
        dplyr::filter(.data$person_id %in% personIds) %>%
        dplyr::inner_join(private$cdm$concept, by = join_by(gender_concept_id == concept_id)) %>%
        dplyr::select("person_id", "concept_name") %>%
        dplyr::collect() %>%
        SqlRender::snakeCaseToCamelCaseNames()

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::inner_join(andromeda$sex, by = join_by(personId == personId)) %>%
        dplyr::rename(sex = "conceptName")

      return(invisible(self))
    },

    # andromeda (`Andromeda::andromeda()`)
    cdmconFetchMetadata = function(andromeda) {
      andromeda$metadata <- private$cdm$cdm_source %>%
        dplyr::select(
          "cdm_source_name",
          "cdm_source_abbreviation",
          "cdm_release_date",
          "vocabulary_version"
        ) %>%
        dplyr::collect() %>%
        SqlRender::snakeCaseToCamelCaseNames() %>%
        dplyr::mutate(
          executionStartDate = as.character(Sys.Date()),
          packageVersion = as.character(utils::packageVersion("TreatmentPatterns")),
          rVersion = base::version$version.string,
          platform = base::version$platform
        )
      return(invisible(self))
    }
  )
)
