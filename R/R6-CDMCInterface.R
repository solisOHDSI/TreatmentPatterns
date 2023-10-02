CDMCInterface <- R6::R6Class(
  classname = "CDMCInterface",
  inherit = CDMInterface,
  ## Public ----
  public = list(
    ### Methods ----
    initialize = function(cdm = NULL) {
      private$.cdm <- cdm
      
      self$validate()
    },
    
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      
      checkmate::assertClass(
        private$.cdm,
        classes = "cdm_reference",
        null.ok = TRUE,
        add = errorMessages
      )
      
      checkmate::reportAssertions(collection = errorMessages)
      
      return(invisible(self))
    },
    
    fetchCohortTable = function(cohortIds, cohortTableName, andromeda, andromedaTableName) {
      andromeda[[andromedaTableName]] <- private$.cdm[[cohortTableName]] %>%
        dplyr::filter(.data$cohort_definition_id %in% cohortIds)
      return(invisible(self))
    },

    addAge = function(andromeda) {
      personIds <- super$fetchPersonIds(andromeda)
      
      andromeda$yearOfBirth <- private$.cdm$person %>%
        dplyr::select("person_id", "year_of_birth")
      
      andromeda$yearOfBirth <- andromeda$yearOfBirth %>%
        SqlRender::snakeCaseToCamelCaseNames()
      
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::inner_join(andromeda$yearOfBirth, by = dplyr::join_by(personId == personId)) %>%
        dplyr::mutate(age = .data$indexYear - .data$yearOfBirth) %>%
        dplyr::select(-"yearOfBirth")
      
      return(invisible(self))
    },
    
    addSex = function(andromeda) {
      personIds <- super$fetchPersonIds(andromeda)
      
      andromeda$sex <- private$.cdm$person %>%
        dplyr::filter(.data$person_id %in% personIds) %>%
        dplyr::inner_join(private$.cdm$concept, by = join_by(gender_concept_id == concept_id)) %>%
        dplyr::select("person_id", "concept_name") %>%
        dplyr::collect() %>%
        SqlRender::snakeCaseToCamelCaseNames()
      
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::inner_join(andromeda$sex, by = join_by(personId == personId)) %>%
        dplyr::rename(sex = "conceptName")
      
      return(invisible(self))
    },
    
    fetchMetadata = function(andromeda) {
      andromeda$metadata <- private$.cdm$cdm_source %>%
        dplyr::select(
          "cdm_source_name",
          "cdm_source_abbreviation",
          "cdm_release_date",
          "vocabulary_version"
        ) %>%
        dplyr::collect() %>%
        SqlRender::snakeCaseToCamelCaseNames() %>%
        dplyr::bind_cols(super$fetchSessionInfo())
      return(invisible(self))
    }
  ),
  ## Private ----
  private = list(
    ### Fields ----
    .cdm = NULL
  ),
  ## Active ----
  active = list(
    ### Bindings ----
    cdm = function() {
      return(private$.cdm)
    }
  )
)
