CDMCInterface <- R6::R6Class(
  classname = "CDMCInterface",
  inherit = "CDMInterface",

  # Public ----
  public = list(
    ## Methods ----
    initialize = function(cdm) {
      private$.cdm <- cdm
      
      self$validate()
    },

    validate = function() {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertClass(private$.cdm, classes = "cdm_reference")
      checkmate::reportAssertions(assertions)
    },

    fetchCohortTable = function(cohorts, cohortTableName, minEraDuration) {
      
    },
    fetchMetaData = function() {}
  ),

  # Private ----
  private = list(
    ## Fields ----
    .cdm = NULL,
    .andromeda = Andromeda::andromeda(),
    
    ## Methods ----
    finalize = function() {
      
    }
  ),

  # Active ----
  active = list(
    cdm = function() {
      return(private$.cdm)
    },
    
    andromeda = function() {
      return(private$.andromeda)
    }
  )
)