Stratifier <- R6::R6Class(
  classname = "Stratifier",
  
  # Public ----
  public = list(
    initialize = function(treatmentPathways) {
      private$.treatmentPathways <- treatmentPathways
    },
    validate = function() {},
    stratify = function() {}
  ),
  
  # Private ----
  private = list(
    ## Fields ----
    .treatmentPathways,

    ## Methods ----
    finalize = function() {}
  ),
  
  # Active ----
  active = list(
    treatmentPathways = function() return(private$.treatmentPathways)
  )
)
