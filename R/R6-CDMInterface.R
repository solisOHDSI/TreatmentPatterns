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
    initialize = function(connectionDetails, cdmSchema, resultSchema, cdm) {},

    #' @description
    #' Validation method
    #'
    #' @return (`invisible(self)`)
    validate = function() {},

    #' @description
    #' Fetch specified cohort IDs from a specified cohort table
    #'
    #' @template param_cohortIds
    #' @template param_cohortTableName
    #' @template param_andromeda
    #' @param andromedaTableName (`character(1)`)\cr
    #' Name of the table in the Andromeda object where the data will be loaded.
    #'
    #' @return (`data.frame`)
    fetchCohortTable = function(cohortIds, cohortTableName, andromeda, andromedaTableName) {},

    #' @description
    #' Stratisfy the treatmentHistory data frame by age.
    #'
    #' @template param_andromeda
    #'
    #' @return (`data.frame()`)
    addAge = function(andromeda) {},

    #' @description
    #' Stratisfy the treatmentHistory data frame by sex.
    #'
    #' @template param_andromeda
    #'
    #' @return (`data.frame()`)
    addSex = function(andromeda) {},

    #' @description
    #' Fetch metadata from CDM
    #'
    #' @template param_andromeda
    #'
    #' @return (`invisible(NULL)`)
    fetchMetadata = function(andromeda) {},
    
    #' @description
    #' Destroys instance
    #' 
    #' @return (NULL)
    destroy = function() {
      private$finalize()
    }
  ),
  
  ## Private ----
  private = list(
    ### Methods ----
    finalize = function() {},
    
    fetchPersonIds = function(andromeda) {
      return(
        andromeda$treatmentHistory %>%
          dplyr::select("personId") %>%
          dplyr::pull()
      )
    },
    
    fetchSessionInfo = function() {
      return(list(
        executionStartDate = as.character(utils::packageVersion("TreatmentPatterns")),
        packageVersion = as.character(Sys.Date()),
        rVersion = base::version$version.string,
        platform = base::version$platform
      ))
    }
  )
)

#' cdmInterfaceFactory
#' 
#' @noRd
#' 
#' @template param_cdm
#' @template param_connectionDetails 
#' @template param_cdmSchema 
#' @template param_resultSchema
#'
#' @return (`CDMInterface`)
cdmInterfaceFactory <- function(
    cdm = NULL,
    connectionDetails = NULL,
    cdmSchema = NULL,
    resultSchema = NULL) {
  
  if (!is.null(connectionDetails)) {
    return(
      DBCInterface$new(
        connectionDetails = connectionDetails,
        cdmSchema = cdmSchema,
        resultSchema = resultSchema
      )
    )
  } else if (!is.null(cdm)) {
    return(
      CDMCInterface$new(
        cdm = cdm
      )
    )
  } else {
    stop("Could not assert ConnectionDetails or CDMConnector object.")
  }
}
