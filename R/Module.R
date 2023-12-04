#' @title Module
#'
#' @description
#' Module super class
#' 
#' @field namespace Namespace of the module.
Module <- R6::R6Class(
  classname = "Module",
  
  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param namespace (`character(1)`)
    #'
    #' @return (`invisible(self)`)
    initialize = function(namespace) {
      private$.namespace <- namespace
      self$validate()
      return(invisible(self))
    },

    #' @description
    #' Validator method
    #'
    #' @return (`invisible(self)`)
    validate = function() {
      private$assertDependencies()
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(private$.namespace, len = 1)
      checkmate::reportAssertions(assertions)
      return(invisible(self))
    },
    
    #' @description
    #' Method to include a \link[shinydashboard]{menuItem} to link to the body.
    #'
    #' @param label (`character(1)`)\cr
    #' Label to show for the `menuItem`.
    #' 
    #' @param tag (`character(1)`)\cr
    #' Tag to use internally in `input`.
    #' 
    #' @return (`menuItem`)
    uiMenu = function(label, tag) {},
    
    #' @description
    #' Method to include a \link[shinydashboard]{tabItem} to include the body.
    #'
    #' @return (`tabItem`)
    uiBody = function() {},
    
    #' @description
    #' Method to handle the back-end.
    #'
    #' @param input (`input`)\cr
    #' Input from the server function.
    #'
    #' @param output (`output`)\cr
    #' Output from the server function.
    #'
    #' @param session (`session`)\cr
    #' Session from the server function.
    #' 
    #' @return (`NULL`)
    server = function(input, output, session) {}
  ),
  
  # Private ----
  private = list(
    ## Fields ----
    .namespace = "",
    
    assertDependencies = function() {
      dependencies <- c("shiny", "shinydashboard", "ggplot2", "plotly", "dplyr")
      missing <- !c(
        require("shiny", quietly = TRUE, mask.ok = TRUE, character.only = TRUE),
        require("shinydashboard", quietly = TRUE, mask.ok = TRUE, character.only = TRUE),
        require("ggplot2", quietly = TRUE, mask.ok = TRUE, character.only = TRUE),
        require("plotly", quietly = TRUE, mask.ok = TRUE, character.only = TRUE),
        require("dplyr", quietly = TRUE, mask.ok = TRUE, character.only = TRUE)
      )
      
      if (length(dependencies[missing]) > 0) {
        stop(sprintf(
          "The following packages are required but not installed: %s",
          paste0(dependencies[missing], collapse = ", ")
        ))
      }
    },

    ## Methods ----
    finalize = function() {}
  ),
  
  # Active ----
  active = list(
    namespace = function() return(private$.namespace)
  )
)
