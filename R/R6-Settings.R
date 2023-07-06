#' @title Settings
#' 
#' @description
#' Settings super object.
Settings <- R6::R6Class(
  classname = "Settings",
  public = list(
    #' @description
    #' Initializer method
    #' 
    #' @return (`invisible(self)`)
    initialize = function() {
      return(invisible(self))
    },
    
    #' @description
    #' Getter method
    #' 
    #' @return (`NULL`)
    get = function() {
      return(NULL)
    },
    
    #' @description
    #' print overload
    #' 
    #' @return (`invisible(self)`)
    print = function() {
      print(self$get())
      return(invisible(self))
    },
    
    #' @description
    #' Validator method
    #' 
    #' @return (`invisible(self)`)
    validate = function() {
      return(invisible(self))
    }
  ),
  private = list()
)
