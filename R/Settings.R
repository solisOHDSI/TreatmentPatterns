# ==== Settings ===============================================================
#' Settings
#' 
#' Settings super object.
#' @export
Settings <- R6::R6Class(
  classname = "Settings",
  
  public = list(
    #' @description
    #' Initialize method
    #' 
    #' @return invisible(self)
    initialize = function() {
      return(invisible(self))
    },
    
    #' @description
    #' get method to overload
    #' 
    #' @return NULL
    get = function() {
      return(NULL)
    },
    
    #' @description
    #' print method to overload
    #' 
    #' @return invisible(self)
    print = function() {
      print(self$get())
      return(invisible(self))
    },
    
    #' @description
    #' validate method to overload
    #' 
    #' @return invisible(self)
    validate = function() {
      return(invisible(self))
    }
  ),
  
  private = list()
)
