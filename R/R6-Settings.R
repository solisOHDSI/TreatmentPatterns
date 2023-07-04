#' @title Settings
#' 
#' @description
#' Settings super object.
Settings <- R6::R6Class(
  classname = "Settings",
  public = list(
    initialize = function() {
      return(invisible(self))
    },
    get = function() {
      return(NULL)
    },
    print = function() {
      print(self$get())
    },
    validate = function() {
      return(invisible(self))
    }
  ),
  private = list()
)
