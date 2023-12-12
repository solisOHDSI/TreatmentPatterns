SunburstPlot <- R6::R6Class(
  classname = "SunburstPlot",
  inherit = TreatmentPathwayPlot,
  
  # Public ----
  public = list(
    # @Override ----
    plot = function(colors = NULL, ...) {
      private$doGroupCombinations()
      
      colors <- private$setColors(colors)
      
      sunburstR::sunburst(
        data = private$.treatmentPathways,
        sortFunction = self$sortFunction,
        ...
      )
    },
    
    setSortFunction = function(sortFunction) {
      private$.sortFunction <- sortFunction
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    .sortFunction = "function (a, b) {return a.value - b.value;}",
    
    formatColors = function(colors) {
      private$getColorPalette()
    }
  ),

  # Active ----
  active = list(
    sortFunction = function() return(htmlwidgets::JS(private$.sortFunction))
  )
)

createSunburstPlot <- function(treatmentPathways, groupCombinations, ...) {
  sb <- SunburstPlot$new(treatmentPathways, groupCombinations)
  plot(sb, ...)
}
