TreatmentPathwayPlot <- R6::R6Class(
  classname = "TreatmentPathwayPlot",
  
  # Public ----
  public = list(
    initialize = function(treatmentPathways, groupCombinations = FALSE) {
      private$.treatmentPathways <- treatmentPathways
      private$.groupCombinations <- groupCombinations
    },

    validate = function() {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertDataFrame(private$.treatmentPathways, ncols = 4, min.rows = 1, null.ok = FALSE, add = assertions)
      checkmate::assertLogical(private$.groupCombinations, len = 1, null.ok = FALSE, add = assertions)
      checkmate::reportAssertions(assertions)
    },
    
    plot = function(colors = NULL, ...) {}
  ),

  # Private ----
  private = list(
    ## Fields ----
    .treatmentPathways = NULL,
    .groupCombinations = NULL,
    .colorPalette = c(
      "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
      "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
    ),
    .plot = NULL,

    ## Methods ----
    finalize = function() {},
    
    getColorPalette = function() {
      n <- private$.treatmentPathways$path |> 
        stringr::str_split(pattern = "-") |>
        unlist() |>
        unique() |>
        length()
      
      return(private$.colorPalette[0:n])
    },
    
    formatColors = function(colors) {},
    
    setColors = function(colors) {
      if (is.null(colors)) {
        private$formatColors(colors)
      } else {
        colors
      }
    },
    
    doGroupCombinations = function() {
      if (private$.groupCombinations) {
        private$.treatmentPathways$path <- private$.treatmentPathways$path %>%
          stringr::str_replace_all(
            pattern = "\\w+\\+\\w+",
            replacement = "Combination"
          )
      }
    }
  ),

  # Active ----
  active = list(
    treatmentPathways = function() return(private$.treatmentPathways),
    groupCombinations = function() return(private$.groupCombinations)
  )
)
