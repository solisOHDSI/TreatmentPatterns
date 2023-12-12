SankeyDiagram <- R6::R6Class(
  classname = "SankeyDiagram",
  inherit = TreatmentPathwayPlot,

  # Public ----
  public = list(
    validate = function() {
      super$validate()
      
      if (length(grep("-", x = private$.treatmentPathways$path)) == 0) {
        stop("Cannot compute Sankey Diagram as there is only one level in the data.")
      }
    },
    
    plot = function(colors = NULL, ...) {
      private$doGroupCombinations()

      colors <- private$setColors(colors)

      data <- private$splitPathItems()

      linkedData <- private$createLinkedData(data)

      networkD3::sankeyNetwork(
        Links = linkedData$links,
        Nodes = linkedData$nodes,
        Source = "source",
        Target = "target",
        Value = "value",
        NodeID = "names",
        units = "%",
        colourScale = colors,
        ...
      )
    }
  ),

  # Private ----
  private = list(
    formatColors = function(colors) {
      sprintf(
        'd3.scaleOrdinal([%s])',
        paste0('"', private$getColorPalette(),'"', collapse = ", ")
      )
    },
    
    splitPathItems = function() {
      data <- private$.treatmentPathways %>%
        dplyr::rowwise() %>%
        dplyr::mutate(path = stringr::str_split(.data$path, pattern = "-")) %>%
        dplyr::mutate(freq = as.integer(.data$freq))

      data <- data %>%
        tidyr::unnest_wider(path, names_sep = "")

      data <- data %>%
        dplyr::group_by_at(grep("path", names(data))) %>%
        dplyr::summarise(freq = sum(.data$freq), .groups = "drop")

      data[is.na(data)] <- "Stopped"
      return(data)
    },
    
    nameToId = function(item, names) {
      item <- item %>%
        stringr::str_replace(pattern = "\\(", replacement = "\\\\(") %>%
        stringr::str_replace(pattern = "\\)", replacement = "\\\\)") %>%
        stringr::str_replace(pattern = "\\+", replacement = "\\\\+") %>%
        stringr::str_replace(pattern = "\\&", replacement = "\\\\&") %>%
        stringr::str_replace(pattern = "\\.", replacement = "\\\\.")
      return(grep(sprintf("^%s$",item), names) - 1)
    },
    
    createLinks = function(data) {
      result1 <- data %>%
        mutate(
          source = paste0("1.", .data$path1),
          target = paste0("2.", .data$path2)
        ) %>%
        select("source", "target", "freq")
      
      
      if (suppressWarnings(!is.null(data$path3))) {
        result2 <- data %>%
          mutate(
            source = paste0("2.", .data$path2),
            target = paste0("3.", .data$path3)
          ) %>%
          select("source", "target", "freq")
        
        links <- dplyr::bind_rows(
          result1, result2
        )
      } else {
        links <- result1
      }
      
      links <- links %>%
        dplyr::mutate(value = round(freq / sum(freq) * 100, 2)) %>%
        dplyr::select(-"freq")
    },
    
    createLinkedData = function(data) {
      links <- private$createLinks(data)
      
      nodes <- data.frame(
        names = c(links$source, links$target) %>% unique()
      )
      
      links$source <- lapply(links$source, nameToId, names = nodes$names) %>%
        unlist()
      
      links$target <- lapply(links$target, nameToId, names = nodes$names) %>%
        unlist()
      
      links <- links %>%
        dplyr::group_by(.data$source, .data$target) %>%
        dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
        as.data.frame()
      
      return(list(links = links, nodes = nodes))
    }
  ),

  # Active ----
  active = list()
)

createSankeyDiagram <- function(treatmentPathways, groupCombinations, ...) {
  sankeyDiagram <- SankeyDiagram$new(treatmentPathways, groupCombinations)
  plot(sankeyDiagram, ...)
}
