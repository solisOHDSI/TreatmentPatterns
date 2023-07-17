#' groupInfrequentCombinations
#'
#' `DEPRECATED`\cr
#' Help function to group combinations
#'
#' @param data
#'     Data
#' @param groupCombinations
#'     Group combinations
#'
#' @returns data.table
groupInfrequentCombinations <- function(data, groupCombinations) {
  data <- as.data.frame(data)
  
  # Find all non-fixed combinations occurring
  findCombinations <- apply(
    X = data,
    MARGIN = 2,
    FUN = function(x) {
      grepl("+", x, fixed = TRUE)
    }
  )
  
  # Group all non-fixed combinations in one group if TRUE
  if (groupCombinations == TRUE) {
    data[findCombinations] <- "Other"
  } else {
    # Otherwise: group infrequent treatments below groupCombinations as "other"
    combinations <- as.matrix(data)[findCombinations == TRUE]
    
    freqCombinations <- matrix(
      rep(data$freq, times = ncol(data)),
      ncol = ncol(data))[findCombinations == TRUE]
    
    summaryCombinations <- data.table::data.table(
      combination = combinations,
      freq = freqCombinations
    )
    
    if (nrow(summaryCombinations) > 0) {
      summaryCombinations <- summaryCombinations[
        , .(freq = sum(freq)),
        by = combination
      ][order(-freq)]
      
      summarizeCombinations <- summaryCombinations$combination[
        summaryCombinations$freq <= as.numeric(groupCombinations)]
      
      selectedCombinations <- apply(
        X = data,
        MARGIN = 2,
        FUN = function(x) {
          x %in% summarizeCombinations
        }
      )
      data[selectedCombinations] <- "Other"
    }
  }
  return(data.table::as.data.table(data))
}

#' createSankeyDiagram
#'
#' Writes the Sankey diagram to a HTML-file, to a specified file path. 
#'
#' @param treatmentPathways (`data.frame()`)\cr
#' Data frame containing treatmentPathways columsn: path, freq.
#' @param outputFile (`character(1)`)\cr
#' Path where the Sankey diagram should be written to.
#' @param groupCombinations (`logical(1)`: `TRUE`)\cr
#' \describe{
#'   \item{`TRUE`}{Group all combination treatments in category `"Combination"`.}
#'   \item{`FALSE`}{Do not group combination treatments.}
#' }
#' @param minFreq (`integer(1)`: `5`)\cr
#' Censor paths with a frequency smaller than specified.
#' @param year (`integer(1)`)\cr
#' Year
#' 
#' @export
#'
#' @returns invisible(NULL)
createSankeyDiagram <- function(
    treatmentPathways,
    outputFile,
    groupCombinations = FALSE,
    minFreq = 5,
    year = "all"){
  data <- treatmentPathways %>%
    rowwise() %>%
    mutate(path = stringr::str_split(.data$path, pattern = "-")) %>%
    mutate(
      freq = case_when(
        startsWith(.data$freq, prefix = "<") ~ stringr::str_split_i(.data$freq, pattern = "<", i = 2),
        .default = .data$freq
      )
    ) %>%
    mutate(freq = as.integer(.data$freq))
  
  data <- data %>%
    tidyr::unnest_wider(path, names_sep = "")
  
  data <- data %>%
    dplyr::group_by_at(grep("path", names(data))) %>%
    dplyr::summarise(freq = sum(.data$freq), .groups = "drop")
  
  data[is.na(data)] <- "Stopped"
  
  result1 <- data %>%
    mutate(
      source = paste("1.", .data$path1),
      target = paste("2.", .data$path2)) %>%
    select("source", "target", "freq")
  
  
  if (suppressWarnings(!is.null(data$path3))) {
    result2 <- data %>%
      mutate(
        source = paste("2.", .data$path2),
        target = paste("3.", .data$path3)) %>%
      select("source", "target", "freq")
    
    links <- dplyr::bind_rows(
      result1, result2
    )
  } else {
    links <- result1
  }
  
  links <- links %>%
    filter(.data$freq >= minFreq)
  
  if (groupCombinations) {
    links$source <- stringr::str_replace_all(
      string = links$source, "\\w+\\+\\w+", replacement = "Combination")
    links$target <- stringr::str_replace_all(
      string = links$target, "\\w+\\+\\w+", replacement = "Combination")
  }
  
  # Draw sankey network
  plot <- googleVis::gvisSankey(
    links,
    from = "source",
    to = "target",
    weight = "freq",
    chartid = 1,
    options = list(sankey = "{node: { colors: ['#B5482A'], width: 5}}")
  )
  
  message(sprintf("Witing Sankey diagram to %s", file.path(outputFile)))
  writeLines(
    text = plot$html$chart,
    con = file.path(outputFile))
  return(invisible(NULL))
}

