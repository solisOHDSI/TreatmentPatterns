#' formatSunburstData
#' 
#' Function to format data from files containing event_cohort_name columns.
#'
#' @param filePath (\link[base]{character})
#' Path to file containing `event_cohort_name1`, `event_cohort_name2`,
#' `event_cohort_name3`, `event_cohort_name4`, `event_cohort_name5` columns.
#' @param indexYear (\link[base]{integer} | \link[base]{character})
#' Index year to filter on. "no year" may be specified, or the year as an integer.
#' @param studyName (\link[base]{character})
#' Name of the study, specified in the `pathwaySerttings` object.
#' 
#'
#' @return \link[base]{data.frame}
#' | column | data type              |
#' | path   | \link[base]{character} |
#' | freq   | \link[base]{numeric}   |
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'   pathData <- formatSunburstData(
#'     filePath = "output/treatmentPathways.csv",
#'     indexYear = NA,
#'     studyName = "Viral_Sinusitis"
#'   )
#'   
#'   createSunburstPlot(
#'     data = pathData,
#'     folder = "output/",
#'     fileName = "sunburst.html"
#'   )
#' }
formatSunburstData <- function(filePath, indexYear, studyName = NULL) {
  dat <- read.csv(filePath)
  
  if (is.na(indexYear)) {
    dat <- dat %>%
      dplyr::filter(is.na(index_year))
  } else {
    dat <- dat %>%
      dplyr::filter(index_year == indexYear)
  }
  
  if (!is.null(studyName)) {
    dat <- dat %>%
      dplyr::filter(.data$studyName == studyName)
  }

  pathData <- dplyr::bind_rows(lapply(seq_len(nrow(dat)), function(i) {
    data.frame(
      path = paste(
        dat[i, "event_cohort_name1"],
        dat[i, "event_cohort_name2"],
        dat[i, "event_cohort_name3"],
        dat[i, "event_cohort_name4"],
        dat[i, "event_cohort_name5"],
        sep = "-"),
      freq = dat[i, "freq"]
    )
  }))
  
  # Fix for shorter paths
  pathData$path <- pathData$path %>%
    stringr::str_remove_all(pattern = "--+") %>%
    stringr::str_remove_all(pattern = "-NA")

  pathData$path <- stringr::str_replace_all(pathData$path, pattern = "-NA", "")
  return(pathData)
}
