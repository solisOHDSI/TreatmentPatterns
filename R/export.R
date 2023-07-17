#' export
#'
#' Export andromeda object to sharable csv-files.
#' 
#' @export
#'
#' @template param_andromeda
#' @template param_outputPath
#' @template param_ageWindow
#' @template param_minFreq
#' @template param_archiveName
#'
#' @return (`invisible(NULL)`)
export <- function(andromeda, outputPath = ".", ageWindow = 10, minFreq = 5, archiveName = NULL) {
  treatmentHistory <- andromeda$treatmentHistory %>% collect()
  treatmentPathways <- computeTreatmentPathways(treatmentHistory, ageWindow, minFreq)
  counts <- computeCounts(treatmentHistory, minFreq)
  
  
  treatmentPathwaysPath <- file.path(outputPath, "treatmentPathways.csv")
  countsYearPath <- file.path(outputPath, "countsYear.csv")
  countsAgePath <- file.path(outputPath, "countsAge.csv")
  countsSexPath <- file.path(outputPath, "countsSex.csv")
  
  message(sprintf("Writing treatmentPathways to %s", treatmentPathwaysPath))
  write.csv(treatmentPathways, file = treatmentPathwaysPath)
  
  message(sprintf("Writing treatmentPathways to %s", countsYearPath))
  write.csv(counts$year, file = countsYearPath)
  
  message(sprintf("Writing treatmentPathways to %s", countsAgePath))
  write.csv(counts$age, file = countsAgePath)
  
  message(sprintf("Writing treatmentPathways to %s", countsSexPath))
  write.csv(counts$sex, file = countsSexPath)
  
  if (!is.null(archiveName)) {
    zipPath <- file.path(outputPath, archiveName)
    
    message(sprintf("Zipping files to %s", zipPath))
    
    utils::zip(zipPath, c(treatmentPathwaysPath, countsYearPath, countsAgePath, countsSexPath))
  }
  return(invisible(NULL))
}

#' computeCounts
#'
#' @param treatmentHistory (`data.frame()`)
#'
#' @return (`list()`)
computeCounts <- function(treatmentHistory, minFreq) {
  # n per Year
  countYear <- treatmentHistory %>%
    group_by(.data$index_year) %>%
    count() %>%
    ungroup() %>%
    mutate(n = case_when(
      .data$n < minFreq ~ glue::glue("<{minFreq}"),
      .default = as.character(.data$n)))
  
  # n per sex
  countSex <- treatmentHistory %>% group_by(sex) %>%
    count() %>%
    ungroup() %>%
    mutate(n = case_when(
      .data$n < minFreq ~ glue::glue("<{minFreq}"),
      .default = as.character(.data$n)))
  
  # n per age
  countAge <- treatmentHistory %>% group_by(age) %>%
    count() %>%
    ungroup() %>%
    mutate(n = case_when(
      .data$n < minFreq ~ glue::glue("<{minFreq}"),
      .default = as.character(.data$n)))
  
  return(list(year = countYear, age = countAge, sex = countSex))
}


#' computeTreatmentPathways
#'
#' @param treatmentHistory (`data.frame()`)
#' @param ageWindow (`integer(1)`)
#' @param minFreq (`integer(1)`)
#'
#' @return (`data.frame()`)
computeTreatmentPathways <- function(treatmentHistory, ageWindow, minFreq) {
  years <- c("all", treatmentHistory$index_year %>% unique())
  
  treatmentHistory <- treatmentHistory %>%
    rowwise() %>%
    dplyr::mutate(
      age_bin = paste(unlist(stringr::str_extract_all(as.character(cut(.data$age, seq(0, 100, ageWindow))), "\\d+")), collapse = "-"))
  
  ages <- treatmentHistory$age_bin %>% unique()
  
  # Per year
  treatmentPathways <- stratisfy(treatmentHistory, years, ages, ageWindow, minFreq)
  
  treatmentPathways <- treatmentPathways %>%
    mutate(index_year = as.character(.data$index_year))
  
  treatmentPathways[is.na(treatmentPathways)] <- "all"
  return(treatmentPathways)
}

#' stratisfy
#'
#' @param treatmentHistory (`data.frame()`)
#' @param years (`vector("character")`)
#' @param ages (`vector("character")`)
#' @param ageWindow (`integer(1)`)
#' @param minFreq (`integer(1)`)
#'
#' @return (`data.frame()`)
stratisfy <- function(treatmentHistory, years, ages, ageWindow, minFreq) {
  dplyr::bind_rows(lapply(years, function(y) {
    all <- prepData(treatmentHistory = treatmentHistory, year = y) %>%
      dplyr::mutate(
        freq = case_when(
          .data$freq < minFreq ~ glue::glue("<{minFreq}"),
          .default = as.character(.data$freq)
        )
      )
    
    sex <- dplyr::bind_rows(
      treatmentHistory %>% 
        filter(.data$sex == "MALE") %>%
        prepData(y) %>%
        dplyr::mutate(freq = case_when(
          freq < minFreq ~ glue::glue("<{minFreq}"),
          .default = as.character(freq)
        )
        ) %>%
        mutate(sex = "male"),
      treatmentHistory %>%
        filter(.data$sex == "FEMALE") %>%
        prepData(y) %>%
        dplyr::mutate(freq = case_when(
          freq < minFreq ~ glue::glue("<{minFreq}"),
          .default = as.character(freq)
        )
        ) %>%
        mutate(sex = "female")
    )
    
    age <- dplyr::bind_rows(lapply(ages, function(ageRange) {
      treatmentHistory %>%
        filter(.data$age_bin == ageRange) %>%
        prepData(y) %>%
        mutate(
          age = ageRange,
          freq = case_when(
            freq < minFreq ~ glue::glue("<{minFreq}"),
            .default = as.character(freq)
          )
        )
    }))
    dplyr::bind_rows(all, sex, age)
  }))
}
