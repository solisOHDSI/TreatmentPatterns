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
  if (!file.exists(outputPath)) {
    dir.create(outputPath)
  }
  
  treatmentHistory <- andromeda$treatmentHistory %>% dplyr::collect()
  
  # metadata
  metadataPath <- file.path(outputPath, "metadata.csv")
  message(sprintf("Writing metadata to %s", metadataPath))
  metadata <- andromeda$metadata %>% dplyr::collect()
  write.csv(metadata, file = metadataPath)
  
  # Treatment Pathways
  treatmentPathwaysPath <- file.path(outputPath, "treatmentPathways.csv")
  message(sprintf("Writing treatmentPathways to %s", treatmentPathwaysPath))
  treatmentPathways <- computeTreatmentPathways(treatmentHistory, ageWindow, minFreq)
  write.csv(treatmentPathways, file = treatmentPathwaysPath)
  
  # Summary statistics duration
  statsTherapyPath <- file.path(outputPath, "summaryStatsTherapyDuraion.csv")
  message(sprintf("Writing summaryStatsTherapyDuraion to %s", statsTherapyPath))
  statsTherapy <- computeStatsTherapy(treatmentHistory)
  write.csv(statsTherapy, file = statsTherapyPath)
  
  # Counts
  counts <- computeCounts(treatmentHistory, minFreq)
  
  countsYearPath <- file.path(outputPath, "countsYear.csv")
  message(sprintf("Writing countsYearPath to %s", countsYearPath))
  write.csv(counts$year, file = countsYearPath)
  
  countsAgePath <- file.path(outputPath, "countsAge.csv")
  message(sprintf("Writing countsAgePath to %s", countsAgePath))
  write.csv(counts$age, file = countsAgePath)
  
  countsSexPath <- file.path(outputPath, "countsSex.csv")
  message(sprintf("Writing countsSexPath to %s", countsSexPath))
  write.csv(counts$sex, file = countsSexPath)
  
  if (!is.null(archiveName)) {
    zipPath <- file.path(outputPath, archiveName)
    
    message(sprintf("Zipping files to %s", zipPath))
    
    utils::zip(
      zipfile = zipPath,
      files = c(
        treatmentPathwaysPath, countsYearPath, countsAgePath, countsSexPath,
        statsTherapyPath))
  }
  return(invisible(NULL))
}

#' computeStatsTherapy
#'
#' @param treatmentHistory (`data.frame()`)
#'
#' @return (`data.frame()`)
computeStatsTherapy <- function(treatmentHistory) {
  stats <- treatmentHistory %>%
    mutate(treatmentType = dplyr::case_when(
      nchar(.data$event_cohort_id) > 1 ~ "combination",
      .default = "monotherapy")
    ) %>%
    group_by(.data$treatmentType) %>%
    summarise(
      avgDuration = mean(.data$duration_era),
      medianDuration = median(.data$duration_era),
      sd = sd(.data$duration_era),
      min = min(.data$duration_era),
      max = max(.data$duration_era),
      count = n())
  
  return(stats)
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
