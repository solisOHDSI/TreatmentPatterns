#' computePathways
#'
#' @template param_cohorts
#' @template param_cohortTableName
#' @template param_cdm
#' @template param_connectionDetails
#' @template param_cdmSchema
#' @template param_resultSchema
#' @template param_studyName
#' @template param_includeTreatments
#' @template param_periodPriorToIndex
#' @template param_minEraDuration
#' @template param_splitEventCohorts
#' @template param_splitTime
#' @template param_eraCollapseSize
#' @template param_combinationWindow
#' @template param_minPostCombinationDuration
#' @template param_filterTreatments
#' @template param_maxPathLength
#' @template param_minCellCount
#' @template param_minCellMethod
#' @template param_groupCombinations
#' @template param_addNoPaths
#'
#' @return (`Andromeda::andromeda()`)
#' \link[Andromeda]{andromeda} object containing non-sharable patient level
#' data outcomes.
#' 
#' @export
computePathways <- function(
    cohorts,
    cohortTableName,
    cdm = NULL,
    connectionDetails = NULL,
    cdmSchema = NULL,
    resultSchema = NULL,
    studyName = "default",
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5,
    minCellCount = 5,
    minCellMethod = "Remove",
    groupCombinations = 10,
    addNoPaths = TRUE) {
  cdmInterface <- CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = cdmSchema,
    resultSchema = resultSchema,
    cdm = cdm
  )
  
  pathwayConstructor <- PathwayConstructor$new(
    cohorts = cohorts,
    cohortTableName = cohortTableName,
    cdmInterface = cdmInterface
  )
  
  pathwayConstructor$editSettings(
    studyName = studyName,
    includeTreatments = includeTreatments,
    periodPriorToIndex = periodPriorToIndex,
    minEraDuration = minEraDuration,
    splitEventCohorts = splitEventCohorts,
    splitTime = splitTime,
    eraCollapseSize = eraCollapseSize,
    combinationWindow = combinationWindow,
    minPostCombinationDuration = minPostCombinationDuration,
    filterTreatments = filterTreatments,
    maxPathLength = maxPathLength,
    minCellCount = minCellCount,
    minCellMethod = minCellMethod,
    groupCombinations = groupCombinations,
    addNoPaths = addNoPaths
  )
  
  pathwayConstructor$construct()
  andromeda <- pathwayConstructor$getAndromeda()
  cdmInterface$addSex(andromeda)
  cdmInterface$addAge(andromeda)
  
  andromeda$metadata <- andromeda$metadata %>%
    dplyr::collect() %>%
    dplyr::mutate(execution_end_date = as.character(Sys.Date()))
  return(andromeda)
}
