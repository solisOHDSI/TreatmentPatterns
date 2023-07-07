runPatientLevel <- function(
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
  a <- pathwayConstructor$getAndromeda()
  cdmInterface$addSex(a)
  cdmInterface$addAge(a)
  return(a)
}
