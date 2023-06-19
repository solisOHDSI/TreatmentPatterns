library(dplyr)

dataSettings <- TreatmentPatterns::createDataSettings(
  connectionDetails = Eunomia::getEunomiaConnectionDetails(),
  cdmDatabaseSchema = "main",
  resultSchema = "main",
  cohortTable = "cohort_table")

cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()

cohortJsonFiles <- list.files(
  system.file(
    package = "TreatmentPatterns",
    "examples", "CDM", "cohorts", "ViralSinusitis", "JSON"),
  full.names = TRUE)

for (i in seq_len(length(cohortJsonFiles))) {
  cohortJsonFileName <- cohortJsonFiles[i]
  cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
  cohortJson <- readChar(cohortJsonFileName, file.info(
    cohortJsonFileName)$size)
  
  cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
  
  cohortSql <- CirceR::buildCohortQuery(
    cohortExpression,
    options = CirceR::createGenerateOptions(generateStats = FALSE))
  cohortsToCreate <- rbind(
    cohortsToCreate,
    data.frame(
      cohortId = i,
      cohortName = cohortName,
      sql = cohortSql,
      stringsAsFactors = FALSE))
}

cohortTableNames <- CohortGenerator::getCohortTableNames(
  cohortTable = dataSettings$cohortTable)

CohortGenerator::createCohortTables(
  connectionDetails = dataSettings$connectionDetails,
  cohortDatabaseSchema = dataSettings$resultSchema,
  cohortTableNames = cohortTableNames)

# Generate the cohorts
cohortsGenerated <- CohortGenerator::generateCohortSet(
  connectionDetails = dataSettings$connectionDetails,
  cdmDatabaseSchema = dataSettings$cdmDatabaseSchema,
  cohortDatabaseSchema = dataSettings$resultSchema,
  cohortTableNames = cohortTableNames,
  cohortDefinitionSet = cohortsToCreate)

# Select Viral Sinusitis Cohort
targetCohort <- cohortsGenerated %>%
  filter(cohortName == "ViralSinusitis") %>%
  select(cohortId, cohortName)

# Select everything BUT Viral Sinusitis cohorts
eventCohorts <- cohortsGenerated %>%
  filter(cohortName != "ViralSinusitis") %>%
  select(cohortId, cohortName)

saveSettings <- TreatmentPatterns::createSaveSettings(
  databaseName = "Eunomia",
  rootFolder = getwd(),
  outputFolder = file.path(getwd(), "output"))

if (file.exists(file.path(saveSettings$outputFolder, "settings"))) {
  fs::dir_create(file.exists(file.path(saveSettings$outputFolder, "settings")))
} else {
  fs::dir_create(file.path(saveSettings$outputFolder, "settings"))
}

cohortSettings <- TreatmentPatterns::createCohortSettings(
  targetCohorts = targetCohort,
  eventCohorts = eventCohorts)

pathwaySettings <- TreatmentPatterns::createPathwaySettings(
  cohortSettings = cohortSettings,
  studyName = "Viral_Sinusitis",
  minEraDuration = 1,
  maxPathLength = 2)

pathwaySettings <- TreatmentPatterns::addPathwayAnalysis(
  pathwaySettings = pathwaySettings,
  targetCohortIds = targetCohort$cohortId,
  eventCohortIds = eventCohorts$cohortId[-1],
  studyName = "someOtherCoolParameters",
  minEraDuration = 14,
  eraCollapseSize = 21,
  combinationWindow = 7,
  minCellCount = 5,
  groupCombinations = 5
)

# Write files
TreatmentPatterns::writeCohortTable(
  saveSettings = saveSettings,
  cohortSettings = cohortSettings,
  dataSettings = dataSettings,
  tableName = dataSettings$cohortTable)

connection <- DatabaseConnector::connect(dataSettings$connectionDetails)

fullCohorts <- data.table::as.data.table(TreatmentPatterns::extractCohortTable(
  connection = connection,
  resultsSchema = dataSettings$resultSchema,
  cohortTableName = dataSettings$cohortTable,
  cohortIds = cohortSettings$cohortsToCreate$cohortId
))

DatabaseConnector::disconnect(connection)

colnames(fullCohorts) <- c(
  "cohort_id", "person_id", "start_date", "end_date")

settings <- pathwaySettings$all_settings

targetCohortId <- settings[
  settings$param == "targetCohortId", "analysis1"]

selectPeople <- fullCohorts$person_id[
  fullCohorts$cohort_id == targetCohortId]

currentCohorts <- fullCohorts[
  fullCohorts$person_id %in% selectPeople, ]

eventCohortIds <- settings[
  settings$param == "eventCohortIds", "analysis1"]

eventCohortIds <- unlist(strsplit(eventCohortIds, split = c(";|,")))

periodPriorToIndex <- as.integer(settings[
  settings$param == "periodPriorToIndex", "analysis1"])

includeTreatments <- settings[
  settings$param == "includeTreatments", "analysis1"]

treatmentHistory <- TreatmentPatterns:::doCreateTreatmentHistory(
  currentCohorts = currentCohorts,
  targetCohortId = targetCohortId,
  eventCohortIds = eventCohortIds,
  exitCohortIds = NULL,
  periodPriorToIndex = periodPriorToIndex,
  includeTreatments = includeTreatments)

minEraDuration <- as.integer(settings[
  settings$param == "minEraDuration", "analysis1"])

doEraDurationTH <- TreatmentPatterns:::doEraDuration(
  treatmentHistory = treatmentHistory,
  minEraDuration = minEraDuration)

splitEventCohorts <- settings[
  settings$param == "splitEventCohorts", "analysis1"]

splitTime <- settings[
  settings$param == "splitTime", "analysis1"]

doSplitEventCohortsTH <- TreatmentPatterns:::doSplitEventCohorts(
  treatmentHistory = doEraDurationTH,
  splitEventCohorts = splitEventCohorts,
  splitTime = splitTime,
  outputFolder = saveSettings$outputFolder)

eraCollapseSize <- as.numeric(settings[
  settings$param == "eraCollapseSize", "analysis1"])

doEraCollapseTH <- TreatmentPatterns:::doEraCollapse(
  doSplitEventCohortsTH,
  eraCollapseSize)

combinationWindow <- settings[
  settings$param == "combinationWindow", "analysis1"]

minPostCombinationDuration <- as.numeric(settings[
  settings$param == "minPostCombinationDuration", "analysis1"])

doCombinationWindowTH <- TreatmentPatterns:::doCombinationWindow(
  doEraCollapseTH,
  combinationWindow,
  minPostCombinationDuration)

filterTreatments <- settings[
  settings$param == "filterTreatments", "analysis1"]

doFilterTreatmentsTH <- TreatmentPatterns:::doFilterTreatments(
  doCombinationWindowTH,
  filterTreatments)

maxPathLength <- as.integer(settings[
  settings$param == "maxPathLength", "analysis1"])

doFilterTreatmentsTHOrdered <- doFilterTreatmentsTH[
  order(person_id, event_start_date, event_end_date), ]

doFilterTreatmentsTHPP <- doFilterTreatmentsTHOrdered[,
                                                      event_seq := seq_len(.N), by = .(person_id)]

doMaxPathLengthTH <- TreatmentPatterns:::doMaxPathLength(
  doFilterTreatmentsTHPP,
  maxPathLength)

addLabelsTH <- TreatmentPatterns:::addLabels(
  doMaxPathLengthTH,
  saveSettings$outputFolder)

TreatmentPatterns::constructPathways(
  dataSettings = dataSettings,
  pathwaySettings = pathwaySettings,
  saveSettings = saveSettings,
  cohortSettings = cohortSettings
)

# Generate output for folder structure etc.
TreatmentPatterns::generateOutput(saveSettings = saveSettings)

treatmentPathways <- TreatmentPatterns:::getPathways(
  outputFolder = saveSettings$outputFolder,
  tempFolder = saveSettings$tempFolder,
  databaseName = saveSettings$databaseName,
  studyName = "Viral_Sinusitis",
  minCellCount = 5
)

eventCohortIds <- pathwaySettings$all_settings[3, 2]
eventCohortIds <-
  unlist(strsplit(eventCohortIds, split = c(";|,")))

groupCombinations <- pathwaySettings$all_settings[17, 2]

outputFolders <- file.path(saveSettings$outputFolder, "Viral_sinusitis")
