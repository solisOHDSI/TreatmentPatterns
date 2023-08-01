library(dplyr)
# library(CDMConnector)

source(system.file(package = "TreatmentPatterns", "R-scripts", "runCG.R"))

# Select Viral Sinusitis Cohort
targetCohorts <- cohortsGenerated %>%
  filter(cohortName == "ViralSinusitis") %>%
  select(cohortId, cohortName)

# Select everything BUT Viral Sinusitis cohorts
eventCohorts <- cohortsGenerated %>%
  filter(cohortName != "ViralSinusitis" & cohortName != "Death") %>%
  select(cohortId, cohortName)

exitCohorts <- cohortsGenerated %>%
  filter(cohortName == "Death") %>%
  select(cohortId, cohortName)

cohorts <- dplyr::bind_rows(
  targetCohorts %>% mutate(type = "target"),
  eventCohorts %>% mutate(type = "event"),
  exitCohorts %>% mutate(type = "exit")
)

andromeda <- TreatmentPatterns::computePathways(
  cohorts = cohorts,
  cohortTableName = "CohortTable",
  connectionDetails = connectionDetails,
  cdmSchema = "main",
  resultSchema = "main"
)

# print(names(andromeda))

setupTempDir <- file.path(tempdir(), "setup")

TreatmentPatterns::export(andromeda, outputPath = setupTempDir)
treatmentPathways <- read.csv(file.path(setupTempDir, "treatmentPathways.csv"))

pathways <- treatmentPathways %>%
  dplyr::filter(.data$sex == "all" & .data$age == "all" & .data$index_year == "all")

TreatmentPatterns::createSunburstPlot(
  treatmentPathways = pathways,
  outputFile = file.path(setupTempDir, "sunburst.html"))

TreatmentPatterns::createSankeyDiagram(
  treatmentPathways = pathways,
  outputFile = file.path(setupTempDir, "sankey.html"))
