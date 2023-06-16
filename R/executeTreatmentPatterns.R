#' executeTreatmentPatterns
#'
#' This is the main function which runs all parts of the treatment pathways
#' analysis. The following tasks are performed sequentially: 1) Construct
#' treatment pathways, 2) Generate output (sunburst plots, Sankey diagrams and
#' more).
#'
#' @param dataSettings
#' dataSettings object created by \link[TreatmentPatterns]{createDataSettings}.
#' @param pathwaySettings
#' pathwaySettings object created by \link[TreatmentPatterns]{createPathwaySettings}.
#' @param saveSettings
#' saveSettings object created by \link[TreatmentPatterns]{createSaveSettings}.
#' @param cohortSettings
#' cohortSettings object created by \link[TreatmentPatterns]{createCohortSettings}.
#'
#' @return NULL
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Select Viral Sinusitis Cohort
#'   targetCohort <- cohortsGenerated %>%
#'     filter(cohortName == "Viral Sinusitis") %>%
#'     select(cohortId, cohortName)
#'
#'   # Select everything BUT Viral Sinusitis cohorts
#'   eventCohorts <- cohortsGenerated %>%
#'     filter(cohortName != "Viral Sinusitis") %>%
#'     select(cohortId, cohortName)
#'
#'   saveSettings <- TreatmentPatterns::createSaveSettings(
#'     databaseName = "Eunomia",
#'     rootFolder = getwd(),
#'     outputFolder = file.path(getwd(), "output", "Eunomia"))
#'
#'   cohortSettings <- TreatmentPatterns::createCohortSettings(
#'     targetCohorts = targetCohort,
#'      eventCohorts = eventCohorts)
#'
#'   pathwaySettings <- createPathwaySettings(
#'     cohortSettings = cohortSettings,
#'     studyName = "Viral_Sinusitis")
#'
#'   executeTreatmentPatterns(
#'   dataSettings = dataSettings,
#'   pathwaySettings = pathwaySettings,
#'   saveSettings = saveSettings,
#'   cohortSettings = cohortSettings
#'   )
#' }
executeTreatmentPatterns <- function(
    dataSettings,
    pathwaySettings,
    saveSettings,
    cohortSettings) {

  # 1) Construct treatment pathways
  TreatmentPatterns::constructPathways(
    dataSettings = dataSettings,
    pathwaySettings = pathwaySettings,
    saveSettings = saveSettings,
    cohortSettings = cohortSettings
  )

  # 2) Generate output (sunburst plots, Sankey diagrams and more)
  TreatmentPatterns::generateOutput(saveSettings)
}
