#' extractFile
#'
#' Extract table with specific name from database server.
#'
#' @export
#'
#' @param connection \link[DatabaseConnector]{connect}\cr
#' Connection object from DatabaseConnector.
#' @param tableName \link[base]{character}\cr
#' Name of table.
#' @param resultsSchema \link[base]{character}\cr
#' Schema of results.
#' \link[DatabaseConnector]{createConnectionDetails}.
#'
#' @return \link[base]{data.frame}
#' The extracted table as a data frame.
#'
#' @examples
#' \dontrun{
#'   con <- DatabaseConnector::connect(Eunomia::getEunomiaConnectionDetails())
#'   extractFile(con, "person", "main", "sqlite")
#'}
extractFile <- function(connection, tableName, resultsSchema) {
  # Assertions
  checkmate::checkClass(connection, "DatabaseConnectorDbiConnection")
  checkmate::checkCharacter(tableName, len = 1)
  checkmate::checkCharacter(resultsSchema, len = 1)

  parameterizedSql <- "SELECT * FROM @resultsSchema.@tableName"
  renderedSql <- SqlRender::render(
    parameterizedSql,
    resultsSchema = resultsSchema,
    tableName = tableName)

  translatedSql <- SqlRender::translate(
    renderedSql,
    targetDialect = connection@dbms)
  DatabaseConnector::querySql(connection, translatedSql)
}


#' extractCohortTable
#'
#' Extracts the cohort table from the database, containing specified cohorts.
#'
#' @export
#'
#' @param connection \link[DatabaseConnector]{connect}\cr
#' Connection object from DatabaseConnector.
#' @param resultsSchema \link[base]{character}\cr
#' Schema of results.
#' @param cohortTableName \link[base]{character}\cr
#' Name of table.
#' @param cohortIds \link[base]{c} of \link[base]{integer}\cr
#' Vector of cohort ID's.
#'
#' @return \link[base]{data.frame}
#' Cohort table from database including the specified cohorts
#' | column               | data type              |
#' | -------------------- | ---------------------- |
#' | COHORT_DEFINITION_ID | \link[base]{integer}   |
#' | SUBJECT_ID           | \link[base]{integer}   |
#' | COHORT_START_DATE    | \link[base]{character} |
#' | COHORT_END_DATE      | \link[base]{character} |
#' 
#' @examples
#' \dontrun{
#'   extractCohortTable(
#'     connection = connection,
#'     cohortTableName = "cohortTable",
#'     resultsSchema = "main",
#'     cohortIds = c(1,2,3)
#'   )
#' }
extractCohortTable <- function(
    connectionDetails,
    resultsSchema,
    cohortTableName,
    cohortIds = NULL) {
  checkmate::checkClass(connectionDetails, "ConnectionDetails")
  checkmate::checkCharacter(cohortTableName, len = 1)
  checkmate::checkCharacter(resultsSchema, len = 1)
  checkmate::checkNumeric(cohortIds, null.ok = TRUE)
  
  if (all(is.null(cohortIds)) | all(is.na(cohortIds))) {
    renderedSql <- SqlRender::render(
      "SELECT * FROM @resultsSchema.@cohortTableName",
      resultsSchema = resultsSchema,
      cohortTableName = cohortTableName
    )
  } else {
    renderedSql <- SqlRender::render(
      "SELECT * FROM @resultsSchema.@cohortTableName WHERE cohort_definition_id IN (@cohortIds)",
      resultsSchema = resultsSchema,
      cohortTableName = cohortTableName,
      cohortIds = cohortIds
    )
  }
  
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  
  translatedSql <- SqlRender::translate(
    renderedSql,
    targetDialect = connectionDetails$dbms
  )
  DatabaseConnector::querySql(connection, translatedSql)
}

#' writeCohortTable
#'
#' Writes the cohortTable from the database to a specified path in saveSettings.
#'
#' @param saveSettings saveSettings object
#' @param tableName Name of the cohort table in the database.
#' @param dataSettings dataSettings object
#' @param cohortSettings cohortSettings object
#'
#' @return NULL
#' @export
writeCohortTable <- function(
    saveSettings,
    cohortSettings,
    dataSettings,
    tableName) {
  # Write cohortTable
  fs::dir_create(file.path(saveSettings$outputFolder))

  write.csv(
    x = cohortSettings$cohortsToCreate,
    file = paste0(saveSettings$outputFolder, "/cohortsToCreate.csv"))

  con <- DatabaseConnector::connect(dataSettings$connectionDetails)
  # Disconnect from database on exit
  on.exit(DatabaseConnector::disconnect(con))

  # Extract files from DB, write to outputFolder
  tbl <- extractCohortTable(
    connection = con,
    cohortTableName = tableName,
    resultsSchema = dataSettings$resultSchema,
    cohortIds = cohortSettings$cohortsToCreate$cohortId)

  write.csv(
    tbl,
    file.path(
      saveSettings$outputFolder,
      paste0(tableName, ".csv")),
    row.names = FALSE)
}
