

#' @title
#' Create an ExecutionSettings object and set its attributes
#'
#' @param connectionDetails A DatabaseConnector connectionDetails object (optional if connection is specified)
#' @param connection A DatabaseConnector connection object (optional if connectionDetails is specified)
#' @param cdmDatabaseSchema The schema of the OMOP CDM database
#' @param workDatabaseSchema The schema to which results will be written
#' @param tempEmulationSchema Some database platforms like Oracle and Snowflake do not truly support temp tables. To emulate temp tables, provide a schema with write privileges where temp tables can be created.
#' @param cohortTable The name of the table where the cohort(s) are stored
#' @param cdmSourceName A human-readable name for the OMOP CDM source
#'
#' @return An ExecutionSettings object
#' @export
createExecutionSettings <- function(connectionDetails,
                                    connection = NULL,
                                    cdmDatabaseSchema,
                                    workDatabaseSchema,
                                    tempEmulationSchema,
                                    cohortTable,
                                    cdmSourceName) {
  executionSettings <- ExecutionSettings$new(connectionDetails = connectionDetails,
                                             connection = connection,
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             workDatabaseSchema = workDatabaseSchema,
                                             tempEmulationSchema = tempEmulationSchema,
                                             cohortTable = cohortTable,
                                             cdmSourceName = cdmSourceName)
  return(executionSettings)
}

#' @title Function to create a execution settings class for a script
#' @description
#' In this function we prepare the execution settings R6 class to use in the analysis script
#' @param configBlock set which configBlock to use in the sourceConfig.yml file. Defaults to the first
#' db block in the file.
#' @param settingsFolder the folder containing the config files used for the Ulysses repo.
#' @return an executionSettings R6 class
#' @export
prepareExecutionSettings <- function(configBlock = NULL, settingsFolder = here::here("settings")) {


  if (is.null(configBlock)) {
    blocks <- yaml::read_yaml(file = fs::path(settingsFolder, "sourceConfig.yml")) |>
      names()
    configBlock <- blocks[2]
  }

  sourceList <- config::get(config = configBlock, file = fs::path(settingsFolder, "sourceConfig.yml"))
  databaseRole <- sourceList$databaseRole

  connConfig <- config::get(
    value = "connectionConfigPath",
    file = fs::path(settingsFolder, "execConfig.yml")
  )

  # get dbms, user and password
  connectionConfigList <- yaml::read_yaml(file = connConfig)$default |>
    purrr::keep_at(c("dbms", "user", "password"))


  # get the snowflake connection string
  snowflakeConnectionString <- createSnowflakeConnectionString(connConfig) |>
    glue::glue()

  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = connectionConfigList$dbms,
    user = connectionConfigList$user,
    password = connectionConfigList$password,
    connectionString = snowflakeConnectionString
  )

  es <- createExecutionSettings(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = sourceList$cdmDatabaseSchema,
    workDatabaseSchema = sourceList$workDatabaseSchema,
    tempEmulationSchema = sourceList$tempEmulationSchema,
    cohortTable = sourceList$cohortTable,
    cdmSourceName = sourceList$databaseName
  )

  return(es)
}

#' @title Function to create the output folder
#' @description
#' In this function we prepare the output folder that will save the output files from the task
#' @param taskName the name of the task as part of the file path.
#' @param configBlock set which configBlock to use in the sourceConfig.yml file. Defaults to the first
#' db block in the file.
#' @param settingsFolder the folder containing the config files used for the Ulysses repo.
#' @return a character string specifying the output folder path
#' @export
setOutputFolder <- function(
    taskName,
    configBlock = NULL,
    settingsFolder = here::here("settings")) {

  release <- config::get(
    value = "version",
    config = "release",
    file = fs::path(settingsFolder, "execConfig.yml")
  )

  resultsFolder <- config::get(
    value = "resultsFolderPath",
    file = fs::path(settingsFolder, "execConfig.yml")
  )

  databaseName <- config::get(
    value = "databaseName",
    config = configBlock,
    file = fs::path(settingsFolder, "sourceConfig.yml")
  )

  outputFolder <- fs::path(resultsFolder, release, databaseName, taskName) |>
    fs::dir_create()

  return(outputFolder)

}
