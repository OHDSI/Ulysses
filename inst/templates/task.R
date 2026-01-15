# {taskName}

# A. Meta Info --------------

# Study: {studyName}
# Author: {author}
# Date: {lubridate::today()}
# Description: {description}

# B. Dependencies ---------------

library(Ulysses)
library(DatabaseConnector)
library(tidyverse)

# C. Connection Settings -------------

# set config block
configBlock <- "!||configBlock||!"

# set connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

# set executionSettings
executionSettings <- Ulysses::createExecutionSettings(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = config::get("cdmDatabaseSchema", config = configBlock),
  workDatabaseSchema = config::get("workDatabaseSchema", config = configBlock),
  tempEmulationSchema = config::get("tempEmulationSchema", config = configBlock),
  cohortTable = config::get("cohortTable", config = configBlock),
  cdmSourceName = config::get("cdmSourceName", config = configBlock)
)


# D. Task Settings ------------------

# set output folder
outputFolder <- here::here("exec/results") |>
  fs::path(snakecase::to_snake_case(executionSettings$cdmSourceName), "{taskName}") |>
  fs::dir_create()

##### Note: Add code that identifies task settings like cohorts or time windows

# E. Script ------------------

##### Note: Add code that runs task
