# {{{ FileName }}}.R

# A. File Info -----------------------

# Study: {{{ Title }}}
# Author: {{{ Author }}}
# Date: {{{ Date }}}
# Description: The purpose of this script is to execute strategus tasks.

# B. Dependencies ----------------------

## include R libraries
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)

source(here::here("analysis/src/_executeStrategus.R"))


# C. Connection ----------------------

# set connection Block
# <<<
configBlock <- "{{{ Block }}}"
# >>>

# provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

# connect to database
con <- DatabaseConnector::connect(connectionDetails)


# D. Variables -----------------------

# build execution settings
executionSettings <- Strategus::createCdmExecutionSettings(
  connectionDetailsReference = connectionDetailsReference,
  workDatabaseSchema = workDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = cohortTableName),
  workFolder = file.path(outputLocation, connectionDetailsReference, "strategusWork"),
  resultsFolder = file.path(outputLocation, connectionDetailsReference, "strategusOutput"),
  minCellCount = minCellCount,
  resultsDatabaseSchema = NULL,
  resultsConnectionDetailsReference  = NULL
)

#make output folder
outputFolder <- here::here("exec/results") %>%
  fs::path(executionSettings$databaseName, "{{{ FileName }}}") %>%
  fs::dir_create()

# E. Script --------------------

# Execute Analysis
executeAnalysis(
  analysisFile = file.path(resourceDirectory, strategusSpecFileName),
  executionSettings = executionSettings,
  analysisName = "poc",
  outputLocation = outputLocation,
  resultsLocation = resultsLocation,
  keyringName = keyringName
)

# F. Session Info ------------------------

DatabaseConnector::disconnect(con)
rm(list = ls())

