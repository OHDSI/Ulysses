# {{{ FileName }}}.R

# A. File Info -----------------------

# Study: {{{ Study }}}
# Author: {{{ Author }}}
# Date: {{{ Date }}}
# Description: The purpose of this Capr script is to develop xxx cohorts....

# B. Dependencies ----------------------

## include R libraries
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
library(Capr)

# C. Connection ----------------------

# set connection Block
configBlock <- "[add block]"

# provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms",config = configBlock),
  user = config::get("user",config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

# connect to database
con <- DatabaseConnector::connect(connectionDetails)

# D. Variables -----------------------

### Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

# specify cohort folder
cohortFolder <- fs::path(here::here(), "cohorts/json")

# E. Concept Sets --------------------

# Add concept sets here for example

# hypertension <- cs(
#   descendants(
#     316866 # hypertension
#   )
# ) %>%
# getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema)


# F. Cohort Definition ----------------

# Add cohort definition  here for example

# hypertensionCohort <- cohort(
#   entry = entry(
#     condition(hypertension),
#     observationWindow = continuousObservation(),
#     primaryCriteriaLimit = "First"
#   ),
#   exit = exit(
#     endStrategy = observationExit()
#   )
# )
#
# writeCohort(hypertensionCohort, path = fs::path(cohortFolder, "hypertension", ext = "json"))


# F. Clean up ------------------------

DatabaseConnector::disconnect(con)
rm(list = ls())

