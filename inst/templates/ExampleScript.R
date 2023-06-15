# A. Meta Info -----------------------

# Example: {{{ Example }}}
# Author: [ Insert Name Here ]
# Date: {{{ Date }}}
# Description: The purpose of {{{ FileName }}}.R is to provide an example for...

# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(CohortGenerator)

# B. Connection ----------------------

#create connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "<dbms>",
  user = "<user>",
  password = "<password>",
  connectionString = "<connectionString>"
)
#set database schemas
vocabularyDatabaseSchema <- "<vocabularyDatabaseSchema>"
cdmDatabaseSchema <- "<cdmDatabaseSchema>"
workDatabasSchema <- "<workDatabaseSchema>"
cohortTable <- "<cohortTable>"

# Connect to database
DatabaseConnector::connect(connectionDetails = connectionDetails)
on.exit(DatabaseConnector::disconnect(connection))

# C. Example Variables ---------------------

# add example variables here

# D. Example Script --------------------

# Add code here

# E. Session Info ------------------------

sessioninfo::session_info()
rm(list=ls())
