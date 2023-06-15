# A. Meta Info -----------------------

# Study: {{{ Study }}}
# Name: {{{ Name }}}
# Author: {{{ Author }}}
# Date: {{{ Date }}}
# Description: The purpose of {{{ FileName }}}.R is to.....

# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)

# C. Connection ----------------------

# set connection Block
configBlock <- "{{{ Block }}}"

# provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("user", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

#connect to database
con <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(con)) #close on exit


# D. Study Variables -----------------------

### Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at( c("dbms", "user", "password", "connectionString"))

outputFolder <- here::here("results/{{{ FileName }}}") %>%
  fs::path(executionSettings$databaseName) %>%
  fs::dir_create()

### Add study variables or load from settings

# E. Script --------------------

# Add script here

# F. Session Info ------------------------

sessioninfo::session_info()
rm(list=ls())
