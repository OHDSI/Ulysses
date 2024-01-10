# Import cohorts from WebApi ----------------

# This file setups a script that will import cohort definitions from webapi. For this
# file to work you must provide a valid url that connects to your instance of ATLAS.
# This url

# A) Dependencies --------------------------

library(tidyverse, quietly = TRUE)
library(ROhdsiWebApi)
library(Ulysses)

# B) Keyring Setup ----------------

## Setup keyring for webapi

keyringName <- "{{{ Study }}}" # the name of the keyring

keyringPassword <- "{{{ Secret }}}" # password for keyring


# setup a study keyring if it hasnt already been done
setupStudyKeyring(keyringName, keyringPassword)

# add credentials for webapi
creds <- c("baseurl", "user", "password")
db <- 'webapi'
setMultipleCredentials(
  creds = creds,
  db = db,
  keyringName = keyringName,
  keyringPassword = keyringPassword
)

credNames <- paste(db, creds, sep = "_")

baseUrl <- keyring::key_get(service = credNames[1], keyring = keyringName)


# C)  Load necessary functions ------------------

# Function to grab cohort json
getWebApiCohortJson <- function(cohortId, baseUrl) {
  cohortDefinition <- ROhdsiWebApi::getCohortDefinition(cohortId = cohortId,
                                                        baseUrl = baseUrl)$expression
  RJSONIO::toJSON(x = cohortDefinition, digits = 23, pretty = TRUE)
}

# function to grab cohort name
getWebApiCohortInfo <- function(cohortId, baseUrl) {
  cohortDefinition <- ROhdsiWebApi::getCohortDefinition(cohortId = cohortId,
                                                        baseUrl = baseUrl)
  paste(cohortDefinition$name, paste0("id", cohortDefinition$id), sep = "_")
}

# D) Authorize WebApi access ------------

# Authorize access to WebApi
authorizeWebApi(
  baseUrl = baseUrl,
  authMethod = 'db',
  webApiUsername = keyring::key_get(service = credNames[2], keyring = keyringName),
  webApiPassword = keyring::key_get(service = credNames[3], keyring = keyringName)
)


# E) Get  Cohorts------------------

# THe below is an example to work from

# set cohort Ids
cohortId <- 467L

# get cohort Names from webapi
cohortName <- getWebApiCohortInfo(cohortId, baseUrl = baseUrl)
cohortName <- gsub("\\s", "_", cohortNames)

# get json from webApi
cohortJson <- getWebApiCohortJson(cohortId = cohortId, baseUrl = baseUrl)


cohortFile <- fs::path("cohorts/json", cohortName, ext = "json")
readr::write_file(cohortJson, file = cohortFile)


# F. Clean up ------------------------

rm(list = ls())
