# Setup Credentials -------------
# This file setups the credential library for your study. The function establishes
# a config.yml file and creates a keyring for the study. Input your credentials
# into the keyring. Keep your database credentials handy before running this script.
# Ask your database administrator if you are unsure of your credentials.

## A) Depedendencies ------------

library(tidyverse, quietly = TRUE)
library(Ulysses)
library(keyring)

## B) Set Parameters ------------

configBlock <- "{{{ Block }}}" # name of config block

database <- "{{{ Database }}}" # the name of the database in the config block

keyringName <- "{{{ Study }}}" # the name of the keyring

keyringPassword <- "{{{ Secret }}}" # password for keyring
# This password is simply to avoid a prompt when creating the keyring

## C) Check or create Config File------------------------

# check if config.yml file exists, make it if it does not exist
checkConfig()

## D) Setup Keyring -----------------

# set keyring
setStudyKeyring(keyringName = keyringName,
                keyringPassword = keyringPassword)

# set credential keys in keyring
setMulitpleCredentials(
  cred = defaultCredentials(),
  db = configBlock,
  keyringName = keyringName,
  keyringPassword = keyringPassword,
  forceCheck = TRUE
)

# If a single credential is incorrect, change it
# setDatabaseCredential(cred = "dbms",
#                       db = configBlock,
#                       keyringName = keyringName,
#                       keyringPassword = keyringPassword,
#                       forceCheck = TRUE
# )

## E) Check (Optional) -----------------------


### Test connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)
connectionDetails$dbms


## G) Session Info ------------------------

sessioninfo::session_info()
rm(list=ls())
