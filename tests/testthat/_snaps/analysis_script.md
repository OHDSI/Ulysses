# make analysis Script works

    Code
      writeLines(read_utf8(proj_path("analysis/studyTasks/01_buildCohorts.R")))
    Output
      # A. Meta Info -----------------------
      
      # Study: test
      # Name: Build Cohorts
      # Author: Ulysses
      # Date: 2023-06-19
      # Description: The purpose of 01_buildCohorts.R is to.....
      
      # B. Dependencies ----------------------
      
      library(tidyverse, quietly = TRUE)
      library(DatabaseConnector)
      library(config)
      
      # C. Connection ----------------------
      
      # set connection Block
      configBlock <- "test"
      
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
      
      outputFolder <- here::here("results/01_buildCohorts") %>%
        fs::path(executionSettings$databaseName) %>%
        fs::dir_create()
      
      ### Add study variables or load from settings
      
      # E. Script --------------------
      
      # Add script here
      
      # F. Session Info ------------------------
      
      sessioninfo::session_info()
      rm(list=ls())

