# main.R

# A. Meta Info --------------

# Study: {studyName}
# Date: {lubridate::today()}
# Description: The purpose of main.R is to run all the tasks required for the study

# B. Dependencies ---------------

renv::restore()

library(Ulysses)
library(DatabaseConnector)
library(tidyverse)

# C. Execution Specification ----------------

dbIds <- c("{configBlocks}")

# D. Setup Manifests ------------------------

initializeManifest(manifestType = "conceptSet")
initializeManifest(manifestType = "cohort")

populateManifest(manifestType = "conceptSet", importFromAtlas = TRUE)
populateManifest(manifestType = "cohort", importFromAtlas = TRUE)

# E. Execute Pipeline --------------------

execStudyPipeline(configBlock = dbIds)

# F. Post Execution Steps ----------------

## run merge files ----------------

## run formatted tables --------------

## build study hub ---------------

buildStudyHub(previewHub = FALSE)

## archive results
zipAndArchive(input = site)



