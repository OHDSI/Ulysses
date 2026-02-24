# main.R

# A. Meta Info --------------

# Study: {studyName}
# Date: {lubridate::today()}
# Description: The purpose of main.R is to run all the tasks required for the study

# B. Dependencies ---------------

#renv::restore()

library(Ulysses)
library(DatabaseConnector)
library(tidyverse)

# C. Post Execution Steps ----------------

## build study hub ---------------

buildStudyHub(previewHub = FALSE)

## archive results
zipAndArchive(input = site)



