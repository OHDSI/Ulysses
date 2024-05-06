# _strategusSrc.R

# A. Meta Info -----------------------

# Author: Joshua Ide and Martin Lavallee
# Date: 02/06/2024
# Description: The purpose of the _strategusSrc.R script is to provide source functions
# for running a strategus pipeline

# B. Functions ------------------------

getAnalaysisSpecifications <- function(analysisFile) {
  analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = analysisFile
  )

  return(analysisSpecifications)
}

executeAnalysis <- function(analysisFile, executionSettings, analysisName, outputLocation, resultsLocation, keyringName) {

  analysisSpecifications <- getAnalaysisSpecifications(analysisFile)

  Strategus::execute(
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings,
    executionScriptFolder = file.path(outputLocation, connectionDetailsReference, "strategusExecution"),
    keyringName = keyringName
  )

  # copy Results to final location
  resultsDir <- file.path(resultsLocation, analysisName, connectionDetailsReference)

  if (dir.exists(resultsDir)) {
    unlink(resultsDir, recursive = TRUE)
  }
  dir.create(file.path(resultsDir), recursive = TRUE)
  file.copy(file.path(outputLocation, connectionDetailsReference, "strategusOutput"),
            file.path(resultsDir), recursive = TRUE)

  #return(NULL)
  invisible(analysisSpecifications)
}



