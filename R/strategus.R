
listStrategusModules <- function() {
  c("SelfControlledCaseSeriesModule",
    "EvidenceSynthesisModule",
    "CohortGeneratorModule",
    "CohortMethodModule",
    "CohortDiagnosticsModule",
    "CharacterizationModule",
    "PatientLevelPredictionModule",
    "CohortIncidenceModule")
}

listModuleVersions <- function(module) {

  #use github api to get all tags
  rr <- gh::gh("GET /repos/{owner}/{repo}/tags",
               owner = "ohdsi",
               repo = module)
  # extract the names which indicate the tag
  modVersions <- purrr::map_chr(rr, ~.x$name)

  return(modVersions)
}


getLatestModules <- function(modules = listStrategusModules()) {

  latestModules <- purrr::map(modules, ~listModuleVersions(.x)[1]) %>%
    purrr::set_names(modules)
  return(latestModules)
}
