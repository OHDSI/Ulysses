
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

#' Function to retrieve table of modules
#' @param projectPath the path to the project
#' @export
moduleTable <- function(projectPath = here::here()){

  strategusMods <- retrieveStudySettings(projectPath = projectPath)$Strategus

  tb <- tibble::tibble(
    'module' = names(strategusMods),
    'version' = purrr::map_chr(strategusMods, ~.x),
    'remoteRepo' = "github.com",
    'remoteUserName' = "ohdsi"
  )
  return(tb)
}
