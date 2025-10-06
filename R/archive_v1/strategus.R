# Strategus Functions ----------------

## Instantiated Folder ---------------------

### Utilities ------------------

make_strategus_path <- function(folderName, root = NULL) {

  if (is.null(root)) {
    root <- fs::path_home()
  }

  path <- root |>
    fs::path(folderName)

  return(path)

}

set_sys_var <- function(path) {
  # if system variable does not exist make it
  Sys.setenv(INSTANTIATED_MODULES_FOLDER = path) # set env var

  # console prints
  cli::cat_bullet(
    glue::glue("Set strategus modules folder as a system variable"),
    bullet = "tick",
    bullet_col = "green"
  )
  # cli::cat_bullet(
  #   glue::glue("{crayon::red('INSTANTIATED_MODULES_FOLDER')}: {crayon::green(path)}"),
  #   bullet = "pointer",
  #   bullet_col = "yellow"
  # )
  invisible(path)
}


create_modules_folder <- function(path) {
  fs::dir_create(path)
  cli::cat_bullet(
    "Created Strategus Instantiated Modules Folder",
    bullet = "tick",
    bullet_col = "green"
  )
  # cli::cat_bullet(
  #   glue::glue("Folder Path: {crayon::green(path)}"),
  #   bullet = "pointer",
  #   bullet_col = "yellow"
  # )
}

#' Function to set strategus instantiated folder
#' @param folderName the name of the instantiated folder
#' @param root the root path to the folder, defaults to the home path
#' @export
setStrategusInstantiatedFolder <- function(folderName, root = NULL) {

  # First create folder path
  strategusPath <- make_strategus_path(folderName = folderName, root = root)

  # Next check if there is a INSTANTIATED_MODULES_FOLDER system variable
  if (Sys.getenv("INSTANTIATED_MODULES_FOLDER") == "" | Sys.getenv("INSTANTIATED_MODULES_FOLDER") != strategusPath) {
    set_sys_var(strategusPath)
  } else{
    cli::cat_bullet(
      glue::glue("Strategus modules folder already set as system variable"),
      bullet = "info",
      bullet_col = "blue"
    )

  }

  # Finally check if INSTANTIATED_MODULES_FOLDER exists in dir
  check3 <- fs::dir_exists(Sys.getenv("INSTANTIATED_MODULES_FOLDER")) |>
    unname()
  if (!check3) {
    create_modules_folder(strategusPath)
  } else{
    cli::cat_bullet(
      "Strategus Instantiated Modules Folder already exists",
      bullet = "info",
      bullet_col = "blue"
    )
  }

  cli::cat_bullet(
    glue::glue("{crayon::red('INSTANTIATED_MODULES_FOLDER')}: {crayon::green(strategusPath)}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  invisible(strategusPath)
}



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

#
getLatestModules <- function(modules = listStrategusModules()) {

  latestModules <- purrr::map(modules, ~listModuleVersions(.x)[1]) %>%
    purrr::set_names(modules)
  return(latestModules)
}


# moduleTable <- function(projectPath = here::here()){
#
#   strategusMods <- retrieveStudySettings(projectPath = projectPath)$Strategus
#
#   tb <- tibble::tibble(
#     'module' = names(strategusMods),
#     'version' = purrr::map_chr(strategusMods, ~.x),
#     'remoteRepo' = "github.com",
#     'remoteUserName' = "ohdsi"
#   )
#   return(tb)
# }
