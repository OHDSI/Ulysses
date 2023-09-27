# Functions to work with cohorts to create


## Helpers -----------------------
findStepNumber <- function(dir = c("cohortsToCreate", "analysis/studyTasks"), projectPath = here::here()) {

  dir <- match.arg(dir, choices = c("cohortsToCreate", "analysis/studyTasks"))

  if (dir == "cohortsToCreate") {
    items <- fs::path(projectPath, dir) %>%
      fs::dir_ls(type = "directory") %>%
      basename()
  }

  if (dir == "analysis/studyTasks") {
    items <- fs::path(projectPath, dir) %>%
      fs::dir_ls(type = "file") %>%
      basename()
  }

  if (length(items) == 0) {
    step <- 1L
  } else {
    lastNumber <- gsub("_.*", "", items) %>%
      as.integer() %>%
      max()
    step <- lastNumber + 1L
  }

  return(step)

}

addFolder <- function(name, path) {
  cli::cat_bullet("Creating new folder ", crayon::cyan(name), " at ", crayon::cyan(path),
                  bullet = "tick", bullet_col = "green")

  fs::path(path, name) %>%
    fs::dir_create()

}

check_cohort_folders <- function(path, folderName) {

  ls <- fs::dir_ls(path = path, type = "dir") %>%
    basename()

  check <- grepl(folderName, ls)
  if (any(check)) {
    folderNameExists <- ls[check]
    txt <- glue::glue("Folder {folderName} already exists. Would you like to remove it?")
    opt <- usethis::ui_yeah(txt)
    if (opt) {
      pp <- fs::path(path, folderNameExists) %>%
        fs::dir_delete()
      cli::cat_bullet("Cohort folder ", crayon::cyan(pp), " has been removed.",
                      bullet = "info", bullet_col = "blue")
    }
  }
  invisible(folderName)

}

## Add Folder -------------

#' Function to create a cohort folder in input/cohortsToCreate
#' @param folderName The name of the new folder
#' @param projectPath the path to the project
#' @export
addCohortFolder <- function(folderName, projectPath = here::here()) {

  dir_path <- fs::path(projectPath, "cohortsToCreate")
  check_cohort_folders(path = dir_path, folderName = folderName)

  folderNumber <- findStepNumber(dir = "cohortsToCreate")

  if (folderNumber < 10L) {
    folderNumber <- scales::label_number(prefix = "0")(folderNumber)
  }

  folderName <- snakecase::to_lower_camel_case(folderName)

  fullName <- paste(folderNumber, folderName, sep = "_")

  addFolder(name = fullName, path = dir_path)

  invisible(fullName)

}

#' Function that lists all cohort definitions loaded into the study
#' @param projectPath the path to the project
#' @return tibble of the cohorts in the project
#' @export
cohortManifest <- function(projectPath = here::here()) {

  cohortFolder <- fs::path(projectPath, "cohortsToCreate")

  #get cohort file paths
  cohortFiles <- fs::dir_ls(cohortFolder, recurse = TRUE, type = "file", glob = "*.json")
  #get cohort names
  cohortNames <- fs::path_file(cohortFiles) %>%
    fs::path_ext_remove()
  #get cohort type
  cohortType <- fs::path_dir(cohortFiles) %>%
    basename() %>%
    gsub(".*_", "", .)

  #future addition of hash
  hash <- purrr::map(cohortFiles, ~readr::read_file(.x)) %>%
    purrr::map_chr(~digest::digest(.x, algo = "sha1")) %>%
    unname()

  #return tibble with info
  tb <- tibble::tibble(
    name = cohortNames,
    type = cohortType,
    hash = hash,
    file = cohortFiles %>% as.character()
  ) %>%
    dplyr::mutate(
      id = dplyr::row_number(), .before = 1
    )

  return(tb)
}
