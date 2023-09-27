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

cohortHash <- function(projectPath = here::here()) {

  #get cohort file paths
  cohortFolder <- fs::path(projectPath, "cohortsToCreate")

  #get cohort file paths
  cohortFiles <- fs::dir_ls(cohortFolder, recurse = TRUE, type = "file", glob = "*.json")

  #future addition of hash
  hash <- purrr::map(cohortFiles, ~readr::read_file(.x)) %>%
    purrr::map_chr(~digest::digest(.x, algo = "sha1")) %>%
    unname()

  return(hash)

}

setCohortManifest <- function(projectPath = here::here()) {

  #get cohort file paths
  cohortFolder <- fs::path(projectPath, "cohortsToCreate")
  #get cohort file paths
  cohortFiles <- fs::dir_ls(cohortFolder, recurse = TRUE, type = "file", glob = "*.json")
  #get cohort names
  cohortNames <- fs::path_file(cohortFiles) %>%
    fs::path_ext_remove()
  #get cohort type
  typeDir <- fs::path_dir(cohortFiles) %>%
    basename()
  cohortType <- typeDir %>%
    gsub(".*_", "", .)

  #future addition of hash
  hash <- cohortHash(projectPath = projectPath)

  # create relative path to circe cohorts
  cleanFilePath <- fs::path("cohortsToCreate", typeDir, fs::path_file(cohortFiles))

  #return tibble with info
  tb <- tibble::tibble(
    name = cohortNames,
    type = cohortType,
    hash = hash,
    file = cleanFilePath %>% as.character()
  ) %>%
    dplyr::mutate(
      id = dplyr::row_number(), .before = 1
    )

  return(tb)
}

#' Function that lists all cohort definitions loaded into the study
#' @param projectPath the path to the project
#' @return tibble of the cohorts in the project
#' @export
cohortManifest <- function(projectPath = here::here()) {

  cohortManifestPath <- fs::path(projectPath, "cohortsToCreate/CohortManifest.csv")
  check <- fs::file_exists(cohortManifestPath)

  if (check) {
    cm <- readr::read_csv(file = cohortManifestPath,
                          show_col_types = FALSE)
    # check for any changes
    # check cohort hash of files
    hash <- cohortHash(projectPath = projectPath)
    #check if any are different
    changedCohorts <- which(cm$hash != hash)

    if (sum(changedCohorts) > 0) {
      cli::cat_bullet("Circe Cohorts have changed since last check",
                      bullet = "warning", bullet_col = "yellow")
      cm <- setCohortManifest(projectPath = projectPath)
      cli::cat_bullet("Updating CohortManifest.csv", bullet = "tick", bullet_col = "green")
      readr::write_csv(cm, file = cohortManifestPath)
    }

  } else{
    cm <- setCohortManifest(projectPath = projectPath)
    cli::cat_bullet("Initializing CohortManifest.csv", bullet = "tick", bullet_col = "green")
    readr::write_csv(cm, file = cohortManifestPath)
    usethis::use_git_ignore(ignores = "cohortsToCreate/CohortManifest.csv")
  }

  return(cm)
}



# Function to get full print logic from circe
getCohortPrint <- function(cohort) {
  #get cohort header
  cohortName <- snakecase::to_title_case(cohort$name)
  cohortId <- cohort$id
  cohortHeader <- glue::glue("## {cohortName} (id: {cohortId}) \n\n\n***Cohort Definition***")

  # get readable cohort logic
  # read json file
  json <- readr::read_file(cohort$file)
  # turn into print friendly
  cdRead <- CirceR::cohortPrintFriendly(json)
  cdRead <- paste(cohortHeader, cdRead, sep = "\n\n")
  # get readable concept set
  csRead <- RJSONIO::fromJSON(json)$ConceptSets |>
    CirceR::conceptSetListPrintFriendly()
  csRead <- paste("***Concept Sets***", csRead, sep = "\n\n")

  #bind to get full read
  readFull <- paste(cdRead, csRead, sep = "\n\n")
  return(readFull)
}

# print circe by section

cohortReadBySection <- function(cm, type) {
  # get the name of the section
  #typeSym <- rlang::sym(type)
  typeName <- snakecase::to_title_case(type)
  nameOfSection <- glue::glue("# {typeName}\n\n")
  # filter cohorts of that type
  cohorts <- cm %>%
    dplyr::filter(type == !!type)
  # split rows into a list
  cohorts <- whisker::rowSplit(cohorts)
  # get print of each cohort in the section
  cohortSections <- purrr::map_chr(cohorts, ~getCohortPrint(.x))
  #add header to string
  cohortSections <- c(nameOfSection, cohortSections)
  cohortSections <- paste(cohortSections, collapse = "\n")
  return(cohortSections)
}

#' Function to create the cohort details for the study
#' @description
#' The cohort details file is a document that provides a human-readable version
#' of the circe cohort definitions used in the study. This function targets
#' the cohortsToCreate folder and makes a quarto of the cohort details.
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @return writes a CohortDetails.qmd file to the documentation folder
#' @export
makeCohortDetails <- function(projectPath = here::here(), open = TRUE) {

  #make file path for cohortDetails
  cohortDetailsPath <- fs::path(projectPath, "documentation/CohortDetails.qmd")

  # get cohort manifest
  cm <- cohortManifest(projectPath = projectPath)
  # find distinct cohort types
  numType <- unique(cm$type)
  #get readable cohort details
  cohortDetails <- purrr::map_chr(numType, ~cohortReadBySection(cm, type = .x))

  headerText <- "---
title: Cohort Details
number-sections: true
number-depth: 1
toc: TRUE
toc-depth: 2
---"
  cohortDetails <- c(headerText, cohortDetails)
  # write to documenation section
  cli::cat_bullet("Render Cohort Details using cohortsToCreate folder",
                  bullet = "tick", bullet_col = "green")
  readr::write_lines(cohortDetails, file = cohortDetailsPath)

  #open file if toggle is on
  if (open) {
    rstudioapi::navigateToFile(cohortDetailsPath)
  }

  invisible(cohortDetails)
}
