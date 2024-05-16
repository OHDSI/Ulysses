# Functions to work with cohorts to create

setCohortManifest <- function(projectPath = here::here()) {

  #get cohort file paths
  cohortFolder <- fs::path(projectPath, "cohorts/json")
  #get cohort file paths
  cohortFiles <- fs::dir_ls(cohortFolder, type = "file", glob = "*.json")

  if (length(cohortFiles) == 0) {
    cli::cli_abort("There are no circe cohorts in this study.")
  }

  #get cohort names
  cohortNames <- fs::path_file(cohortFiles) %>%
    fs::path_ext_remove()

  cohortIds <- cohortNames |>
    stringr::str_rank(numeric = TRUE)

  # todo add modification time
  # modDat <- fs::file_info(cohortFiles) |>
  #   dplyr::pull(modification_time)

  # TODO add lable names
  # TODO add order precedence

  #return tibble with info
  tb <- tibble::tibble(
    cohortId = cohortIds,
    cohortName = cohortNames
    )

  return(tb)
}

check_cm_dif <- function(cm, cmNew) {
  hsh1 <- digest::digest(cm |> dplyr::select(cohortId, cohortName))
  hsh2 <- digest::digest(cmNew |> dplyr::select(cohortId, cohortName))
  return(hsh1 != hsh2)
}


#' Function that lists all cohort definitions loaded into the study
#' @param projectPath the path to the project
#' @return tibble of the cohorts in the project
#' @export
cohortManifest <- function(projectPath = here::here()) {

  cohortManifestPath <- fs::path(projectPath, "cohorts/CohortManifest.csv")
  check <- fs::file_exists(cohortManifestPath)

  if (check) {
    cm <- readr::read_csv(file = cohortManifestPath,
                          show_col_types = FALSE) |>
      dplyr::mutate(
        cohortId = as.integer(cohortId)
      )

    cmNew <- setCohortManifest(projectPath = projectPath)

    if (check_cm_dif(cm, cmNew)) {
      cli::cat_bullet("Cohort Manifest has changed", bullet = "warning", bullet_col = "yellow")
      cli::cat_bullet("Overwriting CohortManifest.csv", bullet = "pointer", bullet_col = "yellow")
      cm <- cmNew
      readr::write_csv(cmNew, file = cohortManifestPath)
    }

  } else{
    cm <- setCohortManifest(projectPath = projectPath)
    cli::cat_bullet("Initializing CohortManifest.csv", bullet = "pointer", bullet_col = "yellow")
    readr::write_csv(cm, file = cohortManifestPath)
    usethis::use_git_ignore(ignores = "cohorts/CohortManifest.csv")
  }

  return(cm)
}



# Function to get full print logic from circe
getCohortPrint <- function(cohort) {
  #get cohort header
  cohortName <- snakecase::to_title_case(cohort$name)
  cohortId <- cohort$id
  cohortHeader <- glue::glue("# {cohortName} (id: {cohortId}) \n")
  # get readable cohort logic
  # get file path
  cohortFile <- fs::path("cohorts/json", cohort$name, ext = "json")
  # read json file
  json <- readr::read_file(cohortFile)
  # turn into print friendly
  cdRead <- CirceR::cohortPrintFriendly(json)
  cdRead <- paste(cohortHeader, "## Cohort Definition", cdRead, sep = "\n\n")
  # get readable concept set
  csRead <- RJSONIO::fromJSON(json)$ConceptSets |>
    CirceR::conceptSetListPrintFriendly()
  csRead <- paste("## Concept Sets", csRead, sep = "\n\n")

  #bind to get full read
  readFull <- paste(cdRead, csRead, sep = "\n\n")
  return(readFull)
}


# cohortReadBySection <- function(cm, type) {
#   # get the name of the section
#   #typeSym <- rlang::sym(type)
#   typeName <- snakecase::to_title_case(type)
#   nameOfSection <- glue::glue("# {typeName}\n\n")
#   # filter cohorts of that type
#   cohorts <- cm %>%
#     dplyr::filter(type == !!type)
#   # split rows into a list
#   cohorts <- whisker::rowSplit(cohorts)
#   # get print of each cohort in the section
#   cohortSections <- purrr::map_chr(cohorts, ~getCohortPrint(.x))
#   #add header to string
#   cohortSections <- c(nameOfSection, cohortSections)
#   cohortSections <- paste(cohortSections, collapse = "\n")
#   return(cohortSections)
# }

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
  cm <- Ulysses::cohortManifest(projectPath = projectPath) %>%
    whisker::rowSplit()

  #get readable cohort details
  cohortDetails <- purrr::map_chr(cm, ~getCohortPrint(cohort = .x))

  headerText <- "---
title: Cohort Details
number-sections: true
number-depth: 1
toc: TRUE
toc-depth: 2
---\n\n\n"
  cohortDetails <- c(headerText, cohortDetails)
  # write to documenation section
  cli::cat_bullet("Render Cohort Details using cohorts/json folder",
                  bullet = "tick", bullet_col = "green")
  readr::write_lines(cohortDetails, file = cohortDetailsPath)

  #open file if toggle is on
  if (open) {
    rstudioapi::navigateToFile(cohortDetailsPath)
  }

  invisible(cohortDetails)
}


# Archive -----------------


# cohortHash <- function(projectPath = here::here()) {
#
#   #get cohort file paths
#   cohortFolder <- fs::path(projectPath, "cohorts/json")
#
#   #get cohort file paths
#   cohortFiles <- fs::dir_ls(cohortFolder, type = "file", glob = "*.json")
#
#   #future addition of hash
#   hash <- purrr::map(cohortFiles, ~readr::read_file(.x)) %>%
#     purrr::map_chr(~digest::digest(.x, algo = "sha1")) %>%
#     unname()
#
#   return(hash)
#
# }


# updateCohortManifest <- function(cm, hash, idx) {
#
#   # identifiy the cohorts that changed
#   cohortsThatChanged <- fs::path("cohorts/json", cm$name[idx], ext = "json")
#   newVersion <- cm$version[idx] + 1L
#   cli::cat_bullet("Update ", crayon::green(cohortsThatChanged), " to version ", crayon::magenta(newVersion),
#                   bullet = "pointer", bullet_col = "yellow")
#
#   #update version
#   cm$hash[idx] <- hash[idx]
#   cm$version[idx] <- newVersion
#
#   return(cm)
# }
