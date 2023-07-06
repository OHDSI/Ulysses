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

#' Function to create a scratch folder
#' @param projectPath the path to the project
#' @export
addScratchFolder <- function(projectPath = here::here()) {

  addFolder(name = "scratch", path = projectPath)
  usethis::use_git_ignore(ignores = "scratch")

}
