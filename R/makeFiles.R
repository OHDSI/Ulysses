#' @title Function initializing an R file for an analysis task
#' @param nameOfTask The name of the analysis task script
#' @param author the name of the person authoring the file. Defaults to template text if NULL
#' @param description a description of the analysis task. Defaults to template text if NULL
#' @param projectPath the path to the project
#' @param openFile toggle on whether the file should be opened
#' @export
makeTaskFile <- function(
    nameOfTask,
    author = NULL,
    description = NULL,
    projectPath = here::here(),
    openFile = TRUE) {

  analysisFolderPath <- fs::path(projectPath, "analysis/tasks")
  dirF <- fs::dir_ls(path = analysisFolderPath, type = "file")
  nFiles <- length(dirF) + 1
  numLead <- stringr::str_pad(nFiles, width = 2, side = "left", pad = "0")
  nameOfTask <- snakecase::to_lower_camel_case(nameOfTask)
  newName <- glue::glue("{numLead}_{nameOfTask}")


  # glue items
  taskName <- glue::glue("{newName}.R")
  studyName <- config::get("projectName", file = fs::path(projectPath, "config.yml"))
  if (is.null(author)) {
    author <- "ADD AUTHOR NAME HERE"
  }
  if (is.null(description)) {
    description <- "The purpose of this script is to....."
  }

  taskTemplate <- fs::path_package(
    package = "Ulysses",
    glue::glue("templates/task.R")
  ) |>
    readr::read_file() |>
    glue::glue()


  txt <- glue::glue_col("Write {cyan {taskName}} to {yellow {analysisFolderPath}}")
  cli::cat_bullet(
    txt,
    bullet = "tick",
    bullet_col = "green"
  )

  # write the new file to analysis/task
  readr::write_file(
    x = taskTemplate,
    file = fs::path(analysisFolderPath, newName, ext = "R")
  )

  if (openFile) {
    rstudioapi::navigateToFile(file = fs::path(analysisFolderPath, newName, ext = "R"))
    cli::cat_bullet(
      "Navigating to new task file",
      bullet = "info",
      bullet_col = "blue"
    )
  }

  invisible(taskTemplate)

}
