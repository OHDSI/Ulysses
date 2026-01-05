#' @title Function to update the study version
#' @param versionNumber the semantive version number to set as the new project version: 1.0.0
#' @param projectPath the path of the project, defaults to the directory of the active Ulysses project
#' @export
updateStudyVersion <- function(versionNumber, projectPath = here::here()) {

  if (check_git_status()) {
    msg <- "There are uncommited changes!!! Please add and commit changes prior to updatng the project version"
    stop(msg)
  }

  # read in yml file
  configYml <- readr::read_lines(fs::path(here::here(), "config.yml"))
  # find the line where the version is
  versionLine <- which(grepl("  version: ", configYml))
  # replace the line with the new version number
  configYml[versionLine] <- glue::glue("  version: {versionNumber}")

  cli::cat_bullet(
    glue::glue_col("Update Study Version to: {yellow {versionNumber}}"),
    bullet = "info",
    bullet_col = "blue"
  )
  cli::cat_bullet(
    glue::glue_col("Overwrite {cyan config.yml} with update!"),
    bullet = "info",
    bullet_col = "blue"
  )

  # update and overwrite the yml file with the new version
  readr::write_lines(x = configYml, file = fs::path(here::here(), "config.yml"))
  updateNews(versionNumber = versionNumber, projectPath = projectPath)
  invisible(versionNumber)
}


#' @title Zip and Archive results from a study execution
#' @param input the type of files to zip and archive. There are three options exportMerge, exportPretty and site. exportMerge is the merged results in long format. The exportPretty are xlsx files with formatted output from the study. The site is the html files of the studyHub
#' @returns invisible return. Stores the input as a zip file in the exec/archive folder
#' @export
zipAndArchive <- function(input) {
  #ensure input is one of three options
  checkmate::assert_choice(x = input, choices = c("exportMerge", "exportPretty", "site"))

  # make the archive folder in exec
  if (!dir.exists("exec/archive")) {
    archivePathRoot <- fs::dir_create("exec/archive")
    usethis::use_git_ignore(archivePathRoot)
  }

  # get time stamp of archive
  timeStamp <- lubridate::now() |> as.character() |> snakecase::to_snake_case()

  # pull version number from config
  repoVersion <- config::get(value = "version")

  # if input is exportMerge grab results and prep for archive
  if (input == "exportMerge") {
    files2zip <- fs::dir_ls("dissemination/export/merge", type = "file")
    zipFileName <- glue::glue("exec/archive/export_merge_{version}_{timeStamp}")
  }

  # if input is exportPretty grab results and prep for archive
  if (input == "exportPretty") {
    files2zip <- fs::dir_ls("dissemination/export/pretty", type = "file")
    zipFileName <- glue::glue("exec/archive/export_pretty_{version}_{timeStamp}")
  }

  # if input is site grab files and prep for archive
  if (input == "exportMerge") {
    files2zip <- fs::dir_ls("dissemination/quarto/_site", type = "any")
    zipFileName <- glue::glue("exec/archive/quarto_site_{version}_{timeStamp}")
  }

  # zip results and place in archive
  utils::zip(zipfile = zipFileName, files = files2zip)
  cli::cat_bullet(
    glue::glue("Archived {input} to {zipFileName}."),
    bullet = "tick",
    bullet_col = "green"
  )

  invisible(zipFileName)

}


#' @title Function to execute a study task in Ulysses
#' @param taskFile the name of the taskFile. Only use the base name
#' @param configBlock the name of the configBlock to use in the execution
#' @param env the execution environment
#' @export
execStudyTask <- function(taskFile, configBlock, env = rlang::caller_env()) {

  cli::cat_rule(glue::glue_col("Run Task: {yellow {taskFile}}"))
  cli::cat_bullet(
    glue::glue_col("Using config: {green {configBlock}}"),
    bullet = "info",
    bullet_col = "blue"
  )

  fullTaskFilePath <- fs::path("analysis/tasks", taskFile) |>
    fs::path_expand()

  rLines <- readr::read_file(fullTaskFilePath) |>
    glue::glue(.open = "!||", .close = "||!")

  exprs <- rlang::parse_exprs(rLines)
  res <- NULL
  for (ex in seq_along(exprs)) {
    res <- eval(exprs[[ex]], env)
  }

  invisible(res)
}

#' @title Function to execute all study task in analysis folder on set of configBlock
#' @param configBlock name of one or multiple configBlock to use in the execution
#' @param env the execution environment
#' @export
execStudyPipeline <- function(configBlock, env = rlang::caller_env()) {

  taskFilesToRun <- fs::dir_ls("analysis/tasks", type = "file") |>
    basename()

  for (db in seq_along(configBlock)) {
    for (task in seq_along(taskFilesToRun)) {
      execStudyTask(
        taskFile = taskFilesToRun[task],
        configBlock = configBlock[db],
        env = env
      )
    }
  }

  invisible(taskFilesToRun)

}


addMainFile <- function(repoName, repoFolder, configBlocks, studyName) {
  repoPath <- fs::path(repoFolder, repoName) |>
    fs::path_expand()

  configBlocks <- paste0(configBlocks, collapse = "\", \"")

  mainR <- fs::path_package("Ulysses", "templates/main.R") |>
    readr::read_file() |>
    glue::glue()

  actionItem(glue::glue_col("Initialize Main Exec File: {green {fs::path(repoPath, 'main.R')}}"))
  readr::write_file(
    x = mainR,
    file = fs::path(repoPath, "main.R")
  )
  invisible(mainR)

}
