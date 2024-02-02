# background jobs

checkFileInTasks <- function(fileName, files) {

  ff <- basename(files) # get basename
  fileName %in% ff # check file in files

}

find_config_block <- function(lines, startBlock = "# <<<", endBlock = "# >>>") {

  start <- which(lines == "# <<<")
  end <- which(lines == "# >>>")

  ll <- c(start + 1L, end - 1L)

  return(ll)
}

prep_studyTask <- function(lines, value) {

  # create new config
  blockLines <- find_config_block(lines)
  start <- blockLines[1]
  end <- blockLines[2]
  configBlock <- lines[rlang::seq2(start, end)]
  new_configBlock <- gsub("\\[block\\]", value, configBlock)


  lines2 <- c(lines[rlang::seq2(1, start - 1L)], new_configBlock, lines[rlang::seq2(end + 1, length(lines))]) |>
    paste(collapse = "\n")

  return(lines2)
}

#' Function to run study task in background
#' @param taskFile a task file to run in the background
#' @param configBlock the configBlock to use in the study
#' @param projectPath the path to the project
#' @return invisble return, launches background job
#' @export
runTaskInBackground <- function(taskFile, configBlock, projectPath = here::here()) {

  #set task path
  ff <- fs::path(projectPath, "analysis/tasks") |>
    fs::dir_ls(type = "file")

  # send error if task does not exist
  if (!checkFileInTasks(taskFile, ff)) {
    stop("Task does not exist in analysis/task folder")
  } else{
    #ow build file path
    taskFile2 <- fs::path(projectPath, "analysis/tasks", taskFile)
    taskName <- basename(tools::file_path_sans_ext(taskFile2))
  }

  # augment configBlock for run
  rLines <- readr::read_lines(taskFile2)
  newLines <- prep_studyTask(lines = rLines, value = configBlock)


  #save as a tempfile
  tempFile <- fs::file_temp(pattern = taskName, ext = "R")
  readr::write_lines(newLines, file = tempFile)

  # run task as a background job
  rstudioapi::jobRunScript(
    path = tempFile,
    name = taskName,
    workingDir = projectPath
  )

  # clean up temp
  fs::file_delete(tempFile)

  invisible(taskName)

}
