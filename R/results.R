#' Funciton to compress results into a zip file
#' @param databaseName the name of the database folder to compress
#' @param projectPath the path to the project
#' @export
zipResults <- function(databaseName, projectPath = here::here()) {

  # create path to results folder of ulysses
  resultsPath <- fs::path(projectPath, "results")

  # get the base name
  bname <- fs::dir_ls(resultsPath, type = "dir") |>
    basename()

  check <- databaseName %in% bname
  if (!check) {
    stop("The databaseName needs to be a name of a folder in the results directory.")
  }

  # get path to results folder of databaseName
  resultsDbPath <- fs::path(resultsPath, databaseName)

  #create zip folder
  zipFile <- fs::path(resultsDbPath, ext = "zip")

  #find all files to zip
  allFiles <- fs::dir_ls(resultsDbPath, recurse = TRUE, type = "file")

  utils::zip(zipfile = zipFile, files = allFiles)

  # console message that files were zipped
  txt <- glue::glue("Zipped results files for {crayon::magenta(databaseName)}")
  cli::cat_bullet(txt, bullet = "pointer", bullet_col = "yellow")
  txt2 <- glue::glue("Saved to: {crayon::cyan(zipFile)}")
  cli::cat_bullet(txt2, bullet = "pointer", bullet_col = "yellow")

  invisible(allFiles)

}
