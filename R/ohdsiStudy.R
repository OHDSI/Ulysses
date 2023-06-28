#' Function that initializes a new ohdsi study project environment
#' @param path the path where the project sits
#' @param projectName the name of the project
#' @param author the name of the study lead
#' @param type the type of study either Characterization, PLP or PLE
#' @param verbose whether the function should provide steps, default TRUE
#' @param openProject should the project be opened if created
#' @import rlang usethis fs
#' @export
newOhdsiStudy <- function(path,
                          projectName = basename(path),
                          author,
                          type,
                          verbose = TRUE,
                          openProject = TRUE) {

  # Step 1: create project directory
  if (verbose) {
    cli::cat_bullet("Step 1: Creating R Project",
                    bullet_col = "yellow", bullet = "info")
  }
  path <- fs::path_expand(path)

  ## Create the directory
  dir_path <- fs::dir_create(path)
  #cli::cat_line("\t- Creating ", crayon::cyan(dir_path))

  ## Make local project
  usethis::local_project(dir_path, force = TRUE)
  usethis::use_rstudio()
  #cli::cat_line("\t- Adding .Rproj")


  # Step 2: add picard directory structure folders
  addDefaultFolders(projectPath = dir_path, verbose = verbose)

  # Step 3: create _picard.yml file
  addStudyMeta(projectName = projectName,
               author = author,
               type = type,
               projectPath = dir_path,
               verbose = verbose)


  # Step 4: create gitignore
  if (verbose) {
    cli::cat_bullet("Step 4: Add to .gitignore file",
                    bullet_col = "yellow", bullet = "info")
  }

  ignores <- c("results/","logs/", "_study.yml")
  usethis::write_union(fs::path(dir_path, ".gitignore"), ignores)


  if (openProject) {
    cli::cat_bullet("Opening project in new session",
                    bullet_col = "yellow", bullet = "info")
    rstudioapi::openProject(dir_path, newSession = TRUE)
  }

  invisible(dir_path)

}

#' Function to check if the directory is a picard project
#' @param basePath the path of the directory
#' @export
isOhdsiStudy <- function(basePath) {

  # check if diretory has _study.yml file
  check1 <- fs::file_exists("_study.yml") %>% unname()

  #check if has subfolders
  folders <- c("analysis", "cohortsToCreate", "results", "logs", "extras")

  ff <- fs::dir_ls(basePath, type = "directory") %>%
    basename()
  check2 <- all(folders %in% ff)

  if (check1 & check2) {
    cli::cat_bullet(
      crayon::cyan(basename(basePath)), " is a picard project",
      bullet = "tick", bullet_col = "green"
    )
  } else{
    cli::cat_bullet(
      crayon::cyan(basename(basePath)), " is not a picard project",
      bullet = "cross", bullet_col = "red"
    )

  }

  invisible(check1)

}


addDefaultFolders <- function(projectPath, verbose = TRUE) {
  if (verbose) {
    cli::cat_bullet("Step 2: Adding OHDSI Study Project Folders",
                    bullet_col = "yellow", bullet = "info")
  }

  cohortFolders <- c('01_target')
  analysisFolders <- c("settings", "studyTasks", "private")
  folders <- c(
    paste('cohortsToCreate', cohortFolders, sep = "/"),
    paste('analysis', analysisFolders, sep = "/"),
    'results',
    'extras',
    'logs',
    'documentation'
  )

  pp <- fs::path("./", folders) %>%
    fs::dir_create(recurse = TRUE)
  invisible(pp)
}

addStudyMeta <- function(projectName,
                         author,
                         type = c("Characterization", "Population-Level Estimation", "Patient-Level Prediction"),
                         projectPath,
                         verbose = TRUE){

  if (verbose) {
    cli::cat_bullet("Step 3: Adding _study.yml file",
                    bullet_col = "yellow", bullet = "info")
  }



  # projName <- basename(projectPath) %>%
  #   snakecase::to_title_case()
  date <- lubridate::today()

  data <- rlang::list2(
    'Title' = projectName,
    'Author' = author,
    'Type' = type,
    'Date' = date
  )

  template_contents <- render_template("_study.yml", data = data)
  save_as <- fs::path(projectPath, "_study.yml")
  new <- write_utf8(save_as, template_contents)
  invisible(new)


}
