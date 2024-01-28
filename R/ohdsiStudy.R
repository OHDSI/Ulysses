#' Function that initializes a new ohdsi study project environment
#' @param path the path where the project sits
#' @param projectName the name of the project
#' @param studyInfo a list object identifying the title, type and study version,
#' defaults to `setStudyInfo`, see documentation
#' @param authors a list object identifying the lead and developer authors names and emails,
#' defaults to `setStudyAuthors`, see documentation.
#' @param timeline a list object identifying the study status, start and end date,
#' defaults to `setStudyTimeline`, see documentation
#' @param about a list object identifying the study description, therapeutic area and databases,
#' defaults to `setStudyDescription`, see documentation
#' @param links a list object identifying the linked resources of the study,
#' defaults to `setStudyLinks`, see documentation
#' @param tags a list object identifying tags to the study,
#' defauls to `setStudyTags`, see documentation
#' @param verbose whether the function should provide steps, default TRUE
#' @param openProject should the project be opened if created
#' @import rlang usethis fs
#' @export
newOhdsiStudy <- function(path,
                          projectName = basename(path),
                          studyInfo = setStudyInfo(id = basename(path)),
                          authors = setStudyAuthors(),
                          timeline = setStudyTimeline(),
                          about = setStudyDescription(),
                          links = setStudyLinks(),
                          tags = setStudyTags(),
                          verbose = TRUE,
                          openProject = TRUE) {


  # Step 0: Bind study meta
  studyMeta <- studyInfo %>%
    append(authors) %>%
    append(timeline) %>%
    append(about) %>%
    append(links) %>%
    append(tags)

  studyMeta2 <- list(
    'study' = studyMeta
  )

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


  # Step 2: add ohdsi directory structure folders
  addDefaultFolders(projectPath = dir_path, verbose = verbose)

  # Step 3: create _study.yml file
  convert_to_yml(studySettings = studyMeta2, savePath = dir_path)


  # Step 4: create gitignore
  if (verbose) {
    cli::cat_bullet("Step 4: Add to .gitignore file",
                    bullet_col = "yellow", bullet = "info")
  }

  ignores <- c("exec/")
  usethis::write_union(fs::path(dir_path, ".gitignore"), ignores)


  if (openProject) {
    cli::cat_bullet("Opening project in new session",
                    bullet_col = "yellow", bullet = "info")
    rstudioapi::openProject(dir_path, newSession = TRUE)
  }

  invisible(dir_path)

}

#' Function to check if the directory is an ohdsi project
#' @param basePath the path of the directory
#' @export
isOhdsiStudy <- function(basePath) {

  # check if diretory has _study.yml file
  check1 <- fs::file_exists("_study.yml") %>% unname()

  #check if has subfolders
  folders <- c("analysis", "cohorts", "exec", "extras", "documentation")

  ff <- fs::dir_ls(basePath, type = "directory") %>%
    basename()
  check2 <- all(folders %in% ff)

  if (check1 & check2) {
    cli::cat_bullet(
      crayon::cyan(basename(basePath)), " is an ohdsi project",
      bullet = "tick", bullet_col = "green"
    )
  } else{
    cli::cat_bullet(
      crayon::cyan(basename(basePath)), " is not an ohdsi project",
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

  analysisFolders <- c("src", "tasks", "migrations")
  execFolders <- c('logs', 'results', "export")
  cohortFolders <- c("json", "sql")
  documentationFolders <- c("hub", "misc")

  folders <- c(
    paste('cohorts', cohortFolders, sep = "/"),
    paste('analysis', analysisFolders, sep = "/"),
    paste('exec', execFolders, sep = "/"),
    paste('documentation', documentationFolders, sep = "/"),
    'extras'
  )

  pp <- fs::path("./", folders) %>%
    fs::dir_create(recurse = TRUE)
  invisible(pp)
}




