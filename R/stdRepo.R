newUlyssesRepo <- function(
    repoName,
    repoFolder = NULL,
    studyMeta,
    dbOptions = NULL,
    gitOptions = NULL,
    renvOptions = NULL,
    verbose = TRUE,
    openProject = TRUE
) {

  if (verbose) {
    notification("Step 1: Creating R Project")
  }

  if (is.null(repoFolder)) {
    repoFolder <- here::here()
  }
  # make a path to repo
  repoPath <- fs::path(repoFolder, repoName) |>
    fs::path_expand() |>
    fs::dir_create() # make repo if it doesnt exist

  ## Make local project
  usethis::local_project(repoPath, force = TRUE)
  usethis::use_rstudio()


  # Step 2: add standard ulysses folders
  if (verbose) {
    notification("Step 2: Adding Standard Ulysses Folders")
  }

  folders <- listDefaultFolders()

  pp <- fs::path("./", folders) |>
    fs::dir_create(recurse = TRUE)

  # Step 3: make default files
  if (verbose) {
    notification("Step 3: Adding Standard Ulysses Files")
  }

  folders <- makeReadMe()

  pp <- fs::path("./", folders) %>%
    fs::dir_create(recurse = TRUE)

}





listDefaultFolders <- function() {
  analysisFolders <- c("src", "tasks", "migrations")
  execFolders <- c('logs', 'results', "export")
  cohortFolders <- c("json", "sql", "conceptSets/json")
  documentationFolders <- c("hub", "misc")

  folders <- c(
    paste('cohorts', cohortFolders, sep = "/"),
    paste('analysis', analysisFolders, sep = "/"),
    paste('exec', execFolders, sep = "/"),
    paste('documentation', documentationFolders, sep = "/"),
    'extras'
  )
  return(folders)
}

listDefaultFiles <- function() {

}
