newUlyssesRepo <- function(
    repoName,
    repoFolder = NULL,
    studyMeta,
    dbOptions = NULL,
    cohortsToLoadPath = NULL,
    conceptSetsToLoadPath = NULL,
    fileLoadOptions = NULL,
    gitRemote = NULL,
    renvLock = NULL,
    aesOptions = NULL,
    verbose = TRUE,
    openProject = TRUE
) {

  if (verbose) {
    notification("Step 1: Creating R Project")
  }
  # if repoFolder is null use current wd
  if (is.null(repoFolder)) {
    repoFolder <- here::here()
  }
  # make a path to repo
  repoPath <- fs::path(repoFolder, repoName) |>
    fs::path_expand() |>
    fs::dir_create() # make repo if it doesnt exist

  ## Make local project
  usethis::local_project(repoPath, force = TRUE)
  initRProj(studyId = repoName, repoPath = repoPath)


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

  #init read me
  initReadMe(studyMeta = studyMeta, studyId = repoName, repoPath = repoPath)
  # init news
  initNews(studyId = repoName, repoPath = repoPath)
  # init execution config
  initExecutionConfigFile(studyId = repoName, repoPath = repoPath)

  # Step 4: Run additional Setup steps

  #Init config Block if not null
  if (!is.null(dbOptions)) {
    if (verbose) {
      notification("Init sourceConfig.yml")
      initSourceConfigFile(studyId = repoName, repoPath = repoPath, dbOptions = dbOptions)
      initConnectionRFile()
    }
  } else{
    notification("No sourceConfig.yml was initialized. Please add one to the settings folder.")
  }

  #Load cohorts
  if (!is.null(cohortsToLoadPath)) {
    if (verbose) {
      notification("Add Atlas cohorts to Ulysses Repo")
      loadCohorts(cohortsToLoadPath = cohortsToLoadPath, repoPath = repoPath)
    }
  }

  #Load concept sets if any
  if (!is.null(conceptSetsToLoadPath)) {
    if (verbose) {
      notification("Add Atlas concept sets to Ulysses Repo")
      loadConceptSets(conceptSetsToLoadPath = conceptSetsToLoadPath, repoPath = repoPath)
    }
  }


  #Init documentation
  if (!is.null(aesOptions)) {
    if (verbose) {
      notification("Initialize Study Hub")
      initStudyHub(studyMeta = studyMeta, aesOptions = aesOptions, repoPath = repoPath)
    }
  }

  #Init analysis
  if (!is.null(fileLoadOptions)) {
    if (verbose) {
      notification("Add files to Ulysses Repo")
      loadFilesToFolder(folder = "analysis", files = fileLoadOptions$taskFiles, repoPath = repoPath)
      loadFilesToFolder(folder = "analysis", files = fileLoadOptions$srcFiles, repoPath = repoPath)
      loadFilesToFolder(folder = "documentation", files = fileLoadOptions$hubFiles, repoPath = repoPath)
      loadFilesToFolder(folder = "documentation", files = fileLoadOptions$reportFiles, repoPath = repoPath)
    }
  }

  #Init renv
  if (!is.null(renvLock)) {
    if (verbose) {
      notification("Set renv.lock")
      loadRenvLock(renvOptions = renvOptions, repoPath = repoPath)
    }
  }

  #Init git
  if (!is.null(gitRemote)) {
    if (verbose) {
      notification("Init local git and set remote url")
      initGit(gitRemoteUrl = gitRemote, repoPath = repoPath)
    }
  }


  if (openProject) {
    notification("Opening project in new session")
    rstudioapi::openProject(repoPath, newSession = TRUE)
  }

  invisible(repoPath)
}





listDefaultFolders <- function() {
  analysisFolders <- c("src", "tasks")
  execFolders <- c('logs', 'results', "export")
  cohortFolders <- c("json", "sql", "conceptSets/json")
  disseminationFolders <- c("migration", "studyHub")


  folders <- c(
    paste('cohorts', cohortFolders, sep = "/"),
    paste('analysis', analysisFolders, sep = "/"),
    paste('execution', execFolders, sep = "/"),
    paste('dissemination', disseminationFolders, sep = "/"),
    'extras',
    'settings'
  )
  return(folders)
}

