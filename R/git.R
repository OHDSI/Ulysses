setGitOptions <- function(repoUrl) {

  ll <- list(
    repoUrl = repoUrl
  )
  return(ll)

}


initGit <- function(gitRemoteUrl, repoPath) {

  #Step1: initialize git
  gert::git_init(repoPath)
  # Step 2: add all files
  stg <- gert::git_add(files = ".")
  #step 3: commit all files
  sha <- gert::git_commit_all(message = "Initialize Ulysses Repo for study")
  #step 4: setup remote
  gert::git_remote_add(url = gitRemoteUrl)
  # Step 5: push
  gert::git_push(remote = "origin")

  invisible(TRUE)
}


checkCloneHasStandardFolders <- function(projectPath) {

  defaultFolders <- listDefaultFolders()

  currentFolders <- fs::dir_ls(path = projectPath,
                               type = "dir",
                               recurse = TRUE) |>
    fs::path_rel(start = projectPath)

  idx <- defaultFolders %in% currentFolders
  return(idx)
}

cloneUlysses <- function(gitRemoteUrl, repoFolder) {

  repoName <- fs::path_file(gitRemoteUrl) |> fs::path_ext_remove()
  repoPath <- fs::path(repoFolder, repoName)

  # clone repo
  notification(glue::glue_col("Cloning Repo {magenta {repoName}} to {cyan {repoPath}}"))
  gert::git_clone(
    url = gitRemoteUrl,
    path = repoPath
  )

  ii <- checkCloneHasStandardFolders(repoPath)

  #list folders that need to be added
  ff <- listDefaultFolders()[!ii]

  # make the console print
  txt <- ff |>
    paste(collapse = ", ")

  actionItem(glue::glue_col("Adding standard folders to Ulysses Clone: {green {txt}}"))
  # create folders missing following clone
  pp <- fs::path(repoPath, ff) |>
    fs::dir_create(recurse = TRUE)

  invisible(pp)

}
