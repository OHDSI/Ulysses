
checkCloneHasStandardFolders <- function(projectPath) {

  defaultFolders <- listDefaultFolders()

  currentFolders <- fs::dir_ls(path = projectPath,
                               type = "dir",
                               recurse = TRUE) |>
    fs::path_rel(start = projectPath)

  idx <- defaultFolders %in% currentFolders
  return(idx)
}

#' @title Function to clone a Ulysses Repo into local
#' @param gitRemoteUrl the url of the git remote used to clone
#' @param repoFolder the location where you wish to place the git repo
#' @returns invisible return. Clones the git remote into local file structure and infuses default folders in clone
#' @export
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

#' @title Launch Ulysses Remote using Bitbucket Data Center
#' @param repoName the name of the repository in project
#' @param projectName the name of the project in Bitbucket Data Center storing the repo
#' @param hostUrl the url hosting bitbucket data center
#' @param httpToken the http access token configured to your bitbucket profile. To find the httpToken go to Manage Account > HTTP access tokens.
#' It is recommended that you store the httpToken as an environment variable fo ease of use.
#' @returns invisible return but initializes the remote on BitbucketDC
#' @export
launchUlyssesRemoteWithBitBucketDC <- function(repoName, hostUrl, httpToken, projectName) {

  cli::cat_rule(glue::glue_col("{magenta Inititialize Bitbucket remote}"))

  # create request
  req <- httr2::request(glue::glue("{hostUrl}/rest/api/latest/projects/{projectName}/repos")) |>
    httr2::req_headers("Accept" = "application/json", "charset" = "UTF-8") |>
    httr2::req_auth_bearer_token(token = httpToken) |>
    httr2::req_body_json(
      type = "application/json",
      data = list(
        defaultBranch = "main",
        name = repoName,
        description = glue::glue("Ulysses repo for study {repoName}. Please update description!"),
        project = list(
          key = toupper(projectName)
        ),
        scmId = "git",
        slug = repoName
      )
    )
  # TODO add options in case resp is not 200
  resp <- httr2::req_perform(req)

  cli::cat_bullet(
    glue::glue_col("Created {green {toupper(projectName)}} repo {green {repoName}} on {cyan {hostUrl}}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  tt <- httr2::resp_body_json(resp) # get body contents

  linkToRepo <- cli::style_hyperlink(
    text = tt$links$self[[1]]$href,
    url = tt$links$self[[1]]$href
  )
  cli::cat_line(
    glue::glue_col("Review new {green {toupper(projectName)}} remote at: {cyan {linkToRepo}}")
  )

  cli::cat_line("To Clone Repo use:")
  cli::cat_line(glue::glue_col("\t+ SSH: {cyan {tt$links$clone[[1]]$href}}"))
  cli::cat_line(glue::glue_col("\t+ HTTP: {cyan {tt$links$clone[[2]]$href}}"))

  invisible(tt)
}

check_git_status <- function() {
  check <- nrow(gert::git_status(staged = FALSE)) > 0
  return(check)
}


syncUlyssesWork <- function(commitMessage, gitRemoteName = "origin") {

  notification(glue::glue_col("Pull changes from {blue {gitRemoteName}}"))
  gert::git_pull(remote = gitRemoteName)

  # add all files
  notification("Adding all files touched since last commit!")
  stg <- gert::git_add(files = ".")

  # commit all with sha
  sha <- gert::git_commit_all(message = commitMessage)
  notification(glue::glue_col("Commit Work SHA: {green {sha}}"))

  # push to remote
  gert::git_push(remote = gitRemoteName)
}


#' @title Function to add a Remote to Ulysses directory
#' @description
#' This function adds a git remote to the Ulysses repo. If user adds a commit message
#' it will add and commit files prior to adding and pushing to remote. This function
#' will check to see if there are untracked files that need to be commited prior to adding remote.
#'
#' @param gitRemoteUrl a character string of a git remote url
#' @param gitRemoteName The name of the remote, defaults to origin
#' @param commitMessage a character string of a commit Message to use. if null then skips commit
#' @export
addGitRemoteToUlysses <- function(gitRemoteUrl, gitRemoteName = "origin", commitMessage = NULL) {

  checkmate::assert_character(gitRemoteUrl)

  if (!is.null(commitMessage)) {
    # Step 2: add all files
    stg <- gert::git_add(files = ".")
    #step 3: commit all files
    sha <- gert::git_commit_all(message = commitMessage)
  } else {
    if (check_git_status()) {
      msg <- "There are uncommited changes!!! Rerun addGitRemoteToUlysses() with a commitMessage!"
      stop(msg)
    }
  }

  #step 4: setup remote
  gert::git_remote_add(url = gitRemoteUrl)
  # Step 5: push
  gert::git_push(remote = gitRemoteName)

  invisible(gitRemoteUrl)
}
