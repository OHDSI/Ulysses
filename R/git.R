# Functions for git related tasks ------------

getGithubUser <- function() {
  safe_whoami <- purrr::safely(gh::gh_whoami)
  gitCreds <- safe_whoami()
  if (length(gitCreds$error) > 0) {
    cli::cat_bullet("Need to set up a Github PAT. Follow instructions from: ",
                    crayon::italic("https://gh.r-lib.org/articles/managing-personal-access-tokens.html"),
                    bullet = "warning",
                    bullet_col = "yellow")
    user <- "githubUser"
  }
  user <- gitCreds$result$login

  return(user)
}

has_git <- function(proj) {
  repo <- tryCatch(gert::git_find(proj), error = function(e) NULL)
  !is.null(repo)
}

#' Publish Study to Repo
#' @param repoUrl an https url for the repo remote. Example: https://github.com/Org/repo.git
#' @param message the commit message for your OHDSI study
#' @export
publishStudyToRepository <- function(repoUrl,
                                     message = "Initial commit for OHDSI study") {

  proj <- usethis::proj_get()
  check <- !has_git(proj)
  if (check) {
    #Step1: initialize git
    gert::git_init(proj)
    # Step 2: add all files
    stg <- gert::git_add(files = ".")
    #step 3: commit all files
    sha <- gert::git_commit_all(message = message)
    #step 4: setup remote
    gert::git_remote_add(url = repoUrl)
    # Step 5: push
    gert::git_push(remote = "origin")
  } else {
    remoteLs <- gert::git_remote_list()$url
    has_remote <- remoteLs %in% repoUrl
    if (!has_remote) {
      #step 4: setup remote
      gert::git_remote_add(url = repoUrl)
      # Step 5: push
      gert::git_push(remote = "origin")
    }
  }
  if(check) {
    #restart R session
    cli::cat_bullet("Restarting RStudio to setup Git Pane in RStudio", bullet = "info", bullet_col = "blue")
    rstudioapi::openProject(proj)
  }
  invisible(TRUE)
}



