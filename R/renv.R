# Renv ----------------

checkHadesLock <- function() {

  #use github api to get hades wide releases
  rr <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
               owner = "ohdsi",
               repo = "Hades",
               path = "hadesWideReleases")

  hadesLockVersions <- purrr::map_chr(rr, ~.x$name)
  return(hadesLockVersions)
}

download_github_file <- function(downloadUrl) {
  #make temp file
  tmp <- tempfile()
  #download file
  utils::download.file(url = downloadUrl,
                       destfile = tmp,
                       quiet = TRUE)
  #read lines
  txt <- readr::read_lines(tmp)
  return(txt)
}

#' Function to grab the hades wide lock file
#' @param version the version of the hades wide lock file to use
#' @param projectPath the path to the project
#' @export
getHadesWideLock <- function(version, projectPath = here::here()) {

  #get the hades lock versions
  hadsLockVersions <- checkHadesLock()
  # check if version is in
  check <- version %in% hadesLockVersions
  if (!check) {
    stop("The version of the Hades wide lock file is not available.")
  }

  #pick the hades lock to use
  hadesLockToUse <- hadesLockVersions[which(version == hadesLockVersions)]

  #set path to file
  pathToFile <- glue::glue("hadesWideReleases/{hadesLockToUse}/renv.lock")

  # get the lock file
  rr <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
               owner = "ohdsi",
               repo = "Hades",
               path = pathToFile)
  #download github file
  lockFile <- download_github_file(rr$download_url)
  #write to project
  readr::write_lines(
    x = lockFile,
    file = fs::path(projectPath, "renv.lock")
  )
  # console print for downloaded hades wide lock file
  txt <- glue::glue("Downloaded Hades-wide lock file version: {crayon::green(version)}")
  cli::cat_bullet(
    txt,
    bullet = "tick",
    bullet_col = "green"
  )
  invisible(pathToFile)
}
