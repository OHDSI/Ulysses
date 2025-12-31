# Renv ----------------

setRenvOptions <- function(lockFilePath) {
  ll <- list('lockFilePath' = lockFilePath)
  class(ll) <- "renvOptions"
  return(ll)
}


checkHadesLock <- function() {

  #use github api to get hades wide releases
  rr <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
               owner = "ohdsi",
               repo = "Hades",
               path = "hadesWideReleases")

  hadesLockVersions <- purrr::map_chr(rr, ~.x$name)
  return(hadesLockVersions)
}

download_url_file <- function(downloadUrl) {
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


getLatestHadesLock <- function() {
  versions <- checkHadesLock()
  # get the latest
  hadesLockToUse<- dplyr::last(versions)

  #set path to file
  pathToFile <- glue::glue("hadesWideReleases/{hadesLockToUse}/renv.lock")

  # get the lock file
  rr <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
               owner = "ohdsi",
               repo = "Hades",
               path = pathToFile)
  downloadUrl <- rr$download_url

  return(downloadUrl)
}


loadRenvLock <- function(renvLock, repoPath) {

  lockFile <- download_url_file(renvLock)
  renvPath <- fs::path(repoPath, "renv.lock")
  #write to project
  readr::write_lines(
    x = lockFile,
    file = renvPath
  )
  actionItem(glue::glue_col("Add renv lock file version: {cyan {renvPath}}"))
  invisible(renvPath)
}

#
# getHadesWideLock <- function(version = latestHadesLock(), projectPath = here::here()) {
#
#   #get the hades lock versions
#   hadesLockVersions <- checkHadesLock()
#   # check if version is in
#   check <- version %in% hadesLockVersions
#   if (!check) {
#     stop("The version of the Hades wide lock file is not available.")
#   }
#
#   #pick the hades lock to use
#   hadesLockToUse <- hadesLockVersions[which(version == hadesLockVersions)]
#
#   #set path to file
#   pathToFile <- glue::glue("hadesWideReleases/{hadesLockToUse}/renv.lock")
#
#   # get the lock file
#   rr <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
#                owner = "ohdsi",
#                repo = "Hades",
#                path = pathToFile)
#   #download github file
#   lockFile <- download_github_file(rr$download_url)
#   #write to project
#   readr::write_lines(
#     x = lockFile,
#     file = fs::path(projectPath, "renv.lock")
#   )
#   # console print for downloaded hades wide lock file
#   actionItem(glue::glue_col("Downloaded Hades-wide lock file version: {green {version}}"))
#   invisible(pathToFile)
# }
