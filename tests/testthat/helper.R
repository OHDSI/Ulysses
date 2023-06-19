## Taken from usethis

## If session temp directory appears to be, or be within, a project, there
## will be large scale, spurious test failures.
## The IDE sometimes leaves .Rproj files behind in session temp directory or
## one of its parents.
## Delete such files manually.
session_temp_proj <- usethis:::proj_find(fs::path_temp())
if (!is.null(session_temp_proj)) {
  Rproj_files <- fs::dir_ls(session_temp_proj, glob = "*.Rproj")
  ui_line(c(
    "Rproj file(s) found at or above session temp dir:",
    paste0("* ", Rproj_files),
    "Expect this to cause spurious test failures."
  ))
}

create_local_study <- function(dir = fs::file_temp(pattern = "testproj"),
                                 env = parent.frame()) {

  if (fs::dir_exists(dir)) {
    ui_stop("Target {ui_code('dir')} {ui_path(dir)} already exists.")
  }

  old_project <- usethis:::proj_get_() # this could be `NULL`, i.e. no active project
  old_wd <- getwd()          # not necessarily same as `old_project`

  withr::defer(
    {
      usethis::ui_done("Deleting temporary project: {dir}")
      fs::dir_delete(dir)
    },
    envir = env
  )

  usethis::ui_silence(
    newOhdsiStudy(path = dir,
                  projectName = "test",
                  author = "Ulysses",
                  type = "Characterization",
                  openProject = FALSE)
  )

  withr::defer(proj_set(old_project, force = TRUE), envir = env)

  usethis::proj_set(dir, force = TRUE)
  withr::defer(
    {
      usethis::ui_done("Restoring original working directory: {old_wd}")
      setwd(old_wd)
    },
    envir = env
  )
  setwd(usethis::proj_get())

  invisible(usethis::proj_get())
}



scrub_testpkg <- function(message) {
  gsub("testpkg[a-zA-Z0-9]+", "{TESTPKG}", message, perl = TRUE)
}


expect_proj_file <- function(...) expect_true(file_exists(proj_path(...)))
