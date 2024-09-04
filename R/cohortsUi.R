
.grabCirceFiles <- function(type,
                            cohortFolder) {

  checkmate::assert_choice(type, choices = c("json", "sql"))

  #get cohort file paths
  files <- fs::dir_ls(
    path = fs::path(cohortFolder, type),
    type = "file",
    glob = glue::glue("*.{type}")
  )

  # get circe json
  contents <- purrr::map_chr(files, ~readr::read_file(.x))

  ll <- list(
    'files' = files,
    'contents' = contents
  )
  return(ll)
}

# function to make the cohortManifest
makeCohortManifest <- function(cohortFolder = here::here("cohorts")) {
  #get cohort file paths
  cohortJson <- .grabCirceFiles(type = "json", cohortFolder = cohortFolder)
  cohortSql <- .grabCirceFiles(type = "sql", cohortFolder = cohortFolder)


  if (length(cohortSql$files) == 0) {
    cli::cli_abort("There are no circe cohorts in this study.")
  }

  #get cohort names
  cohortNames <- fs::path_file(cohortSql$files) %>%
    fs::path_ext_remove()

  #get cohort ids
  cohortIds <- cohortNames |>
    stringr::str_rank(numeric = TRUE)

  # get circe json
  circeJson <- cohortJson$contents

  # get ohdsi sql
  ohdsiSql <- cohortSql$contents

  # determine cohortHash
  cohortHash <- purrr::map_chr(
    ohdsiSql,
    ~digest::digest(.x, algo = "md5", serialize = FALSE)
  )

  cohortManifest <- CohortManifest$new(
    cohortId = cohortIds,
    cohortName = cohortNames,
    cohortPrettyName = cohortNames,
    cohortHash = cohortHash,
    cohortType = rep("target", length(cohortIds)),
    circeJson = circeJson,
    ohdsiSql = ohdsiSql,
    generatedCohorts = NULL
  )

  return(cohortManifest)

}


# Functions to work with cohorts to create

md_to_viewer <- function(txt) {
  #create a temp dir for md file
  tempDir <- tempfile()
  dir.create(tempDir)
  tmpRmd <- file.path(tempDir, "clinChar.md")
  #write file to tmp
  readr::write_lines(txt, file = tmpRmd)
  tmpHtml <- rmarkdown::render(tmpRmd, params = "ask", quiet = TRUE)
  rstudioapi::viewer(tmpHtml)
}
