listDefaultFolders <- function() {
  analysisFolders <- c("src", "tasks")
  execFolders <- c('logs', 'results', "export")
  inputFolders <- c("barista","cohorts/json", "cohorts/sql", "conceptSets/json")
  disseminationFolders <- c("migration", "studyHub")


  folders <- c(
    paste('inputs', inputFolders, sep = "/"),
    paste('analysis', analysisFolders, sep = "/"),
    paste('exec', execFolders, sep = "/"),
    paste('dissemination', disseminationFolders, sep = "/"),
    'extras',
    'settings'
  )
  return(folders)
}

initReadMeFn <- function(sm, repoName, repoPath) {
  # prep title
  title <- glue::glue("# {sm$studyTitle} (Id: {repoName})")
  # prep start badge
  badge <- glue::glue(
    "<!-- badge: start -->

      ![Study Status: Started](https://img.shields.io/badge/Study%20Status-Started-blue.svg)
      ![Version: v0.0.1](https://img.shields.io/badge/Version-v0.0.1-yellow.svg)

    <!-- badge: end -->"
  )

  # create tag list
  tagList <- sm$listStudyTags()

  # prep study info
  info <-c(
    "## Study Information",
    glue::glue("- Study Id: {repoName}"),
    glue::glue("- Study Title: {sm$studyTitle}"),
    glue::glue("- Study Start Date: {lubridate::today()}"),
    glue::glue("- Expected Study End Date: {lubridate::today() + (365 * 2)}"),
    glue::glue("- Study Type: {sm$studyType}"),
    glue::glue("- Therapeutic Area: {sm$therapeuticArea}"),
    tagList
  ) |>
    glue::glue_collapse(sep = "\n")

  # prep placeholder for desc
  desc <- c(
    "## Study Description",
    "Add a short description about the study!"
  ) |>
    glue::glue_collapse(sep = "\n\n")

  # prep contributors
  contributors <- sm$listContributors()

  # prep links
  links <- sm$listStudyLinks()

  # combine and save to README file
  readmeLines <- c(
    title,
    badge,
    info,
    desc,
    contributors,
    links
  ) |>
    glue::glue_collapse(sep = "\n\n")

  readr::write_lines(
    x = readmeLines,
    file = fs::path(repoPath, "README.md")
  )

  actionItem(glue::glue_col("Initialize Readme: {green {fs::path(repoPath, 'README.md')}}"))
  invisible(readmeLines)
}


initNewsFn <- function(repoName, repoPath) {

  header <- glue::glue("# {repoName} v0.0.1")
  items <- c(
    glue::glue("  - Run Date: {lubridate::today()}"),
    "  - Initialize Ulysses Repo"
  ) |>
    glue::glue_collapse(sep = "\n")

  newsLines <- c(header, items) |>
    glue::glue_collapse(sep = "\n")
  #cat(newsLines)

  readr::write_lines(
    x = newsLines,
    file = fs::path(repoPath, "NEWS.md")
  )

  actionItem(glue::glue_col("Initialize NEWS: {green {fs::path(repoPath, 'NEWS.md')}}"))
  invisible(newsLines)
}

initExecConfigFileFn <- function(repoName, repoPath, exOp) {

  header <- glue::glue("# Exec Config File for Ulysses Repo: {repoName}")
  defaultBlock <- glue::glue( # DO NOT TOUCH SPACING
    "
default:
  projectName: {repoName}
  connectionScriptPath: {exOp$connectionLoadScript}
  resultsFolderPath: {exOp$resultsPath}

release:
  version: v0.0.1
  runDate: {lubridate::today()}"
  )

  configFile <- c(header, defaultBlock) |>
    glue::glue_collapse(sep = "\n\n")

  readr::write_lines(
    x = configFile,
    file = fs::path(repoPath, "settings/execConfig.yml")
  )

  actionItem(glue::glue_col("Initialize Execution Config: {green {fs::path(repoPath, 'settings/execConfig.yml')}}"))
  invisible(configFile)
}


initSourceConfigFileFn <- function(repoName, repoPath, exOp) {

  header <- glue::glue("# Source Config File for Ulysses Repo: {repoName}")
  defaultBlock <- glue::glue( # DO NOT TOUCH SPACING
    "
default:
  dbms: {exOp$dbms}
  databaseRole: {exOp$databaseRole}
  workDatabaseSchema: {exOp$workDatabaseSchema}
  tempEmulationSchema: {exOp$tempEmulationSchema}
  ")

  conBlocks <- purrr::map_chr(
    exOp$dbConnectionBlocks,
    ~.x$writeBlockSection(repoName = repoName)
  ) |>
    glue::glue_collapse("\n\n")

  configFile <- c(header, defaultBlock, conBlocks) |>
    glue::glue_collapse(sep = "\n\n")

  readr::write_lines(
    x = configFile,
    file = fs::path(repoPath, "settings/sourceConfig.yml")
  )

  actionItem(glue::glue_col("Initialize config: {green {fs::path(repoPath, 'settings/sourceConfig.yml')}}"))
  invisible(configFile)

}

