listDefaultFolders <- function() {
  analysisFolders <- c("src", "tasks")
  execFolders <- c('logs', 'results')
  inputFolders <- c("cohorts/json", "cohorts/sql", "conceptSets")
  disseminationFolders <- c("quarto", "export/pretty", "export/merge", "documents")


  folders <- c(
    paste('inputs', inputFolders, sep = "/"),
    paste('analysis', analysisFolders, sep = "/"),
    paste('exec', execFolders, sep = "/"),
    paste('dissemination', disseminationFolders, sep = "/"),
    'extras'
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
      ![Version: 0.0.1](https://img.shields.io/badge/Version-0.0.1-yellow.svg)

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

  header <- glue::glue("# {repoName} 0.0.1")
  items <- c(
    glue::glue("- Run Date: {lubridate::today()}"),
    "- Initialize Ulysses Repo"
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


updateNews <- function(versionNumber, projectPath = here::here(), openFile = TRUE) {

  repoName <- basename(projectPath)
  newsFile <- readr::read_file(file = fs::path(projectPath, "NEWS.md"))
  newsHeader <- glue::glue("# {repoName} {versionNumber}\n\t-Run Date: {lubridate::today()}")
  updateNewsFile <- c(newsHeader, newsFile) |> glue::glue_collapse(sep = "\n\n")
  readr::write_file(updateNewsFile, file = fs::path(projectPath, "NEWS.md"))
  actionItem(glue::glue_col("Update NEWS: {green {fs::path(projectPath, 'NEWS.md')}}"))
  cli::cat_bullet(
    "Please add a bulleted description of changes to the new version!!!",
    bullet = "warning",
    bullet_col = "yellow"
  )
  if (openFile) {
    rstudioapi::navigateToFile(file = fs::path(projectPath, "NEWS.md"))
    actionItem("Opening NEWS.md for edits")
  }
  invisible(updateNewsFile)
}


