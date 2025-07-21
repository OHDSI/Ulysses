# function to initialize the readme file

initReadMe <- function(studyMeta, studyId, studyRepo) {
  # check that uses study meta class
  checkmate::assertClass(studyMeta, classes = c("studyMeta"))

  # prep title
  title <- glue::glue("# {studyMeta$studyDescription$studyTitle} (Id: {studyId})")
  # prep start badge
  badge <- glue::glue(
    "<!-- badge: start -->

      ![Study Status: Started](https://img.shields.io/badge/Study%20Status-Started-blue.svg)
      ![Version: v0.0.1](https://img.shields.io/badge/Version%20v0.0.1-yellow.svg)

    <!-- badge: end -->"
  )

  # create tag list
  tagList <- purrr::map(
    studyMeta$studyTags,
    ~glue::glue("\t\t* {.x}")
  ) |>
    glue::glue_collapse(sep = "\n")
  tagList <- c("\t- Tags", tagList) |> glue::glue_collapse(sep = "\n")

  # prep study info
  info <-c(
    "## Study Information",
    glue::glue("\t- Study Id: {studyId}"),
    glue::glue("\t- Study Title: {studyMeta$studyDescription$studyTitle}"),
    glue::glue("\t- Study Start Date: {lubridate::today()}"),
    glue::glue("\t- Expected Study End Date: {lubridate::today() + (365 * 2)}"),
    glue::glue("\t- Study Type: {studyMeta$studyDescription$studyType}"),
    glue::glue("\t- Therapeutic Area: {studyMeta$studyDescription$therapeuticArea}"),
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
  contributors <- purrr::map(
    studyMeta$studyContributors,
    ~glue::glue("\t- {.x$role}: {.x$name} (email: {.x$email})")
    ) |>
    glue::glue_collapse(sep = "\n")
  contributors <- c("## Contributors", contributors) |> glue::glue_collapse(sep = "\n\n")

  # prep links
  if (length(studyMeta$studyLinks) == 0) {
    links <- c("## Resources", "<!-- Place study Links as needed -->") |> glue::glue_collapse(sep = "\n\n")
  } else {
    links <- purrr::map(
      studyMeta$studyLinks,
      ~glue::glue("\t- {.x}:")
    ) |>
      glue::glue_collapse(sep = "\n")
    links <- c("## Resources", links) |> glue::glue_collapse(sep = "\n\n")
  }


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

  notification(glue::glue_col("Initialize Readme: {green {fs::path(repoPath, 'README.md')}}"))
  invisible(readmeLines)
}

# function to initialize the new file

initNews <- function(studyId, studyRepo) {

}

# function to initialize the egp template

initEGP <- function(studyMeta, studyId, studyRepo) {

}
