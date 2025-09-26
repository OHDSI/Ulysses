# function to init rproj

initRProj <- function(studyId, repoPath) {
  projLines <- fs::path_package("Ulysses", "templates/rproj.txt") |>
    readr::read_file()

  projFile <- fs::path(repoPath, studyId, ext = "Rproj")
  readr::write_file(
    x = projLines,
    file = fs::path(projFile)
  )
  actionItem(glue::glue_col("Initializing {green {studyId}.Rproj}"))
  usethis::use_git_ignore(".Rproj.user")
  invisible(projFile)
}

# function to initialize the readme file

initReadMe <- function(studyMeta, studyId, repoPath) {
  # check that uses study meta class
  checkmate::assertClass(studyMeta, classes = c("studyMeta"))

  # prep title
  title <- glue::glue("# {studyMeta$studyDescription$studyTitle} (Id: {studyId})")
  # prep start badge
  badge <- glue::glue(
    "<!-- badge: start -->

      ![Study Status: Started](https://img.shields.io/badge/Study%20Status-Started-blue.svg)
      ![Version: v0.0.1](https://img.shields.io/badge/Version-v0.0.0.9999-yellow.svg)

    <!-- badge: end -->"
  )

  # create tag list
  tagList <- purrr::map(
    studyMeta$studyTags,
    ~glue::glue("\t* {.x}")
  ) |>
    glue::glue_collapse(sep = "\n")
  tagList <- c("- Tags", tagList) |> glue::glue_collapse(sep = "\n")

  # prep study info
  info <-c(
    "## Study Information",
    glue::glue("- Study Id: {studyId}"),
    glue::glue("- Study Title: {studyMeta$studyDescription$studyTitle}"),
    glue::glue("- Study Start Date: {lubridate::today()}"),
    glue::glue("- Expected Study End Date: {lubridate::today() + (365 * 2)}"),
    glue::glue("- Study Type: {studyMeta$studyDescription$studyType}"),
    glue::glue("- Therapeutic Area: {studyMeta$studyDescription$therapeuticArea}"),
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
    ~glue::glue("- {.x$role}: {.x$name} (email: {.x$email})")
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

  actionItem(glue::glue_col("Initialize Readme: {green {fs::path(repoPath, 'README.md')}}"))
  invisible(readmeLines)
}

# function to initialize the new file

initNews <- function(studyId, repoPath) {

  header <- glue::glue("# {studyId} v0.0.0.9999")
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

# function to make source blocks in source config
configBlockSection <- function(studyId, dbBlock) {
  blockSec <- glue::glue(
  "# Config block for {dbBlock$configBlockName}

{dbBlock$configBlockName}:
  databaseName: {dbBlock$databaseName}
  cdmDatabaseSchema: {dbBlock$cdm}
  vocabDatabaseSchema: {dbBlock$vocab}
  cohortTable: {studyId}_{dbBlock$configBlockName}_001
  "
  )
  return(blockSec)
}

# initialize the source config file in Ulysses repo in settings folder
initSourceConfigFile <- function(studyId, repoPath, dbOptions) {

  header <- glue::glue("# Source Config File for Ulysses Repo: {studyId}")
  defaultBlock <- glue::glue( # DO NOT TOUCH SPACING
    "
default:
  dbms: {dbOptions$dbms}
  databaseRole: {dbOptions$databaseRole}
  workDatabaseSchema: {dbOptions$workDatabaseSchema}
  tempEmulationSchema: {dbOptions$tempEmulationSchema}
  ")

  conBlocks <- purrr::map_chr(
    dbOptions$dbBlocks,
    ~configBlockSection(studyId = studyId, dbBlock = .x)
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
# function to create the inital execution config file
initExecutionConfigFile <- function(studyId, repoPath) {

  header <- glue::glue("# Execution Config File for Ulysses Repo: {studyId}")
  defaultBlock <- glue::glue( # DO NOT TOUCH SPACING
    "
default:
  projectName: {studyId}
  connectionConfigPath: !expr Sys.getenv('connectionConfigPath')

release:
  version: v0.0.1
  runDate: {lubridate::today()}"
  )

  configFile <- c(header, defaultBlock) |>
    glue::glue_collapse(sep = "\n\n")

  readr::write_lines(
    x = configFile,
    file = fs::path(repoPath, "executionConfig.yml")
  )

  actionItem(glue::glue_col("Initialize Execution Config: {green {fs::path(repoPath, 'settings/executionConfig.yml')}}"))
  invisible(configFile)

}


# function to initialize the egp template

initStudyHub <- function(studyMeta, aesOptions, repoPath) {

  backgroundColor <- aesOptions$backgroundColor
  foregroundColor <- aesOptions$foregroundColor
  footer <- aesOptions$footer
  studyTitle <- studyMeta$studyDescription$studyTitle

  # set upd hub quarto
  hubQuarto <- fs::path_package("Ulysses", "templates/quartoWebsite.yml") |>
    readr::read_file() |>
    glue::glue()

  writeFileAndNotify(
    x = hubQuarto,
    repoPath = fs::path(repoPath, "dissemination/studyHub"),
    fileName = "_quarto.yml"
  )
  # setup quarto css file
  cssFile <- fs::path_package("Ulysses", "templates/style.css") |>
    readr::read_file() |>
    glue::glue()

  writeFileAndNotify(
    x = cssFile,
    repoPath = fs::path(repoPath, "dissemination/studyHub"),
    fileName = "style.css"
  )

  # copy readme to index file
  readMeQmd <- readr::read_lines(
    file = fs::path(repoPath, "README.md")
  )

  writeFileAndNotify(
    x = readMeQmd,
    repoPath = fs::path(repoPath, "dissemination/studyHub"),
    fileName = "index.qmd"
  )

  # copy news to
  newsQmd <- readr::read_lines(
    file = fs::path(repoPath, "NEWS.md")
  )
  writeFileAndNotify(
    x = newsQmd,
    repoPath = fs::path(repoPath, "dissemination/studyHub"),
    fileName = "news.qmd"
  )

  # add egp template
  if (aesOptions$egpTemplate) {
    authors <- purrr::map_chr(
      studyMeta$studyContributors,
      ~glue::glue("\t- name: {.x$name}")
    ) |>
      glue::glue_collapse("\n")

    egp <- fs::path_package("Ulysses", "templates/EGP.qmd") |>
      readr::read_file() |>
      glue::glue()

    writeFileAndNotify(
      x = egp,
      repoPath = fs::path(repoPath, "dissemination/studyHub"),
      fileName = "egp.qmd"
    )

    # add cohort counts file
    sectionTitle <- "Cohort Counts"
    cohortCounts <- fs::path_package("Ulysses", "templates/reportFile.qmd") |>
      readr::read_file() |>
      glue::glue()

    fs::dir_create(fs::path(repoPath, "dissemination/studyHub/report"))
    actionItem(glue::glue("Create report folder in documentation"))
    writeFileAndNotify(
      x = cohortCounts,
      repoPath = fs::path(repoPath, "dissemination/studyHub/report"),
      fileName = "cohortCounts.qmd"
    )

  }

  invisible(hubQuarto)

}

loadFilesToFolder <- function(folder, files, repoPath) {

  folPath <- fs::path(repoPath, folder)

  fpath <- names(files)
  dirName <- fs::path_dir(fpath) |> unique()
  fs::dir_create(fs::path(folPath, dirName)) # make dir if does not exist
  fpath2 <- fs::path(folPath, fpath)

  fs::file_copy(
    path = files,
    new_path = fpath2,
    overwrite = TRUE
  )
  actionItem(glue::glue_col("Load files into Ulysses {cyan {folder}}"))
  for(i in seq_along(fpath)) {
    cli::cat_line(glue::glue("\t- {fpath[i]}"))
  }
  invisible(fpath2)
}
