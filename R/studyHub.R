# This function takes the README and NEWS md files and updates to qmd for rendering
knitIndexAndNews <- function(projectPath) {

  # copy readme to index file
  readMeQmd <- readr::read_lines(
    file = fs::path(projectPath, "README.md")
  )

  writeFileAndNotify(
    x = readMeQmd,
    repoPath = fs::path(projectPath, "dissemination/quarto"),
    fileName = "index.qmd"
  )

  # copy news to
  newsQmd <- readr::read_lines(
    file = fs::path(projectPath, "NEWS.md")
  )
  writeFileAndNotify(
    x = newsQmd,
    repoPath = fs::path(projectPath, "dissemination/quarto"),
    fileName = "news.qmd"
  )
  invisible(projectPath)
}


initStudyHubFiles <- function(repoName,
                         repoFolder,
                         studyTitle,
                         foregroundColor = "#00E47C",
                         backgroundColor = "#08312A") {

  repoPath <- fs::path(repoFolder, repoName) |>
    fs::path_expand()

  ## Make folders for quarto
  foldersToCreate <- c("R", "results", "images")
  fs::dir_create(
    fs::path(repoPath, "dissemination/quarto", foldersToCreate)
  )
  # add key files
  egp <- readr::read_file(
    file = fs::path_package("Ulysses", "templates/EGP.qmd")
  ) |>
    glue::glue()

  writeFileAndNotify(
    x = egp,
    repoPath = fs::path(repoPath, "dissemination/quarto"),
    fileName = "egp.qmd"
  )

  # set upd hub quarto
  hubQuarto <- fs::path_package("Ulysses", "templates/quartoWebsite.yml") |>
    readr::read_file() |>
    glue::glue()

  writeFileAndNotify(
    x = hubQuarto,
    repoPath = fs::path(repoPath, "dissemination/quarto"),
    fileName = "_quarto.yml"
  )

  resultsFile <- readr::read_file(
    file = fs::path_package("Ulysses", "templates/resultsFile.qmd")
  ) |>
    glue::glue()

  writeFileAndNotify(
    x = resultsFile,
    repoPath = fs::path(repoPath, "dissemination/quarto/results"),
    fileName = "results_init.qmd"
  )

  # setup quarto css file
  cssFile <- fs::path_package("Ulysses", "templates/style.css") |>
    readr::read_file() |>
    glue::glue()

  writeFileAndNotify(
    x = cssFile,
    repoPath = fs::path(repoPath, "dissemination/quarto"),
    fileName = "style.css"
  )

  # update index and news
  knitIndexAndNews(projectPath = repoPath)

  # done
  invisible(repoPath)
}

#' @title Build Study Hub
#' @param projectPath the path to the Ulysses repo, by default takes the path of the active R project
#' @param previewHub toggle to preview the hub after it builds. Default is TRUE
#' @returns invisible return. Creates _site folder with html files to preview site
#' @export
buildStudyHub <- function(projectPath = here::here(), previewHub = TRUE) {

  cli::cat_rule("Build Study Hub")

  cli::cat_bullet("Update Index and NEWS files", bullet = "info", bullet_col = "blue")
  knitIndexAndNews(projectPath)

  docsPath <- fs::path(projectPath, "dissemination/quarto") |>
    fs::path_expand()

  cli::cat_bullet("Render Study Hub", bullet = "info", bullet_col = "blue")
  quarto::quarto_render(
    input = docsPath,
    as_job = FALSE
  )

  if (previewHub) {
    indexFilePath <- fs::path(projectPath, "dissemination/quarto/_site/index.html")
    #check <- fs::file_exists(indexFilePath)
    cli::cat_bullet("Preview Study Hub", bullet = "pointer", bullet_col = "yellow")
    #launch preview
    utils::browseURL(indexFilePath)
  }

  invisible(docsPath)

}
