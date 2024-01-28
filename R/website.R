# check if yml exists
checkWebsiteYml <- function(projectPath = here::here()) {

  #create yml path
  ymlPath <- fs::path(projectPath, "documentation/hub/_quarto.yml")
  check <- fs::file_exists(ymlPath)
  if (check) {
    cli::cat_bullet("_quarto.yml already exists for study hub", bullet = "pointer", bullet_col = "yellow")
  }

  return(check)
}


# Function to create quarto yml
makeWebsiteYaml <- function(footer = NULL,
                            logoPath = NULL,
                            backgroundColor = "#336B91", #OHDSI Blue
                            projectPath = here::here()) {

  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)$study

  if (is.null(footer)) {
    footer <- "Produced using Ulysses Package"
  }

  if (is.null(logoPath)) {

    logoPath <- fs::path_package("Ulysses", "images")

    importImages(imageFolder = logoPath, projectPath = here::here())

    logo <- "images/ohdsi_logo.png"

  }


  # make list of vars for template
  data <- rlang::list2(
    'Title' = replaceTitleColon(studyMeta$title),
    'Footer' = footer,
    'Color' = backgroundColor,
    'Logo' = logo
  )

  check <- checkWebsiteYml(projectPath = projectPath)
  if (!check) {
    #build template
    usethis::use_template(
      template = "quartoWebsite.yml",
      save_as = fs::path("documentation/hub", "_quarto.yml"),
      data = data,
      open = FALSE,
      package = "Ulysses")
  }


  invisible(data)

}

#function to convert readme to index.qmd
makeIndexQuarto <- function(projectPath = here::here()) {

  # find readme
  readMePath <- fs::path(projectPath, "README.md")
  #read lines of readme
  lines <- readr::read_lines(readMePath)
  #find the point to skip
  jumpPoint <- which(lines == "<!-- studyStatus: end -->") + 1
  #create sequence of lines to keep
  keepLines <- rlang::seq2(jumpPoint, length(lines))
  #make the new readme for index.qmd
  newReadMe <- lines[keepLines]

  #set path to documentation
  docPath <- fs::path(projectPath, "documentation/hub/index.qmd")

  #write new readme to index.qmd
  cli::cat_bullet("Convert README.md to index.qmd", bullet = "tick", bullet_col = "green")
  readr::write_lines(newReadMe, file = docPath)

  #ignore index.qmd as it is redundant to README.md
  usethis::use_git_ignore(ignores = "documentation/hub/index.qmd")
  invisible(docPath)
}

#funciton to convert news to news,qmd
makeNewsQuarto <- function(projectPath = here::here()) {

  # find readme
  readMePath <- fs::path(projectPath, "NEWS.md")
  #read lines of readme
  lines <- readr::read_lines(readMePath)

  #set path to documentation
  docPath <- fs::path(projectPath, "documentation/hub/news.qmd")

  #write new readme to index.qmd
  cli::cat_bullet("Convert NEWS to news.qmd", bullet = "tick", bullet_col = "green")
  readr::write_lines(lines, file = docPath)

  #ignore index.qmd as it is redundant to README.md
  usethis::use_git_ignore(ignores = "documentation/hub/news.qmd")
  invisible(docPath)
}

#check which docs are missing so we can generate a template
missingStandardDocs <- function(projectPath = here::here()) {

  # make path to documentation folder
  docsPath <- fs::path(projectPath, "documentation/hub")

  # look up all qmd files
  resourceFiles <- fs::dir_ls(docsPath, glob = "*.qmd") %>%
    basename() %>%
    tools::file_path_sans_ext()

  expectedFiles <- c("AnalysisPlan", "ResultsReport")

  `%notin%` <- Negate("%in%")

  missingFiles <- expectedFiles[which(expectedFiles %notin% resourceFiles)]
  return(missingFiles)

}

makeMissingDocs <- function(missingDocs, projectPath = here::here()) {
  #create function names
  fnNames <- glue::glue("make{missingDocs}")

  for (i in seq_along(fnNames)) {
    eval(rlang::call2(fnNames[i], projectPath = projectPath, open = FALSE, .ns = "Ulysses"))
  }
}



#' Function to preview study hub
#' @param projectPath path to ohdsi study
#' @return previews study hub in browser
#' @export
previewStudyHub <- function(projectPath = here::here()) {

  # make index path and check that it exists
  indexFilePath <- fs::path(projectPath, "documentation/hub/_site/index.html")
  check <- fs::file_exists(indexFilePath)

  if (check ) {
    cli::cat_bullet("Preview Study Hub", bullet = "pointer", bullet_col = "yellow")
    #launch preview
    utils::browseURL(indexFilePath)
  } else {
    cli::cat_bullet("Study Hub has not been rendered. Run `Ulysses::buildStudyHub()` to initialize",
                    bullet = "warning", bullet_col = "yellow")
  }

  invisible(indexFilePath)

}

#' Function to build study hub
#' @param projectPath path to ohdsi study
#' @param logoPath a path to a logo png to use in the quarto website, defaults to
#' ohdsi logo from Ulysses inst.
#' @param footer add a footer to the study Hub
#' @param backgroundColor change background color, defaults to OHDSI blue #336B91
#' @return builds a _site folder in documenation that holds the html website
#' @export
buildStudyHub <- function(projectPath = here::here(),
                          logoPath = NULL,
                          footer = NULL,
                          backgroundColor = "#336B91" #OHDSI Blue
                          ) {

  # Step 1: Make yml
  makeWebsiteYaml(
    footer = footer,
    logoPath = logoPath,
    backgroundColor = backgroundColor,
    projectPath = projectPath
  )

  # step 2: check standard files for website
  missingDocs <- missingStandardDocs(projectPath = projectPath)

  # Step 3: Make Missing Docs
  makeMissingDocs(missingDocs = missingDocs, projectPath = projectPath)

  # Step 4: Make index
  makeIndexQuarto(projectPath = projectPath)

  # Step 5: Update News
  makeNewsQuarto(projectPath = projectPath)

  # make doc path
  docsPath <- fs::path(projectPath, "documentation/hub")

  # step 6: build study hub
  cli::cat_bullet("Build Study Hub", bullet = "tick", bullet_col = "green")
  quarto::quarto_render(input = docsPath,
                        output_format = "html",
                        as_job = FALSE)

  # preview website
  previewStudyHub(projectPath = projectPath)

  invisible(docsPath)
}

#' Function to import a folder of images for a study hub
#' @param imageFolder the file path for an image folder
#' @param projectPath path to ohdsi study
#' @return invisible return of the image file folder
#' @export
importImages <- function(imageFolder, projectPath = here::here()) {

  newImageFolder <- fs::path(projectPath, "documentation/hub/images") %>%
    fs::dir_create()


  imageFiles <- fs::dir_copy(
    path = imageFolder,
    new_path = newImageFolder
  )

  invisible(imageFiles)
}
