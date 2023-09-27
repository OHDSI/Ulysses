# check if yml exists
checkWebsiteYml <- function(projectPath = here::here()) {

  #create yml path
  ymlPath <- fs::path(projectPath, "documentation/_quarto.yml")
  check <- fs::file_exists(ymlPath)
  if (check) {
    cli::cat_bullet("_quarto.yml already exists for study hub", bullet = "pointer", bullet_col = "yellow")
  }

  return(check)
}


# Function to create quarto yml
makeWebsiteYaml <- function(projectPath = here::here()) {

  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)

  # make list of vars for template
  data <- rlang::list2(
    'Study' = studyMeta$Title
  )

  check <- checkWebsiteYml(projectPath = projectPath)
  if (!check) {
    #build template
    usethis::use_template(
      template = "quartoWebsite.yml",
      save_as = fs::path("documentation", "_quarto.yml"),
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
  docPath <- fs::path(projectPath, "documentation/index.qmd")

  #write new readme to index.qmd
  cli::cat_bullet("Convert README.md to index.qmd", bullet = "tick", bullet_col = "green")
  readr::write_lines(newReadMe, file = docPath)

  #ignore index.qmd as it is redundant to README.md
  usethis::use_git_ignore(ignores = "documentation/index.qmd")
  invisible(docPath)
}

#funciton to convert news to news,qmd
makeNewsQuarto <- function(projectPath = here::here()) {

  # find readme
  readMePath <- fs::path(projectPath, "NEWS.md")
  #read lines of readme
  lines <- readr::read_lines(readMePath)

  #set path to documentation
  docPath <- fs::path(projectPath, "documentation/news.qmd")

  #write new readme to index.qmd
  cli::cat_bullet("Convert NEWS to news.qmd", bullet = "tick", bullet_col = "green")
  readr::write_lines(lines, file = docPath)

  #ignore index.qmd as it is redundant to README.md
  usethis::use_git_ignore(ignores = "documentation/news.qmd")
  invisible(docPath)
}

#check which docs are missing so we can generate a template
missingStandardDocs <- function(projectPath = here::here()) {

  # make path to documentation folder
  docsPath <- fs::path(projectPath, "documentation")

  # look up all qmd files
  resourceFiles <- fs::dir_ls(docsPath, glob = "*.qmd") %>%
    basename() %>%
    tools::file_path_sans_ext()

  expectedFiles <- c("AnalysisPlan", "ContributionGuidelines", "HowToRun",
                     "ResultsReport", "TechSpecs")

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
  indexFilePath <- fs::path(projectPath, "documentation/_site/index.html")
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
#' @return builds a _site folder in documenation that holds the html website
#' @export
buildStudyHub <- function(projectPath = here::here()) {

  # Step 1: Make yml
  makeWebsiteYaml(projectPath = projectPath)

  # step 2: check standard files for website
  missingDocs <- missingStandardDocs(projectPath = projectPath)

  # Step 3: Make Missing Docs
  makeMissingDocs(missingDocs = missingDocs, projectPath = projectPath)

  # Step 4: Make index
  makeIndexQuarto(projectPath = projectPath)

  # Step 5: Update News
  makeNewsQuarto(projectPath = projectPath)

  # make doc path
  docsPath <- fs::path(projectPath, "documentation")

  # step 6: build study hub
  cli::cat_bullet("Build Study Hub", bullet = "tick", bullet_col = "green")
  quarto::quarto_render(input = docsPath,
                        output_format = "html",
                        as_job = FALSE)

  # preview website
  previewStudyHub(projectPath = projectPath)

  invisible(docsPath)
}