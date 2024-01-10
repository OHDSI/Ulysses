# Study Files ----------------------------


#' Function to create a README file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeReadMe <- function(projectPath = here::here(), open = TRUE) {

  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)

  #create template vars
  data <- rlang::list2(
    'Project' = studyMeta$Title,
    'StudyType' = studyMeta$Description$StudyType,
    'Contact' = studyMeta$Contact$Name,
    'ContactEmail' = studyMeta$Contact$Email,
    'CdmVersion' = studyMeta$CDM$CdmVersion,
    'VocabVersion' = studyMeta$CDM$VocabVersion,
    'VocabRelease' = studyMeta$CDM$VocabRelease,
    'StudyStatus' = studyMeta$Milestones$Status,
    'ForumPost' = studyMeta$Links$Forum,
    'Protocol' = studyMeta$Links$Protocol,
    'Hub' = studyMeta$Links$StudyHub,
    'Dashboard' = studyMeta$Links$ResultsDashboard,
    'Report' = studyMeta$Links$Report
  )

  #load template with vars
  usethis::use_template(
    template = "README.md",
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}

#' Function to create a NEWS file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeNews <- function(projectPath = here::here(), open = TRUE) {

  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)

  data <- rlang::list2(
    'Project' = studyMeta$Title
  )

  usethis::use_template(
    template = "NEWS.md",
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}

#' Function to create a config.yml file
#' @param block the name of the config block
#' @param database the name of the database for the block, default to block name
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeConfig <- function(block, database = block, projectPath = here::here(), open = TRUE) {

  projFile <- list.files(projectPath, pattern = ".Rproj", full.names = TRUE)
  projName <- basename(tools::file_path_sans_ext(projFile))

  data <- rlang::list2(
    'Project' = projName,
    'Cohort' = paste(projName, database, sep = "_"),
    'Block' = block,
    'Database' = database
  )

  usethis::use_template(
    template = "config.yml",
    data = data,
    open = open,
    package = "Ulysses")

  usethis::use_git_ignore(ignores = "config.yml")

  invisible(data)
}




# Documentation Files -----------------------

#' Function to create a SAP
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeAnalysisPlan <- function(projectPath = here::here(), open = TRUE) {


  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)

  # make list of vars for template
  data <- rlang::list2(
    'Study' = studyMeta$Title
  )

  #create templated output
  usethis::use_template(
    template = "AnalysisPlan.qmd",
    save_as = fs::path("documentation", "AnalysisPlan.qmd"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}


#' R Markdown file to make the contribution guidelines
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeContributionGuidelines <- function(projectPath = here::here(),
                                       open = TRUE) {

  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)

  # make list of vars for template
  data <- rlang::list2(
    'Study' = studyMeta$Title
  )

  usethis::use_template(
    template = "ContributionGuidelines.qmd",
    save_as = fs::path("documentation", "ContributionGuidelines.qmd"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}

#' Quarto file to make a results report
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeResultsReport <- function(projectPath = here::here(),
                              open = TRUE) {

  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)

  # make list of vars for template
  data <- rlang::list2(
    'Study' = studyMeta$Title
  )

  usethis::use_template(
    template = "ResultsReport.qmd",
    save_as = fs::path("documentation", "ResultsReport.qmd"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}



#' Function to create a HowToRun file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeHowToRun <- function(projectPath = here::here(),
                         open = TRUE) {

  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)

  # make list of vars for template
  data <- rlang::list2(
    'Study' = studyMeta$Title
  )


  usethis::use_template(
    template = "HowToRun.qmd",
    save_as = fs::path("documentation", "HowToRun.qmd"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}

#' Function to create a HowToRun file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeTechSpecs <- function(
    projectPath = here::here(),
    open = TRUE) {

  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)

  # make list of vars for template
  data <- rlang::list2(
    'Study' = studyMeta$Title
  )


  usethis::use_template(
    template = "TechSpecs.qmd",
    save_as = fs::path("documentation", "TechSpecs.qmd"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}

# TODO update this to quarto
#' R Markdown file to make the ohdsi protocol
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeOhdsiProtocol <- function(projectPath = here::here(),
                              open = TRUE) {

  data <- rlang::list2(
    'Study' =  getStudyDetails(item = "StudyTitle", projectPath = projectPath),
    'Date' = lubridate::today()
  )

  fileName <- snakecase::to_upper_camel_case(getStudyDetails(item = "StudyTitle", projectPath = projectPath)) %>%
    paste0("Protocol")

  dir_path <- fs::path("documentation", "Protocol") %>%
    fs::dir_create()

  usethis::use_template(
    template = "OhdsiProtocol.Rmd",
    save_as = fs::path(dir_path, fileName, ext = "Rmd"),
    data = data,
    open = open,
    package = "Ulysses")

  #get Protocol Components and move to folder
  fs::path_package("Ulysses", "templates/Protocol-Components") %>%
    fs::dir_copy(new_path = dir_path, overwrite = TRUE)


  invisible(data)
}




# Extra Files --------------------------------


#' Function to create a config.yml file
#' @param database the name of the database for the project, can be NULL
#' @param configBlock the name of the configBlock to use, can be NULL
#' @param secret a keyword to use as the keyring password to access credentials, if NULL password is ulysses
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeKeyringSetup <- function(database = NULL,
                             configBlock = NULL,
                             secret = NULL,
                             projectPath = here::here(),
                             open = TRUE) {

  keyringName <- basename(projectPath)

  if (is.null(database)) {
    database <- "[Add database]"
  }

  if (is.null(configBlock)) {
    configBlock <- "[Add config block]"
  }

  if (is.null(secret)) {
    keyringPassword <- "ohdsi"
  }

  data <- rlang::list2(
    'Block' = configBlock,
    'Database' = database,
    'Study' = keyringName,
    'Secret'= keyringPassword
  )

  usethis::use_template(
    template = "KeyringSetup.R",
    save_as = fs::path("extras", "KeyringSetup.R"),
    data = data,
    open = open,
    package = "Ulysses")


  invisible(data)


}


#' Email asking to initialize an ohdsi-studies repo
#'
#' @param senderName your name as the person sending the email
#' @param senderEmail your email address
#' @param githubUserName your github username.
#' @param recipientName the recipients name, defaults to Admin
#' @param recipientEmail the recipients email, defaults to a dummy email
#' @param projectPath the path to the Ulysses project
#' @param open toggle on whether the file should be opened
#'
#' @details
#' This function works best if you have properly setup a Github PAT. To configure the PAT
#' follow the \href{https://gh.r-lib.org/articles/managing-personal-access-tokens.html}{instructions}
#' from the gh package.
#'
#'
#' @export
requestStudyRepository <- function(senderName,
                             senderEmail,
                             githubUserName = NULL,
                             recipientName = NULL,
                             recipientEmail = NULL,
                             projectPath = here::here(),
                             open = TRUE) {
  #get repo name
  repoName <- basename(projectPath) %>%
    snakecase::to_upper_camel_case()


  if (is.null(recipientName)) {
    recipientName <- "Admin"
  }

  if (is.null(recipientEmail)) {
    recipientEmail <- "adminEmail@ohdsi.org"
  }


  if (is.null(githubUserName)) {
    githubUser <- getGithubUser()
  }

  data <- rlang::list2(
    'RepoName' = repoName,
    'GithubUser' = githubUser,
    'SenderName' = senderName,
    'SenderEmail' = senderEmail,
    'RecipientName' = recipientName,
    'RecipientEmail' = recipientEmail
  )

  usethis::use_template(
    template = "RequestRepositoryEmail.R",
    save_as = fs::path("extras", "RequestRepositoryEmail.R"),
    data = data,
    open = open,
    package = "Ulysses")


  usethis::use_git_ignore(ignores = "extras/RequestRepositoryEmail.R")

  invisible(data)

}


#' Function to create a meeting minutes file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeMeetingMinutes <- function(projectPath = here::here(), open = TRUE) {

  data <- rlang::list2(
    'Study' = getStudyDetails("StudyTitle", projectPath = projectPath),
    'Author' = getStudyDetails("StudyLead", projectPath = projectPath),
    'Date' = lubridate::today()
  )

  saveName <- glue::glue("minutes_{lubridate::today()}") %>%
    snakecase::to_snake_case()

  usethis::use_template(
    template = "MeetingMinutes.qmd",
    save_as = fs::path("extras/minutes", saveName, ext = "qmd"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}

# Analysis Files ---------------------


#' Function to create a pipeline task as an R file
#' @param scriptName The name of the capr file that is being created
#' @param configBlock the name of the config block to use for the script
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeCaprScript <- function(scriptName,
                           configBlock = NULL,
                           author = NULL,
                           date = lubridate::today(),
                           projectPath = here::here(),
                           open = TRUE) {

  intFileName <- paste0("Capr_", scriptName)

  if (is.null(configBlock)) {
    configBlock <- "[Add config block]"
  }

  # retrieve study meta
  studyMeta <- Ulysses:::retrieveStudySettings(projectPath = projectPath)

  # specify author
  if (is.null(author)) {
    authorName <- studyMeta$Authors %>%
      dplyr::slice(1) %>%
      dplyr::pull(name)
  } else{
    authorName <- author
  }

  data <- rlang::list2(
    'Study' = studyMeta$Title,
    'Author' = authorName,
    'Date' = date,
    'FileName' = intFileName
  )



  usethis::use_template(
    template = "Capr.R",
    save_as = fs::path("analysis/misc/R", intFileName, ext = "R"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}
#' Function to create a pipeline task as an R file
#' @param keyringName The name of the keyring for the study, if null defaults to dir basename
#' @param keyringPassword The password for the keyring, if null defauls to ohdsi
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeWebApiScript <- function(keyringName = NULL,
                             keyringPassword = NULL,
                             projectPath = here::here(),
                             open = TRUE) {


  fileName <- "ImportCohortsFromWebApi"

  if (is.null(keyringName)) {
    keyringName <- basename(projectPath)
  }

  if (is.null(keyringPassword)) {
    keyringPassword <- "ohdsi"
  }


  data <- rlang::list2(
    'Study' = keyringName,
    'Secret'= keyringPassword,
    'FileName' = fileName
  )


  usethis::use_template(
    template = "WebApi.R",
    save_as = fs::path("extras/ImportCohortsFromWebApi.R"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}


#' Function to create a pipeline task as a Rmd file
#' @param scriptName The name of the analysis script
#' @param configBlock the name of the config block to use for the script
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeAnalysisScript <- function(scriptName,
                               configBlock = NULL,
                               author = NULL,
                               date = lubridate::today(),
                               projectPath = here::here(),
                               open = TRUE) {


  if (is.null(configBlock)) {
    configBlock <- "[Add config block]"
  }


  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)

  # specify author
  if (is.null(author)) {
    authorName <- studyMeta$Authors %>%
      dplyr::slice(1) %>%
      dplyr::pull(name)
  } else{
    authorName <- author
  }

  data <- rlang::list2(
    'Study' = studyMeta$Title,
    'Author' = authorName,
    'Date' = date,
    'FileName' = scriptName,
    'Block' = configBlock
  )

  usethis::use_template(
    template = "AnalysisScript.R",
    save_as = fs::path("analysis/misc/tasks", scriptName, ext = "R"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)


}


#' Function to create a pipeline task as an R file
#' @param internalsName The name of the internals file that is being created
#' @param projectPath the path to the project
#' @param author the author of the R file, defaults to first author in _study.yml
#' @param date the date the file was initialized, defaults to today
#' @param open toggle on whether the file should be opened
#' @export
makeInternals <- function(internalsName,
                          projectPath = here::here(),
                          author = NULL,
                          date = lubridate::today(),
                          open = TRUE) {

  intFileName <- paste0("_", internalsName)

  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)

  # specify author
  if (is.null(author)) {
    authorName <- studyMeta$Authors %>%
      dplyr::slice(1) %>%
      dplyr::pull(name)
  } else{
    authorName <- author
  }

  data <- rlang::list2(
    'Study' = studyMeta$Title,
    'Author' = authorName,
    'Date' = date,
    'FileName' = intFileName
  )


  usethis::use_template(
    template = "Internals.R",
    save_as = fs::path("analysis/misc/R", intFileName, ext = "R"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}
