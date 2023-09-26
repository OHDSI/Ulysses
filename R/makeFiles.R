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
makeStudySAP <- function(projectPath = here::here(), open = TRUE) {

  title <- getStudyDetails("StudyTitle", projectPath = projectPath) %>%
    snakecase::to_title_case()

  data <- rlang::list2(
    'Study' = title,
    'Author' = getStudyDetails("StudyLead", projectPath = projectPath),
    'Date' = lubridate::today()
  )

  usethis::use_template(
    template = "StudySAP.qmd",
    save_as = fs::path("documentation", "StudySAP.qmd"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}

#' Function to create a Synopsis file
#' @param org the name of the organization hosting the repo, for example 'ohdsi-studies'.
#' If null defaults to a dummy text
#' @param repo the name of the study repository on github, defaults to the study title
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeHowToRun <- function(org = NULL, repo = NULL,
                         projectPath = here::here(),
                         open = TRUE) {

  if (is.null(repo)) {
    repo <- snakecase::to_snake_case(getStudyDetails("StudyTitle", projectPath = projectPath))
  }

  if (is.null(org)) {
    org <- "[ORG]"
  }

  data <- rlang::list2(
    'Study' = getStudyDetails("StudyTitle", projectPath = projectPath),
    'Url' = glue::glue("https://github.com/{org}/{repo}"),
    'Org' = org,
    'Repo' = repo
  )


  usethis::use_template(
    template = "HowToRun.md",
    save_as = fs::path("documentation", "HowToRun.md"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}

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


#' R Markdown file to make the pass protocol
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makePassProtocol <- function(projectPath = here::here(),
                              open = TRUE) {

  data <- rlang::list2(
    'Study' =  getStudyDetails(item = "StudyTitle", projectPath = projectPath),
    'Author' = getStudyDetails(item = 'StudyLead', projectPath = projectPath),
    'Date' = lubridate::today()
  )

  fileName <- snakecase::to_upper_camel_case(getStudyDetails(item = "StudyTitle", projectPath = projectPath)) %>%
    paste0("Protocol")

  dir_path <- fs::path("documentation", "Protocol") %>%
    fs::dir_create()

  usethis::use_template(
    template = "PassProtocol.Rmd",
    save_as = fs::path(dir_path, fileName, ext = "Rmd"),
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

  data <- rlang::list2(
    'Study' = getStudyDetails("StudyTitle", projectPath = projectPath),
    'Lead' = getStudyDetails('StudyLead', projectPath = projectPath)
  )


  usethis::use_template(
    template = "ContributionGuidelines.md",
    save_as = fs::path("documentation", "ContributionGuidelines.md"),
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

  data <- rlang::list2(
    'Title' = getStudyDetails("StudyTitle", projectPath = projectPath),
    'Author' = getStudyDetails('StudyLead', projectPath = projectPath),
    'Date' = lubridate::today()
  )


  usethis::use_template(
    template = "ResultsReport.qmd",
    save_as = fs::path("documentation", "ResultsReport.qmd"),
    data = data,
    open = open,
    package = "Ulysses")

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
