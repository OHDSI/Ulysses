# Study Files ----------------------------


#' Function to create a README file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeReadMe <- function(projectPath = here::here(), open = TRUE) {

  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)$study

  #prep author info as single line name(email)
  leadAuthor <- glue::glue("{studyMeta$authors$lead$name} ({studyMeta$authors$lead$email})")
  developerAuthor <- glue::glue("{studyMeta$authors$developer$name} ({studyMeta$authors$developer$email})")

  # prep data sources
  dataSources <- paste(studyMeta$about$`data-sources`, collapse = ", ")

  # prep tags
  tags <- paste(studyMeta$tags, collapse = ", ")

  data <- rlang::list2(
    'Title' = studyMeta$title,
    'ID' = studyMeta$id,
    'Type' = studyMeta$type,
    'Start' = studyMeta$timeline$`start-date`,
    'End' = studyMeta$timeline$`end-date`,
    'Lead' = leadAuthor,
    'Developer' = developerAuthor,
    'Tags' = tags,
    'TA' = studyMeta$about$`therapeutic-area`,
    'Description' = studyMeta$about$description,
    'DS' = dataSources
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
  studyMeta <- retrieveStudySettings(projectPath = projectPath)$study

  data <- rlang::list2(
    'ID' = studyMeta$id,
    'Version' = studyMeta$version
  )

  usethis::use_template(
    template = "NEWS.md",
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}



# Documentation Files -----------------------

replaceTitleColon <- function(title){
    gsub("\\:", "-", title)
}


#' Function to create a SAP
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeAnalysisPlan <- function(projectPath = here::here(), open = TRUE) {


  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)$study

  # make list of vars for template
  data <- rlang::list2(
    'Title' = replaceTitleColon(studyMeta$title),
    'Developer' = studyMeta$authors$developer$name
  )

  #create templated output
  usethis::use_template(
    template = "AnalysisPlan.qmd",
    save_as = fs::path("documentation/hub", "AnalysisPlan.qmd"),
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
  studyMeta <- retrieveStudySettings(projectPath = projectPath)$study

  # make list of vars for template
  data <- rlang::list2(
    'Title' = replaceTitleColon(studyMeta$title)
  )

  usethis::use_template(
    template = "ContributionGuidelines.qmd",
    save_as = fs::path("documentation/hub", "ContributionGuidelines.qmd"),
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
  studyMeta <- retrieveStudySettings(projectPath = projectPath)$study

  # make list of vars for template
  data <- rlang::list2(
    'Title' = replaceTitleColon(studyMeta$title),
    'Developer' = studyMeta$authors$developer$name
  )

  usethis::use_template(
    template = "ResultsReport.qmd",
    save_as = fs::path("documentation/hub", "ResultsReport.qmd"),
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
  studyMeta <- retrieveStudySettings(projectPath = projectPath)$study

  # make list of vars for template
  data <- rlang::list2(
    'Title' = replaceTitleColon(studyMeta$title),
    'Developer' = studyMeta$authors$developer$name
  )


  usethis::use_template(
    template = "HowToRun.qmd",
    save_as = fs::path("documentation/hub", "HowToRun.qmd"),
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
  studyMeta <- retrieveStudySettings(projectPath = projectPath)$study

  # make list of vars for template
  data <- rlang::list2(
    'Title' = replaceTitleColon(studyMeta$title)
  )


  usethis::use_template(
    template = "TechSpecs.qmd",
    save_as = fs::path("documentation/hub", "TechSpecs.qmd"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}

# TODO update this to quarto

# makeOhdsiProtocol <- function(projectPath = here::here(),
#                               open = TRUE) {
#
#   data <- rlang::list2(
#     'Study' =  getStudyDetails(item = "StudyTitle", projectPath = projectPath),
#     'Date' = lubridate::today()
#   )
#
#   fileName <- snakecase::to_upper_camel_case(getStudyDetails(item = "StudyTitle", projectPath = projectPath)) %>%
#     paste0("Protocol")
#
#   dir_path <- fs::path("documentation", "Protocol") %>%
#     fs::dir_create()
#
#   usethis::use_template(
#     template = "OhdsiProtocol.Rmd",
#     save_as = fs::path(dir_path, fileName, ext = "Rmd"),
#     data = data,
#     open = open,
#     package = "Ulysses")
#
#   #get Protocol Components and move to folder
#   fs::path_package("Ulysses", "templates/Protocol-Components") %>%
#     fs::dir_copy(new_path = dir_path, overwrite = TRUE)
#
#
#   invisible(data)
# }




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


# Email asking to initialize an ohdsi-studies repo
#
# senderName your name as the person sending the email
# senderEmail your email address
# githubUserName your github username.
# recipientName the recipients name, defaults to Admin
# recipientEmail the recipients email, defaults to a dummy email
# projectPath the path to the Ulysses project
# open toggle on whether the file should be opened
#
#
# This function works best if you have properly setup a Github PAT. To configure the PAT
# follow the \href{https://gh.r-lib.org/articles/managing-personal-access-tokens.html}{instructions}
# from the gh package.
#


# requestStudyRepository <- function(senderName,
#                              senderEmail,
#                              githubUserName = NULL,
#                              recipientName = NULL,
#                              recipientEmail = NULL,
#                              projectPath = here::here(),
#                              open = TRUE) {
#   #get repo name
#   repoName <- basename(projectPath) %>%
#     snakecase::to_upper_camel_case()
#
#
#   if (is.null(recipientName)) {
#     recipientName <- "Admin"
#   }
#
#   if (is.null(recipientEmail)) {
#     recipientEmail <- "adminEmail@ohdsi.org"
#   }
#
#
#   if (is.null(githubUserName)) {
#     githubUser <- getGithubUser()
#   }
#
#   data <- rlang::list2(
#     'RepoName' = repoName,
#     'GithubUser' = githubUser,
#     'SenderName' = senderName,
#     'SenderEmail' = senderEmail,
#     'RecipientName' = recipientName,
#     'RecipientEmail' = recipientEmail
#   )
#
#   usethis::use_template(
#     template = "RequestRepositoryEmail.R",
#     save_as = fs::path("extras", "RequestRepositoryEmail.R"),
#     data = data,
#     open = open,
#     package = "Ulysses")
#
#
#   usethis::use_git_ignore(ignores = "extras/RequestRepositoryEmail.R")
#
#   invisible(data)
#
# }



# makeMeetingMinutes <- function(projectPath = here::here(), open = TRUE) {
#
#   data <- rlang::list2(
#     'Study' = getStudyDetails("StudyTitle", projectPath = projectPath),
#     'Author' = getStudyDetails("StudyLead", projectPath = projectPath),
#     'Date' = lubridate::today()
#   )
#
#   saveName <- glue::glue("minutes_{lubridate::today()}") %>%
#     snakecase::to_snake_case()
#
#   usethis::use_template(
#     template = "MeetingMinutes.qmd",
#     save_as = fs::path("extras/minutes", saveName, ext = "qmd"),
#     data = data,
#     open = open,
#     package = "Ulysses")
#
#   invisible(data)
#
# }

# Analysis Files ---------------------


#' Function to create a pipeline task as an R file
#' @param scriptName The name of the capr file that is being created
#' @param configBlock the name of the config block to use for the script
#' @param date the date the script was built, default to today's date
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeCaprScript <- function(scriptName,
                           configBlock = NULL,
                           date = lubridate::today(),
                           projectPath = here::here(),
                           open = TRUE) {

  intFileName <- paste0("Capr_", scriptName)

  if (is.null(configBlock)) {
    configBlock <- "[Add config block]"
  }

  # retrieve study meta
  studyMeta <- Ulysses:::retrieveStudySettings(projectPath = projectPath)$study


  data <- rlang::list2(
    'Title' = replaceTitleColon(studyMeta$title),
    'Author' = studyMeta$authors$developer$name,
    'Date' = date,
    'FileName' = intFileName
  )



  usethis::use_template(
    template = "Capr.R",
    save_as = fs::path("analysis/src", intFileName, ext = "R"),
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
#' @param date the date the script was built, default to today's date
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeAnalysisScript <- function(scriptName,
                               configBlock = NULL,
                               date = lubridate::today(),
                               projectPath = here::here(),
                               open = TRUE) {


  if (is.null(configBlock)) {
    configBlock <- "[Add config block]"
  }


  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)$study


  data <- rlang::list2(
    'Title' = replaceTitleColon(studyMeta$title),
    'Author' = studyMeta$authors$developer$name,
    'Date' = date,
    'FileName' = scriptName,
    'Block' = configBlock
  )

  usethis::use_template(
    template = "AnalysisScript.R",
    save_as = fs::path("analysis/tasks", scriptName, ext = "R"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)


}


#' Function to create a pipeline task as an R file
#' @param internalsName The name of the internals file that is being created
#' @param date the date the script was built, default to today's date
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeInternals <- function(internalsName,
                          date = lubridate::today(),
                          projectPath = here::here(),
                          open = TRUE) {

  intFileName <- paste0("_", internalsName)

  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)$study


  data <- rlang::list2(
    'Author' = studyMeta$authors$developer$name,
    'Date' = date,
    'FileName' = intFileName
  )


  usethis::use_template(
    template = "Internals.R",
    save_as = fs::path("analysis/src", intFileName, ext = "R"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}
