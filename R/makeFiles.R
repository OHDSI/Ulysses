# Study Files ----------------------------


#' Function to create a README file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeReadMe <- function(projectPath = here::here(), open = TRUE) {


  data <- rlang::list2(
    'Project' = getStudyDetails("StudyTitle", projectPath = projectPath),
    'Author' = getStudyDetails("StudyLead", projectPath = projectPath),
    'StudyType' = getStudyDetails("StudyType", projectPath = projectPath),
    'StartDate' = getStudyDetails("StudyStartDate", projectPath = projectPath),
    'EndDate' = getStudyDetails("StudyEndDate", projectPath = projectPath),
    'StudyTags' = getStudyDetails("StudyTags", projectPath = projectPath),
    'Protocol' = getStudyDetails("LinksProtocol", projectPath = projectPath),
    'Publications' = getStudyDetails("LinksPublication", projectPath = projectPath),
    'Dashboard' = getStudyDetails("LinksDashboard", projectPath = projectPath)
  )

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

  data <- rlang::list2(
    'Project' = getStudyDetails("StudyTitle", projectPath = projectPath)
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

  projName <- basename(projectPath)

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


# Cohort Files --------------------------

#' Function to create a cohort details file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeCohortDetails <- function(projectPath = here::here(), open = TRUE) {


  data <- rlang::list2(
    'Study' = getStudyDetails("StudyTitle", projectPath = projectPath),
    'Author' = getStudyDetails("StudyLead", projectPath = projectPath)
  )

  usethis::use_template(
    template = "CohortDetails.md",
    save_as = fs::path("cohortsToCreate", "CohortDetails.md"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}


# Analysis Files --------------------------
#' Function to create an example OHDSI script
#' @param fileName The name of for the file
#' @param savePath the path to save the file, defaults to active directory
#' @param open toggle on whether the file should be opened
#' @export
makeExample <- function(fileName, savePath = here::here(), open = TRUE) {


  data <- rlang::list2(
    Example = snakecase::to_sentence_case(fileName),
    Date = lubridate::today(),
    FileName = paste(fileName, "ex", sep = "_")
  )


  template_contents <- render_template("ExampleScript.R",
                                                 data = data,
                                                 package = "Ulysses")

  save_as <- fs::path(savePath, fileName, ext = "R")
  new <- write_utf8(save_as, template_contents)
  rstudioapi::navigateToFile(save_as)
  invisible(new)

}

#' Function to create a pipeline task as a Rmd file
#' @param scriptName The name of the analysis script
#' @param configBlock the name of the config block to use for the script
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeAnalysisScript <- function(scriptName,
                               configBlock = NULL,
                               projectPath = here::here(),
                               open = TRUE) {


  taskNum <- findStepNumber(dir = "analysis/studyTasks")
  step <- scales::label_number(prefix = "0")(taskNum)
  scriptFileName <- paste(step, scriptName, sep = "_")

  if (is.null(configBlock)) {
    configBlock <- "[Add config block]"
  }

  data <- rlang::list2(
    'Study' = getStudyDetails("StudyTitle", projectPath = projectPath),
    'Name' = snakecase::to_title_case(scriptName),
    'Author' = getStudyDetails("StudyLead", projectPath = projectPath),
    'FileName' = scriptFileName,
    'Block' = configBlock
  )

  usethis::use_template(
    template = "AnalysisScript.R",
    save_as = fs::path("analysis/studyTasks", scriptFileName, ext = "R"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)


}

#
# makeRmdTask <- function(taskName, step = NULL, projectPath = here::here(), author = NULL, open = TRUE) {
#
#   if (is.null(step)) {
#     taskFileName <- taskName
#   } else{
#     step <- scales::label_number(prefix = "0")(step)
#     taskFileName <- paste(step, taskName, sep = "_")
#   }
#
#   if (is.null(author)) {
#     author <- "[Add Name of Author]"
#   }
#
#   data <- rlang::list2(
#     'Task' = snakecase::to_title_case(taskName),
#     'Author' = author,
#     'Date' = lubridate::today(),
#     'FileName' = taskFileName
#   )
#
#
#   usethis::use_template(
#     template = "PipelineTask.Rmd",
#     save_as = fs::path("analysis", taskFileName, ext = "Rmd"),
#     data = data,
#     open = open,
#     package = "Ulysses")
#
#   invisible(data)
# }


#' Function to create a pipeline task as an R file
#' @param internalsName The name of the internals file that is being created
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeInternals <- function(internalsName, projectPath = here::here(), open = TRUE) {

  intFileName <- paste0("_", internalsName)
  intPath <- fs::path(projectPath, "analysis/private")


  data <- rlang::list2(
    'Task' = snakecase::to_title_case(internalsName),
    'Author' = getStudyDetails("StudyLead", projectPath = projectPath),
    'Date' = lubridate::today(),
    'FileName' = intFileName
  )


  usethis::use_template(
    template = "Internals.R",
    save_as = fs::path("analysis/private", intFileName, ext = "R"),
    data = data,
    open = open,
    package = "Ulysses")

  invisible(data)

}


#' Function to create a pipeline task as an R file
#' @param scriptName The name of the capr file that is being created
#' @param configBlock the name of the config block to use for the script
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeCaprScript <- function(scriptName,
                           configBlock = NULL,
                           projectPath = here::here(),
                           open = TRUE) {

  intFileName <- paste0("Capr_", scriptName)
  intPath <- fs::path(projectPath, "scratch/Capr") %>%
    fs::dir_create()

  if (is.null(configBlock)) {
    configBlock <- "[Add config block]"
  }

  data <- rlang::list2(
    'Study' = getStudyDetails("StudyTitle", projectPath = projectPath),
    'Name' = scriptName,
    'Author' = getStudyDetails("StudyLead", projectPath = projectPath),
    'FileName' = intFileName
  )


  usethis::use_template(
    template = "Capr.R",
    save_as = fs::path("scratch/Capr", intFileName, ext = "R"),
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

  if (is.null(keyringPasword)) {
    keyringPassword <- "ohdsi"
  }


  data <- rlang::list2(
    'Study' = keyringName,
    'Secret'= keyringPassword,
    'FileName' = fileName
  )


  usethis::use_template(
    template = "WebApi.R",
    save_as = fs::path("scratch/ImportCohortsFromWebApi.R"),
    data = data,
    open = open,
    package = "Ulysses")

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
