# Meta info ------------

#' Function to set study info
#' @param id specify the study id
#' @param title specify the study title
#' @param type specify the type of study
#' @param version specify the study version
#' @return a list containing study info
#' @export
setStudyInfo <- function(id,
                         title = id,
                         type = c("Characterization"),
                         version = "0.0.0.999") {
  ll <- list(
    id = id,
    title = title,
    type = type,
    version = version
  )

  return(ll)

}

#' Function to set study authors
#' @param developer specify the study developer, default to system variable
#' @param developerEmail the email of the developer
#' @param lead specify the lead of the study
#' @param leadEmail specify the lead of the study email
#' @return a list containing study authors
#' @export
setStudyAuthors <- function(developer = Sys.getenv("USERNAME"),
                            developerEmail = glue::glue("{developer}@ohdsi.org"),
                            lead = "Ulysses",
                            leadEmail = "Ulysses@ohdsi.org") {

  ll <- list(
    'authors' = list(
      'developer' = list(
        'name' = developer,
        'email' = developerEmail
      ),
      'lead' = list(
        'name' = lead,
        'email' = leadEmail
      )
    )
  )

  return(ll)

}

#' Function to set study timeline
#' @param status the study status, default is started
#' @param startDate the start of the study, defaults to todays date
#' @param endDate the date specifying the end (or expected end) of the study
#' @return a list containing study timeline
#' @export
setStudyTimeline <- function(status = "Started",
                            startDate = as.character(lubridate::today()),
                            endDate = as.character(lubridate::today() + (365 * 2))) {

  ll <- list(
    'timeline' = list(
      'status' = status,
      'start-date' = startDate,
      'end-date' = endDate
    )
  )

  return(ll)

}

#' Function to set study description
#' @param desc a brief description of the study
#' @param ta a character string of the therapeutic area
#' @param dataSources a list of data sources
#' @return a list containing study description info
#' @export
setStudyDescription <- function(desc = "Provide a 1 to 2 sentence description of your study. Be concise.",
                                ta = "Specify the therapeutic area of the study",
                                dataSources = list("Truven MarketScan", "Optum Market Clarity")) {

  ll <- list(
    'about' = list(
      'description' = desc,
      'therapeutic-area' = ta,
      'data-sources' = dataSources
    )
  )

  return(ll)

}

#' Function to set study links
#' @param ... a series of resource links for the study
#' @return a list containing links to resources
#' @export
setStudyLinks <- function(...) {

  ll <- list(
    'links' = rlang::list2(...)
  )

  return(ll)

}

#' Function to set study tags
#' @param ... a series of tags for the study
#' @return a list containing study tags
#' @export
setStudyTags <- function(...) {

  ll <- list(
    'tags' = rlang::list2(...)
  )

  return(ll)

}


convert_to_yml <- function(studySettings, savePath) {

  #create file path for yml
  filePath <- fs::path(savePath, "_study", ext = "yml")

  cli::cat_bullet(
    "Initialize _study.yml",
    bullet = "tick", bullet_col = "green"
  )

  #write _study.yml file
  yaml::write_yaml(
    x = studySettings,
    file = filePath,
    column.major = FALSE)

  invisible(filePath)

}

#' Function to convert yml into a specified list format
#' @param projectPath the path to the project
#' @return the study yml as an R object (list)
#' @export
retrieveStudySettings <- function(projectPath){
  ymlPath <- fs::path(projectPath, "_study.yml")
  studyYml <- yaml::read_yaml(ymlPath)
  return(studyYml)
}

getStudySettings <- function(projectPath, field) {

  studyYml <- retrieveStudySettings(projectPath = projectPath)
  studyElement <- studyYml$study[[field]]

  return(studyElement)
}

pushStudyUpdate <- function(newStudyYml, projectPath, update, value) {

  cli::cat_bullet(
    "Updated _study.yml",
    bullet = "tick", bullet_col = "green"
  )

  cli::cat_bullet(
    glue::glue("Changed {update} to {crayon::green(value)}"),
    bullet = "info", bullet_col = "blue"
  )

  #write _study.yml file
  ymlPath <- fs::path(projectPath, "_study", ext = "yml")
  yaml::write_yaml(
    x = newStudyYml,
    file = ymlPath,
    column.major = FALSE)

  invisible(ymlPath)

}

pushStudyAdd <- function(newStudyYml, projectPath, update, value) {

  cli::cat_bullet(
    "Updated _study.yml",
    bullet = "tick", bullet_col = "green"
  )

  cli::cat_bullet(
    glue::glue("Added {update}: {crayon::green(value)}"),
    bullet = "info", bullet_col = "blue"
  )

  #write _study.yml file
  ymlPath <- fs::path(projectPath, "_study", ext = "yml")
  yaml::write_yaml(
    x = newStudyYml,
    file = ymlPath,
    column.major = FALSE)

  invisible(ymlPath)

}

# Update Study settings functions ------------------------

#' Function to update study end date
#' @param newEndDate the new end date of the study, suggest format YYYY-MM-DD
#' @param projectPath the path to the project
#' @return invisible studyYml, prints specifying what changes happened
#' @export
updateStudyEndDate <- function(newEndDate,
                               projectPath = here::here()){

  studyYml <- retrieveStudySettings(projectPath = projectPath)
  studyYml$study$timeline$`end-date` <- as.character(newEndDate)

  pushStudyUpdate(studyYml, projectPath, update = "Study End Date", value = newEndDate)

  invisible(studyYml)

}

#' Function to update study status
#' @param newStudyStatus the new status of the study, accepts Started, In-Progress, Stopped, Completed
#' @param projectPath the path to the project
#' @return invisible studyYml, prints specifying what changes happened
#' @export
updateStudyStatus <- function(newStudyStatus = c("Started", "In-Progress", "Stopped", "Completed"),
                              projectPath = here::here()) {

  studyYml <- retrieveStudySettings(projectPath = projectPath)
  studyYml$study$timeline$status <- newStudyStatus

  pushStudyUpdate(studyYml, projectPath, update = "Study Status", value = newStudyStatus)

  invisible(studyYml)

}

#' Function to update study title
#' @param newStudyTitle the new study title
#' @param projectPath the path to the project
#' @return invisible studyYml, prints specifying what changes happened
#' @export
updateStudyTitle <- function(newStudyTitle,
                             projectPath = here::here()) {

  studyYml <- retrieveStudySettings(projectPath = projectPath)
  studyYml$study$title <- newStudyTitle

  pushStudyUpdate(studyYml, projectPath, update = "Study Title", value = newStudyTitle)

  invisible(studyYml)

}

#' Function to update study version
#' @param newStudyVersion the new study version
#' @param projectPath the path to the project
#' @return invisible studyYml, prints specifying what changes happened
#' @export
updateStudyVersion <- function(newStudyVersion,
                             projectPath = here::here()) {

  studyYml <- retrieveStudySettings(projectPath = projectPath)
  studyYml$study$version <- newStudyVersion

  pushStudyUpdate(studyYml, projectPath, update = "Study Version", value = newStudyVersion)

  #TODO add cascade to add line to NEWS

  invisible(studyYml)

}

#' Function to update study description
#' @param newStudyVersion the new study description
#' @param projectPath the path to the project
#' @return invisible studyYml, prints specifying what changes happened
#' @export
updateStudyDescription <- function(newStudyDescription,
                                   projectPath = here::here()) {

  studyYml <- retrieveStudySettings(projectPath = projectPath)
  studyYml$study$about$description <- newStudyDescription

  pushStudyUpdate(studyYml, projectPath, update = "Study Description", value = newStudyDescription)

  invisible(studyYml)

}


#' Function to update study therapeutic area
#' @param newTA the new study therapeutic area
#' @param projectPath the path to the project
#' @return invisible studyYml, prints specifying what changes happened
#' @export
updateTherapeuticArea <- function(newTA,
                                   projectPath = here::here()) {

  studyYml <- retrieveStudySettings(projectPath = projectPath)
  studyYml$study$about$`therapeutic-area` <- newTA

  pushStudyUpdate(studyYml, projectPath, update = "Therapeutic Area", value = newTA)

  invisible(studyYml)

}


#' Function to update developer Infor
#' @param newName change the developer name
#' @param newEmail change the developer email
#' @return invisible studyYml, prints specifying what changes happened
#' @export
updateDeveloperInfo <- function(newName,
                                newEmail,
                                projectPath = here::here()) {

  studyYml <- retrieveStudySettings(projectPath = projectPath)
  studyYml$study$authors$developer$name <- newName
  studyYml$study$authors$developer$email <- newEmail


  memberText <- glue::glue("name: {newName}, email: {newEmail}") %>%
    paste(collapse = ", ")

  pushStudyUpdate(studyYml, projectPath, update = "Developer", value = memberText)

  invisible(studyYml)

}


#' Function to update lead Infor
#' @param newName change the lead name
#' @param newEmail change the lead email
#' @return invisible studyYml, prints specifying what changes happened
#' @export
updateLeadInfo <- function(newName,
                           newEmail,
                           projectPath = here::here()) {

  studyYml <- retrieveStudySettings(projectPath = projectPath)
  studyYml$study$authors$lead$name <- newName
  studyYml$study$authors$lead$email <- newEmail


  memberText <- glue::glue("name: {newName}, email: {newEmail}") %>%
    paste(collapse = ", ")

  pushStudyUpdate(studyYml, projectPath, update = "Lead", value = memberText)

  invisible(studyYml)

}


# Add to study settings Options -----------------------
#' Function to add a study member
#' @param name the name of the study member
#' @param email the study member's email
#' @param projectPath the path to the project
#' @return invisible studyYml, prints specifying what changes happened
#' @export
addStudyMember <- function(name, email = NULL,
                           projectPath = here::here()) {


  if (is.null(email)) {
    email <- "author@email.com"
  }


  studyMember <- list(
    'member' = list(
      name = name,
      email = email
    )
  )


  studyYml <- retrieveStudySettings(projectPath = projectPath)
  studyYml$study$authors <- append(studyYml$study$authors, studyMember)

  memberText <- glue::glue("{names(studyMember[[1]])}: {studyMember[[1]]}") %>%
    paste(collapse = ", ")

  pushStudyAdd(studyYml, projectPath, update = "Study Author", value = memberText)
  invisible(studyYml)
}

#' Function to add study tags
#' @param ... a list of study tags to add
#' @param projectPath the path to the project
#' @return invisible studyYml, prints specifying what changes happened
#' @export
addTags <- function(..., projectPath = here::here()) {

  tags <- list(...)
  studyYml <- retrieveStudySettings(projectPath = projectPath)
  studyYml$study$tags <- append(studyYml$study$tags, tags)

  tagsText <- paste(tags, collapse = ", ")

  pushStudyAdd(studyYml, projectPath, update = "Study Tags", value = tagsText)
  invisible(studyYml)
}

#' Function to add study data sources
#' @param ... a list of data sources to add
#' @param projectPath the path to the project
#' @return invisible studyYml, prints specifying what changes happened
#' @export
addDataSources <- function(..., projectPath = here::here()) {

  dataSources <- list(...)
  studyYml <- retrieveStudySettings(projectPath = projectPath)
  studyYml$study$about$`data-sources` <- append(studyYml$study$about$`data-sources`, dataSources)

  sdText <- paste(dataSources, collapse = ", ")

  pushStudyAdd(studyYml, projectPath, update = "Data Sources", value = sdText)
  invisible(studyYml)
}

#' Function to add study links
#' @param ... a list of links to add
#' @param projectPath the path to the project
#' @return invisible studyYml, prints specifying what changes happened
#' @export
addLinks <- function(..., projectPath = here::here()) {

  links <- rlang::list2(...)
  studyYml <- retrieveStudySettings(projectPath = projectPath)
  studyYml$study$links <- append(studyYml$study$links, links)

  linkText <- glue::glue("{names(links)}: {links}") %>%
    paste(collapse = ", ")

  pushStudyAdd(studyYml, projectPath, update = "Links", value = linkText)
  invisible(studyYml)
}
