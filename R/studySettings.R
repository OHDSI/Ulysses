# Meta info ------------

## Authors -----------
setAuthors <- function(name, organization, role) {
  tibble::tibble(
    name = name,
    organization = organization,
    role = role
  )
}

defaultAuthors <- function() {
  setAuthors(
    name = c("Ulysses", "Eurylochus"),
    organization = c("OHDSI", "OHDSI"),
    role = c("Lead", "Developer")
  )
}

## Milestones -----------
setMilestones <- function(status, startDate, endDate) {
  list(
    'Status' = status,
    'StartDate' = startDate,
    'EndDate' = endDate
  )
}

defaultMilestones <- function() {
  setMilestones(
    status = "Started",
    startDate = "01-01-1960",
    endDate = "31-12-2099"
  )
}

## Descriptions -----------
setDescription <- function(studyType, tags) {
  list(
    'StudyType' = studyType,
    'Tags' = as.list(tags)
  )
}

defaultDesc <- function() {
  setDescription(studyType = "Characterization",
                 tags = c("Observational Study", "OMOP", "OHDSI"))
}

## Links -----------
setLinks <- function(forumPostLink,
                     protocolLink,
                     studyHubLink,
                     resultsDashboardLink,
                     reportLink) {
  list(
    'Forum' = forumPostLink,
    'Protocol' = protocolLink,
    'StudyHub' = studyHubLink,
    'ResultsDashboard' = resultsDashboardLink,
    'Report' = reportLink
  )
}

defaultLinks <- function() {
  setLinks(
    forumPostLink = "TBA",
    protocolLink = "TBA",
    studyHubLink = "TBA",
    resultsDashboardLink = "TBA",
    reportLink = "TBA"
  )
}

## Contact -----------
setContact <- function(name, email) {
  list(
    'Name' = name,
    'Email' = email
  )
}
defaultContact <- function() {
  setContact(name = "Ulysses", email = "ulysses@ohdsi.org")
}


## Cdm -----------
setCdmDetails <- function(cdmVersion, vocabVersion, vocabRelease) {
  list(
    'CdmVersion' = cdmVersion,
    'VocabVersion' = vocabVersion,
    'VocabRelease' = vocabRelease
  )
}
defaultCdmDetails <- function() {
  setCdmDetails(
    cdmVersion = "v5.3",
    vocabVersion = "v5.0",
    vocabRelease = "22-06-2022")
}

## Data -----------
# setDataSources <- function(databaseName, location, type, persons, timeFrame) {
#   tibble::tibble(
#     databaseName = databaseName,
#     location = location,
#     type = type,
#     persons = persons,
#     timeFrame
#   )
# }
#
# defaultDataSources <- function() {
#   setDataSources(
#     databaseName = c("CMS Synpuf"),
#     location = c("US"),
#     type = c("Claims"),
#     persons = c("110K"),
#     timeFrame = c("2008-2010")
#   )
# }

#' Function to initialize study settings
#' @param title the title of the study
#' @param authors the author list for the study
#' @param milestones list of milestone information including study status and timeframe for study
#' @param cdm list of info about the cdm
#' @param desc a list of attributes describing the study including the study type and tages
#' @param contact a list of contact information for study
#' @param links a list of links to files used in study
#' @return a list containing study settings
#' @export
makeStudySettings <- function(title,
                              authors = defaultAuthors(),
                              milestones = defaultMilestones(),
                              cdm = defaultCdmDetails(),
                              desc = defaultDesc(),
                              contact = defaultContact(),
                              links = defaultLinks()) {




  studySettings <- list(
    'Title' = snakecase::to_title_case(title),
    'Authors' = authors,
    'Description' = desc,
    'Milestones' = milestones,
    'CDM' = cdm,
    'Links' = links,
    'Contact' = contact
  )

  return(studySettings)

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

#Function to convert yml into a specified list format
retrieveStudySettings <- function(projectPath){
  ymlPath <- fs::path(projectPath, "_study.yml")
  studyYml <- yaml::read_yaml(ymlPath)
  studyYml$Authors <- purrr::map_dfr(studyYml$Authors, ~.x)
  return(studyYml)
}

