#' @title Set Ulysses Contributor
#' @param name the name of the contributor as a character string
#' @param email the email of the contributor as a character string
#' @param role the role of the contirbutor as a character string
#' @returns A ContributorLine R6 class with the contributor info
#' @export
setContributor <- function(name, email, role) {
  contributorLine <- ContributorLine$new(name = name, email = email, role = role)
  return(contributorLine)
}

#' @title Make Study Meta for Ulysses
#' @param studyTitle the title of the study as a character string
#' @param therapeuticArea the TA as a character string
#' @param studyType the study type (typically characterization)
#' @param studyLinks a list of study links
#' @param studyTags a list of study tags
#' @returns A StudyMeta R6 class with the study meta
#' @export
makeStudyMeta <- function(studyTitle,
                          therapeuticArea,
                          studyType,
                          contributors,
                          studyLinks = NULL,
                          studyTags = NULL) {
  sm <- StudyMeta$new(
    studyTitle = studyTitle,
    therapeuticArea = therapeuticArea,
    studyType = studyType,
    contributors = contributors,
    studyLinks = studyLinks,
    studyTags = studyTags
  )

  return(sm)
}
#' @title set the config block for a database
#' @param configBlockName the name of the config block
#' @param cdmDatabaseSchema the cdmDatabaseSchema specified as a character string
#' @param databaseName the name of the database, typically uses the db name and id. For example optum_dod_202501
#' @param databaseLabel the labelling name of the database, typically a common name for a db. For example Optum DOD
#' @returns A StudyMeta R6 class with the study meta
#' @export
setDbConfigBlock <- function(configBlockName,
                             cdmDatabaseSchema,
                             databaseName = NULL,
                             databaseLabel = NULL) {
  dbConfigBlock <- DbConfigBlock$new(configBlockName = configBlockName,
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     databaseName = databaseName,
                                     databaseLabel = databaseLabel)
  return(dbConfigBlock)
}

#' @title Make ExecOptions for Ulysses
#' @param dbms specify the dbms used in the exec options
#' @param workDatabaseSchema the name of the workDatabaseSchema as a character string, location in DB where user has write access
#' @param tempEmulationSchema he name of the tempEmulationSchema as a character strings
#' @param dbConnectionBlocks a list of DbConfigBlock R6 classes specifying the dbs to connect
#' @returns A ExecOptions R6 class with the execOptions
#' @export
makeExecOptions <- function(dbms,
                            workDatabaseSchema,
                            tempEmulationSchema = NULL,
                            dbConnectionBlocks) {

  execOptions <- ExecOptions$new(
    dbms = dbms,
    workDatabaseSchema = workDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    dbConnectionBlocks = dbConnectionBlocks
  )
  return(execOptions)
}


#' @title Make Ulysses Study Settings
#' @param repoName the name of repo as a character string
#' @param repoFolder the folder path where the repo is stored in local as a character string
#' @param studyMeta a StudyMeta R6 class with the details describing the study
#' @param execOptions a ExecOptions R6 class with the execution details needed for the study
#' @param gitRemote a remote url used to clone and set remote git
#' @param renvLock file path to a renvLock file
#' @returns A UlyssesStudy R6 class with the ulysses study details to make
#' @export
makeUlyssesStudySettings <- function(repoName,
                                     repoFolder,
                                     studyMeta,
                                     execOptions,
                                     gitRemote = NULL,
                                     renvLock = NULL) {

  execOptions <- UlyssesStudy$new(
    repoName = repoName,
    repoFolder = repoFolder,
    studyMeta = studyMeta,
    execOptions = execOptions,
    gitRemote = gitRemote,
    renvLock = renvLock
  )
  return(execOptions)

}

#' @title Function to Launch new Ulysses Repo
#' @param ulyssesStudySettings UlyssesStudy R6 class with the ulysses study details to make
#' @param verbose a toggle whether to print details of launch in console
#' @param openProject a toggle whether to open the repo as a new R project
#' @returns invisible return. Creates the ulysses repo in the local file structure
#' @export
launchUlyssesRepo <- function(ulyssesStudySettings, verbose = TRUE, openProject = FALSE) {
  ulyssesStudySettings$initUlyssesRepo(verbose = verbose, openProject = openProject)
}


#' @title
#' Create an ExecutionSettings object and set its attributes
#'
#' @param connectionDetails A DatabaseConnector connectionDetails object (optional if connection is specified)
#' @param connection A DatabaseConnector connection object (optional if connectionDetails is specified)
#' @param cdmDatabaseSchema The schema of the OMOP CDM database
#' @param workDatabaseSchema The schema to which results will be written
#' @param tempEmulationSchema Some database platforms like Oracle and Snowflake do not truly support temp tables. To emulate temp tables, provide a schema with write privileges where temp tables can be created.
#' @param cohortTable The name of the table where the cohort(s) are stored
#' @param cdmSourceName A human-readable name for the OMOP CDM source
#'
#' @return An ExecutionSettings object
#' @export
createExecutionSettings <- function(connectionDetails,
                                    connection = NULL,
                                    cdmDatabaseSchema,
                                    workDatabaseSchema,
                                    tempEmulationSchema,
                                    cohortTable,
                                    cdmSourceName) {
  executionSettings <- ExecutionSettings$new(connectionDetails = connectionDetails,
                                             connection = connection,
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             workDatabaseSchema = workDatabaseSchema,
                                             tempEmulationSchema = tempEmulationSchema,
                                             cohortTable = cohortTable,
                                             cdmSourceName = cdmSourceName)
  return(executionSettings)
}
