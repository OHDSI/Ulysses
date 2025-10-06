setContributor <- function(name, email, role) {
  contributorLine <- ContributorLine$new(name = name, email = email, role = role)
  return(contributorLine)
}

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


makeExecOptions <- function(connectionLoadScript,
                            hideConnectionScriptPath = TRUE,
                            dbms,
                            resultsPath = "exec/results",
                            databaseRole = NULL,
                            workDatabaseSchema,
                            tempEmulationSchema = NULL,
                            dbConnectionBlocks) {

  if (hideConnectionScriptPath) {
    notification(
      glue::glue_col("Run {magenta `usethis::edit_r_environ()`} to save system variable to {cyan .Renviron}!!!")
    )
    cli::cat_line(glue::glue_col("Copy to {cyan .Renviron}"))
    cli::cat_line(glue::glue_col("{yellow connectionLoadScript}={green '{connectionLoadScript}'}"))
    connectionLoadScript2 <- "!expr Sys.getenv('connectionLoadScript')"
  } else {
    connectionLoadScript2 <- connectionLoadScript
  }

  execOptions <- ExecOptions$new(
    connectionLoadScript = connectionLoadScript2,
    dbms = dbms,
    resultsPath = resultsPath,
    databaseRole = databaseRole,
    workDatabaseSchema = workDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    dbConnectionBlocks = dbConnectionBlocks
  )
  return(execOptions)
}

makeWebApiCredentials <- function(webApiUrl, authMethod, user, password, checkCreds = TRUE) {
  wac <- WebApiCreds$new(
    webApiUrl = webApiUrl,
    authMethod = authMethod,
    user = user,
    password = password
  )
  if (checkCreds) {
    wac$checkAllCredentials()
  }
  return(wac)
}

makeCirceCohortsToLoad <- function(cohortsToLoadTable, webApiCreds) {
  tblObj <- CirceCohortsToLoad$new(
    cohortsToLoadTable = cohortsToLoadTable,
    webApiCreds = webApiCreds
  )
  return(tblObj)
}

makeCirceConceptSetsToLoad <- function(conceptSetsToLoadTable, webApiCreds) {
  tblObj <- CirceConceptSetsToLoad$new(
    conceptSetsToLoadTable = conceptSetsToLoadTable,
    webApiCreds = webApiCreds
  )
  return(tblObj)
}

makeInputOptions <- function(circeCohortsToLoad = NULL,
                             circeConceptSetsToLoad = NULL,
                             analysisTaskFilesToLoad = NULL,
                             studyHubFilesToLoad = NULL) {
  io <- InputOptions$new(
    circeCohortsToLoad = circeCohortsToLoad,
    circeConceptSetsToLoad = circeConceptSetsToLoad,
    analysisTaskFilesToLoad = analysisTaskFilesToLoad,
    studyHubFilesToLoad = studyHubFilesToLoad
  )
  return(io)
}


makeUlyssesStudySettings <- function(repoName,
                                     repoFolder,
                                     studyMeta,
                                     execOptions,
                                     inputOptions = NULL,
                                     gitRemote = NULL,
                                     renvLock = NULL) {

  execOptions <- UlyssesStudy$new(
    repoName = repoName,
    repoFolder = repoFolder,
    studyMeta = studyMeta,
    execOptions = execOptions,
    inputOptions = inputOptions,
    gitRemote = gitRemote,
    renvLock = renvLock
  )
  return(execOptions)

}


launchUlyssesRepo <- function(ulyssesStudySettings, verbose = TRUE, openProject = FALSE) {
  ulyssesStudySettings$initUlyssesRepo(verbose = verbose, openProject = openProject)
}
