# Study Meta info

setStudyDescription <- function(studyTitle,
                                therapeuticArea,
                                studyType) {
  ll <- list(
    'studyTitle' = studyTitle,
    'therapeuticArea' = therapeuticArea,
    'studyType' = studyType
  )
  class(ll) <- "studyDescription"
  return(ll)

}

setStudyLinks <- function(...) {
  ll <- rlang::list2(...)
  class(ll) <- "studyLinks"
  return(ll)
}

setStudyTags <- function(...) {
  ll <- rlang::list2(...)
  class(ll) <- "studyTags"
  return(ll)
}

setStudyContributors <- function(...) {
  ll <- rlang::list2(...)
  class(ll) <- "contributors"
  return(ll)
}

companyEmail <- function(userName, domainName) {
  em <- glue::glue("{userName}@{domainName}.com")
  return(em)
}


contributorLine <- function(name, email, role) {
  ll <- list(
    'name' = name,
    'email' = email,
    'role' = role
  )
  class(ll) <- "contributorLine"
  return(ll)
}

initStudyMeta <- function(studyDescription, studyContributors, tags, links) {
  sm <- list(
    'studyDescription' = studyDescription,
    'studyContributors' = studyContributors,
    'studyTags' = tags,
    'studyLinks' = links
  )
  class(sm) <- "studyMeta"
  return(sm)
}



dbBlock <- function(configBlockName, dbName, dbms, cdm, vocab) {
  ll <- list(
    configBlockName = configBlockName,
    databaseName = dbName,
    cdm = cdm,
    vocab = vocab
  )
  class(ll) <- "dbBlock"
  return(ll)
}


initDbOptions <- function(..., dbms, workDatabaseSchema, tempEmulationSchema) {
  ll <- list(
    dbms = dbms,
    workDatabaseSchema = workDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    dbBlocks = rlang::list2(...)
  )
  #checkmate::assertClass(ll, classes = c("dbBlock"))
  class(ll) <- "dbOptions"
  return(ll)
}


setAesOptions <- function(foregroundColor,
                          backgroundColor,
                          footerText,
                          egpTemplate = TRUE) {
  ll <- list(
    foregroundColor = foregroundColor,
    backgroundColor = backgroundColor,
    footerText = footerText,
    egpTemplate = egpTemplate
  )
  class(ll) <- "aesOptions"
  return(ll)
}



setFileLoadOptions <- function(taskFiles,
                               srcFiles) {
  ll <- list(
    taskFiles = taskFiles,
    srcFiles = srcFiles
  )

  class(ll) <- "fileLoadOptions"
  return(ll)
}
