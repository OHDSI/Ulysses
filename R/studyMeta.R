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
