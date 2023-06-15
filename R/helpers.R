
flattenStudyYml <- function(projectPath = here::here()) {
  yml <- fs::path(projectPath, "_study.yml") %>%
    yaml::read_yaml()

  studyYml <- yml$Study
  names(studyYml) <- paste0("Study", names(studyYml))

  linksYml <- yml$Links
  names(linksYml) <- paste0("Links", names(linksYml))

  yml2 <- c(studyYml, linksYml)
  return(yml2)
}


getStudyDetails <- function(item, projectPath) {

  yml <- flattenStudyYml(projectPath = projectPath)
  yml[[item]]

}


findStepNumber <- function(dir = c("cohortsToCreate", "analysis/studyTasks"), projectPath = here::here()) {

  dir <- match.arg(dir, choices = c("cohortsToCreate", "analysis/studyTasks"))

  if (dir == "cohortsToCreate") {
    items <- fs::path(projectPath, dir) %>%
      fs::dir_ls(type = "directory") %>%
      basename()
  }

  if (dir == "analysis/studyTasks") {
    items <- fs::path(projectPath, dir) %>%
      fs::dir_ls(type = "file") %>%
      basename()
  }

  if (length(items) == 0) {
    step <- 1L
  } else {
    lastNumber <- gsub("_.*", "", items) %>%
      as.integer() %>%
      max()
    step <- lastNumber + 1L
  }

  return(step)

}
