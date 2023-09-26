makeWebsiteYaml <- function(projectPath = here::here()) {

  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)

  # make list of vars for template
  data <- rlang::list2(
    'Study' = studyMeta$Title
  )


  usethis::use_template(
    template = "quartoWebsite.yml",
    save_as = fs::path("documentation", "_quarto.yml"),
    data = data,
    open = FALSE,
    package = "Ulysses")

  invisible(data)

}
