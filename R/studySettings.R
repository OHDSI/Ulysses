defaultAuthors <- function() {
  tibble::tibble(
    name = c("Ulysses", "Eurylochus"),
    organization = c("OHDSI", "OHDSI"),
    role = c("Lead", "Developer")
  )
}

defaultMilestones <- function() {
  list(
    'Status' = "Initialized",
    'Start Date' = "1960-01-01",
    'End Date' = "2099-12-31"
  )
}

defaultDesc <- function() {
  list(
    'StudyType' = "Characterization",
    'Tags' = list(
      "Observational Study",
      "OMOP",
      "OHDSI"
    )
  )
}

defaultLinks <- function() {
  list(
    'Forum' = "TBA",
    'Protocol' = "TBA",
    'HowToRun' = "TBA",
    'ContributionGuidelines' = "TBA",
    'ResultsDashboard' = "TBA",
    'Report' = "TBA"
  )
}

#' Function to initialize study settings
#' @param title the title of the study
#' @param authors the author list for the study
#' @param milestones list of milestone information including study status and timeframe for study
#' @param desc a list of attributes describing the study including the study type and tages
#' @param links a list of links to files used in study
makeStudySettings <- function(title,
                              authors = defaultAuthors(),
                              milestones = defaultMilestones(),
                              desc = defaultDesc(),
                              links = defaultLinks()) {




  studySettings <- list(
    'Title' = snakecase::to_title_case(title),
    'Authors' = authors,
    'Description' = desc,
    'Milestones' = milestones,
    'Links' = links
  )

  return(studySettings)

}


convert_to_yml <- function(studySettings, savePath) {

  #create file path for yml
  filePath <- fs::path(savePath, "_study", ext = "yml")

  cli::cat_bullet(
    "Initialize _study.yml",
    bullet = "pointer", bullet_col = "yellow"
  )

  #write _study.yml file
  yaml::write_yaml(
    x = studySettings,
    file = filePath,
    column.major = FALSE)

  invisible(filePath)

}
