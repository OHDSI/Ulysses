# .cohortBuilder <- function(private, executionSettings, cohortFolder = here::here("cohorts")) {
#
#   cli::cat_bullet(
#     glue::glue_col("{yellow Building Circe Cohorts in Ulysses directory}"),
#     bullet = "pointer",
#     bullet_col = "yellow"
#   )
#   # get cohor table names
#   cohortTableNms <- CohortGenerator::getCohortTableNames(
#     cohortTable = executionSettings$targetCohortTable
#   )
#
#   # prep cohorts for generator
#   cohortsToCreate <- private$.pullManifest()|>
#     dplyr::select(
#       cohortId, cohortPrettyName, circeJson, ohdsiSql
#     ) |>
#     dplyr::rename(
#       cohortName = cohortPrettyName,
#       json = circeJson,
#       sql = ohdsiSql
#     )
#
#   # establish connection to database
#   connection <- executionSettings$getConnection()
#
#   if (is.null(connection)) {
#     connection <- executionSettings$connect()
#   }
#
#   #generate cohorts
#   cohortGenRes <- CohortGenerator::generateCohortSet(
#     connection = connection,
#     cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
#     cohortDatabaseSchema =  executionSettings$workDatabaseSchema,
#     tempEmulationSchema = executionSettings$tempEmulationSchema,
#     cohortTable = cohortTableNms,
#     cohortDefinitionSet = cohortsToCreate
#   ) |>
#     dplyr::select(-c(cohortName)) |>
#     dplyr::left_join(
#       private$.pullManifest(), by = c("cohortId")
#     ) |>
#     dplyr::select(
#       cohortId, cohortHash, startTime, endTime
#     )
#
#   # Count and save
#   cli::cat_bullet(
#     glue::glue("Getting cohort counts"),
#     bullet= "pointer",
#     bullet_col = "yellow"
#   )
#
#   # retrieve counts
#   cohortCounts <- CohortGenerator::getCohortCounts(
#     connection = executionSettings$getConnection(),
#     cohortDatabaseSchema = executionSettings$workDatabaseSchema,
#     cohortTable = executionSettings$targetCohortTable,
#     cohortIds = cohortsToCreate$cohortId
#   ) |>
#     dplyr::inner_join(
#       cohortGenRes,
#       by = "cohortId"
#     ) |>
#     dplyr::mutate(
#       cohortId = as.integer(cohortId)
#     )
#
#   # turn results into R6 class
#   genCohorts <- GeneratedCohorts$new(
#     cohortId = cohortCounts$cohortId,
#     cohortHash = cohortCounts$cohortHash,
#     startTime = cohortCounts$startTime,
#     endTime = cohortCounts$endTime,
#     cohortSubjects = cohortCounts$cohortSubjects,
#     cohortEntries = cohortCounts$cohortEntries,
#     cdmSourceName = executionSettings$cdmSourceName,
#     cohortTableName = executionSettings$targetCohortTable
#   )
#   tb <- genCohorts$showTable() |>
#     dplyr::inner_join(
#       private$.pullManifest(),
#       by = c("cohortId", "cohortHash")
#     )
#   .saveGeneratedCohorts(tb, cohortFolder)
#
#   private$.addGeneratedCohorts(genCohorts)
#   invisible(tb)
# }


.saveGeneratedCohorts <- function(tb, cohortFolder = here::here("cohorts")) {
    readr::write_csv(
      tb, file = fs::path(cohortFolder, "GeneratedCohorts.csv")
    )
  cli::cat_bullet(
    glue::glue_col("{blue Saving Generated Cohorts to Ulysses cohorts folder}"),
    bullet = "info",
    bullet_col = "blue"
  )
}


.grabCirceFiles <- function(type,
                            cohortFolder) {

  checkmate::assert_choice(type, choices = c("json", "sql"))

  #get cohort file paths
  files <- fs::dir_ls(
    path = fs::path(cohortFolder, type),
    type = "file",
    glob = glue::glue("*.{type}")
  )

  # get circe json
  contents <- purrr::map_chr(files, ~readr::read_file(.x))

  ll <- list(
    'files' = files,
    'contents' = contents
  )
  return(ll)
}


.cm <- function(cohortFolder = here::here("cohorts")) {
  #get cohort file paths
  #cohortJson <- .grabCirceFiles(type = "json", cohortFolder)
  cohortSql <- .grabCirceFiles(type = "sql", cohortFolder)


  if (length(cohortSql$files) == 0) {
    cli::cli_abort("There are no circe cohorts in this study.")
  }

  #get cohort names
  cohortNames <- fs::path_file(cohortSql$files) %>%
    fs::path_ext_remove()

  #get cohort ids
  cohortIds <- cohortNames |>
    stringr::str_rank(numeric = TRUE)


  # get ohdsi sql
  ohdsiSql <- cohortSql$contents

  # determine cohortHash
  cohortHash <- purrr::map_chr(
    ohdsiSql,
    ~digest::digest(.x, algo = "md5", serialize = FALSE)
  )

  tb <- tibble::tibble(
    cohortId = cohortIds,
    cohortHash = cohortHash,
    ohdsiSql = ohdsiSql
  )

  return(tb)
}

# function to make the cohortManifest
makeCohortManifest <- function(cohortFolder = here::here("cohorts")) {

  cmTb <- .cm(cohortFolder)

  # get reamining details

  #get cohort file paths
  cohortJson <- .grabCirceFiles(type = "json", cohortFolder)

  #get cohort names
  cohortNames <- fs::path_file(cohortJson$files) %>%
    fs::path_ext_remove()

  # get circe json
  circeJson <- cohortJson$contents


  cohortManifest <- CohortManifest$new(
    cohortId = cmTb$cohortId,
    cohortName = cohortNames,
    cohortPrettyName = cohortNames,
    cohortHash = cmTb$cohortHash,
    cohortType = rep("target", nrow(cmTb)),
    circeJson = circeJson,
    ohdsiSql = cmTb$ohdsiSql,
    generatedCohorts = NULL
  )

  return(cohortManifest)

}


# Functions to work with cohorts to create

md_to_viewer <- function(txt) {
  #create a temp dir for md file
  tempDir <- tempfile()
  dir.create(tempDir)
  tmpRmd <- file.path(tempDir, "clinChar.md")
  #write file to tmp
  readr::write_lines(txt, file = tmpRmd)
  tmpHtml <- rmarkdown::render(tmpRmd, params = "ask", quiet = TRUE)
  rstudioapi::viewer(tmpHtml)
}
