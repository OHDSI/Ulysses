#' @title Define the load table
#' @param atlas a vector of atlas Ids to load
#' @param label a vector of naming labels to identify the atlas Ids, must be in order of atlas ids
#' @param category a vector of categories to identify the atlas ids, must be in order of atlas ids
#' @param subCategory a vector of sub-categories to identify the atlas ids, must be in order of atlas ids
#' @return a tibble where each row is an atlas asset with meta data
#' @export
defineLoadTable <- function(atlasId, label, category, subCategory) {
  tb <- tibble::tibble(
    atlasId = atlasId,
    label = label,
    category = category,
    subCategory = subCategory
  )
  return(tb)
}

#' @title Initialize Manifests
#' @param manifestType the type of manifest to initialize, either conceptSet or cohort
#' @param loadTable a tibble made using defineLoadTable which prespecifies atlas ids to initialize the manifest
#' @param overwrite toggle whether to overwrite existing manifests. Default is FALSE
#' @param repoPath the location of the repo specifying where to save the manifest files
#' @return invisible return but initializes the manifest in its appropriate folder location
#' @export
initializeManifest <- function(manifestType,
                               loadTable = NULL,
                               overwrite = FALSE,
                               repoPath = here::here("inputs")) {

  checkmate::assert_choice(x = manifestType, choices = c("conceptSet", "cohort"))

  # if manifesttype is concept set set route to csv files
  if (manifestType == "conceptSet") {
    manifestFile <- fs::path(repoPath, "conceptSets/conceptSetManifest.csv")
    manifestLogFile <- fs::path(repoPath, "conceptSets/conceptSetManifestLog.csv")
  }

  # if manifesttype is cohort set route to csv files
  if (manifestType == "cohort") {
    manifestFile <- fs::path(repoPath, "cohorts/cohortManifest.csv")
    manifestLogFile <- fs::path(repoPath, "cohorts/cohortManifestLog.csv")
  }

  # if manifest exists and overwrite is FALSE then stop function
  if (file.exists(manifestFile) & overwrite == FALSE) {
    stop("Manifest already exists!  Set overwrite to TRUE if you'd like to replace it.")
  }

  # if files doesnt exist and the load table is not null than make the loadTable the start of the manifest
  if (!file.exists(manifestFile) & !is.null(loadTable)) {
    man <- loadTable |>
      dplyr::mutate(
        id = NA_integer_,
        name = NA_character_,
        path = NA_character_
      )
  } else {
    if (manifestType == "conceptSet") {
      man <- data.frame(
        atlasId = integer(),
        label = character(),
        category = character(),
        subCategory = character(),
        id = integer(),
        name = character(),
        path = character(),
        sourceCs = logical(),
        domain = character()
      )
    } else {
      man <- data.frame(
        atlasId = integer(),
        label = character(),
        category = character(),
        subCategory = character(),
        id = integer(),
        name = character(),
        path = character()
      )
    }
  }

  readr::write_csv(x = man, file = manifestFile)
  cli::cat_bullet(
    glue::glue_col("Initializing {manifestType} Manifest to {cyan {manifestFile}}"),
    bullet = "tick",
    bullet_col = "green"
  )

  if (file.exists(manifestLogFile)) {
    fs::file_delete(manifestLogFile)
    cli::cat_bullet(
      glue::glue_col("Removed old log file at {cyan {manifestLogFile}}"),
      bullet = "tick",
      bullet_col = "red"
    )
  }

  invisible(man)

}


#' @title Import Atlas Concept sets from the manifest
#' @param conceptSetManifest the location of the concept set manifest
#' @param atlasConnection an WebApiConnection R6 class that holds the creds to connect to webapi
#' @param outputFolder the location where the concept sets should be written
#' @return invisible return but stores the concept set files into Ulysses
#' @export
importAtlasConceptSetsFromManifest <- function(
    conceptSetManifest,
    atlasConnection,
    outputFolder = here::here("inputs/conceptSets/json")
) {

  for (i in 1:nrow(conceptSetManifest)) {
    if (is.na(conceptSetManifest$atlasId[i])) {
      next
    }
    concept_set <- atlasConnection$getConceptSetDefinition(conceptSetId = conceptSetManifest$atlasId[i])
    conceptSetManifest$name[i] <- concept_set$saveName[1]
    conceptSetManifest$expression[i] <- concept_set$expression[1]
  }

  for (j in 1:nrow(conceptSetManifest)) {
    if (is.na(conceptSetManifest$atlasId[i])) {
      next
    }
    csCategory <- snakecase::to_snake_case(conceptSetManifest$category[j])
    csSubCategory <- ifelse(is.na(conceptSetManifest$subCategory[j]), "", snakecase::to_snake_case(conceptSetManifest$subCategory[j]))
    subDirs<- fs::path(csCategory, csSubCategory)
    savePath <- outputFolder |>
      fs::dir_create(subDirs)
    savePathRel <- fs::path_rel(savePath)

    # Save concept set expression to json folder
    saveNameTmp <- conceptSetManifest$name[j]
    fileNameTmp <- fs::path(savePath, saveNameTmp, ext = "json")
    csExpTmp <- conceptSetManifest$expression[j]
    readr::write_file(csExpTmp, file = fileNameTmp)
      cli::cat_bullet(
        glue::glue("Circe ConceptSet Json {crayon::magenta(saveNameTmp)} saved to: {crayon::cyan(savePath)}"),
        bullet = "pointer",
        bullet_col = "yellow"
      )
    conceptSetManifest$path[j] <- fs::path(savePathRel, saveNameTmp, ext = "json")
  }

  conceptSetManifest <- conceptSetManifest |>
    dplyr::select(-expression)

  invisible(conceptSetManifest)
}

#' @title Import Atlas Cohorts from the manifest
#' @param conceptSetManifest the location of the cohort manifest
#' @param atlasConnection an WebApiConnection R6 class that holds the creds to connect to webapi
#' @param outputFolder the location where the cohorts should be written
#' @return invisible return but stores the cohort files into Ulysses
#' @export
importAtlasCohortsFromManifest <- function(cohortManifest,
                                           atlasConnection,
                                           outputFolder = here::here("inputs/cohorts/json")) {

  for (i in 1:nrow(cohortManifest)) {
    if (is.na(cohortManifest$atlasId[i])) {
      next
    }
    cohort_tb <- atlasConnection$getCohortDefinition(cohortId = cohortManifest$atlasId[i])
    cohortManifest$name[i] <- cohort_tb$saveName[1]
    cohortManifest$expression[i] <- cohort_tb$expression[1]
  }

  for (j in 1:nrow(cohortManifest)) {
    if (is.na(cohortManifest$atlasId[j])) {
      next
    }
    cohortCategory <- snakecase::to_snake_case(cohortManifest$category[j])
    cohortSubCategory <- ifelse(is.na(cohortManifest$subCategory[j]), "", snakecase::to_snake_case(cohortManifest$subCategory[j]))
    subDirs<- fs::path(cohortCategory, cohortSubCategory)
    savePath <- outputFolder |>
      fs::dir_create(subDirs)
    savePathRel <- fs::path_rel(savePath)

    # Save concept set expression to json folder
    saveNameTmp <- cohortManifest$name[j]
    fileNameTmp <- fs::path(savePath, saveNameTmp, ext = "json")
    cdExpTmp <- cohortManifest$expression[j]
    readr::write_file(cdExpTmp, file = fileNameTmp)
    cli::cat_bullet(
      glue::glue("Circe Cohort Json {crayon::magenta(saveNameTmp)} saved to: {crayon::cyan(savePath)}"),
      bullet = "pointer",
      bullet_col = "yellow"
    )
    cohortManifest$path[j] <- fs::path(savePathRel, saveNameTmp, ext = "json")
  }

  cohortManifest <- cohortManifest |>
    dplyr::select(-expression)

  invisible(cohortManifest)
}

#' @title Populate Manifests
#' @param manifestType the type of manifest to initialize, either conceptSet or cohort
#' @param importFromAtlas toggle whether to import content from atlas. Default is TRUE
#' @param atlasConnection add atlasConnection object.
#' @param repoPath the location of the repo specifying where to save the manifest files
#' @return invisible return but populates the manifest in its appropriate folder location
#' @export
populateManifest <- function(manifestType,
                             importFromAtlas = FALSE,
                             atlasConnection = setAtlasConnection(),
                             repoPath = here::here("inputs")) {

  checkmate::assert_choice(x = manifestType, choices = c("conceptSet", "cohort"))

  if (manifestType == "conceptSet") {
    manifestFile <- fs::path(repoPath, "conceptSets/conceptSetManifest.csv")
    #manifestFile <- here::here("inputs/conceptSets/conceptSetManifest.csv")
    stopifnot(file.exists(manifestFile))
    man <- readr::read_csv(manifestFile, show_col_types = FALSE)

    if (!("sourceCs" %in% names(man))) {
      man <- man |>
        dplyr::mutate(sourceCs = NA)
    }

    if (!("domain" %in% names(man))) {
      man <- man |>
        dplyr::mutate(domain = NA)
    }

    manifestLogFile <- fs::path(repoPath, "conceptSets/conceptSetManifestLog.csv")
    #manifestLogFile <- here::here("inputs/conceptSets/conceptSetManifestLog.csv")
    jsonFolder <- fs::path(repoPath, "conceptSets/json")
    sqlFolder <- NA
    if (importFromAtlas & nrow(man) != 0) {
      man <- importAtlasConceptSetsFromManifest(
        conceptSetManifest = man,
        atlasConnection = atlasConnection
      )
    }
  }

  if (manifestType == "cohort") {
    manifestFile <- fs::path(repoPath, "cohorts/cohortManifest.csv")
    #manifestFile <- here::here("inputs/cohorts/cohortManifest.csv")
    stopifnot(file.exists(manifestFile))
    man <- readr::read_csv(manifestFile, show_col_types = FALSE) |>
      dplyr::mutate(sourceCs = NA, domain = NA)
    manifestLogFile <- fs::path(repoPath, "cohorts/cohortManifestLog.csv")
    #manifestLogFile <- here::here("inputs/cohorts/cohortManifestLog.csv")
    jsonFolder <- fs::path(repoPath, "cohorts/json")
    sqlFolder <- fs::path(repoPath, "cohorts/sql")
    if (importFromAtlas & nrow(man) != 0) {
      man <- importAtlasCohortsFromManifest(
        cohortManifest = man,
        atlasConnection = atlasConnection
        )
    }
  }

  filePaths <- fs::dir_ls(jsonFolder, type = "file", glob = "*.json", recurse = TRUE) |>
    fs::path_rel()

  if (length(filePaths) == 0 & nrow(man) == 0) {
    cli::cat_bullet(glue::glue("No files found and {manifestType} manifest is empty."),
                    bullet = "info", bullet_col = "yellow")
    return()
  }

  fileNames <- fs::path_rel(filePaths, start = jsonFolder) |>
    fs::path_ext_remove() |>
    basename()

  if (!file.exists(manifestLogFile)) {
    if (all(is.na(man$id))) {
      man <- man |>
        dplyr::arrange(id,path) |>
        dplyr::mutate(id = seq_along(id))
    } else {
      stop(glue::glue("No logfile exists to track {manifestType} IDs!  Manifest generation cannot proceed.  Please delete all concept set IDs from your Manifest and try again."))
    }

    manLog <- man |>
      dplyr::mutate(isDeprecated = FALSE)
    readr::write_csv(x = manLog, file = manifestLogFile)
    cli::cat_bullet("Saving Manifest Log to ", crayon::cyan(manifestLogFile),
                    bullet = "tick", bullet_col = "green")
  }

  if (!is.na(sqlFolder)) {
    sqlFilePaths <- fs::dir_ls(sqlFolder, type = "file", glob = "*.sql", recurse = TRUE) |>
      fs::path_rel()
    sqlFileNames <- fs::path_rel(sqlFilePaths, start = sqlFolder) |>
      fs::path_ext_remove() |>
      basename()
    filePaths <- c(filePaths, sqlFilePaths)
    fileNames <- c(fileNames, sqlFileNames)
  }

  namesDf <- data.frame(name = fileNames,
                        path = filePaths,
                        inRepo = TRUE)

  manLog <- readr::read_csv(manifestLogFile, show_col_types = FALSE)

  if (manifestType == "cohort") {
    manLog <- manLog |>
      dplyr::mutate(sourceCs = NA, domain = NA)
  }

  manLog <- manLog |>
    dplyr::full_join(man, by = c("id")) |>
    dplyr::mutate(atlasId = dplyr::coalesce(atlasId.y, atlasId.x),
                  label = dplyr::coalesce(label.y, label.x),
                  category = dplyr::coalesce(category.y, category.x),
                  subCategory = dplyr::coalesce(subCategory.y, subCategory.x),
                  name = dplyr::coalesce(name.y, name.x),
                  path = dplyr::coalesce(path.y, path.x),
                  sourceCs = dplyr::coalesce(sourceCs.y, sourceCs.x),
                  domain = dplyr::coalesce(domain.y, domain.x)) |>
    dplyr::select(id, isDeprecated, atlasId, label, category, subCategory, name, path, sourceCs, domain) |>
    dplyr::full_join(namesDf, by = c("name","path")) |>
    dplyr::arrange(id,path) |>
    dplyr::mutate(id = dplyr::if_else(is.na(id),
                                      dplyr::row_number(),
                                      id),
                  isDeprecated = dplyr::if_else(is.na(inRepo),TRUE,FALSE))

  man <- manLog |>
    dplyr::filter(inRepo == TRUE) |>
    dplyr::select(-c(inRepo,isDeprecated))

  if (manifestType == "cohort") {
    man <- man |>
      dplyr::select(-c(sourceCs, domain))

    manLog <- manLog |>
      dplyr::select(-c(sourceCs, domain))
  }

  readr::write_csv(x = man, file = manifestFile)
  cli::cat_bullet("Saving Manifest to ", crayon::cyan(manifestFile),
                  bullet = "tick", bullet_col = "green")

  manLog <- manLog |>
    dplyr::select(-inRepo)
  readr::write_csv(x = manLog, file = manifestLogFile)
  cli::cat_bullet("Saving Manifest Log to ", crayon::cyan(manifestLogFile),
                  bullet = "tick", bullet_col = "green")

  invisible(man)
}

#' @title function to subset the cohortManifest based on category and sub category
#' @param cohortCategory the category label in the cohort manifest
#' @param cohortSubCategory the subCategory label to filter the cohort manifest, default is null
#' @param cohortManifestPath the path to the manifest. Defaults to the standard ulysses path
#' @returns a tibble subsetting the cohort manifest by the id and label
#' @export
getAnalysisCohortsByCategory <- function(cohortCategory,
                               cohortSubCategory = NULL,
                               cohortManifestPath = here::here("inputs/cohorts/cohortManifest.csv")) {
  if (is.null(cohortSubCategory)) {
    #category <- rlang::enquo(category)
    analysisCohorts <- readr::read_csv(cohortManifestPath, show_col_types = FALSE) |>
      dplyr::filter(
        category == cohortCategory
      ) |>
      dplyr::select(id, label)
  } else {
    analysisCohorts <- readr::read_csv(cohortManifestPath, show_col_types = FALSE) |>
      dplyr::filter(
        category == category,
        subCategory %in% cohortSubCategory
      ) |>
      dplyr::select(id, label)

  }

  return(analysisCohorts)
}
