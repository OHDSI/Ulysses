# initCohortBarista.R

# Description: This file begins the cohort manifest and loads atlas cohorts


# take cohorts to Load file and load it
cohortsToLoad <- readr::read_csv(
  file = '{cohortsToLoadPath}',
  show_col_types = FALSE
)|>
  dplyr::mutate(
    # set the savePath
    savePath = fs::path('{repoPath}', glue::glue("cohorts/json/{{type}}"))
  )

# create cohort folders
foldersToCreate <- cohortsToLoad |>
  dplyr::distinct(savePath) |>
  dplyr::pull() |>
  fs::dir_create()

# load atlas cohorts into Ulysses
purrr::pwalk(
  cohortsToLoad,
  ~Ulysses::importAtlasCohorts(
    cohortIds = ..1,
    savePath = here::here(..5)
  )
)


# Step 3: Create Cohort Manifest TEMPORARY
cohortFolder <- fs::path('{repoPath}', "cohorts/json")
#get cohort file paths
cohortFiles <- fs::dir_ls(cohortFolder, type = "file", glob = "*.json", recurse = TRUE)

#get cohort names
cohortNames <- fs::path_file(cohortFiles) |>
  fs::path_ext_remove()

atlasId <- gsub("_.*", "", cohortNames) |>
  as.numeric()

cohortManifest <- tibble::tibble(
  atlasId = atlasId,
  cohortName = cohortNames,
  file = cohortFiles
) |>
  dplyr::mutate(
    file = gsub(paste0(repoPath, "/"), "", file)
  ) |>
  dplyr::left_join(
    cohortsToLoad, by = c("atlasId")
  ) |>
  dplyr::mutate(
    cohortId = studyId, .before = 1
  ) |>
  dplyr::select(
    cohortId, cohortLabel, type, atlasId, cohortName, file
  ) |>
  dplyr::arrange(cohortId)

# Step 4: save cohort manifest

readr::write_csv(
  cohortManifest,
  file = fs::path('{repoPath}', "cohorts/cohortManifest.csv")
)

cli::cat_bullet(
  glue::glue_col("Create Cohort Manifest"),
  bullet = "pointer",
  bullet_col = "yellow"
)
