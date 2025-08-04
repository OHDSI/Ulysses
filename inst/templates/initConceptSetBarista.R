# initCohortBarista.R

# Description: This file begins the concept set manifest and loads atlas concept sets


# take cohorts to Load file and load it
conceptSetLoad <- readr::read_csv(
  file = '{conceptSetsToLoadPath}',
  show_col_types = FALSE
) |>
  dplyr::mutate(
    savePath = fs::path('{repoPath}', glue::glue("cohorts/conceptSets/json/{{type}}"))
  )

# create cohort folders
foldersToCreate <- conceptSetLoad  |>
  dplyr::distinct(savePath) |>
  dplyr::pull() |>
  fs::dir_create()

# load atlas cohorts into Ulysses
purrr::pwalk(
  conceptSetLoad,
  ~Ulysses::importAtlasConceptSets(
    conceptSetIds = ..1,
    savePath = here::here(..4)
  )
)

# Step 3: Create Cohort Manifest TEMPORARY

conceptSetFiles <- fs::path('{repoPath}', "cohorts/conceptSets/json") |>
  fs::dir_ls(type = "file", recurse = TRUE)

commonPath <- glue::glue("{{fs::path_common(conceptSetFiles)}}/")
ll <- gsub(commonPath, "", conceptSetFiles) |>
  fs::path_split()

csManifest <- tibble::tibble(
  type = purrr::map_chr(ll, ~.x[1]),
  conceptSetFile = purrr::map_chr(ll, ~.x[2])
) |>
  dplyr::mutate(
    atlasId = gsub("_.*", "", conceptSetFile) |> as.numeric()
  ) |>
  dplyr::right_join(
    conceptSetLoad,
    by = c("type", "atlasId")
  ) |>
  dplyr::select(
    atlasId, conceptLabel, type, conceptSetFile, savePath,
  ) |>
  dplyr::mutate(
    path = fs::path_rel(savePath)
  ) |>
  dplyr::select(-c(savePath))

readr::write_csv(
  csManifest,
  file = fs::path('{repoPath}', "cohorts/conceptSetManifest.csv")
)

cli::cat_bullet(
  glue::glue_col("Create Concept Set Manifest"),
  bullet = "pointer",
  bullet_col = "yellow"
)
