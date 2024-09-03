# Functions to work with cohorts to create

# set the full string vector
.setCharacter <- function(private, key, value) {
  checkmate::assert_character(x = value, min.chars = 1, null.ok = FALSE)
  private[[key]] <- value
  invisible(private)
}

# set the full integer vector
.setInteger <- function(private, key, value) {
  checkmate::assert_integer(x = value, null.ok = FALSE)
  private[[key]] <- value
  invisible(private)
}


#set a single item in string vector
.setString <- function(private, key, value, idx) {
  checkmate::assert_string(x = value, na.ok = FALSE, min.chars = 1, null.ok = FALSE)
  private[[key]][idx] <- value
  invisible(private)
}


.setActiveString <- function(private, key, value, idx) {
  # return the value if nothing added
  if(missing(value)) {
    vv <- private[[key]][idx]
    return(vv)
  }
  # replace the value if value assigned
  .setSingleString(private = private, key = key, value = value, idx = idx)
}


.setActiveCharacter <- function(private, key, value) {
  # return the value if nothing added
  if(missing(value)) {
    vv <- private[[key]]
    return(vv)
  }
  # replace the value if value assigned
  .setCharacter(private = private, key = key, value = value)
}



.setActiveInteger <- function(private, key, value) {
  # return the value if nothing added
  if(missing(value)) {
    vv <- private[[key]]
    return(vv)
  }
  # replace the value if value assigned
  .setInteger(private = private, key = key, value = value)
}



# R6 class for the cohort manifest
CohortManifest <- R6::R6Class(
  classname = "CohortManifest",
  public = list(
    # initialize class
    initialize = function(
      cohortId,
      cohortName,
      cohortPrettyName,
      cohortHash,
      circeJson,
      ohdsiSql
    ) {
      .setInteger(private = private, key = ".cohortId", value = cohortId)
      .setCharacter(private = private, key = ".cohortName", value = cohortName)
      .setCharacter(private = private, key = ".cohortPrettyName", value = cohortPrettyName)
      .setCharacter(private = private, key = ".cohortHash", value = cohortHash)
      .setCharacter(private = private, key = ".circeJson", value = circeJson)
      .setCharacter(private = private, key = ".ohdsiSql", value = ohdsiSql)
    },
    showTable = function() {
      tb = tibble::tibble(
        cohortId = self$cohortId,
        cohortName = self$cohortName,
        cohortPrettyName = self$cohortPrettyName,
        cohortHash = self$cohortHash,
        circeJson = self$circeJson,
        ohdsiSql = self$ohdsiSql
      ) |>
        dplyr::arrange(
          cohortId
        )
      return(tb)
    }
  ),
  private = list(
    .cohortId = NULL,
    .cohortName = NULL,
    .cohortPrettyName = NULL,
    .cohortHash = NULL,
    .circeJson = NULL,
    .ohdsiSql = NULL
  ),

  active = list(
    cohortId = function(value) {
      .setActiveInteger(private = private, key = ".cohortId", value = value)
    },
    cohortName = function(value) {
      .setActiveCharacter(private = private, key = ".cohortName", value = value)
    },
    cohortPrettyName = function(value, idx = NULL) {
      if (is.null(idx)) {
        .setActiveCharacter(private = private, key = ".cohortPrettyName", value = value)
      } else {
        .setActiveString(private = private, key = ".cohortPrettyName", value = value, idx = idx)
      }
    },
    cohortHash = function(value) {
      .setActiveCharacter(private = private, key = ".cohortHash", value = value)
    },
    circeJson = function(value) {
      .setActiveCharacter(private = private, key = ".circeJson", value = value)
    },
    ohdsiSql = function(value) {
      .setActiveCharacter(private = private, key = ".ohdsiSql", value = value)
    }
  )
)



makeCohortManifest <- function(cohortFolder = here::here("cohorts")) {
  #get cohort file paths
  cohortJsonFiles <- fs::dir_ls(
    path = fs::path(cohortFolder, "json"),
    type = "file",
    glob = "*.json"
  )

  cohortSqlFiles <- fs::dir_ls(
    path = fs::path(cohortFolder, "sql"),
    type = "file",
    glob = "*.sql"
  )


  if (length(cohortSqlFiles) == 0) {
    cli::cli_abort("There are no circe cohorts in this study.")
  }

  #get cohort names
  cohortNames <- fs::path_file(cohortSqlFiles) %>%
    fs::path_ext_remove()

  #get cohort ids
  cohortIds <- cohortNames |>
    stringr::str_rank(numeric = TRUE)

  # get circe json
  circeJson <- purrr::map_chr(cohortJsonFiles, ~readr::read_file(.x))

  # get ohdsi sql
  ohdsiSql <- purrr::map_chr(cohortSqlFiles, ~readr::read_file(.x))

  # determine cohortHash
  cohortHash <- purrr::map_chr(
    ohdsiSql,
    ~digest::digest(.x, algo = "md5", serialize = FALSE)
  )

  cohortManifest <- CohortManifest$new(
    cohortId = cohortIds,
    cohortName = cohortNames,
    cohortPrettyName = cohortNames,
    cohortHash = cohortHash,
    circeJson = circeJson,
    ohdsiSql = ohdsiSql
  )

  return(cohortManifest)

}

# check_cm_dif <- function(cm, cmNew) {
#   hsh1 <- digest::digest(cm |> dplyr::select(cohortId, cohortName))
#   hsh2 <- digest::digest(cmNew |> dplyr::select(cohortId, cohortName))
#   return(hsh1 != hsh2)
# }


# cohortManifest <- function(projectPath = here::here()) {
#
#   cohortManifestPath <- fs::path(projectPath, "cohorts/CohortManifest.csv")
#   check <- fs::file_exists(cohortManifestPath)
#
#   if (check) {
#     cm <- readr::read_csv(file = cohortManifestPath,
#                           show_col_types = FALSE) |>
#       dplyr::mutate(
#         cohortId = as.integer(cohortId)
#       )
#
#     cmNew <- setCohortManifest(projectPath = projectPath)
#
#     if (check_cm_dif(cm, cmNew)) {
#       cli::cat_bullet("Cohort Manifest has changed", bullet = "warning", bullet_col = "yellow")
#       cli::cat_bullet("Overwriting CohortManifest.csv", bullet = "pointer", bullet_col = "yellow")
#       cm <- cmNew
#       readr::write_csv(cmNew, file = cohortManifestPath)
#     }
#
#   } else{
#     cm <- setCohortManifest(projectPath = projectPath)
#     cli::cat_bullet("Initializing CohortManifest.csv", bullet = "pointer", bullet_col = "yellow")
#     readr::write_csv(cm, file = cohortManifestPath)
#     usethis::use_git_ignore(ignores = "cohorts/CohortManifest.csv")
#   }
#
#   return(cm)
# }



# Function to get full print logic from circe
# getCohortPrint <- function(cohort) {
#   #get cohort header
#   cohortName <- snakecase::to_title_case(cohort$name)
#   cohortId <- cohort$id
#   cohortHeader <- glue::glue("# {cohortName} (id: {cohortId}) \n")
#   # get readable cohort logic
#   # get file path
#   cohortFile <- fs::path("cohorts/json", cohort$name, ext = "json")
#   # read json file
#   json <- readr::read_file(cohortFile)
#   # turn into print friendly
#   cdRead <- CirceR::cohortPrintFriendly(json)
#   cdRead <- paste(cohortHeader, "## Cohort Definition", cdRead, sep = "\n\n")
#   # get readable concept set
#   csRead <- RJSONIO::fromJSON(json)$ConceptSets |>
#     CirceR::conceptSetListPrintFriendly()
#   csRead <- paste("## Concept Sets", csRead, sep = "\n\n")
#
#   #bind to get full read
#   readFull <- paste(cdRead, csRead, sep = "\n\n")
#   return(readFull)
# }


# cohortReadBySection <- function(cm, type) {
#   # get the name of the section
#   #typeSym <- rlang::sym(type)
#   typeName <- snakecase::to_title_case(type)
#   nameOfSection <- glue::glue("# {typeName}\n\n")
#   # filter cohorts of that type
#   cohorts <- cm %>%
#     dplyr::filter(type == !!type)
#   # split rows into a list
#   cohorts <- whisker::rowSplit(cohorts)
#   # get print of each cohort in the section
#   cohortSections <- purrr::map_chr(cohorts, ~getCohortPrint(.x))
#   #add header to string
#   cohortSections <- c(nameOfSection, cohortSections)
#   cohortSections <- paste(cohortSections, collapse = "\n")
#   return(cohortSections)
# }


# makeCohortDetails <- function(projectPath = here::here(), open = TRUE) {
#
#   #make file path for cohortDetails
#   cohortDetailsPath <- fs::path(projectPath, "documentation/CohortDetails.qmd")
#
#   # get cohort manifest
#   cm <- Ulysses::cohortManifest(projectPath = projectPath) %>%
#     whisker::rowSplit()
#
#   #get readable cohort details
#   cohortDetails <- purrr::map_chr(cm, ~getCohortPrint(cohort = .x))
#
#   headerText <- "---
# title: Cohort Details
# number-sections: true
# number-depth: 1
# toc: TRUE
# toc-depth: 2
# ---\n\n\n"
#   cohortDetails <- c(headerText, cohortDetails)
#   # write to documenation section
#   cli::cat_bullet("Render Cohort Details using cohorts/json folder",
#                   bullet = "tick", bullet_col = "green")
#   readr::write_lines(cohortDetails, file = cohortDetailsPath)
#
#   #open file if toggle is on
#   if (open) {
#     rstudioapi::navigateToFile(cohortDetailsPath)
#   }
#
#   invisible(cohortDetails)
# }


# Archive -----------------


# cohortHash <- function(projectPath = here::here()) {
#
#   #get cohort file paths
#   cohortFolder <- fs::path(projectPath, "cohorts/json")
#
#   #get cohort file paths
#   cohortFiles <- fs::dir_ls(cohortFolder, type = "file", glob = "*.json")
#
#   #future addition of hash
#   hash <- purrr::map(cohortFiles, ~readr::read_file(.x)) %>%
#     purrr::map_chr(~digest::digest(.x, algo = "sha1")) %>%
#     unname()
#
#   return(hash)
#
# }


# updateCohortManifest <- function(cm, hash, idx) {
#
#   # identifiy the cohorts that changed
#   cohortsThatChanged <- fs::path("cohorts/json", cm$name[idx], ext = "json")
#   newVersion <- cm$version[idx] + 1L
#   cli::cat_bullet("Update ", crayon::green(cohortsThatChanged), " to version ", crayon::magenta(newVersion),
#                   bullet = "pointer", bullet_col = "yellow")
#
#   #update version
#   cm$hash[idx] <- hash[idx]
#   cm$version[idx] <- newVersion
#
#   return(cm)
# }
