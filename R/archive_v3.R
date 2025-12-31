
# initConfigFileFn <- function(repoName, repoPath) {
#
#   header <- glue::glue("# Exec Config File for Ulysses Repo: {repoName}")
#   defaultBlock <- glue::glue( # DO NOT TOUCH SPACING
#     "
# default:
#   projectName: {repoName}
#   version: v0.0.1"
#   )
#
#   configFile <- c(header, defaultBlock) |>
#     glue::glue_collapse(sep = "\n\n")
#
#   readr::write_lines(
#     x = configFile,
#     file = fs::path(repoPath, "config.yml")
#   )
#
#   actionItem(glue::glue_col("Initialize Config: {green {fs::path(repoPath, 'config.yml')}}"))
#   invisible(configFile)
# }
#
#
# initSourceConfigFileFn <- function(repoName, repoPath, exOp) {
#
#   header <- glue::glue("# Source Config File for Ulysses Repo: {repoName}")
#   defaultBlock <- glue::glue( # DO NOT TOUCH SPACING
#     "
# default:
#   dbms: {exOp$dbms}
#   databaseRole: {exOp$databaseRole}
#   workDatabaseSchema: {exOp$workDatabaseSchema}
#   tempEmulationSchema: {exOp$tempEmulationSchema}
#   ")
#
#   conBlocks <- purrr::map_chr(
#     exOp$dbConnectionBlocks,
#     ~.x$writeBlockSection(repoName = repoName)
#   ) |>
#     glue::glue_collapse("\n\n")
#
#   configFile <- c(header, defaultBlock, conBlocks) |>
#     glue::glue_collapse(sep = "\n\n")
#
#   readr::write_lines(
#     x = configFile,
#     file = fs::path(repoPath, "settings/sourceConfig.yml")
#   )
#
#   actionItem(glue::glue_col("Initialize config: {green {fs::path(repoPath, 'settings/sourceConfig.yml')}}"))
#   invisible(configFile)
#
# }

# grabAtlasCredential <- function(credential) {
#   checkmate::assert_choice(
#     x = credential,
#     choices = c("baseUrl", "authMethod", "user", "password")
#   )
#
#   cred <- glue::glue("atlas_{credential}") |>
#     snakecase::to_lower_camel_case() |>
#     Sys.getenv()
#
#   return(cred)
# }
#
# checkAtlasCredentials <- function() {
#
#   credsToCheck <- c("baseUrl", "authMethod", "user", "password")
#   headerTxt <- glue::glue_col("Checking Atlas Credentials from {cyan .Renviron}")
#   cli::cat_rule(headerTxt)
#   cli::cat_line()
#   for(i in seq_along(credsToCheck)) {
#     credItem <- credsToCheck[i]
#     tmp <- grabAtlasCredential(credItem)
#     txt <- glue::glue_col("- {yellow {credItem}}: {green '{tmp}'}")
#     cli::cat_line(txt)
#   }
#
#   cli::cat_line()
#   messageTxt <- glue::glue_col("To modify credentials run function {magenta 'usethis::edit_r_environ()'} and change system variables for Atlas credentials")
#   cli::cat_bullet(messageTxt, bullet = "warning", bullet_col = "yellow")
#
#   invisible(credsToCheck)
# }


### ATLAS CREDS OLD ########

# setAtlasCredentials <- function(keyringName = "atlas",
#                                 keyringPassword = "ohdsi") {
#   creds <- c("baseUrl", "authMethod", "user", "password")
#
#   keyringFn <- purrr::safely(maybeUnlockKeyring)
#   tt <- keyringFn(keyringName = keyringName, keyringPassword)
#
#   if (!is.null(tt$error)) {
#     setKeyring(keyringName = keyringName, keyringPassword = keyringPassword)
#   }
#
#   cli::cat_bullet("Input your Atlas credentials....",
#                   bullet = "info", bullet_col = "blue")
#
#   db <- "atlas"
#   # set credentials
#   purrr::walk(
#     creds,
#     ~set_cred2(
#       cred = .x,
#       db = keyringName)
#   )
#
#   # check credentials
#   purrr::walk(
#     creds,
#     ~checkDatabaseCredential(
#       cred = .x,
#       keyringName = keyringName,
#       verbose = FALSE)
#   )
#
#   invisible(creds)
#
# }

# helpers -----------------
# format_cohort_expression <- function(expression) {
#   # reformat to standard circe
#   circe <- list(
#     'ConceptSets' = expression$ConceptSets,
#     'PrimaryCriteria' = expression$PrimaryCriteria,
#     'AdditionalCriteria' = expression$AdditionalCriteria,
#     'QualifiedLimit' = expression$QualifiedLimit,
#     'ExpressionLimit' = expression$ExpressionLimit,
#     'InclusionRules' = expression$InclusionRules,
#     'EndStrategy' = expression$EndStrategy,
#     'CensoringCriteria' = expression$CensoringCriteria,
#     'CollapseSettings' = expression$CollapseSettings,
#     'CensorWindow' = expression$CensorWindow,
#     'cdmVersionRange' = expression$cdmVersionRange
#   )
#   if (is.null(circe$AdditionalCriteria)) {
#     circe$AdditionalCriteria <- NULL
#   }
#   if (is.null(circe$EndStrategy)) {
#     circe$EndStrategy <- NULL
#   }
#
#   circeJson <- RJSONIO::toJSON(circe, digits = 23)
#
#   return(circeJson)
# }

#
# maybeUnlockKeyring <- function(keyringName = NULL, keyringPassword = NULL, silent = TRUE) {
#
#   # list keyrings
#   allKeyrings <- keyring::keyring_list()$keyring
#
#   #error if keyring name not in list
#   if (!is.null(keyringName) && !(keyringName %in% allKeyrings)) {
#     stop(sprintf("keyring %s does not exist", keyringName))
#   }
#
#   # check if keyring is locked
#   keyringLocked <- keyring::keyring_is_locked(keyring = keyringName)
#
#   #unlock if locked
#   if (keyringLocked) {
#     keyring::keyring_unlock(keyring = keyringName, password = keyringPassword)
#   }
#
#   invisible(keyringLocked)
# }
#
# # Function to get a cohort from atlas by Id
# get_cohort_from_atlas <- function(cohortId,
#                                   authFirst = FALSE,
#                                   keyringName = "atlas",
#                                   keyringPassword = "ohdsi") {
#   # check to unlock keyring
#   maybeUnlockKeyring(keyringName = keyringName, keyringPassword = keyringPassword)
#
#   baseUrl <- keyring::key_get("baseUrl", keyring = keyringName)
#
#
#   if (authFirst) {
#
#     cli::cat_bullet(
#       glue::glue("Authorizing Web Api connection for {crayon::cyan(baseUrl)}"),
#       bullet = "pointer",
#       bullet_col = "yellow"
#     )
#
#     ROhdsiWebApi::authorizeWebApi(
#       baseUrl = baseUrl,
#       authMethod = keyring::key_get("authMethod", keyring = keyringName),
#       webApiUsername = keyring::key_get("user", keyring = keyringName),
#       webApiPassword = keyring::key_get("password", keyring = keyringName)
#     )
#   }
#
#   cohort <- ROhdsiWebApi::getCohortDefinition(cohortId = cohortId, baseUrl = baseUrl)
#   tb <- tibble::tibble(
#     id = cohort$id,
#     name = cohort$name,
#     expression = format_cohort_expression(cohort$expression),
#     saveName = glue::glue("{id}_{name}") |> snakecase::to_snake_case()
#   )
#
#   return(tb)
# }


# get_cs_from_atlas <- function(id,
#                               authFirst = FALSE,
#                               keyringName = "atlas",
#                               keyringPassword = "ohdsi") {
#
#   # check to unlock keyring
#   maybeUnlockKeyring(keyringName = keyringName, keyringPassword = keyringPassword)
#
#   baseUrl <- keyring::key_get(
#     service = "baseUrl",
#     keyring = keyringName
#   )
#
#   if (authFirst) {
#
#     cli::cat_bullet(
#       glue::glue("Authorizing Web Api connection for {crayon::cyan(baseUrl)}"),
#       bullet = "pointer",
#       bullet_col = "yellow"
#     )
#
#     ROhdsiWebApi::authorizeWebApi(
#       baseUrl = baseUrl,
#       authMethod = keyring::key_get("authMethod", keyring = keyringName),
#       webApiUsername = keyring::key_get("user", keyring = keyringName),
#       webApiPassword = keyring::key_get("password", keyring = keyringName)
#     )
#   }
#
#   cs <- ROhdsiWebApi::getConceptSetDefinition(conceptSetId = id, baseUrl = baseUrl)
#
#
#   tb <- tibble::tibble(
#     id = cs$id,
#     name = cs$name,
#     expression = RJSONIO::toJSON(cs$expression, digits = 23),
#     saveName = glue::glue("{id}_{name}") |> snakecase::to_snake_case()
#   )
#
#   return(tb)
# }
#
#
# write_cohorts_to_ulysses <- function(circeJson, saveName, savePath = here::here("cohorts/json")) {
#
#   file_name <- fs::path(savePath, saveName, ext = "json")
#
#   readr::write_file(circeJson, file = file_name)
#   cli::cat_bullet(
#     glue::glue("Circe Cohort Json {crayon::magenta(saveName)} saved to: {crayon::cyan(savePath)}"),
#     bullet = "pointer",
#     bullet_col = "yellow"
#   )
#   invisible(file_name)
# }
#
#
# write_cs_to_ulysses <- function(circeJson, saveName, savePath = here::here("cohorts/conceptSets/json")) {
#
#   file_name <- fs::path(savePath, saveName, ext = "json")
#
#   readr::write_file(circeJson, file = file_name)
#   cli::cat_bullet(
#     glue::glue("Circe ConceptSet Json {crayon::magenta(saveName)} saved to: {crayon::cyan(savePath)}"),
#     bullet = "pointer",
#     bullet_col = "yellow"
#   )
#   invisible(file_name)
# }

# Atlas import UI --------------------

# importAtlasCohorts <- function(
#     cohortIds,
#     keyringName = "atlas",
#     keyringPassword = "ohdsi",
#     savePath = here::here("cohorts/json")
# ) {
#
#   first_cohort <- get_cohort_from_atlas(cohortId = cohortIds[1],
#                                         authFirst = TRUE,
#                                         keyringName = keyringName,
#                                         keyringPassword = keyringPassword)
#
#   if (length(cohortIds) > 1) {
#     remaining_ids <- cohortIds[-1]
#     remaining_cohorts <- purrr::map_dfr(
#       remaining_ids,
#       ~get_cohort_from_atlas(cohortId = .x,
#                              authFirst = FALSE, # already open
#                              keyringName = keyringName,
#                              keyringPassword = keyringPassword)
#     )
#     cohort_tb <- dplyr::bind_rows(first_cohort, remaining_cohorts)
#   } else {
#     cohort_tb <- first_cohort
#   }
#
#   purrr::pwalk(
#     cohort_tb,
#     ~write_cohorts_to_ulysses(
#       circeJson = ..3,
#       saveName = ..4,
#       savePath = savePath
#     )
#   )
#   invisible(cohortIds)
# }



# importAtlasConceptSets <- function(
#     conceptSetIds,
#     keyringName = "atlas",
#     keyringPassword = "ohdsi",
#     savePath = here::here("cohorts/conceptSets/json")
# ) {
#
#   first_concept_set <- get_cs_from_atlas(id = conceptSetIds[1],
#                                          authFirst = TRUE,
#                                          keyringName = keyringName,
#                                          keyringPassword = keyringPassword)
#
#   if (length(conceptSetIds) > 1) {
#     remaining_ids <- conceptSetIds[-1]
#     remaining_cs <- purrr::map_dfr(
#       remaining_ids,
#       ~get_cs_from_atlas(id = .x,
#                          authFirst = FALSE, # already open
#                          keyringName = keyringName,
#                          keyringPassword = keyringPassword)
#     )
#     cs_tb <- dplyr::bind_rows(first_concept_set, remaining_cs)
#   } else {
#     cs_tb <- first_concept_set
#   }
#
#   purrr::pwalk(
#     cs_tb,
#     ~write_cs_to_ulysses(
#       circeJson = ..3,
#       saveName = ..4,
#       savePath = savePath
#     )
#   )
#   invisible(conceptSetIds)
# }
#
#
#
# loadCohorts <- function(cohortsToLoadPath, repoPath) {
#
#   barFileExec <- fs::path_package("Ulysses", "templates/initCohortBarista.R") |>
#     readr::read_file() |>
#     glue::glue()
#   cohortsToLoadPath2 <- cohortsToLoadPath
#   cohortsToLoadPath <- 'extras/cohortsToLoad.csv'
#   barFileSave <- fs::path_package("Ulysses", "templates/initCohortBarista.R") |>
#     readr::read_file() |>
#     glue::glue()
#
#
#   exprs <- rlang::parse_exprs(barFileExec)
#   res <- NULL
#   for (i in seq_along(exprs)) {
#     res <- eval(exprs[[i]], env = rlang::caller_env())
#   }
#
#   # read init barista to extras
#   readr::write_file(barFileSave, file = fs::path(repoPath, "extras/initCohortBarista.R"))
#   actionItem(glue::glue_col("Save initCohortBarista: {cyan {fs::path(repoPath, 'extras/initCohortBarista.R')}}"))
#
#   # move temp to repo
#   #fs::file_create(fs::path(repoPath, "extras/cohortsToLoad.csv"))
#   fs::file_copy(
#     path = cohortsToLoadPath2,
#     new_path = fs::path(repoPath, "extras/cohortsToLoad.csv")
#   )
#   actionItem(glue::glue_col("Copy {cyan cohortsToLoad.csv} into Ulysses {cyan extras/}"))
#
#   invisible(res)
# }
#
#
#
# loadConceptSets <- function(conceptSetsToLoadPath, repoPath) {
#
#   barFileExec <- fs::path_package("Ulysses", "templates/initConceptSetBarista.R") |>
#     readr::read_file() |>
#     glue::glue()
#   conceptSetsToLoadPath2 <- conceptSetsToLoadPath
#   conceptSetsToLoadPath <- 'extras/conceptSetsToLoad.csv'
#   barFileSave <- fs::path_package("Ulysses", "templates/initConceptSetBarista.R") |>
#     readr::read_file() |>
#     glue::glue()
#
#
#   exprs <- rlang::parse_exprs(barFileExec)
#   res <- NULL
#   for (i in seq_along(exprs)) {
#     res <- eval(exprs[[i]], env = rlang::caller_env())
#   }
#
#   # update news
#   readr::write_file(barFileSave, file = fs::path(repoPath, "extras/initConceptSetBarista.R"))
#   actionItem(glue::glue_col("Save initConceptSetBarista: {cyan {fs::path(repoPath, 'extras/initConceptSetBarista.R')}}"))
#
#   # move temp to repo
#   fs::file_copy(
#     path = conceptSetsToLoadPath2,
#     new_path = fs::path(repoPath, "extras/conceptSetsToLoad.csv")
#   )
#   actionItem(glue::glue_col("Copy {cyan conceptSetsToLoad.csv} into Ulysses {cyan extras/}"))
#
#
#   invisible(res)
# }


###############################
# Alternative Style using httr2
###############################

#
# setWebApiConnection <- function(baseUrl, authMethod, user, password) {
#
#   token <- getAuthBearerToken(baseUrl = baseUrl,
#                               authMethod = authMethod,
#                               user = user,
#                               password = password)
#
#
#   webApiConnectionDetails <- list(
#     'baseUrl' = baseUrl,
#     'authMethod' = authMethod
#   )
#   userExpression <- rlang::enquo(user)
#   passwordExpression <- rlang::enquo(password)
#   bearerToken <- rlang::enquo(token)
#
#   webApiConnectionDetails$user <- function() rlang::eval_tidy(userExpression)
#   webApiConnectionDetails$password <- function() rlang::eval_tidy(passwordExpression)
#   webApiConnectionDetails$bearerToken <- function() rlang::eval_tidy(bearerToken)
#
#   class(webApiConnectionDetails) <- "WebApiConnectionDetails"
#   return(webApiConnectionDetails)
# }
#
#
# getAuthBearerToken <- function(baseUrl, authMethod, user, password) {
#
#   authUrl <- paste0(baseUrl, glue::glue("user/login/{authMethod}"))
#
#   req <- httr2::request(authUrl) |>
#     httr2::req_body_form(
#       login = user,
#       password = password
#     )
#
#   bearerToken <- httr2::req_perform(req)$headers$Bearer
#
#   return(bearerToken)
# }
#
# # function to pull any atlas asset by id
# req_id <- function(id, category, webApiConnectionDetails) {
#
#   fixUrl <- gsub("/$", "", webApiConnectionDetails$baseUrl)
#   req <- glue::glue("{fixUrl}/{category}/{id}") |>
#     httr2::request() |>
#     httr2::req_auth_bearer_token(token = webApiConnectionDetails$bearerToken())
#   return(req)
# }
#
# getCohortById <- function(cohortId, webApiConnectionDetails) {
#
#   # generic to pull id
#   req <- req_id(
#     id = cohortId,
#     category = "cohortdefinition",
#     webApiConnectionDetails = webApiConnectionDetails
#   )
#
#   resp <- httr2::req_perform(req = req) # TODO add some sort of error messaging
#   tt <- httr2::resp_body_json(resp)
#   #TODO fix dates and json
#   return(tt)
# }
#
# getConceptSetById <- function(conceptId, webApiConnectionDetails) {
#
#   # generic to pull id
#   req <- req_id(
#     id = conceptId,
#     category = "conceptset",
#     webApiConnectionDetails = webApiConnectionDetails
#   )
#
#   resp <- httr2::req_perform(req = req) # TODO add some sort of error messaging
#   tt <- httr2::resp_body_json(resp)
#   #TODO fix dates and json
#   return(tt)
#
# }



# TODO fix this probably a post of some kind
# getCohortByTag <- function(tag, webApiConnectionDetails) {
#
#
#   fixUrl <- gsub("/$", "", webApiConnectionDetails$baseUrl)
#   req <- glue::glue("{fixUrl}/cohortdefinition/byTags") |>
#     httr2::request() |>
#     httr2::req_auth_bearer_token(token = webApiConnectionDetails$bearerToken())
#
#   resp <- httr2::req_perform(req = req) # TODO add some sort of error messaging
#   tt <- httr2::resp_body_json(resp)
#   #TODO fix dates and json
#   return(tt)
# }

## Import Circe

# writeCohortsToUlysses <- function(circeCohortsToLoad, repoPath) {
#
#   checkmate::assert_class(
#     x = circeCohortsToLoad,
#     classes = "CirceCohortsToLoad"
#   )
#
#   tbl <- circeCohortsToLoad$getCirce()
#
#   # make cohort folders
#   cohortFolders <- fs::path(repoPath, tbl$savePath) |>
#     fs::path_dir() |>
#     unique() |>
#     fs::dir_create()
#
#   #import files into inputs/cohrots
#   cohortFiles <- fs::path(repoPath, tbl$savePath)
#   for (i in seq_along(cohortFiles)) {
#     readr::write_file(tbl$expression[i], file = cohortFiles[i])
#     actionItem(
#       glue::glue_col(
#         "Circe Cohort {yellow {tbl$atlasId[i]}}: {green {tbl$assetLabel[i]}} json saved to: {cyan {tbl$savePath[i]}}"
#       )
#     )
#   }
#
#   tblToLoad <- tbl |>
#     dplyr::select(
#       atlasId, assetLabel, analysisType, savePath
#     )
#   chToLoadPath <- fs::path(repoPath, "inputs/cohorts/cohortsToLoad.csv")
#   readr::write_csv(tblToLoad, file = chToLoadPath)
#   actionItem(
#     glue::glue_col("Save cohortsToLoad to: {cyan {chToLoadPath}}")
#   )
#
#   invisible(tbl)
#
# }
#
#
# writeConceptSetsToUlysses <- function(circeConceptSetsToLoad, repoPath) {
#
#   checkmate::assert_class(
#     x = circeConceptSetsToLoad,
#     classes = "CirceConceptSetsToLoad"
#   )
#
#   tbl <- circeConceptSetsToLoad$getCirce()
#
#   # make cs folders
#   csFolders <- fs::path(repoPath, tbl$savePath) |>
#     fs::path_dir() |>
#     unique() |>
#     fs::dir_create()
#
#   #import files into inputs/conceptSets
#   csFiles <- fs::path(repoPath, tbl$savePath)
#   for (i in seq_along(csFiles)) {
#     readr::write_file(tbl$expression[i], file = csFiles[i])
#     actionItem(
#       glue::glue_col(
#         "Circe Concept Set {yellow {tbl$atlasId[i]}}: {green {tbl$assetLabel[i]}} json saved to: {cyan {tbl$savePath[i]}}"
#       )
#     )
#   }
#
#   tblToLoad <- tbl |>
#     dplyr::select(
#       atlasId, assetLabel, analysisType, savePath
#     )
#   csToLoadPath <- fs::path(repoPath, "inputs/conceptSets/conceptSetsToLoad.csv")
#   readr::write_csv(tblToLoad, file = csToLoadPath)
#   actionItem(
#     glue::glue_col("Save conceptSetsToLoad to: {cyan {csToLoadPath}}")
#   )
#
#   invisible(tbl)
#
#}

#
# grabCohortFromWebApi <- function(cohortId, baseUrl) {
#
#   cohort <- ROhdsiWebApi::getCohortDefinition(cohortId = cohortId, baseUrl = baseUrl)
#   exp <- cohort$expression
#   circe <- list(
#     'ConceptSets' = exp$ConceptSets,
#     'PrimaryCriteria' = exp$PrimaryCriteria,
#     'AdditionalCriteria' = exp$AdditionalCriteria,
#     'QualifiedLimit' = exp$QualifiedLimit,
#     'ExpressionLimit' = exp$ExpressionLimit,
#     'InclusionRules' = exp$InclusionRules,
#     'EndStrategy' = exp$EndStrategy,
#     'CensoringCriteria' = exp$CensoringCriteria,
#     'CollapseSettings' = exp$CollapseSettings,
#     'CensorWindow' = exp$CensorWindow,
#     'cdmVersionRange' = exp$cdmVersionRange
#   )
#   if (is.null(circe$AdditionalCriteria)) {
#     circe$AdditionalCriteria <- NULL
#   }
#   if (is.null(circe$EndStrategy)) {
#     circe$EndStrategy <- NULL
#   }
#
#   circeJson <- RJSONIO::toJSON(circe, digits = 23)
#
#   tb <- tibble::tibble(
#     id = cohort$id,
#     name = cohort$name,
#     expression = circeJson,
#     saveName = glue::glue("{id}_{name}") |> snakecase::to_snake_case()
#   )
#   return(tb)
# }
#
#
#
# grabConceptSetFromWebApi <- function(conceptSetId, baseUrl) {
#
#   cs <- ROhdsiWebApi::getConceptSetDefinition(conceptSetId = conceptSetId, baseUrl = baseUrl)
#
#   tb <- tibble::tibble(
#     id = cs$id,
#     name = cs$name,
#     expression = RJSONIO::toJSON(cs$expression, digits = 23),
#     saveName = glue::glue("{id}_{name}") |> snakecase::to_snake_case()
#   )
#
#   return(tb)
# }
