# Atlas Connection ---------------

#' Function to set atlas credentials for keyring
#' @param keyringName the name of the keyring to save credentials. Defaults to atlas
#' @param keyringPassword the password for the keyring to save credentials. Defaults to ohdsi
#' @return a series of dialog box inputs for the webapi credentials including the baseUrl,
#' authMethod, user and password
#' @export
setAtlasCredentials <- function(keyringName = "atlas",
                                keyringPassword = "ohdsi") {
  creds <- c("baseUrl", "authMethod", "user", "password")

  keyringFn <- purrr::safely(maybeUnlockKeyring)
  tt <- keyringFn(keyringName = keyringName, keyringPassword)

  if (!is.null(tt$error)) {
    setKeyring(keyringName = keyringName, keyringPassword = keyringPassword)
  }

  cli::cat_bullet("Input your Atlas credentials....",
                  bullet = "info", bullet_col = "blue")

  db <- "atlas"
  # set credentials
  purrr::walk(
    creds,
    ~set_cred2(
      cred = .x,
      db = keyringName)
  )

  # check credentials
  purrr::walk(
    creds,
    ~checkDatabaseCredential(
      cred = .x,
      keyringName = keyringName,
      verbose = FALSE)
  )

  invisible(creds)

}

 # helpers -----------------
format_cohort_expression <- function(expression) {
  # reformat to standard circe
  circe <- list(
    'ConceptSets' = expression$ConceptSets,
    'PrimaryCriteria' = expression$PrimaryCriteria,
    'AdditionalCriteria' = expression$AdditionalCriteria,
    'QualifiedLimit' = expression$QualifiedLimit,
    'ExpressionLimit' = expression$ExpressionLimit,
    'InclusionRules' = expression$InclusionRules,
    'EndStrategy' = expression$EndStrategy,
    'CensoringCriteria' = expression$CensoringCriteria,
    'CollapseSettings' = expression$CollapseSettings,
    'CensorWindow' = expression$CensorWindow,
    'cdmVersionRange' = expression$cdmVersionRange
  )
  if (is.null(circe$AdditionalCriteria)) {
    circe$AdditionalCriteria <- NULL
  }
  if (is.null(circe$EndStrategy)) {
    circe$EndStrategy <- NULL
  }

  circeJson <- RJSONIO::toJSON(circe, digits = 23)

  return(circeJson)
}


# Function to get a cohort from atlas by Id
get_cohort_from_atlas <- function(cohortId,
                               authFirst = FALSE,
                               keyringName = "atlas",
                               keyringPassword = "ohdsi") {
  # check to unlock keyring
  maybeUnlockKeyring(keyringName = keyringName, keyringPassword = keyringPassword)

  baseUrl <- keyring::key_get("baseUrl", keyring = keyringName)


  if (authFirst) {

    cli::cat_bullet(
      glue::glue("Authorizing Web Api connection for {crayon::cyan(baseUrl)}"),
      bullet = "pointer",
      bullet_col = "yellow"
    )

    ROhdsiWebApi::authorizeWebApi(
      baseUrl = baseUrl,
      authMethod = keyring::key_get("authMethod", keyring = keyringName),
      webApiUsername = keyring::key_get("user", keyring = keyringName),
      webApiPassword = keyring::key_get("password", keyring = keyringName)
    )
  }

  cohort <- ROhdsiWebApi::getCohortDefinition(cohortId = cohortId, baseUrl = baseUrl)
  tb <- tibble::tibble(
    id = cohort$id,
    name = cohort$name,
    expression = format_cohort_expression(cohort$expression),
    saveName = glue::glue("{id}_{name}") |> snakecase::to_snake_case()
  )

  return(tb)
}


get_cs_from_atlas <- function(id,
                              authFirst = FALSE,
                              keyringName = "atlas",
                              keyringPassword = "ohdsi") {

  # check to unlock keyring
  maybeUnlockKeyring(keyringName = keyringName, keyringPassword = keyringPassword)

  baseUrl <- keyring::key_get(
    service = "baseUrl",
    keyring = keyringName
  )

  if (authFirst) {

    cli::cat_bullet(
      glue::glue("Authorizing Web Api connection for {crayon::cyan(baseUrl)}"),
      bullet = "pointer",
      bullet_col = "yellow"
    )

    ROhdsiWebApi::authorizeWebApi(
      baseUrl = baseUrl,
      authMethod = keyring::key_get("authMethod", keyring = keyringName),
      webApiUsername = keyring::key_get("user", keyring = keyringName),
      webApiPassword = keyring::key_get("password", keyring = keyringName)
    )
  }

  cs <- ROhdsiWebApi::getConceptSetDefinition(conceptSetId = id, baseUrl = baseUrl)


  tb <- tibble::tibble(
    id = cs$id,
    name = cs$name,
    expression = RJSONIO::toJSON(cs$expression, digits = 23),
    saveName = glue::glue("{id}_{name}") |> snakecase::to_snake_case()
  )

  return(tb)
}


write_cohorts_to_ulysses <- function(circeJson, saveName, savePath = here::here("cohorts/json")) {

  file_name <- fs::path(savePath, saveName, ext = "json")

  readr::write_file(circeJson, file = file_name)
  cli::cat_bullet(
    glue::glue("Circe Cohort Json {crayon::magenta(saveName)} saved to: {crayon::cyan(savePath)}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  invisible(file_name)
}


write_cs_to_ulysses <- function(circeJson, saveName, savePath = here::here("cohorts/conceptSets/json")) {

  file_name <- fs::path(savePath, saveName, ext = "json")

  readr::write_file(circeJson, file = file_name)
  cli::cat_bullet(
    glue::glue("Circe ConceptSet Json {crayon::magenta(saveName)} saved to: {crayon::cyan(savePath)}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  invisible(file_name)
}

# Atlas import UI --------------------

#' Function to import Atlas cohorts into Ulysses
#' @param cohortIds the atlas ids of the cohorts you want to import
#' @param keyringName the name of the keyring to save credentials. Defaults to atlas
#' @param keyringPassword the password for the keyring to save credentials. Defaults to ohdsi
#' @return saves circe json of ids given to the cohorts/json folder of Ulysses
#' @export
importAtlasCohorts <- function(
    cohortIds,
    keyringName = "atlas",
    keyringPassword = "ohdsi"
) {

  first_cohort <- get_cohort_from_atlas(cohortId = cohortIds[1],
                                        authFirst = TRUE,
                                        keyringName = keyringName,
                                        keyringPassword = keyringPassword)

  if (length(cohortIds) > 1) {
    remaining_ids <- cohortIds[-1]
    remaining_cohorts <- purrr::map_dfr(
      remaining_ids,
      ~get_cohort_from_atlas(cohortId = .x,
                             authFirst = FALSE, # already open
                             keyringName = keyringName,
                             keyringPassword = keyringPassword)
    )
    cohort_tb <- dplyr::bind_rows(first_cohort, remaining_cohorts)
  } else {
    cohort_tb <- first_cohort
  }

  purrr::pwalk(
    cohort_tb,
    ~write_cohorts_to_ulysses(
      circeJson = ..3,
      saveName = ..4
    )
  )
  invisible(cohortIds)
}



#' Function to import Atlas concept sets into Ulysses
#' @param conceptSetIds the atlas ids of the cconcept sets you want to import
#' @param keyringName the name of the keyring to save credentials. Defaults to atlas
#' @param keyringPassword the password for the keyring to save credentials. Defaults to ohdsi
#' @return saves circe json of ids given to the cohorts/conceptSets/json folder of Ulysses
#' @export
importAtlasConceptSets <- function(
    conceptSetIds,
    keyringName = "atlas",
    keyringPassword = "ohdsi"
) {

  first_concept_set <- get_cs_from_atlas(id = conceptSetIds[1],
                                         authFirst = TRUE,
                                         keyringName = keyringName,
                                         keyringPassword = keyringPassword)

  if (length(conceptSetIds) > 1) {
    remaining_ids <- conceptSetIds[-1]
    remaining_cs <- purrr::map_dfr(
      remaining_ids,
      ~get_cs_from_atlas(id = .x,
                             authFirst = FALSE, # already open
                             keyringName = keyringName,
                             keyringPassword = keyringPassword)
    )
    cs_tb <- dplyr::bind_rows(first_concept_set, remaining_cs)
  } else {
    cs_tb <- first_concept_set
  }

  purrr::pwalk(
    cs_tb,
    ~write_cs_to_ulysses(
      circeJson = ..3,
      saveName = ..4
    )
  )
  invisible(conceptSetIds)
}




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
