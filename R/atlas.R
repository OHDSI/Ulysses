# Atlas Connection ---------------


setAtlasCredentials <- function(keyringName = "atlas",
                                keyringPassword = "ohdsi") {
  creds <- c("baseurl", "authMethod", "user", "password")

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
    ~set_cred(
      cred = .x,
      db = db,
      keyringName = keyringName)
  )

  # check credentials
  purrr::walk(
    creds,
    ~checkDatabaseCredential(
      cred = .x,
      db = db,
      keyringName = keyringName,
      verbose = FALSE)
  )

  invisible(creds)

}

# Function to get a cohort from atlas by Id
getAtlasCohortById <- function(cohortId,
                               authFirst = FALSE,
                               keyringName = "atlas",
                               keyringPassword = "ohdsi") {
  # check to unlock keyring
  maybeUnlockKeyring(keyringName = keyringName, keyringPassword = keyringPassword)

  baseUrl <- keyring::key_get("atlas_baseurl", keyring = "atlas")

  if (authFirst) {
    ROhdsiWebApi::authorizeWebApi(
      baseUrl = baseUrl,
      authMethod = keyring::key_get("atlas_authMethod", keyring = "atlas"),
      webApiUsername = keyring::key_get("atlas_user", keyring = "atlas"),
      webApiPassword = keyring::key_get("atlas_password", keyring = "atlas")
    )
  }

  c <- ROhdsiWebApi::getCohortDefinition(cohortId = cohortId, baseUrl = baseUrl)

  return(c)
}


###############################
# Alternative Style using httr2
###############################


setWebApiConnection <- function(baseUrl, authMethod, user, password) {

  token <- getAuthBearerToken(baseUrl = baseUrl,
                              authMethod = authMethod,
                              user = user,
                              password = password)


  webApiConnectionDetails <- list(
    'baseUrl' = baseUrl,
    'authMethod' = authMethod
  )
  userExpression <- rlang::enquo(user)
  passwordExpression <- rlang::enquo(password)
  bearerToken <- rlang::enquo(token)

  webApiConnectionDetails$user <- function() rlang::eval_tidy(userExpression)
  webApiConnectionDetails$password <- function() rlang::eval_tidy(passwordExpression)
  webApiConnectionDetails$bearerToken <- function() rlang::eval_tidy(bearerToken)

  class(webApiConnectionDetails) <- "WebApiConnectionDetails"
  return(webApiConnectionDetails)
}


getAuthBearerToken <- function(baseUrl, authMethod, user, password) {

  authUrl <- paste0(baseUrl, glue::glue("user/login/{authMethod}"))

  req <- httr2::request(authUrl) |>
    httr2::req_body_form(
      login = user,
      password = password
    )

  bearerToken <- httr2::req_perform(req)$headers$Bearer

  return(bearerToken)
}

# function to pull any atlas asset by id
req_id <- function(id, category, webApiConnectionDetails) {

  fixUrl <- gsub("/$", "", webApiConnectionDetails$baseUrl)
  req <- glue::glue("{fixUrl}/{category}/{id}") |>
    httr2::request() |>
    httr2::req_auth_bearer_token(token = webApiConnectionDetails$bearerToken())
  return(req)
}

getCohortById <- function(cohortId, webApiConnectionDetails) {

  # generic to pull id
  req <- req_id(
    id = cohortId,
    category = "cohortdefinition",
    webApiConnectionDetails = webApiConnectionDetails
  )

  resp <- httr2::req_perform(req = req) # TODO add some sort of error messaging
  tt <- httr2::resp_body_json(resp)
  #TODO fix dates and json
  return(tt)
}

getConceptSetById <- function(conceptId, webApiConnectionDetails) {

  # generic to pull id
  req <- req_id(
    id = conceptId,
    category = "conceptset",
    webApiConnectionDetails = webApiConnectionDetails
  )

  resp <- httr2::req_perform(req = req) # TODO add some sort of error messaging
  tt <- httr2::resp_body_json(resp)
  #TODO fix dates and json
  return(tt)

}



# TODO fix this probably a post of some kind
getCohortByTag <- function(tag, webApiConnectionDetails) {


  fixUrl <- gsub("/$", "", webApiConnectionDetails$baseUrl)
  req <- glue::glue("{fixUrl}/cohortdefinition/byTags") |>
    httr2::request() |>
    httr2::req_auth_bearer_token(token = webApiConnectionDetails$bearerToken())

  resp <- httr2::req_perform(req = req) # TODO add some sort of error messaging
  tt <- httr2::resp_body_json(resp)
  #TODO fix dates and json
  return(tt)
}
