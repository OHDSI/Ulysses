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
