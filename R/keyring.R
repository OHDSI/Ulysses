# Keyring Functions ----------------------

## Helpers ------------
set_cred <- function(cred, db, keyringName) {

  key_name <- paste(db, cred, sep = "_")
  prompt_txt <- glue::glue("Set {key_name}: ")

  keyring::key_set(
    service = key_name,
    keyring = keyringName,
    prompt = prompt_txt
  )
  invisible(key_name)
}

blurCreds <- function(item,  keyringName) {
  cred <- keyring::key_get(service = item, keyring = keyringName)
  txt <- glue::glue(item, ": ", crayon::blurred(cred))
  cli::cat_bullet(txt, bullet = "info", bullet_col = "blue")
  invisible(item)
}


checkKeyring <- function(keyringName, keyringPassword) {
  allKeyrings <- keyring::keyring_list()
  keyringName %in% allKeyrings$keyring
}


dropKeyring <- function(keyringName, keyringPassword) {

  if (keyring::keyring_is_locked(keyring = keyringName)) {
    keyring::keyring_unlock(keyring = keyringName, password = keyringPassword)
  }
  # Delete all keys from the keyring so we can delete it
  cli::cat_bullet("Delete existing keyring: ", keyringName,
                  bullet = "warning", bullet_col = "yellow")
  keys <- keyring::key_list(keyring = keyringName)
  if (nrow(keys) > 0) {
    for (i in 1:nrow(keys)) {
      # drop keys
      keyring::key_delete(keys$service[i], keyring = keyringName)
    }
  }
  # drop keyring
  keyring::keyring_delete(keyring = keyringName)

  invisible(allKeyrings)

}


setKeyring <- function(keyringName, keyringPassword) {

  cli::cat_bullet(
    "Creating a study keyring for: ", crayon::cyan(keyringName),
    bullet = "info", bullet_col = "blue"
  )
  # create keyring
  keyring::keyring_create(keyring = keyringName, password = keyringPassword)

  invisible(keyringName)
}

## UI ------------

#' Function to list default credentials
#' @description
#' This function builds the standard credential set needed for most connections. If
#' another credential is needed use `c()` to bind the character vector.
#' @export
defaultCredentials <- function() {
  creds <- c(
    "dbms", # the database dialect
    "user", # the user name for the db
    "password", # the password for the db
    "connectionString", # the connection string to access the db
    "cdmDatabaseSchema", # the database + schema (or just schema) hosting the cdm
    "vocabDatabaseSchema", # the database + schema (or just schema) hosting the vocabulary, usually same as cdm
    "workDatabaseSchema" # the database + schema (or just schema) hosting the work or scratch
  )
  return(creds)
}

#' Function to check the database credential
#' @param cred the credential to set (i.e dbms, user, connectionString)
#' @param db the database prefix for the credential
#' @param keyringName the name of the keyringName for the credential check, this will be the keyring namec
#' @param verbose toggle option to print console message
#' @export
checkDatabaseCredential <- function(cred, db, keyringName, verbose = TRUE) {

  #paste name to set full credential
  key_name <- paste(db, cred, sep = "_")

  if (verbose) {
    cli::cat_bullet("Check that credential ", crayon::green(key_name), " is correct.",
                    bullet = "warning", bullet_col = "yellow")
  }

  #print credential
  blurCreds(item = key_name, keyringName = keyringName)

  invisible(key_name)
}

#' Function to set single database credential
#' @param cred the credential to set (i.e dbms, user, connectionString)
#' @param db the database prefix for the credential
#' @param keyringName the name of the keyringName for the credential set, this will be the keyring namec
#' @param keyringPasssword the password for the keyring.
#' @param forceCheck a toggle that will print blurred credentials to check credential
#' @export
setDatabaseCredential <- function(cred, db, keyringName, keyringPassword, forceCheck = TRUE) {

  if (keyring::keyring_is_locked(keyring = keyringName)) {
    keyring::keyring_unlock(keyring = keyringName, password = keyringPassword)
  }

  cli::cat_bullet("Input your credentials in the dialog box",
                  bullet = "warning", bullet_col = "yellow")

  set_cred(cred = cred, db = db, keyringName = keyringName)

  if (forceCheck) {
    checkDatabaseCredential(cred = cred, db = db, keyringName = keyringName)
  }
  invisible(key_name)
}

#' Function to set all database credentials
#' @param cred a vector of credentials to set (i.e dbms, user, connectionString). See defaultCredentials on building set
#' @param db the database prefix for the credential
#' @param keyringName the name of the keyringName where the credential will be set
#' @param keyringPasssword the password for the keyring.
#' @param forceCheck a toggle that will print blurred credentials to check credential
#' @export
setAllDatabaseCredentials <- function(cred, db, keyringName, keyringPassword, forceCheck = TRUE) {

  if (keyring::keyring_is_locked(keyring = keyringName)) {
    keyring::keyring_unlock(keyring = keyringName, password = keyringPassword)
  }

  cli::cat_bullet("Input your credentials in the dialog box",
                  bullet = "warning", bullet_col = "yellow")
  purrr::walk(
    cred,
    ~set_cred(cred = .x, db = db, keyringName = keyringName)
  )

  if (forceCheck) {
    purrr::walk(key_names, ~blurCreds(item = .x,  keyringName = keyringName, verbose = FALSE))
  }
  invisible(cred)

}



#' Function to set study keyring
#' @param keyringName the name of the keyring, this should be the reponame for the study
#' @param keyringPassword a password to access the study keyring
#' @export
setStudyKeyring <- function(keyringName, keyringPassword) {

  # check if keyring exists
  check <- checkKeyring(keyringName = keyringName, keyringPassword = keyringPassword)

  if (check) {
    ask1 <- usethis::ui_yeah(
      "This keyring already exists. Do you want to drop the keyring and its contents?"
    )
    if (ask1) {
      dropKeyring(keyringName = keyringName, keyringPassword = keyringPassword)
      #Set keyring
      setKeyring(keyringName = keyringName, keyringPassword = keyringPassword)
    } else{
      cli::cat_bullet("Keeping keyring ", crayon::cyan(keyringName), ". ")
    }
  } else{
    #Set keyring
    setKeyring(keyringName = keyringName, keyringPassword = keyringPassword)
  }
  invisible(keyringName)
}

