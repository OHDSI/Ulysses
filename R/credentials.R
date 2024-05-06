# helpers -------------------

build_config <- function(studyMeta, config_txt, projectPath) {
  header <- glue::glue(" # Config File for {studyMeta$title}\n
default:
  projectName: {studyMeta$id}
  ")

  full_txt <- c(header, config_txt)
  readr::write_lines(full_txt, file = fs::path(projectPath, "config.yml"))

  cli::cat_bullet("Initializing config.yml using imported credentials",
                  bullet = "tick", bullet_col = "green")
  invisible(full_txt)
}

config_block_text_from_shhh <- function(
    block, database, dbms, user,
    password, connectionString, cdmDatabaseSchema,
    workDatabaseSchema, tempEmulationSchema, cohortTable
) {

  txt <- glue::glue(
    "\n\n# {database} Credentials\n
{block}:
  databaseName: {database}
  dbms: {dbms}
  user: {user}
  password: {password}
  connectionString: {connectionString}
  cdmDatabaseSchema: {cdmDatabaseSchema}
  vocabDatabaseSchema: {cdmDatabaseSchema}
  workDatabaseSchema: {workDatabaseSchema}
  tempEmulationSchema: {tempEmulationSchema}
  cohortTable: {cohortTable}
  \n\n")
  return(txt)
}


config_block_text_from_keyring <- function(
    db_id, cohortTable
) {

  txt <- glue::glue(
    "\n\n# {db_id} Credentials\n
{db_id}:
  databaseName: {db_id}
  dbms: !expr keyring::key_get('dbms', keyring = '{db_id}')
  user: !expr keyring::key_get('user', keyring = '{db_id}')
  password: !expr keyring::key_get('password', keyring = '{db_id}')
  connectionString: !expr keyring::key_get('connectionString', keyring = '{db_id}')
  cdmDatabaseSchema: !expr keyring::key_get('cdmDatabaseSchema', keyring = '{db_id}')
  workDatabaseSchema: !expr keyring::key_get('workDatabaseSchema', keyring = '{db_id}')
  tempEmulationSchema: !expr keyring::key_get('tempEmulationSchema', keyring = '{db_id}')
  cohortTable: {cohortTable}
  \n\n")
  return(txt)
}




# keyring option ------------------------------

check_keyring_exists <- function(keyringName) {
  keys <- keyring::keyring_list()$keyring
  check <- keyringName %in% keys
  return(check)
}

#' Function to set multi credentials
#' @param db_id the database id to distinguish the set of credentials
#' @param keyringPassword the password for the keyring. Defaults to ulysses.
#' @param forceCheck a toggle that will print blurred credentials to check credential
#' @export
initCredentialsKeyring <- function(db_id, keyringPassword = "ulysses", forceCheck = TRUE) {

  # setup
  keyringName <- db_id
  creds <- c("dbms", "user", "password", "connectionString",
             "cdmDatabaseSchema", "workDatabaseSchema", "tempEmulationSchema")

  if (!check_keyring_exists(keyringName)) {
    setStudyKeyring(keyringName = keyringName, keyringPassword = keyringPassword)
  }

  if (keyring::keyring_is_locked(keyring = keyringName)) {
    keyring::keyring_unlock(keyring = keyringName, password = keyringPassword)
  }

  cli::cat_bullet("Input your credentials in the dialog box",
                  bullet = "warning", bullet_col = "yellow")
  purrr::walk(
    creds,
    ~set_cred2(cred = .x, db = keyringName)
  )

  if (forceCheck) {
    purrr::walk(creds, ~blurCreds(item = .x, keyringName = keyringName))
  }

  invisible(creds)

}


#' Function to initialize config.yml using keyring
#' @param dbIds the dbIds to add to the config file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
initConfigUsingKeyring <- function(dbIds,
                                   projectPath = here::here(),
                                   open = TRUE) {


  # get study name
  studyMeta <- retrieveStudySettings(projectPath)$study
  studyId <- studyMeta$id

  creds <- tibble::tibble(
    db_id = dbIds
  ) |>
    dplyr::mutate(
      cohortTable = glue::glue("{studyId}_{db_id}")
    )

  # make cred text for config file
  config_txt <- purrr::pmap_chr(
    creds,
    ~config_block_text_from_keyring(
      db_id = ..1,
      cohortTable = ..2
    )
  )

  full_txt <- build_config(studyMeta, config_txt, projectPath)
  usethis::use_git_ignore(ignores = "config.yml")

  if (open) {

    cli::cat_bullet("Check config.yml",
                    bullet = "bullet", bullet_col = "red")

    rstudioapi::navigateToFile(file = fs::path(projectPath, "config.yml"))
  }

  invisible(full_txt)

}

# shhh option ------------------------------

#' Function to start a credential table
#' @param db_id an id or shortName for a database. Please use snakecase
#' @param db_full_name the full name of the database. If not specified defaults to db_id
#' @param dbms the dbms for your database. Can be either redshift, postgres, snowflake, sql server or oracle
#' @param user the user name to connect to the database
#' @param password the password to connect to the databse
#' @param connection_string a jdbc connection string to use with DatabaseConnector
#' @param cdm_database_schema the cdm database schema of the datbase you want to use
#' @param work_database_schema a scratch schema the user has read and write access to in order to build cohort tables
#' @param temp_emulation_schema a schema required for oracle and snowflake to make temp tables.
#' @return writes a csv to your home directory called shhh.csv storing your database credentials
#' @export
initCredentialsTable <- function(
    db_id,
    db_full_name = db_id,
    dbms,
    user,
    password,
    connection_string,
    cdm_database_schema,
    work_database_schema,
    temp_emulation_schema = work_database_schema) {


  checkmate::check_choice(dbms, choices = c("redshift", "postgres", "snowflake", "sql server", "oracle"))


  tb <- tibble::tibble(
    'db_id' = db_id,
    'db_full_name' = db_full_name,
    'dbms' = dbms,
    'user' = user,
    'password' = password,
    'connection_string' = connection_string,
    'cdm_database_schema' = cdm_database_schema,
    'work_database_schema' = work_database_schema,
    'temp_emulation_schema' = temp_emulation_schema
  )

  pth <- fs::path_home()

  readr::write_csv(
    x = tb,
    file = fs::path(pth, "shhh.csv")
  )

  cli::cat_bullet(
    glue::glue("Initialized credentials table in csv file. Saved to: {crayon::cyan(pth)}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

}

check_shhh <- function(credsPath = fs::path_home("shhh.csv")) {

  # check shhh
  check <- fs::file_exists(credsPath)
  if (!check) {
    cli::cli_abort(
      c(
        "File {credsPath} does not exist!",
        "x" = "You have not initialized a shhh.csv credential table."
      )
    )
  }
  return(check)
}


#' Function to open the credentials table to edit or peak
#' @param credFile the path to shhh.csv
#' @return tibble of credentials
#' @export
openCredentials <- function(credFile = fs::path_home("shhh.csv")) {

  check_shhh(credFile)
  creds <- readr::read_csv(credFile, show_col_types = FALSE)
  cli::cat_bullet(
    glue::glue("Opening credentials in {crayon::cyan(credFile)}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  return(creds)

}




#' Function to import credentials in stored csv to study config.yml
#' @param credFile a credential.csv file stored in a secure location
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
initConfigUsingTable <- function(credFile = fs::path_home("shhh.csv"),
                                      projectPath = here::here(), open = TRUE) {

  # get study name
  studyMeta <- retrieveStudySettings(projectPath)$study
  studyId <- studyMeta$id

  # import credentials
  check_shhh(credFile)
  creds <- readr::read_csv(file = credFile, show_col_types = FALSE) |>
    dplyr::mutate(
      cohort_table = glue::glue("{studyId}_{db_id}"),
      password = dplyr::if_else(is.na(password), "''", password)
    )

  # make cred text for config file
  config_txt <- purrr::pmap_chr(
    creds,
    ~config_block_text_from_shhh(
      block = ..1,
      database = ..2,
      dbms = ..3,
      user = ..4,
      password = ..5,
      connectionString = ..6,
      cdmDatabaseSchema = ..7,
      workDatabaseSchema = ..8,
      tempEmulationSchema = ..9,
      cohortTable = ..10
    )
  )

  full_txt <- build_config(studyMeta, config_txt, projectPath)
  usethis::use_git_ignore(ignores = "config.yml")

  if (open) {

    cli::cat_bullet("Check config.yml",
                    bullet = "bullet", bullet_col = "red")

    rstudioapi::navigateToFile(file = fs::path(projectPath, "config.yml"))
  }

  invisible(full_txt)

}
