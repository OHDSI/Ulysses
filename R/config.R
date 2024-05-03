config_check <- function() {
  pp <- usethis::proj_path()

  fs::path(pp, "config.yml") %>%
    fs::file_exists() %>%
    unname()
}

#' check if config file is in ohdsi study
#' @export
checkConfig <- function() {

  #get project path
  check <- config_check()

  if (check) {

    pp <- usethis::proj_path() %>%
      fs::path("config.yml")

    cli::cat_bullet("config.yml exists in ", crayon::green(pp),
                    bullet = "info", bullet_col = "blue")
    openConfig <- usethis::ui_yeah("Would you like to edit config.yml?")

    if (openConfig) {
      rstudioapi::navigateToFile(pp)
    }

  } else {
    cli::cat_bullet("config.yml does not exist",
                    bullet = "warning", bullet_col = "yellow")

    txt <- glue::glue(
      "`Ulysses::initConfig()` "
    )
    cli::cat_line("To create config.yml edit and run function:\n\n  ", crayon::red(txt), "\n")
    cli::cat_line()
  }
  invisible(check)

}



config_block_text <- function(block, database, title, dbms, user,
                              password, connectionString, cdmDatabaseSchema, resultsDatabaseSchema,
                              workDatabaseSchema, tempEmulationSchema, cohortTable) {

  txt <- glue::glue(
    "\n\n# {title} Credentials\n
{block}:
  databaseName: {database}
  dbms: {dbms}
  user: {user}
  password: {password}
  connectionString: {connectionString}
  cdmDatabaseSchema: {cdmDatabaseSchema}
  resultsDatabaseSchema: {resultsDatabaseSchema}
  vocabDatabaseSchema: {cdmDatabaseSchema}
  workDatabaseSchema: {workDatabaseSchema}
  tempEmulationSchema: {tempEmulationSchema}
  cohortTable: {cohortTable}
  \n\n")
  return(txt)
}

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
initCredentials <- function(
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
    glue::glue("Initialized credentials file. Saved to: {crayon::cyan(pth)}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

}

#' Function to open the credentials table to edit or peak
#' @param credsPath the path to shhh.csv
#' @return tibble of credentials
#' @export
openCredentials <- function(credsPath = fs::path_home("shhh.csv")) {

  creds <- readr::read_csv(credsPath, show_col_types = FALSE)
  cli::cat_bullet(
    glue::glue("Opening credentials in {crayon::cyan(credsPath)}"),
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
importCredentialsToConfig <- function(credFile = fs::path_home("shhh.csv"),
                                      projectPath = here::here(), open = TRUE) {

  # import credentials
  creds <- readr::read_csv(file = credFile, show_col_types = FALSE)

  # check _study.yml for loaded databases
  studyMeta <- retrieveStudySettings(projectPath)$study
  dataSources <- studyMeta$about$`data-sources`

  # subset creds with loaded Data Sources
  dt <- creds %>%
    dplyr::filter(
      db_title %in% dataSources
    )
  # make cred text for config file
  config_txt <- purrr::pmap_chr(
    dt,
    ~config_block_text(
      block = ..1,
      database = ..2,
      title = ..3,
      dbms = ..4,
      user = ..9,
      password = ..10,
      connectionString = ..5,
      cdmDatabaseSchema = ..6,
      resultsDatabaseSchema = ..8,
      workDatabaseSchema = ..7,
      tempEmulationSchema = ..7,
      cohortTable = paste0("cohort_", ..1)
    )
  )

  header <- glue::glue(" # Config File for {studyMeta$title}\n
default:
  projectName: {studyMeta$id}
  ")

  full_txt <- c(header, config_txt)
  readr::write_lines(full_txt, file = fs::path(projectPath, "config.yml"))

  cli::cat_bullet("Initializing config.yml using imported credentials",
                  bullet = "tick", bullet_col = "green")


  if (open) {

    cli::cat_bullet("Check config.yml",
                    bullet = "bullet", bullet_col = "red")

    rstudioapi::navigateToFile(file = fs::path(projectPath, "config.yml"))
  }

  invisible(full_txt)

}


#' Function to create a config.yml file
#' @param block the name of the config block, defaults to BlockName
#' @param database the name of the database for the block, default to DatabaseName
#' @param withKeyring should the config file use keyring, default FALSE
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
initConfig <- function(block = "BlockName",
                       database = "DatabaseName",
                       withKeyring = FALSE,
                       projectPath = here::here(),
                       open = TRUE) {

  # retrieve study meta
  studyMeta <- retrieveStudySettings(projectPath = projectPath)$study


  data <- rlang::list2(
    'Title' = studyMeta$title,
    'ID' = studyMeta$id,
    'Cohort' = paste(studyMeta$id, database, sep = "_"),
    'Block' = block,
    'Database' = database
  )

  if (withKeyring) {
    template_file <- "config_keyring.yml"
  } else {
    template_file <- "config_raw.yml"
  }

  usethis::use_template(
    template = template_file,
    save_as = fs::path("config.yml"),
    data = data,
    open = open,
    package = "Ulysses")

  usethis::use_git_ignore(ignores = "config.yml")

  invisible(data)
}



#' Add a line to the config file
#' @param block the name of the config block
#' @param database the name of the database for the block, default to block name
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
addConfig <- function(block, database = block, projectPath = here::here(), open = TRUE) {


  check <- config_check()

  if (check) {
    projFile <- list.files(projectPath, pattern = ".Rproj", full.names = TRUE)
    projName <- basename(tools::file_path_sans_ext(projFile))
    cohortTable <- paste(projName, database, sep = "_")

    config_block_txt <- glue::glue(
      "\n\n# {block} Credentials\n
{block}:
  databaseName: {database}
  dbms: !expr keyring::key_get('{block}_dbms', keyring = '{projName}')
  user: !expr keyring::key_get('{block}_user', keyring = '{projName}')
  password: !expr keyring::key_get('{block}_password', keyring = '{projName}')
  connectionString: !expr keyring::key_get('{block}_connectionString', keyring = '{projName}')
  cdmDatabaseSchema: !expr keyring::key_get('{block}_cdmDatabaseSchema', keyring = '{projName}')
  vocabDatabaseSchema: !expr keyring::key_get('{block}_vocabDatabaseSchema', keyring = '{projName}')
  workDatabaseSchema: !expr keyring::key_get('{block}_workDatabaseSchema', keyring = '{projName}')
  cohortTable: {cohortTable}
  ")
    save_as <- fs::path(projectPath, "config.yml")
    write_utf8(path = save_as,
                             lines = config_block_txt,
                             append = TRUE)
    if (open) {
      rstudioapi::navigateToFile(save_as)
    }
    cli::cat_bullet("Added block ", crayon::green(block), " to config.yml",
                    bullet = "info", bullet_col = "blue")

  } else {
    cli::cat_bullet("config.yml does not exist",
                    bullet = "warning", bullet_col = "yellow")
    txt <- glue::glue(
      "`Ulysses::makeConfig(block = {block}, database = {database})` "
    )

    cli::cat_line("To create config.yml edit and run function:\n\n  ", crayon::red(txt), "\n")
  }

  cli::cat_bullet("Restart R session to implement changes to config.yml",
                  bullet = "info", bullet_col = "blue")
  invisible(check)

}
