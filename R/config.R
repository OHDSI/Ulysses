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
      "`Ulysses::makeConfig(block = '[block_name]', database = '[database_name]')` "
    )
    cli::cat_line("To create config.yml edit and run function:\n\n  ", crayon::red(txt), "\n")
    cli::cat_line()
  }
  invisible(check)

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
  user: !expr keyring::key_get('{block}_user', keyring = '{projName}'),
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
