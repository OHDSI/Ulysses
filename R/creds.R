createSnowflakeConnectionYml <- function(configFolderPath,
                                         user,
                                         password = NULL,
                                         database,
                                         warehouse,
                                         role = NULL,
                                         schema = NULL,
                                         privateKeyFilePath,
                                         privateKeyPasswordPath
) {

  configFolderPath <- configFolderPath |>
    fs::dir_create()


  # checkmates ----

  checkmate::assertDirectoryExists(x = configFolderPath, access = "w")

  checkmate::assertString(x = snowflakeDatabase, min.chars = 1)
  checkmate::assertString(x = snowflakeWarehouse, min.chars = 1)

  checkmate::assertFileExists(x = privateKeyFilePath, access = "r")
  checkmate::assertFileExists(x = privateKeyPasswordPath, access = "r")

  configFilePath <- fs::path(configFolderPath, "connectionConfig", ext = "yml")
  privateKeyPassword <- readr::read_file(privateKeyPasswordPath)

  connectionConfig <- list(
    default = list(
      dbms = "snowflake",
      user = user,
      password = privateKeyPassword,
      db = database,
      warehouse = warehouse,
      role = role,
      schema = schema,
      privateKeyFile = privateKeyFilePath,
      privateKeyPassword = privateKeyPassword
    )
  )

  yaml::write_yaml(x = connectionConfig, file = configFilePath)

  invisible(connectionConfig)
}

createSnowflakeConnectionString <- function(connectionConfigYml) {

  connectionConfigList <- yaml::read_yaml(file = connectionConfigYml)


  connectionParamsString <- purrr::map2_chr(
    names(connectionConfigList),
    connectionConfigList,
    ~sprintf("%s=%s", .x, .y)
  ) |>
    glue::glue_collapse(sep = "&")

  connectionString <- glue::glue(
    "jdbc:snowflake://{accountId}.snowflakecomputing.com?{connectionParamsString}"
  ) |>
    as.character()
}
