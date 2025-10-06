.setClass <- function(private, key, value, class, nullable = FALSE) {
  checkmate::assert_class(x = value, classes = class, null.ok = nullable)
  private[[key]] <- value
  invisible(private)
}

.setString <- function(private, key, value, naOk = FALSE) {
  checkmate::assert_string(x = value, na.ok = naOk, min.chars = 1, null.ok = FALSE)
  private[[key]] <- value
  invisible(private)
}

# Study Options Class -------------
UlyssesStudy <- R6::R6Class(
  classname = "UlyssesStudy",
  public = list(
    initialize = function(repoName,
                          repoFolder,
                          studyMeta,
                          execOptions,
                          inputOptions = NULL,
                          gitRemote = NULL,
                          renvLock = NULL
    ) {

      checkmate::assert_string(x = repoName, min.chars = 1)
      private[[".repoName"]] <- repoName

      checkmate::assert_string(x = repoFolder, min.chars = 1)
      private[[".repoFolder"]] <- repoFolder

      .setClass(private = private, key = ".studyMeta", value = studyMeta, class = "StudyMeta")
      .setClass(private = private, key = ".execOptions", value = execOptions, class = "ExecOptions")
      .setClass(private = private, key = ".inputOptions", value = inputOptions, class = "InputOptions", nullable = TRUE)


      checkmate::assert_string(x = gitRemote, null.ok = TRUE)
      private[[".gitRemote"]] <- gitRemote

      checkmate::assert_string(x = renvLock, null.ok = TRUE)
      private[[".renvLock"]] <- renvLock
    },

    initUlyssesRepo = function(verbose, openProject) {

      # get vars
      repoName <- private$.repoName
      repoFolder <- private$.repoFolder

      if (verbose) {
        notification("Step 1: Creating R Project")
      }
      # make a path to repo
      repoName <- private$.repoName
      repoFolder <- private$.repoFolder

      repoPath <- fs::path(repoFolder, repoName) |>
        fs::path_expand() |>
        fs::dir_create() # make repo if it doesnt exist

      ## Make local project
      usethis::local_project(repoPath, force = TRUE)
      private$.initRProj()

      # Step 2: add standard ulysses folders
      if (verbose) {
        notification("Step 2: Adding Standard Ulysses Folders")
      }

      folders <- listDefaultFolders()

      pp <- fs::path("./", folders) |>
        fs::dir_create(recurse = TRUE)

      # Step 3: make default files
      if (verbose) {
        notification("Step 3: Adding Standard Ulysses Files")
      }

      private$.initReadMe() #init read me
      private$.initNews() # init news
      private$.initExecConfigFile() # init exec config
      private$.initSourceConfigFile() # init source config

      if (openProject) {
        notification("Opening project in new session")
        rstudioapi::openProject(repoPath, newSession = TRUE)
      }

      invisible(repoPath)
    }
  ),
  private = list(
    .repoName = NULL,
    .repoFolder = NULL,
    .studyMeta = NULL,
    .execOptions = NULL,
    .inputOptions = NULL,
    .gitRemote = NULL,
    .renvLock = NULL,

    #functions to build Files
    .initRProj = function() {

      repoName <- private$.repoName
      repoFolder <- private$.repoFolder
      repoPath <- fs::path(repoFolder, repoName) |>
        fs::path_expand()

      projLines <- fs::path_package("Ulysses", "templates/rproj.txt") |>
        readr::read_file()

      projFile <- fs::path(repoPath, repoName, ext = "Rproj")
      readr::write_file(
        x = projLines,
        file = fs::path(projFile)
      )
      actionItem(glue::glue_col("Initializing {green {repoName}.Rproj}"))
      usethis::use_git_ignore(".Rproj.user")
      invisible(projFile)

    },

    .initReadMe = function() {

      sm <- private$.studyMeta
      repoName <- private$.repoName
      repoFolder <- private$.repoFolder
      repoPath <- fs::path(repoFolder, repoName) |>
        fs::path_expand()

      initReadMeFn(sm = sm, repoName = repoName, repoPath = repoPath)
    },

    .initNews = function() {
      repoName <- private$.repoName
      repoFolder <- private$.repoFolder
      repoPath <- fs::path(repoFolder, repoName) |>
        fs::path_expand()

      initNewsFn(repoName = repoName, repoPath = repoPath)
    },

    .initExecConfigFile = function() {
      repoName <- private$.repoName
      repoFolder <- private$.repoFolder
      repoPath <- fs::path(repoFolder, repoName) |>
        fs::path_expand()
      exOp <- private$.execOptions

      initExecConfigFileFn(repoName = repoName, repoPath = repoPath, exOp = exOp)
    },

    .initSourceConfigFile = function() {
      repoName <- private$.repoName
      repoFolder <- private$.repoFolder
      repoPath <- fs::path(repoFolder, repoName) |>
        fs::path_expand()
      exOp <- private$.execOptions

      initSourceConfigFileFn(repoName = repoName, repoPath = repoPath, exOp = exOp)
    }
  ),
  active = list(

    repoName = function(value) {
      if(missing(value)) {
        sm <- private$.repoName
        return(sm)
      }
      checkmate::assert_string(x = value, min.chars = 1)
      private[[".repoName"]] <- value
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('repoName')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },

    repoFolder = function(value) {
      if(missing(value)) {
        sm <- private$.repoFolder
        return(sm)
      }
      checkmate::assert_string(x = value, min.chars = 1)
      private[[".repoFolder"]] <- value
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('repoFolder')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    studyMeta = function(value) {
      if(missing(value)) {
        sm <- private$.studyMeta
        return(sm)
      }
      .setCkass(private = private, key = ".studyMeta", value = value, class = "StudyMeta")
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('studyMeta')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    execOptions = function(value) {
      if(missing(value)) {
        sm <- private$.execOptions
        return(sm)
      }
      .setCkass(private = private, key = ".execOptions", value = value, class = "ExecOptions")
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('execOptions')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    inputOptions = function(value) {
      if(missing(value)) {
        sm <- private$.inputOptions
        return(sm)
      }
      .setCkass(private = private, key = ".inputOptions", value = value, class = "InputOptions")
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('inputOptions')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    gitRemote = function(value) {
      if(missing(value)) {
        sm <- private$.gitRemote
        return(sm)
      }
      .setString(private = private, key = ".gitRemote", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('gitRemote')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    renvLock = function(value) {
      if(missing(value)) {
        sm <- private$.renvLock
        return(sm)
      }
      .setString(private = private, key = ".renvLock", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('renvLock')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    }
  )
)

# Sub options classes ------------

# sub class for contributors in study meta
ContributorLine <- R6::R6Class(
  classname = "ContributorLine",
  public = list(
    initialize = function(name, email, role) {
      .setString(private = private, key = ".name", value = name)
      .setString(private = private, key = ".email", value = email)
      .setString(private = private, key = ".role", value = role)
    },
    printContributorLine = function() {
      txt <- glue::glue("Name: {private$.name} | Email: {private$.email} | Role: {private$.role}")
      return(txt)
    }
  ),
  private = list(
    .name = NA_character_,
    .email = NA_character_,
    .role = NA_character_
  ),
  active = list(
    name = function(value) {
      if(missing(value)) {
        sm <- private$.name
        return(sm)
      }
      .setString(private = private, key = ".name", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('Contributor Name')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    email = function(value) {
      if(missing(value)) {
        sm <- private$.email
        return(sm)
      }
      .setString(private = private, key = ".email", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('Contributor Email')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    role = function(value) {
      if(missing(value)) {
        sm <- private$.role
        return(sm)
      }
      .setString(private = private, key = ".role", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('Contributor Role')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    }
  )
)

StudyMeta <- R6::R6Class(
  classname = "StudyMeta",
  public = list(
    initialize = function(studyTitle,
                          therapeuticArea,
                          studyType,
                          contributors,
                          studyLinks = NULL,
                          studyTags = NULL) {
      .setString(private = private, key = ".studyTitle", value = studyTitle)
      .setString(private = private, key = ".therapeuticArea", value = therapeuticArea)
      .setString(private = private, key = ".studyType", value = studyType)

      checkmate::assert_character(x = studyLinks, null.ok = TRUE)
      if (!is.null(studyLinks)) {
        private[[".studyLinks"]] <- studyLinks
      }


      checkmate::assert_character(x = studyTags, null.ok = TRUE)
      if (!is.null(studyTags)) {
        private[[".studyTags"]] <- studyTags
      }

      checkmate::assert_list(x = contributors, min.len = 1, types = "ContributorLine")
      private[[".contributors"]] <- contributors

    },

    listContributors = function() {
      ctbs <- private$.contributors
      ctbsList <- purrr::map(
        private$.contributors,
        ~glue::glue("- {.x$role}: {.x$name} (email: {.x$email})")
      ) |>
        glue::glue_collapse(sep = "\n")
      ctbs2 <- c("## Contributors", ctbsList) |> glue::glue_collapse(sep = "\n\n")

      return(ctbs2)
    },

    listStudyTags = function() {
      tags <- private$.studyTags
      if (length(tags) > 0) {
        tagList <- purrr::map(
          private$.studyTags,
          ~glue::glue("\t* {.x}")
        ) |>
          glue::glue_collapse(sep = "\n")
        tagList <- c("- Tags", tagList) |> glue::glue_collapse(sep = "\n")
      } else {
        tagList <- "- Tags (Please Add)"
      }

      return(tagList)
    },

    listStudyLinks = function() {
      links <- private$.studyLinks
      if (length(links) > 0) {
        linksList <- purrr::map(
          private$.studyLinks,
          ~glue::glue("\t* {.x}")
        ) |>
          glue::glue_collapse(sep = "\n")
        linksList <- c("## Resources", links) |> glue::glue_collapse(sep = "\n\n")
      } else {
        linksList <- c("## Resources", "<!-- Place study Links as needed -->") |> glue::glue_collapse(sep = "\n\n")
      }

      return(linksList)
    }

  ),
  private = list(
    .studyTitle = NULL,
    .therapeuticArea = NULL,
    .studyType = NULL,
    .contributors = NULL,
    .studyLinks = NULL,
    .studyTags = NULL
  ),
  active = list(
    studyTitle = function(value) {
      if(missing(value)) {
        sm <- private$.studyTitle
        return(sm)
      }
      .setString(private = private, key = ".studyTitle", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('studyTitle')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    therapeuticArea = function(value) {
      if(missing(value)) {
        sm <- private$.therapeuticArea
        return(sm)
      }
      .setString(private = private, key = ".therapeuticArea", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('therapeuticArea')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    studyType = function(value) {
      if(missing(value)) {
        sm <- private$.studyType
        return(sm)
      }
      .setString(private = private, key = ".studyType", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('studyType')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    studyTags = function(value) {
      if(missing(value)) {
        sm <- private$.studyTags
        return(sm)
      }
      checkmate::assert_character(x = value)
      private[[".studyTags"]] <- value
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('Study Tags')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    studyLinks = function(value) {
      if(missing(value)) {
        sm <- private$.studyLinks
        return(sm)
      }
      checkmate::assert_character(x = value)
      private[[".studyLinks"]] <- value
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('Study Links')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    contributors = function(value) {
      if(missing(value)) {
        ctbs <- private$.contributors
        return(ctbs)
      }
      checkmate::assert_list(x = value, min.len = 1, types = "ContributorLine")
      private[[".contributors"]] <- value
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('Study Contributors')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    }
  )
)


DbConfigBlock <- R6::R6Class(
  classname = "DbConfigBlock",
  public = list(
    initialize = function(configBlockName,
                          cdmDatabaseSchema,
                          databaseName = NULL,
                          databaseLabel = NULL) {

      .setString(private = private, key = ".configBlockName", value = configBlockName)
      .setString(private = private, key = ".cdmDatabaseSchema", value = cdmDatabaseSchema)

      checkmate::assert_string(x = databaseName, min.chars = 1, null.ok = TRUE)
      if (is.null(databaseName)) {
        private[[".databaseName"]] <- configBlockName
      } else {
        private[[".databaseName"]] <- databaseName
      }


      checkmate::assert_string(x = databaseLabel, min.chars = 1, null.ok = TRUE)
      if (is.null(databaseName) & is.null(databaseLabel)) {
        private[[".databaseLabel"]] <- configBlockName
      } else if (!is.null(databaseName) & is.null(databaseLabel)) {
        private[[".databaseLabel"]] <- databaseName
      } else {
        private[[".databaseLabel"]] <- databaseLabel
      }
    },

    writeBlockSection = function(repoName) {

      configBlockName <- private$.configBlockName
      databaseName <- private$.databaseName
      databaseLabel <- private$.databaseLabel
      cdm <- private$.cdmDatabaseSchema

      configBlock <- fs::path_package(package = "Ulysses", "templates/configBlock.txt") |>
        readr::read_file() |>
        glue::glue()
      return(configBlock)
    }
  ),
  private = list(
    .configBlockName = NULL,
    .cdmDatabaseSchema = NULL,
    .databaseName = NULL,
    .databaseLabel = NULL
  ),
  active = list(
    configBlockName = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        cds <- private$.configBlockName
        return(cds)
      }
      # replace the cdmDatabaseSchema
      .setString(private = private, key = ".configBlockName", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('configBlockName')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    cdmDatabaseSchema = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        cds <- private$.cdmDatabaseSchema
        return(cds)
      }
      .setString(private = private, key = ".cdmDatabaseSchema", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('cdmDatabaseSchema')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    databaseName = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        cds <- private$.databaseName
        return(cds)
      }
      .setString(private = private, key = ".databaseName", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('databaseName')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    databaseLabel = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        cds <- private$.databaseLabel
        return(cds)
      }
      .setString(private = private, key = ".databaseLabel", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('databaseLabel')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    }
  )
)

ExecOptions <- R6::R6Class(
  classname = "ExecOptions",
  public = list(
    initialize = function(
    dbms,
    connectionLoadScript,
    resultsPath,
    databaseRole,
    workDatabaseSchema,
    tempEmulationSchema,
    dbConnectionBlocks) {

      .setString(private = private, key = ".dbms", value = dbms)

      .setString(private = private, key = ".connectionLoadScript", value = connectionLoadScript)

      .setString(private = private, key = ".resultsPath", value = resultsPath)

      .setString(private = private, key = ".workDatabaseSchema", value = workDatabaseSchema)

      checkmate::assert_string(x = tempEmulationSchema, min.chars = 1, null.ok = TRUE)
      if (!is.null(tempEmulationSchema)) {
        private[[".tempEmulationSchema"]] <- tempEmulationSchema
      }

      checkmate::assert_list(x = dbConnectionBlocks, min.len = 1, types = "DbConfigBlock")
      private[[".dbConnectionBlocks"]] <- dbConnectionBlocks

      checkmate::assert_string(x = databaseRole, min.chars = 1, null.ok = TRUE)
      if (!is.null(databaseRole)) {
        private[[".databaseRole"]] <- databaseRole
      }

    }
  ),
  private = list(
    .dbms = NA_character_,
    .connectionLoadScript = NA_character_,
    .resultsPath = NULL,
    .databaseRole = NA_character_,
    .workDatabaseSchema = NULL,
    .tempEmulationSchema = NULL,
    .dbConnectionBlocks = NULL
  ),
  active = list(
    dbms = function(value) {
      if(missing(value)) {
        sm <- private$.dbms
        return(sm)
      }
      .setString(private = private, key = ".dbms", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('dbms')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    connectionLoadScript = function(value) {
      if(missing(value)) {
        sm <- private$.connectionLoadScript
        return(sm)
      }
      .setString(private = private, key = ".connectionLoadScript", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('connectionLoadScript')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    resultsPath = function(value) {
      if(missing(value)) {
        sm <- private$.resultsPath
        return(sm)
      }
      .setString(private = private, key = ".resultsPath", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('resultsPath')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    databaseRole = function(value) {
      if(missing(value)) {
        sm <- private$.databaseRole
        return(sm)
      }
      checkmate::assert_string(x = value, min.chars = 1, null.ok = TRUE)
      private[[".databaseRole"]] <- value

      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('databaseRole')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    workDatabaseSchema = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        cds <- private$.workDatabaseSchema
        return(cds)
      }
      # replace the workDatabaseSchema
      .setString(private = private, key = ".workDatabaseSchema", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('workDatabaseSchema')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    tempEmulationSchema = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        tes <- private$.tempEmulationSchema
        return(tes)
      }
      # replace the tempEmulationSchema
      .setString(private = private, key = ".tempEmulationSchema", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('tempEmulationSchema')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    dbConnectionBlocks = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        tes <- private$.dbConnectionBlocks
        return(tes)
      }
      # replace the dbConnectionBlocks
      checkmate::assert_list(x = value, min.len = 1, types = "DbConfigBlock")
      private[[".dbConnectionBlocks"]] <- value

      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('dbConnectionBlocks')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    }
  )
)

InputOptions <- R6::R6Class(
  classname = "InputOptions",
  public = list(),
  private = list(
    .cohortsToLoadFilePath = NULL,
    .conceptSetsToLoadFilePath = NULL,
    .analysisTaskFiles = NULL,
    .studyHubFiles = NULL
  ),
  active = list()
)


# ExecutionSettings ----

#' @title ExecutionSettings
#' @description
#' An R6 class to define an ExecutionSettings object
#'
#' @export
ExecutionSettings <- R6::R6Class(
  classname = "ExecutionSettings",
  public = list(
    #' @param connectionDetails a connectionDetails object
    #' @param connection a connection to a dbms
    #' @param cdmDatabaseSchema The schema of the OMOP CDM database
    #' @param workDatabaseSchema The schema to which results will be written
    #' @param tempEmulationSchema Some database platforms like Oracle and Snowflake do not truly support temp tables. To emulate temp tables, provide a schema with write privileges where temp tables can be created.
    #' @param cohortTable The name of the table where the cohort(s) are stored
    #' @param cdmSourceName A human-readable name for the OMOP CDM source
    initialize = function(connectionDetails = NULL,
                          connection = NULL,
                          cdmDatabaseSchema = NULL,
                          workDatabaseSchema = NULL,
                          tempEmulationSchema = NULL,
                          cohortTable = NULL,
                          cdmSourceName = NULL) {
      stopifnot(is.null(connectionDetails) || is.null(connection))
      .setClass(private = private, key = "connectionDetails", value = connectionDetails,
                class = "ConnectionDetails", nullable = TRUE)
      .setClass(private = private, key = ".connection", value = connection,
                class = "DatabaseConnectorJdbcConnection", nullable = TRUE)
      .setString(private = private, key = ".cdmDatabaseSchema", value = cdmDatabaseSchema)
      .setString(private = private, key = ".workDatabaseSchema", value = workDatabaseSchema)
      .setString(private = private, key = ".tempEmulationSchema", value = tempEmulationSchema)
      .setString(private = private, key = ".cohortTable", value = cohortTable)
      .setString(private = private, key = ".cdmSourceName", value = cdmSourceName)
    },
    #' @description extract the dbms dialect
    getDbms = function() {
      conObj <- private$.connection
      if (!is.null(conObj)) {
        dbms <- conObj@dbms
      } else {
        dbms <- private$connectionDetails$dbms
      }
      return(dbms)
    },
    #' @description connect to dbms
    connect = function() {

      # check if private$connection is NULL
      conObj <- private$.connection
      if (is.null(conObj)) {
        private$.connection <- DatabaseConnector::connect(private$connectionDetails)
      } else{
        cli::cat_bullet(
          "Connection object already open",
          bullet = "info",
          bullet_col = "blue"
        )
      }
    },

    #' @description disconnect from dbms
    disconnect = function() {

      # check if private$connection is NULL
      conObj <- private$.connection
      if (class(conObj) == "DatabaseConnectorJdbcConnection") {
        # disconnect connection
        DatabaseConnector::disconnect(private$.connection)
        private$.connection <- NULL
      }

      cli::cat_bullet(
        "Connection object has been disconected",
        bullet = "info",
        bullet_col = "blue"
      )
      invisible(conObj)
    },

    #TODO make this more rigorous
    # add warning if no connection available
    #' @description retrieve the connection object
    getConnection = function() {
      conObj <- private$.connection
      return(conObj)
    }

  ),

  private = list(
    connectionDetails = NULL,
    .connection = NULL,
    .cdmDatabaseSchema = NULL,
    .workDatabaseSchema = NULL,
    .tempEmulationSchema = NULL,
    .cohortTable = NULL,
    .cdmSourceName = NULL
  ),

  active = list(
    #' @field cdmDatabaseSchema the schema containing the OMOP CDM
    cdmDatabaseSchema = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        cds <- private$.cdmDatabaseSchema
        return(cds)
      }
      # replace the cdmDatabaseSchema
      .setString(private = private, key = ".cdmDatabaseSchema", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('cdmDatabaseSchema')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },

    #' @field workDatabaseSchema the schema containing the cohort table
    workDatabaseSchema = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        cds <- private$.workDatabaseSchema
        return(cds)
      }
      # replace the workDatabaseSchema
      .setString(private = private, key = ".workDatabaseSchema", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('workDatabaseSchema')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },

    #' @field tempEmulationSchema the schema needed for temp tables
    tempEmulationSchema = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        tes <- private$.tempEmulationSchema
        return(tes)
      }
      # replace the tempEmulationSchema
      .setString(private = private, key = ".tempEmulationSchema", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('tempEmulationSchema')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    #' @field cohortTable the table containing the cohorts
    cohortTable = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        tct <- private$.cohortTable
        return(tct)
      }
      # replace the cohortTable
      .setString(private = private, key = ".cohortTable", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('cohortTable')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    #' @field cdmSourceName the name of the source data of the cdm
    cdmSourceName = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        csn <- private$.cdmSourceName
        return(csn)
      }
      # replace the cdmSourceName
      .setString(private = private, key = ".cdmSourceName", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('cdmSourceName')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    }

  )
)
