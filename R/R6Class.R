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
                          #inputOptions = NULL,
                          gitRemote = NULL,
                          renvLock = NULL
    ) {

      checkmate::assert_string(x = repoName, min.chars = 1)
      private[[".repoName"]] <- repoName

      checkmate::assert_string(x = repoFolder, min.chars = 1)
      private[[".repoFolder"]] <- repoFolder

      .setClass(private = private, key = ".studyMeta", value = studyMeta, class = "StudyMeta")
      .setClass(private = private, key = ".execOptions", value = execOptions, class = "ExecOptions")
      #.setClass(private = private, key = ".inputOptions", value = inputOptions, class = "InputOptions", nullable = TRUE)


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
      private$.initConfigFile() # init config
      private$.initQuarto()
      private$.initMainExec()

      if (!is.null(private$.gitRemote)) {
        notification("Step 5 (Optional): Initialize git with Remote")
        private$.initGit() #initialize git with remote
      }

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
      usethis::use_git_ignore(
        c(".Rproj.user", ".Ruserdata",
          ".Rhistory", ".RData",
          ".Renviron", "exec/results")
      )
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

    .initConfigFile = function() {
      repoName <- private$.repoName
      repoFolder <- private$.repoFolder
      repoPath <- fs::path(repoFolder, repoName) |>
        fs::path_expand()

      private$.execOptions$makeConfigFile(repoName = repoName, repoPath = repoPath)

    },

    .initGit = function(gitRemoteUrl) {

      gitRemoteUrl <- private$.gitRemote
      repoName <- private$.repoName
      repoFolder <- private$.repoFolder
      repoPath <- fs::path(repoFolder, repoName) |>
        fs::path_expand()

      #Step1: initialize git
      gert::git_init(repoPath)
      # Step 2: add all files
      stg <- gert::git_add(files = ".")
      #step 3: commit all files
      sha <- gert::git_commit_all(message = "Initialize Ulysses Repo for study")
      #step 4: setup remote
      gert::git_remote_add(url = gitRemoteUrl)
      # Step 5: push
      gert::git_push(remote = "origin")

      invisible(TRUE)
    },

    .initQuarto = function() {
      repoName <- private$.repoName
      repoFolder <- private$.repoFolder
      studyTitle <- self$studyMeta$studyTitle

      initStudyHubFiles(
        repoName = repoName,
        repoFolder = repoFolder,
        studyTitle = self$studyMeta$studyTitle
      )

    },

    .initMainExec = function() {

      # get elements
      studyName <- private$.studyMeta$studyTitle
      configBlocks <- purrr::map_chr(
        private$.execOptions$dbConnectionBlocks,
        ~.x$configBlockName
      )
      repoName <- private$.repoName
      repoFolder <- private$.repoFolder

      addMainFile(
        repoName = repoName,
        repoFolder = repoFolder,
        configBlocks = configBlocks,
        studyName = studyName
      )
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
# Contributor Line ---------------
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

# Study Meta ---------------
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

# Db Config Block -----------------------
DbConfigBlock <- R6::R6Class(
  classname = "DbConfigBlock",
  public = list(
    initialize = function(configBlockName,
                          #dbms,
                          cdmDatabaseSchema,
                          #workDatabaseSchema,
                          #tempEmulationSchema,
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

    writeBlockSection = function(repoName, dbms, workSchema, tempSchema) {

      configBlockName <- private$.configBlockName
      databaseName <- private$.databaseName
      databaseLabel <- private$.databaseLabel
      cdmSchema <- private$.cdmDatabaseSchema

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

# Exec Options ---------------------
ExecOptions <- R6::R6Class(
  classname = "ExecOptions",
  public = list(
    initialize = function(
    dbms,
    workDatabaseSchema,
    tempEmulationSchema,
    dbConnectionBlocks) {

      .setString(private = private, key = ".dbms", value = dbms)

      .setString(private = private, key = ".workDatabaseSchema", value = workDatabaseSchema)

      checkmate::assert_string(x = tempEmulationSchema, min.chars = 1, null.ok = TRUE)
      if (!is.null(tempEmulationSchema)) {
        private[[".tempEmulationSchema"]] <- tempEmulationSchema
      }

      checkmate::assert_list(x = dbConnectionBlocks, min.len = 1, types = "DbConfigBlock")
      private[[".dbConnectionBlocks"]] <- dbConnectionBlocks

    },

    makeConfigFile = function(repoName, repoPath) {

      dbBlocks <- vector('list', length = length(private$.dbConnectionBlocks))
      for (i in seq_along(dbBlocks)) {
        dbBlocks[[i]] <- private$.dbConnectionBlocks[[i]]$writeBlockSection(
          repoName = repoName,
          dbms = private$.dbms,
          workSchema = private$.workDatabaseSchema,
          tempSchema = private$.tempEmulationSchema
        )
      }
      dbBlocks <- do.call('c', dbBlocks) |>
        glue::glue_collapse(sep = "\n\n")

      header <- fs::path_package(package = "Ulysses", "templates/configHeader.txt") |>
        readr::read_file() |>
        glue::glue()

      configFile <- c(header, dbBlocks) |>
        glue::glue_collapse(sep = "\n\n")

      readr::write_lines(
        x = configFile,
        file = fs::path(repoPath, "config.yml")
      )

      actionItem(glue::glue_col("Initialize Config: {green {fs::path(repoPath, repoName, 'config.yml')}}"))
      invisible(configFile)

    }
  ),
  private = list(
    .dbms = NA_character_,
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

# WebApiConnection ---------------------
WebApiConnection <- R6::R6Class(
  classname = "WebApiConnection",
  public = list(
    initialize = function(baseUrl, authMethod, user, password) {
      # check baseUrl
      checkmate::assert_string(x = baseUrl, min.chars = 1)
      private[[".baseUrl"]] <- baseUrl
      # check authMethod
      checkmate::assert_string(x = authMethod, min.chars = 1)
      private[[".authMethod"]] <- authMethod
      # check user
      checkmate::assert_string(x = user, min.chars = 1)
      private[[".user"]] <- user
      # check user
      checkmate::assert_string(x = password, min.chars = 1)
      private[[".password"]] <- password
    },

    checkUser = function() {
      usr <- private$.user
      cli::cat_line(glue::glue("- user: {crayon::green(usr)}"))
      invisible(usr)
    },

    checkPassword = function() {
      pwd <- private$.password
      cli::cat_line(glue::glue("- password: {crayon::blurred(pwd)}"))
      invisible(pwd)
    },

    checkBaseUrl = function() {
      baseUrl <- private$.baseUrl
      cli::cat_line(glue::glue("- baseUrl: {crayon::green(baseUrl)}"))
      invisible(baseUrl)
    },

    checkAuthMethod = function() {
      am <- private$.authMethod
      cli::cat_line(glue::glue("- authMethod: {crayon::green(am)}"))
      invisible(am)
    },

    getWebApiUrl = function() {
      baseUrl <- private$.baseUrl
      return(baseUrl)
    },

    checkAtlasCredentials = function() {

      headerTxt <- glue::glue_col("Checking Atlas Credentials from {cyan .Renviron}")
      cli::cat_rule(headerTxt)
      cli::cat_line()

      self$checkBaseUrl()
      self$checkAuthMethod()
      self$checkUser()
      self$checkPassword()

      cli::cat_line()
      messageTxt <- glue::glue_col("To modify credentials run function {magenta 'usethis::edit_r_environ()'} and change system variables for Atlas credentials")
      cli::cat_bullet(messageTxt, bullet = "warning", bullet_col = "yellow")

    },

    getCohortDefinition = function(cohortId) {

      if (is.null(private$.bearerToken)) {
        private$authorizeWebApi()
      }
      baseUrl <- private$.baseUrl
      req <- glue::glue("{baseUrl}/cohortdefinition/{cohortId}") |>
            httr2::request() |>
            httr2::req_auth_bearer_token(token = private$.bearerToken)
      resp <- httr2::req_perform(req = req)
      cd <- httr2::resp_body_json(resp)
      cdExp <- RJSONIO::fromJSON(cd$expression, nullValue = NA, digits = 23)

      tb <- tibble::tibble(
        id = cd$id,
        name = cd$name,
        expression = formatCohortExpression(cdExp),
        saveName = glue::glue("{id}_{name}") |> snakecase::to_snake_case()
      )

      return(tb)
    },

    getConceptSetDefinition = function(conceptSetId) {

      if (is.null(private$.bearerToken)) {
        private$authorizeWebApi()
      }
      baseUrl <- private$.baseUrl
      req <- glue::glue("{baseUrl}/conceptset/{conceptSetId}") |>
        httr2::request() |>
        httr2::req_auth_bearer_token(token = private$.bearerToken)
      resp <- httr2::req_perform(req = req)
      cs <- httr2::resp_body_json(resp)

      # get the expression from the right spot
      csExp <-pluckConceptSetExpression(
        conceptSetId = conceptSetId,
        baseUrl = baseUrl,
        bearerToken = private$.bearerToken
      )

      tb <- tibble::tibble(
        id = cs$id,
        name = cs$name,
        expression = csExp,
        saveName = glue::glue("{id}_{name}") |> snakecase::to_snake_case()
      )

      return(tb)
    }

  ),
  private = list(
    .baseUrl = NULL,
    .authMethod = NULL,
    .user = NULL,
    .password = NULL,
    .bearerToken = NULL,

    # functions
    authorizeWebApi = function() {

      baseUrl <- private$.baseUrl
      authMethod <- private$.authMethod
      user <- private$.user
      password <- private$.password

      cli::cat_bullet(
        glue::glue("Authorizing Web Api connection for {crayon::cyan(baseUrl)}"),
        bullet = "pointer",
        bullet_col = "yellow"
      )

      authUrl <- paste0(baseUrl, glue::glue("/user/login/{authMethod}"))

      req <- httr2::request(authUrl) |>
        httr2::req_body_form(
          login = user,
          password = password
        )

      bearerToken <- httr2::req_perform(req)$headers$Bearer

      .setString(private = private, key = ".bearerToken", value = bearerToken)

      invisible(bearerToken)
    }
  )
)


CirceCohortsToLoad <- R6::R6Class(
  classname = "CirceCohortsToLoad",
  public = list(
    initialize = function(cohortsToLoadTable,
                          webApiCreds) {
      # check and init cohortsToLoadTable
      checkmate::assert_data_frame(
        x = cohortsToLoadTable,
        min.rows = 1,
        ncols = 3
      )
      private[[".cohortsToLoadTable"]] <- cohortsToLoadTable

      # check webApi creds
      checkmate::assert_class(x = webApiCreds, classes = "WebApiCreds")
      private[[".webApiCreds"]] <- webApiCreds
    },

    getCirce = function() {

      private$.webApiCreds$authorizeWebApi()
      circeIds <- private$.cohortsToLoadTable$atlasId
      circeTb <- vector('list', length = length(circeIds))
      for (i in seq_along(circeIds)) {
        circeTb[[i]] <- grabCohortFromWebApi(
          cohortId = circeIds[i],
          baseUrl = private$.webApiCreds$getWebApiUrl()
          )
      }
      circeTb2 <- do.call('rbind', circeTb)
      circeTb3 <- private$.cohortsToLoadTable |>
        dplyr::left_join(
          circeTb2, by = c('atlasId' = "id")
        ) |>
        dplyr::mutate(
          savePath = fs::path("inputs/cohorts/json", analysisType, saveName, ext = "json")
        ) |>
        dplyr::select(
          atlasId, assetLabel, analysisType, expression, saveName, savePath
        )

      return(circeTb3)
    }


  ),
  private = list(
    .webApiCreds = NULL,
    .cohortsToLoadTable = NULL
  ),
  active = list(
    cohortsToLoadTable = function(value) {
      if(missing(value)) {
        res <- private$.cohortsToLoadTable
        return(res)
      }
      checkmate::assert_data_frame(
        x = value,
        min.rows = 1,
        ncols = 3
      )
      private[[".cohortsToLoadTable"]] <- value

      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('cohortsToLoadTable')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    }
  )
)


CirceConceptSetsToLoad <- R6::R6Class(
  classname = "CirceConceptSetsToLoad",
  public = list(
    initialize = function(conceptSetsToLoadTable,
                          webApiCreds) {
      # check and init cohortsToLoadTable
      checkmate::assert_data_frame(
        x = conceptSetsToLoadTable,
        min.rows = 1,
        ncols = 3
      )
      private[[".conceptSetsToLoadTable"]] <- conceptSetsToLoadTable

      # check webApi creds
      checkmate::assert_class(x = webApiCreds, classes = "WebApiCreds")
      private[[".webApiCreds"]] <- webApiCreds
    },

    getCirce = function() {

      private$.webApiCreds$authorizeWebApi()
      circeIds <- private$.conceptSetsToLoadTable$atlasId
      circeTb <- vector('list', length = length(circeIds))
      for (i in seq_along(circeIds)) {
        circeTb[[i]] <- grabConceptSetFromWebApi(
          conceptSetId = circeIds[i],
          baseUrl = private$.webApiCreds$getWebApiUrl()
        )
      }
      circeTb2 <- do.call('rbind', circeTb)
      circeTb3 <- private$.conceptSetsToLoadTable |>
        dplyr::left_join(
          circeTb2, by = c('atlasId' = "id")
        ) |>
        dplyr::mutate(
          savePath = fs::path("inputs/conceptSets/json", analysisType, saveName, ext = "json")
        ) |>
        dplyr::select(
          atlasId, assetLabel, analysisType, expression, saveName, savePath
        )

      return(circeTb3)
    }


  ),
  private = list(
    .webApiCreds = NULL,
    .conceptSetsToLoadTable = NULL
  ),
  active = list(
    conceptSetsToLoadTable = function(value) {
      if(missing(value)) {
        res <- private$.conceptSetsToLoadTable
        return(res)
      }
      checkmate::assert_data_frame(
        x = value,
        min.rows = 1,
        ncols = 3
      )
      private[[".conceptSetsToLoadTable"]] <- value

      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('conceptSetsToLoadTable')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    }
  )
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
