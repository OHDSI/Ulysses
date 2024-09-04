
# Cohort Manifest ---------------------------

# TODO: add cohort routing. specify target, comparator, outcome and indication type cohorts
# R6 class for the cohort manifest
CohortManifest <- R6::R6Class(
  classname = "CohortManifest",
  public = list(
    # initialize class
    initialize = function(
    cohortId,
    cohortName,
    cohortPrettyName,
    cohortHash,
    cohortType,
    circeJson,
    ohdsiSql,
    generatedCohorts
    ) {
      .setInteger(private = private, key = ".cohortId", value = cohortId)
      .setCharacter(private = private, key = ".cohortName", value = cohortName)
      .setCharacter(private = private, key = ".cohortPrettyName", value = cohortPrettyName)
      .setCharacter(private = private, key = ".cohortHash", value = cohortHash)
      .setCharacter(private = private, key = ".cohortType", value = cohortType)
      .setCharacter(private = private, key = ".circeJson", value = circeJson)
      .setCharacter(private = private, key = ".ohdsiSql", value = ohdsiSql)
      .setClass(private = private, key = "generatedCohorts", class = "GeneratedCohorts",
                value = generatedCohorts, nullable = TRUE)
    },
    #show the manifest table
    showTable = function() {
      tb <- private$.pullManifest()
      return(tb)
    },
    # function to describe the cohorts
    describeCohort = function(idx) {
      cdRead <- private$.getCohortPrintFriendly(idx)
      md_to_viewer(cdRead)
    },

    specifyCohortType = function(orderIds, type) {
      checkmate::assert_choice(type, choices = c("target", "comparator", "outcome", "indication"))
      self$cohortType[orderIds] <- type
      invisible(self)
    },

    checkHash = function() {
      ohdsiSqlFile <- names(self$cohortHash)
      hash <- unname(self$cohortHash)
      newHash <- purrr::map(ohdsiSqlFile, ~readr::read_file(file = .x)) |>
        purrr::map_chr(~digest::digest(object = .x, algo = "md5", serialize = FALSE))
      check <- which(hash != newHash)
      return(check)
    },

    #function to initialize cohort tables
    initCohortTables = function(executionSettings) {

      # establish connection to database
      connection <- executionSettings$getConnection()

      if (is.null(connection)) {
        connection <- executionSettings$connect()
      }

      # get cohort table names
      cohortTableNms <- CohortGenerator::getCohortTableNames(
        cohortTable = executionSettings$targetCohortTable
      )
      #initialize cohort tables
      CohortGenerator::createCohortTables(
        connection = connection,
        cohortDatabaseSchema = executionSettings$workDatabaseSchema,
        cohortTableNames = cohortTableNms,
        incremental = TRUE
      )
      invisible(cohortTableNms)

    },

    # function to generate the cohorts
    generateCohorts = function(executionSettings) {
      cli::cat_bullet(
        glue::glue_col("{yellow Building Circe Cohorts in Ulysses directory}"),
        bullet = "pointer",
        bullet_col = "yellow"
      )
      # get cohor table names
      cohortTableNms <- CohortGenerator::getCohortTableNames(
        cohortTable = executionSettings$targetCohortTable
      )

      # prep cohorts for generator
      cohortsToCreate <- private$.pullManifest()|>
        dplyr::select(
          cohortId, cohortPrettyName, circeJson, ohdsiSql
        ) |>
        dplyr::rename(
          cohortName = cohortPrettyName,
          json = circeJson,
          sql = ohdsiSql
        )

      # establish connection to database
      connection <- executionSettings$getConnection()

      if (is.null(connection)) {
        connection <- executionSettings$connect()
      }

      #generate cohorts
      cohortGenRes <- CohortGenerator::generateCohortSet(
        connection = connection,
        cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
        cohortDatabaseSchema =  executionSettings$workDatabaseSchema,
        tempEmulationSchema = executionSettings$tempEmulationSchema,
        cohortTable = cohortTableNms,
        cohortDefinitionSet = cohortsToCreate
      ) |>
        dplyr::select(-c(cohortName)) |>
        dplyr::left_join(
          private$.pullManifest(), by = c("cohortId")
        ) |>
        dplyr::select(
          cohortId, cohortHash, startTime, endTime
        )

      # Count and save
      cli::cat_bullet(
        glue::glue("Getting cohort counts"),
        bullet= "pointer",
        bullet_col = "yellow"
      )

      # retrieve counts
      cohortCounts <- CohortGenerator::getCohortCounts(
        connection = executionSettings$getConnection(),
        cohortDatabaseSchema = executionSettings$workDatabaseSchema,
        cohortTable = executionSettings$targetCohortTable,
        cohortIds = cohortsToCreate$cohortId
      ) |>
        dplyr::inner_join(
          cohortGenRes,
          by = "cohortId"
        ) |>
        dplyr::mutate(
          cohortId = as.integer(cohortId)
        )

      # turn results into R6 class
      genCohorts <- GeneratedCohorts$new(
        cohortId = cohortCounts$cohortId,
        cohortHash = cohortCounts$cohortHash,
        startTime = cohortCounts$startTime,
        endTime = cohortCounts$endTime,
        cohortSubjects = cohortCounts$cohortSubjects,
        cohortEntries = cohortCounts$cohortEntries,
        cdmSourceName = executionSettings$cdmSourceName,
        cohortTableName = executionSettings$targetCohortTable
      )
      tb <- genCohorts$showTable()

      private$.addGeneratedCohorts(genCohorts)

      return(tb)
    }
  ),
  private = list(
    .cohortId = NULL,
    .cohortName = NULL,
    .cohortPrettyName = NULL,
    .cohortHash = NULL,
    .cohortType = NULL,
    .circeJson = NULL,
    .ohdsiSql = NULL,
    generatedCohorts = NULL,
    #internal functions

    # private function to pull the cohort manifest as a table
    .pullManifest = function() {
      tb = tibble::tibble(
        cohortId = private$.cohortId,
        cohortName = private$.cohortName,
        cohortPrettyName = private$.cohortPrettyName,
        cohortHash = private$.cohortHash,
        cohortType = private$.cohortType,
        circeJson = private$.circeJson,
        ohdsiSql = private$.ohdsiSql
      ) |>
        dplyr::mutate(
          orderId = dplyr::row_number(), .before = 1
        )
      return(tb)
    },

    .updateManifest = function() {

      changeIds <- private$.checkHash()

      circeJson <- circeJson <- purrr::map_chr(cohortJsonFiles, ~readr::read_file(.x))


    },

    # check hash
    .checkHash = function() {
      ohdsiSqlFile <- names(self$cohortHash)
      hash <- unname(self$cohortHash)
      newHash <- purrr::map(ohdsiSqlFile, ~readr::read_file(file = .x)) |>
        purrr::map_chr(~digest::digest(object = .x, algo = "md5", serialize = FALSE))
      check <- which(hash != newHash)
      return(check)
    },

    .addGeneratedCohorts = function(generatedCohorts) {
      .setClass(private = private, key = "generatedCohorts",
                class = "GeneratedCohorts", value = generatedCohorts)
    },

    # create print friendly cohort
    .getCohortPrintFriendly = function(idx) {
      # retrieve cohort
      singleCohort <- private$.pullManifest() |>
        dplyr::filter(
          cohortId == idx
        )
      #get cohort header
      cohortName <- snakecase::to_title_case(singleCohort$cohortPrettyName)
      cohortId <- singleCohort$cohortId
      cohortHeader <- glue::glue("% Cohort Description for: {cohortName} (id: {cohortId}) \n")
      # get readable cohort logic
      # turn into print friendly
      cdRead <- CirceR::cohortPrintFriendly(singleCohort$circeJson)
      cdRead <- paste(cohortHeader, "## Cohort Definition", cdRead, sep = "\n\n")
      # get readable concept set
      csRead <- RJSONIO::fromJSON(singleCohort$circeJson)$ConceptSets |>
        CirceR::conceptSetListPrintFriendly()
      csRead <- paste("## Concept Sets", csRead, sep = "\n\n")
      #bind to get full read
      readFull <- paste(cdRead, csRead, sep = "\n\n")
      return(readFull)
    }


  ),

  active = list(
    cohortId = function(value) {
      .setActiveInteger(private = private, key = ".cohortId", value = value)
    },
    cohortName = function(value) {
      .setActiveCharacter(private = private, key = ".cohortName", value = value)
    },
    cohortPrettyName = function(value) {
      .setActiveCharacter(private = private, key = ".cohortPrettyName", value = value)
    },
    cohortHash = function(value) {
      .setActiveCharacter(private = private, key = ".cohortHash", value = value)
    },
    cohortType = function(value) {
      .setActiveCharacter(private = private, key = ".cohortType", value = value)
    },
    circeJson = function(value) {
      .setActiveCharacter(private = private, key = ".circeJson", value = value)
    },
    ohdsiSql = function(value) {
      .setActiveCharacter(private = private, key = ".ohdsiSql", value = value)
    }
  )
)




# Generated Cohorts -------------------------

# R6 class to track generated cohorts
GeneratedCohorts <- R6::R6Class(
  classname = "GeneratedCohorts",
  public = list(
    # initialize class
    initialize = function(
      cohortId,
      cohortHash,
      startTime,
      endTime,
      cohortSubjects,
      cohortEntries,
      cdmSourceName,
      cohortTableName
    ) {
      .setInteger(private = private, key = "cohortId", value = cohortId)
      .setCharacter(private = private, key = "cohortHash", value = cohortHash)
      .setTimeStamp(private = private, key = "startTime", value = startTime)
      .setTimeStamp(private = private, key = "endTime", value = endTime)
      .setNumeric(private = private, key = "cohortSubjects", value = cohortSubjects)
      .setNumeric(private = private, key = "cohortEntries", value = cohortEntries)
      .setString(private = private, key = "cdmSourceName", value = cdmSourceName)
      .setString(private = private, key = "cohortTableName", value = cohortTableName)
    },
    showTable = function() {
      tb <- private$.pullTable()
      return(tb)
    }

  ),
  private = list(
    cohortId = NULL,
    cohortHash = NULL,
    startTime = NULL,
    endTime = NULL,
    cohortSubjects = NULL,
    cohortEntries = NULL,
    cdmSourceName = NULL,
    cohortTableName = NULL,

    # private functions
    .pullTable = function() {

      tb <- tibble::tibble(
        cohortId = private$cohortId,
        cohortHash = private$cohortHash,
        generationTimeStamp = private$endTime,
        timeLapse = private$endTime - private$startTime,
        cohortSubjects = private$cohortSubjects,
        cohortEntries = private$cohortEntries
      ) |>
        dplyr::mutate(
          databaseId = tolower(private$cdmSourceName),
          .before = 1
        )
      return(tb)
    }

  )
)


