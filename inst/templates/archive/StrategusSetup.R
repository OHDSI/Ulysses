# _strategusSetup.R

# A. Meta Info -----------------------

# Description: The purpose of the _strategusSetup.R script is to setup strategus
# modules for run.

# B. Functions ------------------------

############################################ Cohorts ####################################################
# set directory for saving analysis settings
library(Strategus)


analysisDir <- here::here() |>
  fs::dir_create("analysis/settings")

strategusSettingsFile <- "strategus_analysis_spec.json"

# set directory for covariates
cohortDefDir <- here::here() |>
  fs::path("cohorts/json")

# get file name
targetJsonFile <- cohortDefDir |> fs::dir_ls()
# get cohort name
targetCohortName <- fs::path_ext_remove(targetJsonFile) |> fs::path_file()
# pluck atlas id
targetCohortId <- purrr::map_dbl(
  targetCohortName,
  ~stringr::str_split_1(.x, pattern = "_")[1] |> as.numeric()
)
# get cohort json
targetJson <- purrr::map_chr(
  targetJsonFile,
  ~readr::read_file(file = .x)
)

# get cohort sql
targetSql <- purrr::map_chr(
  targetJson,
  ~CirceR::buildCohortQuery(
    CirceR::cohortExpressionFromJson(.x),
    CirceR::createGenerateOptions(generateStats = TRUE))
)


#make set of target cohorts
cohortDefinitionSet <- data.frame(
  cohortId = targetCohortId,
  cohortName = targetCohortName,
  json = targetJson,
  sql = targetSql
)

############################################ CohortGenerator ############################################
# CohortGeneratorModule --------------------------------------------------------
source("https://raw.githubusercontent.com/OHDSI/CohortGeneratorModule/v0.2.1/SettingsFunctions.R")

# define CohortGeneratorModule specifications
cohortGeneratorModuleSpecs <- createCohortGeneratorModuleSpecifications(
  incremental = TRUE,
  generateStats = TRUE
)

cohortDefinitionShared <- createCohortSharedResourceSpecifications(cohortDefinitionSet)


############################################ CohortDiagnostics ############################################
source("https://raw.githubusercontent.com/OHDSI/CohortDiagnosticsModule/v0.1.0/SettingsFunctions.R")


temporalCovariateSettings <- FeatureExtraction::createTemporalCovariateSettings(
  useDemographicsGender = TRUE,
  useDemographicsAge = TRUE,
  useDemographicsAgeGroup = TRUE,
  useDemographicsRace = TRUE,
  useDemographicsEthnicity = TRUE,
  useDemographicsIndexYear = TRUE,
  useDemographicsPriorObservationTime = TRUE,
  useDemographicsPostObservationTime = TRUE,
  useDemographicsTimeInCohort = TRUE,
  useConditionOccurrence = TRUE,
  useDrugEraGroupStart = TRUE,
  useCharlsonIndex = TRUE,
  useDistinctIngredientCount = TRUE,
  useVisitConceptCount = TRUE,
  temporalStartDays = c(-365, 0, 1, 1, 1, 1),
  temporalEndDays = c(-1, 0, 30, 183, 365, 730)
)



cohortDiagnosticsModuleSpecifications <- createCohortDiagnosticsModuleSpecifications(
  runInclusionStatistics = TRUE, # get inclusion breakdown
  runIncludedSourceConcepts = FALSE,
  runOrphanConcepts = FALSE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE, # visit context where was event setting
  runBreakdownIndexEvents = FALSE,
  runIncidenceRate = TRUE, # overall incidence of cohorts
  runCohortRelationship = FALSE,
  runTemporalCohortCharacterization = TRUE, # temporal covariates pre and post index
  temporalCovariateSettings = temporalCovariateSettings,
  incremental = TRUE
)

######################################## Build Analysis Specs ############################################

analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecs) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications)


ParallelLogger::saveSettingsToJson(object = analysisSpecifications,
                                   fileName = file.path(analysisDir, strategusSettingsFile))
