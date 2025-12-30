# Atlas Connection ---------------

#' @title Set Atlas Connection
#' @returns an R6 class of WebApiConnection
#' @export
setAtlasConnection <- function() {

  atlasCon <- WebApiConnection$new(
    baseUrl = Sys.getenv("atlasBaseUrl"),
    authMethod = Sys.getenv("atlasAuthMethod"),
    user = Sys.getenv("atlasUser"),
    password = Sys.getenv("atlasPassword")
  )
  return(atlasCon)
}

pluckConceptSetExpression <- function(conceptSetId, baseUrl, bearerToken) {
  req <- glue::glue("{baseUrl}/conceptset/{conceptSetId}/expression") |>
    httr2::request() |>
    httr2::req_auth_bearer_token(token = bearerToken)
  resp <- httr2::req_perform(req = req)
  csExp <- httr2::resp_body_json(resp)
  csExp2 <- RJSONIO::toJSON(csExp, digits = 23, pretty = TRUE)
  return(csExp2)
}


formatCohortExpression <- function(expression) {
  # reformat to standard circe
  circe <- list(
    'ConceptSets' = expression$ConceptSets,
    'PrimaryCriteria' = expression$PrimaryCriteria,
    'AdditionalCriteria' = expression$AdditionalCriteria,
    'QualifiedLimit' = expression$QualifiedLimit,
    'ExpressionLimit' = expression$ExpressionLimit,
    'InclusionRules' = expression$InclusionRules,
    'EndStrategy' = expression$EndStrategy,
    'CensoringCriteria' = expression$CensoringCriteria,
    'CollapseSettings' = expression$CollapseSettings,
    'CensorWindow' = expression$CensorWindow,
    'cdmVersionRange' = expression$cdmVersionRange
  )
  if (is.null(circe$AdditionalCriteria)) {
    circe$AdditionalCriteria <- NULL
  }
  if (is.null(circe$EndStrategy)) {
    circe$EndStrategy <- NULL
  }

  circeJson <- RJSONIO::toJSON(circe, digits = 23, pretty = TRUE)

  return(circeJson)
}



#' @title Template for setting Atlas Credentials
#' @returns no return; prints info to console
#' @export
templateAtlasCredentials <- function() {

  credsToSetTxt <- c("atlasBaseUrl='https://organization-atlas.com/WebAPI'",
                  "atlasAuthMethod='ad'",
                  "atlasUser='atlas.user@company.com'",
                  "atlasPassword='TisASecret'") |>
    glue::glue_collapse(sep = "\n")

  headerTxt <- "Atlas Credential Template"
  instructionsTxt1 <- "Providing a template for setting Atlas Credentials. Please alter to the correct credentials!!!"
  instructionsTxt2 <- glue::glue_col("To set Atlas Credentials run function {magenta 'usethis::edit_r_environ()'} and paste template to {cyan .Renviron} changing the credentials accordingly.")
  noteTxt <- "The variable name of the atlas credentials must be in this format!!!"

  cli::cat_rule(headerTxt)
  cli::cat_line()
  cli::cat_bullet(instructionsTxt1, bullet = "info", bullet_col = "blue")
  cli::cat_bullet(instructionsTxt2, bullet = "info", bullet_col = "blue")
  cli::cat_bullet(noteTxt, bullet = "warning", bullet_col = "yellow")
  cli::cat_line()
  cli::cat_line(credsToSetTxt)

  invisible(credsToSetTxt)
}





getAtlasAuthBearerToken <- function(baseUrl, authMethod, user, password) {

  authUrl <- paste0(baseUrl, glue::glue("user/login/{authMethod}"))

  req <- httr2::request(authUrl) |>
    httr2::req_body_form(
      login = user,
      password = password
    )

  bearerToken <- httr2::req_perform(req)$headers$Bearer

  return(bearerToken)
}


