% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeFiles.R
\name{makeKeyringSetup}
\alias{makeKeyringSetup}
\title{Function to create a config.yml file}
\usage{
makeKeyringSetup(
  database = NULL,
  configBlock = NULL,
  secret = NULL,
  projectPath = here::here(),
  open = TRUE
)
}
\arguments{
\item{database}{the name of the database for the project, can be NULL}

\item{configBlock}{the name of the configBlock to use, can be NULL}

\item{secret}{a keyword to use as the keyring password to access credentials, if NULL password is ulysses}

\item{projectPath}{the path to the project}

\item{open}{toggle on whether the file should be opened}
}
\description{
Function to create a config.yml file
}
