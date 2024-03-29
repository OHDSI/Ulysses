% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{initConfig}
\alias{initConfig}
\title{Function to create a config.yml file}
\usage{
initConfig(
  block = "BlockName",
  database = "DatabaseName",
  withKeyring = FALSE,
  projectPath = here::here(),
  open = TRUE
)
}
\arguments{
\item{block}{the name of the config block, defaults to BlockName}

\item{database}{the name of the database for the block, default to DatabaseName}

\item{withKeyring}{should the config file use keyring, default FALSE}

\item{projectPath}{the path to the project}

\item{open}{toggle on whether the file should be opened}
}
\description{
Function to create a config.yml file
}
