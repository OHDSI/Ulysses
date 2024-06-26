% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/credentials.R
\name{initCredentialsTable}
\alias{initCredentialsTable}
\title{Function to start a credential table}
\usage{
initCredentialsTable(
  db_id,
  db_full_name = db_id,
  dbms,
  user,
  password,
  connection_string,
  cdm_database_schema,
  work_database_schema,
  temp_emulation_schema = work_database_schema
)
}
\arguments{
\item{db_id}{an id or shortName for a database. Please use snakecase}

\item{db_full_name}{the full name of the database. If not specified defaults to db_id}

\item{dbms}{the dbms for your database. Can be either redshift, postgres, snowflake, sql server or oracle}

\item{user}{the user name to connect to the database}

\item{password}{the password to connect to the databse}

\item{connection_string}{a jdbc connection string to use with DatabaseConnector}

\item{cdm_database_schema}{the cdm database schema of the datbase you want to use}

\item{work_database_schema}{a scratch schema the user has read and write access to in order to build cohort tables}

\item{temp_emulation_schema}{a schema required for oracle and snowflake to make temp tables.}
}
\value{
writes a csv to your home directory called shhh.csv storing your database credentials
}
\description{
Function to start a credential table
}
