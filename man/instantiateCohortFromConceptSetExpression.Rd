% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InstantiateCohortFromConceptSetExpression.R
\name{instantiateCohortFromConceptSetExpression}
\alias{instantiateCohortFromConceptSetExpression}
\title{Given a concept set expression, instantiate a cohort.}
\usage{
instantiateCohortFromConceptSetExpression(
  connectionDetails = NULL,
  connection = NULL,
  cdmDatabaseSchema,
  vocabularyDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortId,
  cohortTable = "cohort",
  restrictToObservationPeriod = TRUE,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
  conceptSetExpression
)
}
\arguments{
\item{connectionDetails}{An object of type \code{connectionDetails} as created using the
\code{\link[DatabaseConnector]{createConnectionDetails}} function in the
DatabaseConnector package. Can be left NULL if \code{connection} is
provided.}

\item{connection}{An object of type \code{connection} as created using the
\code{\link[DatabaseConnector]{connect}} function in the
DatabaseConnector package. Can be left NULL if \code{connectionDetails}
is provided, in which case a new connection will be opened at the start
of the function, and closed when the function finishes.}

\item{cdmDatabaseSchema}{Schema name where your patient-level data in OMOP CDM format resides.
Note that for SQL Server, this should include both the database and
schema name, for example 'cdm_data.dbo'.}

\item{vocabularyDatabaseSchema}{The schema name of containing the vocabulary tables.}

\item{cohortId}{An integer value to identify the cohort.}

\item{cohortTable}{the name of the cohort table}

\item{restrictToObservationPeriod}{(Default = TRUE) Do you want to restrict to Observation period? i.e
Cohort dates are restricted to observation period.}

\item{tempEmulationSchema}{Some database platforms like Oracle and Impala do not truly support temp tables. To emulate temp 
tables, provide a schema with write privileges where temp tables can be created.}

\item{conceptSetExpression}{An R object (list) representation of a concept set expression}
}
\description{
Given a concept set expression, instantiate a cohort. The cohort is generated
by checking for the occurrence of any of the resolved conceptId's in the conceptSetExpression
in any domain tables. The domain tables are condition_occurrence, drug_exposure, procedure_occurrence,
visit_occurrence, observation.
}
