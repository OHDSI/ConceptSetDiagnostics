# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of ConceptSetDiagnostics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

#' Given a concept set expression, find the occurrence of the concept set
#'
#' @description
#' Given a concept set expression, find the occurrence of the concept set. The query will check
#' for the occurrence of any of the resolved conceptId's in the conceptSetExpression
#' in any domain tables. The domain tables are condition_occurrence, drug_exposure, procedure_occurrence,
#' visit_occurrence, observation.
#' Return objected is the name of the temp table
#'
#' @param connection          An object of type \code{connection} as created using the
#'                            \code{\link[DatabaseConnector]{connect}} function in the
#'                            DatabaseConnector package. Can be left NULL if \code{connectionDetails}
#'                            is provided, in which case a new connection will be opened at the start
#'                            of the function, and closed when the function finishes.
#'
#' @template VocabularyDatabaseSchema
#'
#' @template CdmDatabaseSchema
#'
#' @template TempEmulationSchema
#' 
#' @param restrictToObservationPeriod (Default = TRUE) Do you want to restrict to Observation period? i.e
#'                                      Cohort dates are restricted to observation period.
#'
#' @param conceptIds An array of concept ids
#'
#' @return
#' NULL
#'
#' @export
getConceptSetOccurrenceDate <- function(connection,
                                        cdmDatabaseSchema,
                                        vocabularyDatabaseSchema = cdmDatabaseSchema,
                                        conceptIds,
                                        subset = c("all"),
                                        restrictToObservationPeriod = TRUE,
                                        tempEmulationSchema = NULL) {
  subset <- tolower(subset) %>%
    stringr::str_trim() %>%
    stringr::str_squish()
  
  checkmate::assertChoice(
    x = subset,
    choices = c("all", "first", "last"),
    null.ok = FALSE
  )
  
  checkmate::assertIntegerish(
    x = conceptIds,
    lower = 0,
    any.missing = FALSE,
    min.len = 1
  )
  
  tempTableName <-
    paste0("#t", (as.numeric(as.POSIXlt(Sys.time(
      
    )))) * 100000)
  
  
  tempConceptTableName <- loadTempConceptTable(
    conceptIds = conceptIds %>% unique(),
    connection = connection,
    tempEmulationSchema = tempEmulationSchema
  )
  
  sql <- SqlRender::loadRenderTranslateSql(
    "GetConceptSetExpressionOccurrenceDates.sql",
    packageName = utils::packageName(),
    dbms = connection@dbms,
    tempEmulationSchema = tempEmulationSchema,
    cdm_database_schema = cdmDatabaseSchema,
    concept_id_table = tempConceptTableName,
    temp_table_name = tempTableName,
    first_occurrence_only = FALSE,
    last_occurrence_only = FALSE
  )
  ParallelLogger::logInfo(" Looking for resolved concepts in domain tables.")
  DatabaseConnector::executeSql(
    connection = connection,
    sql = sql,
    profile = FALSE,
    progressBar = TRUE,
    reportOverallTime = FALSE
  )
  
  return(tempTableName)
}