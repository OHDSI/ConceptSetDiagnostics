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


#' get concept id count
#'
#' @description
#' Get the count for an array of concept id(s) from concept prevalence table.
#'
#' @template Connection
#'
#' @template ConceptIds
#'
#' @template ConceptPrevalenceSchema
#'
#' @template TempEmulationSchema
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
getConceptPrevalenceCounts <- function(conceptIds = NULL,
                                       connection = NULL,
                                       connectionDetails = NULL,
                                       conceptPrevalenceSchema = "concept_prevalence",
                                       tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  conceptPrevalenceTables <-
    DatabaseConnector::getTableNames(connection = connection,
                                     databaseSchema = conceptPrevalenceSchema) %>%
    tolower()
  
  conceptPrevalenceTablesExist <- FALSE
  
  if (all(
    "recommender_set" %in% conceptPrevalenceTables,
    "cp_master" %in% conceptPrevalenceTables,
    "recommended_blacklist" %in% conceptPrevalenceTables
  )) {
    conceptPrevalenceTablesExist <- TRUE
  }
  
  if (!conceptPrevalenceTablesExist) {
    stop(
      "Concept Prevalence schema does not have the required concept prevalence tables. recommender_set, cp_master, recommended_blacklist"
    )
  }
  
  tempTableName <- NULL
  if (!is.null(conceptIds)) {
    tempTableName <- loadTempConceptTable(
      conceptIds = conceptIds,
      connection = connection,
      tempEmulationSchema = tempEmulationSchema
    )
  }
  
  sql <- "SELECT cp.*
          FROM
            @concept_prevalence_schema.cp_master cp
        {@concept_id_table != ''} ? {
                INNER JOIN
                  @concept_id_table t
                ON cp.concept_id = t.concept_id
        };"
  
  data <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      concept_prevalence_schema = conceptPrevalenceSchema,
      concept_id_table = tempTableName,
      sql = sql,
      snakeCaseToCamelCase = TRUE
    ) %>% dplyr::tibble()
  
  if (!is.null(conceptIds)) {
    dropTempConceptTable(
      connection = connection,
      tempTableName = tempTableName,
      tempEmulationSchema = tempEmulationSchema
    )
  }
  return(data)
}
