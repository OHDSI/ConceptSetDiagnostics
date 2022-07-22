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

#' Get all the domain id(s) in the vocabulary schema.
#'
#' @description
#' Get all the domain id(s) in the vocabulary schema.
#'
#' @template Connection
#'
#' @template VocabularyDatabaseSchema
#'
#' @template CdmDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @param conceptIds an array of conceptIds that are used as reference to search for orphan concepts
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
findOrphanConcepts <- function(connectionDetails = NULL,
                               connection = NULL,
                               cdmDatabaseSchema,
                               vocabularyDatabaseSchema = cdmDatabaseSchema,
                               tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                               conceptIds) {
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  tempTableName <- loadTempConceptTable(
    conceptIds = conceptIds,
    connection = connection,
    tempEmulationSchema = tempEmulationSchema
  )

  sql <- SqlRender::loadRenderTranslateSql(
    "OrphanCodes.sql",
    packageName = utils::packageName(),
    dbms = connection@dbms,
    tempEmulationSchema = tempEmulationSchema,
    vocabulary_database_schema = vocabularyDatabaseSchema,
    concept_id_table = tempTableName,
    orphan_concept_table = paste0(tempTableName, "oo")
  )
  DatabaseConnector::executeSql(connection, sql)
  
  sql <- "SELECT * FROM @orphan_concept_table;"
  orphanConcepts <-
    DatabaseConnector::renderTranslateQuerySql(
      sql = sql,
      connection = connection,
      tempEmulationSchema = tempEmulationSchema,
      orphan_concept_table = paste0(tempTableName, "oo"),
      snakeCaseToCamelCase = TRUE
    ) %>%
    tidyr::tibble()

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "DROP TABLE IF EXISTS @orphan_concept_table;",
    orphan_concept_table = paste0(tempTableName, "oo"),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  return(orphanConcepts)
}
