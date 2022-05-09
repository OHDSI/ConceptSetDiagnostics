# Copyright 2021 Observational Health Data Sciences and Informatics
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

#' get concept ancestor
#'
#' @description
#' given an array of conceptIds, get their ancestor and descendants.
#'
#' @template Connection
#'
#' @template ConceptIds
#'
#' @template VocabularyDatabaseSchema
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
getConceptAncestor <-
  function(conceptIds,
           connection = NULL,
           connectionDetails = NULL,
           vocabularyDatabaseSchema = "vocabulary") {
    if (length(conceptIds) == 0) {
      stop("No concept id provided")
    }
    
    start <- Sys.time()
    
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    
    
    conceptIdTable <-
      dplyr::tibble(conceptId = conceptIds %>% unique())
    
    tempTableName <-
      paste0("#t", (as.numeric(as.POSIXlt(Sys.time(
        
      )))) * 100000)
    DatabaseConnector::insertTable(
      connection = connection,
      tableName = tempTableName,
      dropTableIfExists = TRUE,
      tempTable = TRUE,
      tempEmulationSchema = tempEmulationSchema,
      data = conceptIdTable,
      camelCaseToSnakeCase = TRUE,
      bulkLoad = TRUE,
      progressBar = FALSE,
      createTable = TRUE
    )
    
    
    sql <- "SELECT DESCENDANT_CONCEPT_ID concept_id,
              ANCESTOR_CONCEPT_ID,
            	MIN_LEVELS_OF_SEPARATION,
            	MAX_LEVELS_OF_SEPARATION
            FROM @vocabulary_database_schema.concept_ancestor ca
            INNER JOIN @concept_id_table cid
            ON ca.descendant_concept_id = cid.concept_id;"
    
    data <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        concept_ids = conceptIds,
        concept_id_table = tempTableName,
        snakeCaseToCamelCase = TRUE
      ) %>%
      tidyr::tibble()
    
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = "DROP TABLE IF EXISTS @concept_id_table;",
      profile = FALSE,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema,
      concept_id_table = tempTableName
    )
    
    return(data)
  }
