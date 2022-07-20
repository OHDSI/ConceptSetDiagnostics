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

#' given a list of conceptIds, get their mapped
#'
#' @description
#' Given a concept set expression, get the resolved concepts
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
getMappedSourceConcepts <-
  function(conceptIds,
           connection = NULL,
           connectionDetails = NULL,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           vocabularyDatabaseSchema = "vocabulary") {
    if (length(conceptIds) == 0) {
      stop("No concept id provided")
    }
    
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    
    tempTableName <- loadTempConceptTable(conceptIds = conceptIds,
                                          connection = connection)
    
    sql <-
      "--get all source codes that map to the list of provided standard concept id
            SELECT cr.CONCEPT_ID_2 AS SEARCHED_CONCEPT_ID,
              c.*
            FROM @vocabulary_database_schema.concept_relationship cr
            INNER JOIN @concept_id_table cid
            ON cr.concept_id_2 = cid.concept_id
            INNER JOIN @vocabulary_database_schema.concept c ON c.concept_id = cr.concept_id_1
            WHERE relationship_id IN ('Maps to');"
    
    data <-
      DatabaseConnector::renderTranslateQuerySql(
        sql = sql,
        connection = connection,
        concept_id_table = tempTableName,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        tempEmulationSchema = tempEmulationSchema,
        snakeCaseToCamelCase = TRUE
      ) %>%
      tidyr::tibble()
    
    dropTempConceptTable(connection = connection, 
                         tempTableName = tempTableName)
    
    return(data)
  }
