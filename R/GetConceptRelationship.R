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

#' given a list of conceptIds, get their relationship
#'
#' @template Connection
#'
#' @template ConceptIds
#'
#' @template VocabularyDatabaseSchema
#' 
#' @template TempEmulationSchema
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
getConceptRelationship <-
  function(conceptIds,
           connection = NULL,
           connectionDetails = NULL,
           tempEmulationSchema = NULL,
           vocabularyDatabaseSchema = "vocabulary") {
    if (length(conceptIds) == 0) {
      stop("No concept id provided")
    }
    
    start <- Sys.time()
    
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    
    tempTableName <- loadTempConceptTable(conceptIds = conceptIds,
                                          connection = connection)
    
    sql <- "SELECT DISTINCT *
            FROM
            (SELECT cr.*
            FROM @vocabulary_database_schema.concept_relationship cr
            INNER JOIN @concept_id_table cid
            ON CONCEPT_ID_1 = CONCEPT_ID

            UNION

            SELECT cr.*
            FROM @vocabulary_database_schema.concept_relationship cr
            INNER JOIN @concept_id_table cid
            ON CONCEPT_ID_2 = CONCEPT_ID) f
            ;"
    
    data <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = sql,
      snakeCaseToCamelCase = TRUE,
      concept_id_table = tempTableName,
      vocabulary_database_schema = vocabularyDatabaseSchema
    ) %>%
      tidyr::tibble()
    
    dropTempConceptTable(connection = connection, 
                         tempTableName = tempTableName)
    return(data)
  }
