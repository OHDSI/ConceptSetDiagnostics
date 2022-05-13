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

#' get MedDRA PT for LLT
#'
#' @description
#' given an array of conceptIds belonging to MedDRA LLT, get their corresponding PT
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
#' Returns a tibble data frame with fields lltConceptId, lltConceptName, 
#' ptConceptId, ptConceptName
#'
#' @export
getMedraPtForLlt <-
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
    
    sql <- "SELECT c.concept_id llt_concept_id,
            	c.concept_name llt_concept_name,
            	c2.concept_id pt_concept_id,
            	c2.concept_name pt_concept_name
            FROM @vocabulary_database_schema.concept c
            INNER JOIN @vocabulary_database_schema.concept_relationship cr ON c.concept_id = cr.concept_id_1
            INNER JOIN @vocabulary_database_schema.concept c2 ON cr.concept_id_2 = c2.concept_id
            INNER JOIN @concept_id_table cid ON c.concept_id = cid.concept_id
            WHERE cr.relationship_id = 'Is a'
            	AND c.concept_class_id = 'LLT'
            	AND c2.concept_class_id = 'PT';"
    
    data <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        concept_id_table = tempTableName,
        snakeCaseToCamelCase = TRUE
      ) %>%
      tidyr::tibble()
    
    dropTempConceptTable(connection = connection, 
                         tempTableName = tempTableName)
    return(data)
  }
