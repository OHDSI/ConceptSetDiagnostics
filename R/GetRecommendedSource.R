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

#' given a list of non standard conceptIds, get recommended conceptIds
#'
#' @template Connection
#'
#' @template ConceptIds
#'
#' @template VocabularyDatabaseSchema
#'
#' @export
getRecommendedSource <-
  function(conceptIds,
           vocabularyDatabaseSchema = 'vocabulary',
           connection = NULL,
           connectionDetails = NULL,
           conceptPrevalenceSchema = 'concept_prevalence') {
    # Filtering strings to letters, numbers and spaces only to avoid SQL injection:
    conceptIds <-  gsub("[^a-zA-Z0-9 ,]", " ", conceptIds)
    
    sql <-
      SqlRender::readSql(
        sourceFile = system.file("sql", "sql_server", 'RecommendationSource.sql',
                                 package = "ConceptSetDiagnostics")
      )
    
    data <-
      renderTranslateQuerySql(
        connection = connection,
        connectionDetails = connectionDetails,
        sql = sql,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        concept_prevalence = conceptPrevalenceSchema,
        source_list = conceptIds[[1]],
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::filter(!.data$conceptId %in% conceptIds) %>%
      dplyr::arrange(dplyr::desc(.data$descendantRecordCount)) %>%
      dplyr::rename(
        rc = .data$recordCount,
        dc = .data$databaseCount,
        drc = .data$descendantRecordCount,
        dbc = .data$descendantDatabaseCount
      )
    return(data)
  }
