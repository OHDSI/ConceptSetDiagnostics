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

#' given a list of conceptIds, get their synonyms
#'
#' @template Connection
#'
#' @template ConnectionDetails
#'
#' @template ConceptIds
#'
#' @template VocabularyDatabaseSchema
#'
#' @export
getConceptSynonym <-
  function(conceptIds,
           connection = NULL,
           connectionDetails = NULL,
           vocabularyDatabaseSchema = 'vocabulary') {
    
    if (length(conceptIds) == 0) {
      stop('No concept id provided')
    }
    
    sql <-
      SqlRender::readSql(
        sourceFile = system.file("sql", "sql_server", 'GetConceptSynonym.sql',
                                 package = "ConceptSetDiagnostics")
      )
    
    data <-
      renderTranslateQuerySql(
        connection = connection,
        connectionDetails = connectionDetails,
        sql = sql,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        concept_ids = conceptIds,
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::arrange(1)
    return(data)
  }