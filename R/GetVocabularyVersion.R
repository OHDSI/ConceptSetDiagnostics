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

#' Get vocabulary version.
#'
#' @template Connection
#'
#' @template VocabularyDatabaseSchema
#'
#' @export
getVocabularyVersion <-
  function(connection = NULL,
           connectionDetails = NULL,
           vocabularyDatabaseSchema = "vocabulary") {
    start <- Sys.time()
    
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    
    data <-
      DatabaseConnector::querySql(
        connection = connection,
        sql = "select VOCABULARY_VERSION 
              from @vocabulary_database_schema.vocabulary
              where VOCABULARY_ID = 'None';",
        vocabulary_database_schema = vocabularyDatabaseSchema,
        snakeCaseToCamelCase = TRUE
      ) %>%
      tidyr::tibble()
    return(data)
  }