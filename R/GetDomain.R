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
#' @return
#' Returns a tibble data frame.
#'
#' @export
getDomain <-
  function(connection = NULL,
           connectionDetails = NULL,
           vocabularyDatabaseSchema = "vocabulary") {
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }

    data <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = "SELECT * FROM @vocabulary_database_schema.domain;",
        vocabulary_database_schema = vocabularyDatabaseSchema,
        snakeCaseToCamelCase = TRUE
      ) %>%
      tidyr::tibble()

    return(data)
  }
