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
#' @template TempEmulationSchema
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
getConceptAncestor <-
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

    tempTableName <- loadTempConceptTable(
      conceptIds = conceptIds,
      connection = connection,
      tempEmulationSchema = tempEmulationSchema
    )

    sql <- "SELECT ca.*
            FROM @vocabulary_database_schema.concept_ancestor ca
            WHERE ca.descendant_concept_id IN
            (
              SELECT DISTINCT concept_id
              FROM @concept_id_table t
            );"

    data <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        tempEmulationSchema = tempEmulationSchema,
        concept_id_table = tempTableName,
        snakeCaseToCamelCase = TRUE
      ) %>%
      tidyr::tibble()

    dropTempConceptTable(
      connection = connection,
      tempTableName = tempTableName,
      tempEmulationSchema = tempEmulationSchema
    )
    return(data)
  }
