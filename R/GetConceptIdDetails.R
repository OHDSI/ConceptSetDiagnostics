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

#' get concept id details
#'
#' @description
#' given an array of conceptIds, get their details
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
getConceptIdDetails <-
  function(conceptIds,
           connection = NULL,
           connectionDetails = NULL,
           vocabularyDatabaseSchema = "vocabulary",
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }

    tempTableName <- loadTempConceptTable(
      conceptIds = conceptIds,
      connection = connection,
      tempEmulationSchema = tempEmulationSchema
    )

    sql <- "
      SELECT c.*
      FROM @vocabulary_database_schema.concept c
      INNER JOIN
        @concept_id_table t
      ON c.concept_id = t.concept_id
      ORDER BY c.concept_id;"

    data <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = sql,
      snakeCaseToCamelCase = TRUE,
      concept_id_table = tempTableName,
      tempEmulationSchema = tempEmulationSchema,
      vocabulary_database_schema = vocabularyDatabaseSchema
    )

    data <- data %>%
      tidyr::tibble() %>%
      dplyr::mutate(
        standardConceptCaption = dplyr::case_when(
          .data$standardConcept == "S" ~ "Standard",
          .data$standardConcept == "C" ~ "Classification"
        )
      ) %>%
      dplyr::mutate(
        invalidReasonCaption = dplyr::case_when(
          invalidReason == "V" ~ "Valid",
          invalidReason == "D" ~ "Deleted",
          invalidReason == "U" ~ "Updated",
          TRUE ~ "Valid"
        )
      )

    dropTempConceptTable(
      connection = connection,
      tempTableName = tempTableName,
      tempEmulationSchema = tempEmulationSchema
    )

    return(data)
  }
