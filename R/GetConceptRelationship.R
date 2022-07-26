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
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           vocabularyDatabaseSchema = "vocabulary") {
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
              SELECT cr1.*
              FROM @vocabulary_database_schema.concept_relationship cr1
              INNER JOIN @concept_id_table t1
              ON cr1.concept_id_1 = t1.concept_id

              UNION

              SELECT cr2.*
              FROM @vocabulary_database_schema.concept_relationship cr2
              INNER JOIN @concept_id_table t2
              ON cr2.CONCEPT_ID_2 = t2.concept_id
            ;"

    data <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = sql,
      snakeCaseToCamelCase = TRUE,
      concept_id_table = tempTableName,
      tempEmulationSchema = tempEmulationSchema,
      vocabulary_database_schema = vocabularyDatabaseSchema
    ) %>%
      tidyr::tibble()

    dropTempConceptTable(
      connection = connection,
      tempTableName = tempTableName,
      tempEmulationSchema = tempEmulationSchema
    )
    return(data)
  }
