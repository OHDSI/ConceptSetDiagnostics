# Copyright 2024 Observational Health Data Sciences and Informatics
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

#' Given a concept set expression, get the resolved concept ids.
#'
#' @template Connection
#'
#' @template ConceptSetExpression
#'
#' @template VocabularyDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @return
#' Returns a tibble data frame of distinct sorted concept ids.
#'
#' @export
resolveConceptSetExpression <- function(conceptSetExpression,
                                        connection = NULL,
                                        connectionDetails = NULL,
                                        tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                        vocabularyDatabaseSchema) {
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  conceptSetSql <- CirceR::buildConceptSetQuery(conceptSetJSON = conceptSetExpression |> RJSONIO::toJSON(digits = 23))
  
  resolvedConceptIds <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = conceptSetSql,
      vocabulary_database_schema = vocabularyDatabaseSchema,
      snakeCaseToCamelCase = TRUE,
      tempEmulationSchema = tempEmulationSchema
    ) |>
    dplyr::distinct() |> 
    dplyr::arrange(conceptId) |> 
    dplyr::pull()
  
  return(resolvedConceptIds)
}
