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

#' Given a cohort definition expression, get the resolved concepts for all concept sets
#'
#' @template Connection
#'
#' @template CohortExpression
#'
#' @template VocabularyDatabaseSchema
#'
#' @export
resolveConceptSetsInCohortExpression <- function(cohortExpression,
                                                 connection = NULL,
                                                 connectionDetails = NULL,
                                                 vocabularyDatabaseSchema = "vocabulary") {
  conceptSetExpressionDataFrame <-
    extractConceptSetsInCohortDefinition(
      cohortExpression =
        cohortExpression
    )

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  resolvedConceptSet <- list()
  for (i in (1:nrow(conceptSetExpressionDataFrame))) {
    sql <- conceptSetExpressionDataFrame[i, ]$conceptSetSql
    resolvedConceptSet[[i]] <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabulary_database_schema = vocabularyDatabaseSchema, 
        snakeCaseToCamelCase = TRUE
      )
  }

  resolvedConceptSet <- dplyr::bind_rows(resolvedConceptSet) %>% 
    dplyr::arrange(codesetId,
                   conceptId) %>% 
    dplyr::distinct()
  
  return(resolvedConceptSet)
}
