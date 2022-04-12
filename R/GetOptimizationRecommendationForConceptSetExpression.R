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


#' given a concept set expression, get optimization recommendation
#'
#' @description
#' given a concept set expression, get optimization recommendation
#'
#' @template Connection
#'
#' @template VocabularyDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @template ConceptSetExpression
#' 
#' @return
#' Returns a tibble data frame.
#'
#' @export
getOptimizationRecommendationForConceptSetExpression <-
  function(conceptSetExpression,
           vocabularyDatabaseSchema = 'vocabulary',
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           connectionDetails = NULL,
           connection = NULL) {
    conceptSetExpressionDataFrame <-
      getConceptSetExpressionDataFrameFromConceptSetExpression(conceptSetExpression)
    if (nrow(conceptSetExpressionDataFrame) <= 1) {
      # no optimization necessary
      return(
        conceptSetExpressionDataFrame %>%
          dplyr::mutate(
            excluded = as.integer(.data$isExcluded),
            removed = 0
          ) %>%
          dplyr::select(.data$conceptId, .data$excluded, .data$removed)
      )
    }
    
    if (all(is.null(connectionDetails),
            is.null(connection))) {
      stop('Please provide either connection or connectionDetails to connect to database.')
    }
    # Set up connection to server----
    if (is.null(connection)) {
      if (!is.null(connectionDetails)) {
        connection <- DatabaseConnector::connect(connectionDetails)
        on.exit(DatabaseConnector::disconnect(connection))
      }
    }
    
    conceptSetConceptIdsExcluded <-
      conceptSetExpressionDataFrame %>%
      dplyr::filter(.data$isExcluded == TRUE) %>%
      dplyr::pull(.data$conceptId)
    
    conceptSetConceptIdsDescendantsExcluded <-
      conceptSetExpressionDataFrame %>%
      dplyr::filter(.data$isExcluded == TRUE) %>%
      dplyr::filter(.data$includeDescendants == TRUE) %>%
      dplyr::pull(.data$conceptId)
    
    conceptSetConceptIdsNotExcluded <-
      conceptSetExpressionDataFrame %>%
      dplyr::filter(!.data$isExcluded == TRUE) %>%
      dplyr::pull(.data$conceptId)
    
    conceptSetConceptIdsDescendantsNotExcluded <-
      conceptSetExpressionDataFrame %>%
      dplyr::filter(!.data$isExcluded == TRUE) %>%
      dplyr::filter(.data$includeDescendants == TRUE) %>%
      dplyr::pull(.data$conceptId)
    
    if (!hasData(conceptSetConceptIdsExcluded)) {
      conceptSetConceptIdsExcluded <- 0
    }
    if (!hasData(conceptSetConceptIdsDescendantsExcluded)) {
      conceptSetConceptIdsDescendantsExcluded <- 0
    }
    if (!hasData(conceptSetConceptIdsNotExcluded)) {
      conceptSetConceptIdsNotExcluded <- 0
    }
    if (!hasData(conceptSetConceptIdsDescendantsNotExcluded)) {
      conceptSetConceptIdsDescendantsNotExcluded <- 0
    }
    
    sql <- SqlRender::loadRenderTranslateSql(
      "OptimizeConceptSet.sql",
      packageName = utils::packageName(),
      dbms = connection@dbms,
      tempEmulationSchema = tempEmulationSchema,
      vocabulary_database_schema = vocabularyDatabaseSchema,
      conceptSetConceptIdsExcluded = conceptSetConceptIdsExcluded,
      conceptSetConceptIdsDescendantsExcluded = conceptSetConceptIdsDescendantsExcluded,
      conceptSetConceptIdsNotExcluded = conceptSetConceptIdsNotExcluded,
      conceptSetConceptIdsDescendantsNotExcluded = conceptSetConceptIdsDescendantsNotExcluded
    )
    
    DatabaseConnector::executeSql(
      connection = connection,
      sql = sql,
      reportOverallTime = FALSE,
      progressBar = FALSE
    )
    
    data <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = "SELECT * FROM #optimized_set;",
        snakeCaseToCamelCase = TRUE
      )
    
    sqlCleanUp <- "DROP TABLE IF EXISTS #optimized_set;"
    
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sqlCleanUp,
      reportOverallTime = FALSE,
      progressBar = FALSE
    )
    data <- data %>%
      dplyr::filter(.data$conceptId != 0)
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    return(data)
  }