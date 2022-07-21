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

#' given a concept set expression, get optimized concept set expression
#'
#' @template Connection
#'
#' @template ConceptSetExpression
#'
#' @template VocabularyDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @export
optimizeConceptSetExpression <-
  function(conceptSetExpression,
           vocabularyDatabaseSchema = "vocabulary",
           connection = NULL,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           connectionDetails = NULL) {
    conceptSetExpressionDataFrame <-
      convertConceptSetExpressionToDataFrame(
        connection = connection,
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        tempEmulationSchema = tempEmulationSchema,
        conceptSetExpression =
          conceptSetExpression
      )

    optimizationRecommendation <-
      getOptimizationRecommendationForConceptSetExpression(
        connection = connection,
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        conceptSetExpression = conceptSetExpression,
        tempEmulationSchema = tempEmulationSchema
      )

    removed <- optimizationRecommendation %>%
      dplyr::mutate(isExcluded = as.logical(.data$excluded)) %>%
      dplyr::filter(.data$removed == 1) %>%
      dplyr::select(.data$conceptId, .data$isExcluded)

    retained <- optimizationRecommendation %>%
      dplyr::mutate(isExcluded = as.logical(.data$excluded)) %>%
      dplyr::filter(.data$removed == 0) %>%
      dplyr::anti_join(removed %>%
        dplyr::select(.data$conceptId) %>%
        dplyr::distinct(),
      by = "conceptId"
      ) %>%
      dplyr::select(.data$conceptId, .data$isExcluded)

    conceptSetExpressionDataFrame <- NULL
    if (nrow(retained) > 0) {
      conceptSetExpressionDataFrame <- conceptSetExpression %>%
        convertConceptSetExpressionToDataFrame() %>%
        dplyr::inner_join(retained, by = c("conceptId", "isExcluded"))
    }

    if (nrow(removed) > 0) {
      removed <- conceptSetExpression %>%
        convertConceptSetExpressionToDataFrame() %>%
        dplyr::inner_join(removed, by = c("conceptId", "isExcluded"))
    }

    conceptSetExpressionDataFrame <-
      conceptSetExpressionDataFrame %>%
      dplyr::arrange(.data$conceptId)

    conceptSetExpression <-
      convertConceptSetDataFrameToExpression(conceptSetExpressionDataFrame = conceptSetExpressionDataFrame)

    data <- list(
      recommended = conceptSetExpression,
      recommendedAsDataFrame = conceptSetExpressionDataFrame,
      removed = removed
    )

    return(data)
  }



getOptimizationRecommendationForConceptSetExpression <-
  function(conceptSetExpression,
           vocabularyDatabaseSchema = "vocabulary",
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           connectionDetails = NULL,
           connection = NULL) {
    conceptSetExpressionDataFrame <-
      convertConceptSetExpressionToDataFrame(conceptSetExpression)
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

    if (all(
      is.null(connectionDetails),
      is.null(connection)
    )) {
      stop("Please provide either connection or connectionDetails to connect to database.")
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
        tempEmulationSchema = tempEmulationSchema,
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
    return(data)
  }
