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
#' @param replaceNonStandardWithStandardEquivalent Do you want to replace non standard with standard equivalent?
#'
#' @export
optimizeConceptSetExpression <-
  function(conceptSetExpression,
           vocabularyDatabaseSchema = "vocabulary",
           connection = NULL,
           connectionDetails = NULL,
           replaceNonStandardWithStandardEquivalent = TRUE,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
    writeLines("reminder: incomplete replaceNonStandardWithStandardEquivalent. i.e. for every non standard, replace its standard equivalent option")
    optimizationRecommendation <-
      getOptimizationRecommendationForConceptSetExpression(
        connection = connection,
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        conceptSetExpression = conceptSetExpression,
        tempEmulationSchema = tempEmulationSchema
      )
    conceptSetExpressionDataFrame <- conceptSetExpression %>%
      convertConceptSetExpressionToDataFrame()
    
    removed <- conceptSetExpressionDataFrame %>%
      dplyr::anti_join(
        optimizationRecommendation %>%
          convertConceptSetExpressionToDataFrame() %>%
          dplyr::select(.data$conceptId,
                        .data$isExcluded),
        by = c("conceptId", "isExcluded")
      ) %>%
      dplyr::select(dplyr::all_of(colnames(conceptSetExpressionDataFrame)))
    
    optimizedConceptSetDf <- optimizationRecommendation %>%
      convertConceptSetExpressionToDataFrame()
    
    data <-
      list(
        optimizedConceptSetExpression = optimizationRecommendation,
        optimizedConceptSet = optimizedConceptSetDf,
        removedRecords = removed %>%
          dplyr::distinct()
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
    
    # Set up connection to server----
    if (is.null(connection)) {
      if (!is.null(connectionDetails)) {
        connection <- DatabaseConnector::connect(connectionDetails)
        on.exit(DatabaseConnector::disconnect(connection))
      }
    }
    
    if (nrow(conceptSetExpressionDataFrame) <= 1) {
      # no optimization necessary
      data <- conceptSetExpressionDataFrame %>%
        convertConceptSetDataFrameToExpression(
          updateVocabularyFields = TRUE,
          vocabularyDatabaseSchema = vocabularyDatabaseSchema,
          connection = connection
        )
      return(data)
    }
    
    includedConcepts <-
      conceptSetExpressionDataFrame %>%
      dplyr::filter(!.data$isExcluded == TRUE) %>%
      dplyr::pull(.data$conceptId)
    
    includedConceptsWithDescendants <-
      conceptSetExpressionDataFrame %>%
      dplyr::filter(!.data$isExcluded == TRUE) %>%
      dplyr::filter(.data$includeDescendants == TRUE) %>%
      dplyr::pull(.data$conceptId)
    
    excludedConcepts <-
      conceptSetExpressionDataFrame %>%
      dplyr::filter(.data$isExcluded == TRUE) %>%
      dplyr::pull(.data$conceptId)
    
    excludedConceptsWithDescendants <-
      conceptSetExpressionDataFrame %>%
      dplyr::filter(.data$isExcluded == TRUE) %>%
      dplyr::filter(.data$includeDescendants == TRUE) %>%
      dplyr::pull(.data$conceptId)
    
    inlcudedConceptSetExpression <- dplyr::tibble()
    if (length(includedConcepts) > 0) {
      if (length(includedConcepts) > 1) {
        sql <- SqlRender::loadRenderTranslateSql(
          sqlFilename = "OptimizeConceptSet.sql",
          packageName = "ConceptSetDiagnostics",
          dbms = connection@dbms,
          tempEmulationSchema = tempEmulationSchema,
          vocabulary_database_schema = vocabularyDatabaseSchema,
          conceptIds = includedConcepts,
          conceptIdsWithIncludeDescendants = includedConceptsWithDescendants
        )
        writeLines(" -  Optimizing concept set")
        DatabaseConnector::executeSql(
          connection = connection,
          sql = sql,
          reportOverallTime = FALSE,
          progressBar = TRUE
        )
        
        included <-
          DatabaseConnector::renderTranslateQuerySql(
            connection = connection,
            sql = "SELECT * FROM #optimized;",
            tempEmulationSchema = tempEmulationSchema,
            snakeCaseToCamelCase = TRUE
          ) %>%
          dplyr::tibble()
        
        sqlCleanUp <- "DROP TABLE IF EXISTS #optimized;"
        
        DatabaseConnector::renderTranslateExecuteSql(
          connection = connection,
          sql = sqlCleanUp,
          reportOverallTime = FALSE,
          progressBar = FALSE
        )
      } else if (length(includedConcepts) == 0) {
        included <- dplyr::tibble(
          originalConceptId = includedConcepts,
          replacementConceptId = includedConcepts,
          removedConceptId = NA
        )
      }
      inlcudedConceptSetExpression <-
        conceptSetExpressionDataFrame %>%
        dplyr::select(
          .data$conceptId,
          .data$includeMapped,
          .data$includeDescendants,
          .data$isExcluded
        ) %>%
        dplyr::filter(.data$isExcluded == FALSE) %>%
        dplyr::inner_join(
          included %>%
            dplyr::select(.data$originalConceptId,
                          .data$replacementConceptId),
          by = c("conceptId" = "originalConceptId")
        ) %>%
        dplyr::select(-.data$conceptId) %>%
        dplyr::rename("conceptId" = .data$replacementConceptId) %>%
        dplyr::group_by(.data$conceptId) %>%
        dplyr::summarise(
          includeMapped = as.logical(max(as.integer(
            .data$includeMapped
          ))),
          includeDescendants = as.logical(max(
            as.integer(.data$includeDescendants)
          )),
          isExcluded = as.logical(max(as.integer(
            .data$isExcluded
          )))
        ) %>%
        dplyr::select(
          .data$conceptId,
          .data$includeMapped,
          .data$includeDescendants,
          .data$isExcluded
        ) %>%
        dplyr::distinct()
    }
    
    
    excludedConceptSetExpression <- dplyr::tibble()
    if (length(excludedConcepts) > 0) {
      if (length(excludedConcepts) > 1) {
        sql <- SqlRender::loadRenderTranslateSql(
          sqlFilename = "OptimizeConceptSet.sql",
          packageName = "ConceptSetDiagnostics",
          dbms = connection@dbms,
          tempEmulationSchema = tempEmulationSchema,
          vocabulary_database_schema = vocabularyDatabaseSchema,
          conceptIds = excludedConcepts,
          conceptIdsWithIncludeDescendants = excludedConceptsWithDescendants
        )
        
        DatabaseConnector::executeSql(
          connection = connection,
          sql = sql,
          reportOverallTime = FALSE,
          progressBar = FALSE
        )
        
        excluded <-
          DatabaseConnector::renderTranslateQuerySql(
            connection = connection,
            sql = "SELECT * FROM #optimized;",
            tempEmulationSchema = tempEmulationSchema,
            snakeCaseToCamelCase = TRUE
          ) %>%
          dplyr::tibble()
        
        sqlCleanUp <- "DROP TABLE IF EXISTS #optimized;"
        
        DatabaseConnector::renderTranslateExecuteSql(
          connection = connection,
          sql = sqlCleanUp,
          reportOverallTime = FALSE,
          progressBar = FALSE
        )
      } else if (length(excludedConcepts) == 1) {
        excluded <- dplyr::tibble(
          originalConceptId = excludedConcepts,
          replacementConceptId = excludedConcepts,
          removedConceptId = NA
        )
      }
      
      excludedConceptSetExpression <-
        conceptSetExpressionDataFrame %>%
        dplyr::select(
          .data$conceptId,
          .data$includeMapped,
          .data$includeDescendants,
          .data$isExcluded
        ) %>%
        dplyr::filter(.data$isExcluded == TRUE) %>%
        dplyr::inner_join(
          included %>%
            dplyr::select(.data$originalConceptId,
                          .data$replacementConceptId),
          by = c("conceptId" = "originalConceptId")
        ) %>%
        dplyr::select(-.data$conceptId) %>%
        dplyr::rename("conceptId" = .data$replacementConceptId) %>%
        dplyr::group_by(.data$conceptId) %>%
        dplyr::summarise(
          includeMapped = as.logical(max(as.integer(
            .data$includeMapped
          ))),
          includeDescendants = as.logical(max(
            as.integer(.data$includeDescendants)
          )),
          isExcluded = as.logical(max(as.integer(
            .data$isExcluded
          )))
        ) %>%
        dplyr::select(
          .data$conceptId,
          .data$includeMapped,
          .data$includeDescendants,
          .data$isExcluded
        ) %>%
        dplyr::distinct()
    }
    
    finalConceptSetExpressionDf <-
      dplyr::bind_rows(inlcudedConceptSetExpression,
                       excludedConceptSetExpression) %>%
      dplyr::arrange(.data$conceptId,
                     .data$isExcluded)
    
    finalConceptSetExpression <- finalConceptSetExpressionDf  %>%
      dplyr::distinct() %>%
      convertConceptSetDataFrameToExpression(
        updateVocabularyFields = TRUE,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        connection = connection
      )
    
    return(finalConceptSetExpression)
  }
