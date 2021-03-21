# Copyright 2020 Observational Health Data Sciences and Informatics
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

# given a concept set tble, get optimization recommendation
#' @export
getOptimizationRecommendationForConceptSetTable <-
  function(conceptSetExpressionDataFrame,
           vocabularyDatabaseSchema = 'vocabulary',
           connection) {
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
    
    conceptSetConceptIdsExcluded <- conceptSetExpressionDataFrame %>%
      dplyr::filter(.data$isExcluded == TRUE) %>%
      dplyr::pull(.data$conceptId)
    
    conceptSetConceptIdsDescendantsExcluded <-
      conceptSetExpressionDataFrame %>%
      dplyr::filter(.data$isExcluded == TRUE &&
                      .data$includeDescendants == TRUE) %>%
      dplyr::pull(.data$conceptId)
    
    conceptSetConceptIdsNotExcluded <-
      conceptSetExpressionDataFrame %>%
      dplyr::filter(!.data$isExcluded == TRUE) %>%
      dplyr::pull(.data$conceptId)
    
    conceptSetConceptIdsDescendantsNotExcluded <-
      conceptSetExpressionDataFrame %>%
      dplyr::filter(!.data$isExcluded == TRUE &&
                      .data$includeDescendants == TRUE) %>%
      dplyr::pull(.data$conceptId)
    
    if (any(
      is.null(conceptSetConceptIdsExcluded),
      is.na(conceptSetConceptIdsExcluded),
      length(conceptSetConceptIdsExcluded) == 0
    )) {
      conceptSetConceptIdsExcluded <- 0
    }
    
    if (any(
      is.null(conceptSetConceptIdsDescendantsExcluded),
      is.na(conceptSetConceptIdsDescendantsExcluded),
      length(conceptSetConceptIdsDescendantsExcluded) == 0
    )) {
      conceptSetConceptIdsDescendantsExcluded <- 0
    }
    
    if (any(
      is.null(conceptSetConceptIdsNotExcluded),
      is.na(conceptSetConceptIdsNotExcluded),
      length(conceptSetConceptIdsNotExcluded) == 0
    )) {
      conceptSetConceptIdsNotExcluded <- 0
    }
    
    if (any(
      is.null(conceptSetConceptIdsDescendantsNotExcluded),
      is.na(conceptSetConceptIdsDescendantsNotExcluded),
      length(conceptSetConceptIdsDescendantsNotExcluded) == 0
    )) {
      conceptSetConceptIdsDescendantsNotExcluded <- 0
    }
    
    #switch between sql with or without temp table based on
    #number of concept ids to optimize
    numberOfConceptIds <- length(unique(
      c(
        conceptSetConceptIdsExcluded,
        conceptSetConceptIdsDescendantsExcluded,
        conceptSetConceptIdsNotExcluded,
        conceptSetConceptIdsDescendantsNotExcluded
      )
    ))
    
    sqlWithTemporaryTable <-
      SqlRender::loadRenderTranslateSql(
        sqlFilename = "OptimizeConceptSetWithTemporaryTable.sql",
        packageName = "ConceptSetDiagnostics",
        vocabulary_database_schema = vocabularyDatabaseSchema,
        conceptSetConceptIdsExcluded = conceptSetConceptIdsExcluded,
        conceptSetConceptIdsDescendantsExcluded = conceptSetConceptIdsDescendantsExcluded,
        conceptSetConceptIdsNotExcluded = conceptSetConceptIdsNotExcluded,
        conceptSetConceptIdsDescendantsNotExcluded = conceptSetConceptIdsDescendantsNotExcluded
      )
    
    sqlWithoutTemporaryTable <-
      SqlRender::loadRenderTranslateSql(
        sqlFilename = "OptimizeConceptSetWithoutTempTable.sql",
        packageName = "ConceptSetDiagnostics",
        vocabulary_database_schema = vocabularyDatabaseSchema,
        conceptSetConceptIdsExcluded = conceptSetConceptIdsExcluded,
        conceptSetConceptIdsDescendantsExcluded = conceptSetConceptIdsDescendantsExcluded,
        conceptSetConceptIdsNotExcluded = conceptSetConceptIdsNotExcluded,
        conceptSetConceptIdsDescendantsNotExcluded = conceptSetConceptIdsDescendantsNotExcluded
      )
    
    
    if (numberOfConceptIds > 100) {
      sql <- sqlWithTemporaryTable
      renderTranslateExecuteSql(connection = connection,
                                sql = sql)
      retrieveSql <-
        SqlRender::translate(sql = "SELECT * FROM #optimized_set;", targetDialect = "postgresql")
    } else {
      sql <- sqlWithoutTemporaryTable
      retrieveSql <- sql
    }
    
    data <-
      renderTranslateQuerySql(
        connection = connection,
        sql = retrieveSql,
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::arrange(1) %>%
      dplyr::tibble() %>%
      dplyr::filter(.data$conceptId != 0)
    
    return(data)
  }