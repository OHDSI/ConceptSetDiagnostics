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

# given a concept set expression, get optimized concept set expression
#' @export
optimizeConceptSetExpression <-
  function(conceptSetExpression,
           vocabularyDatabaseSchema = 'vocabulary',
           connection) {
    conceptSetExpressionDataFrame <-
      getConceptSetExpressionDataFrameFromConceptSetExpression(connection = connection,
                                                               conceptSetExpression =
                                                                 conceptSetExpression)
    
    optimizationRecommendation <- getOptimizationRecommendationForConceptSetTable(
      connection = connection,
      conceptSetExpressionDataFrame = conceptSetExpressionDataFrame,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema)
    
    removed <- optimizationRecommendation %>%
      dplyr::mutate(isExcluded = as.logical(.data$excluded)) %>%
      dplyr::filter(.data$removed == 1) %>%
      dplyr::select(.data$conceptId, .data$isExcluded)
    
    retained <- optimizationRecommendation %>%
      dplyr::mutate(isExcluded = as.logical(.data$excluded)) %>%
      dplyr::filter(.data$removed == 0) %>%
      dplyr::anti_join(removed %>% 
                         dplyr::select(.data$conceptId) %>% 
                         dplyr::distinct()) %>% 
      dplyr::select(.data$conceptId, .data$isExcluded)
    
    if (nrow(retained) > 0) {
      conceptSetExpressionDataFrame <- conceptSetExpression %>% 
        getConceptSetExpressionDataFrameFromConceptSetExpression() %>%
        dplyr::inner_join(retained, by = c('conceptId', 'isExcluded'))
    }
    
    conceptSetExpressionDataFrame <-
      conceptSetExpressionDataFrame %>%
      dplyr::arrange(.data$conceptId)
    
    conceptSetExpression <-
      getConceptSetExpressionFromConceptSetExpressionDataFrame(conceptSetExpressionDataFrame = conceptSetExpressionDataFrame)
    return(conceptSetExpression)
  }