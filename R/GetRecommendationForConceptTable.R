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

#' @export
getRecommendationForConceptSetExpressionDataFrame <-
  function(conceptSetExpressionDataFrame,
           vocabularyDatabaseSchema = 'vocabulary',
           vocabularyIdForRecommender = c('SNOMED', 'ICD'),
           connection) {
    searchVocabularyId <-
      paste0(vocabularyIdForRecommender, collapse = "|")
    
    forRecommendation <- conceptSetExpressionDataFrame %>%
      dplyr::filter(.data$domainId %in% c('Observation', 'Condition')) %>%
      dplyr::filter(stringr::str_detect(string = .data$vocabularyId,
                                        pattern = searchVocabularyId)) %>%
      dplyr::pull(.data$conceptId) %>%
      unique()
    recommendedStandard <-
      getRecommendedStandard(connection = connection,
                             conceptList = forRecommendation)
    recommendedSource <-
      getRecommendedSource(connection = connection,
                           conceptList = forRecommendation)
    
    data <- list()
    data$recommendedStandard <- recommendedStandard
    data$recommendedSource <- recommendedSource
    return(data)
  }