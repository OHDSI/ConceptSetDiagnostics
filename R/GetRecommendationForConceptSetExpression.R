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

#' Get recommended concepts for a concept set expression.
#'
#' @template Connection
#'
#' @template ConnectionDetails
#'
#' @template VocabularyDatabaseSchema
#'
#' @template ConceptSetExpression
#'
#' @param vocabularyIdOfInterest A list of vocabulary id from OMOP to filter the results
#'
#' @param domainIdOfInterest A list of domain id from OMOP to filter the results
#'
#' @export
getRecommendationForConceptSetExpression <-
  function(conceptSetExpression,
           vocabularyDatabaseSchema = 'vocabulary',
           vocabularyIdOfInterest = c('SNOMED', 'HCPCS', 'ICD10CM', 'ICD10', 'ICD9CM', 'ICD9', 'Read'),
           domainIdOfInterest = c('Condition', 'Procedure', 'Observation'),
           connection = NULL,
           connectionDetails = NULL) {
    conceptSetExpressionDataFrame <-
      getConceptSetExpressionDataFrameFromConceptSetExpression(conceptSetExpression = conceptSetExpression)
    
    if (length(vocabularyIdOfInterest) > 0) {
      conceptSetExpressionDataFrame <- conceptSetExpressionDataFrame %>%
        dplyr::filter(.data$vocabularyId %in% vocabularyIdOfInterest)
    }
    if (length(domainIdOfInterest) > 0) {
      conceptSetExpressionDataFrame <- conceptSetExpressionDataFrame %>%
        dplyr::filter(.data$domainId %in% domainIdOfInterest)
    }
    
    conceptSetExpression <-
      getConceptSetExpressionFromConceptSetExpressionDataFrame(conceptSetExpressionDataFrame = conceptSetExpressionDataFrame)
    
    resolvedConceptIds <-
      resolveConceptSetExpression(
        conceptSetExpression = conceptSetExpression,
        connection = connection,
        connectionDetails = connectionDetails
      )
    forRecommendation <-
      c(
        resolvedConceptIds$resolvedConcepts$conceptId,
        resolvedConceptIds$mappedConcepts$conceptId
      ) %>% unique()
    recommendedStandard <-
      getRecommendedStandard(
        connection = connection,
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        conceptIds = forRecommendation
      )
    recommendedSource <-
      getRecommendedSource(
        connection = connection,
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        conceptIds = forRecommendation
      )
    
    data <- list()
    data$recommendedStandard <- recommendedStandard
    data$recommendedSource <- recommendedSource
    return(data)
  }
