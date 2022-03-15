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

#' Get concept set expression object from concept set expression data frame
#'
#' @param conceptSetExpressionDataFrame   Concept set expression in data frame format.
#'
#' @param selectAllDescendants            Do you want to over ride the concept set
#'                                        expression by add select descendants for concept ids
#'                                        in concept set expression.
#'
#' @param purgeVocabularyDetails          Do you want to purge the details of concepts in the
#'                                        concept set expression.
#'
#' @export
getConceptSetExpressionFromConceptSetExpressionDataFrame <-
  function(conceptSetExpressionDataFrame,
           selectAllDescendants = FALSE,
           purgeVocabularyDetails = FALSE) {
    if (!'includeMapped' %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$includeMapped <- FALSE
    }
    if (!'isExcluded' %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$isExcluded <- FALSE
    }
    if (!'includeDescendants' %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$includeDescendants <- FALSE
    }
    if (selectAllDescendants) {
      conceptSetExpressionDataFrame <-
        dplyr::bind_rows(
          conceptSetExpressionDataFrame %>%
            dplyr::filter(.data$standardConcept == 'S') %>%
            dplyr::mutate(includeDescendants = TRUE),
          conceptSetExpressionDataFrame %>%
            dplyr::filter(!.data$standardConcept == 'S') %>%
            dplyr::mutate(includeDescendants = FALSE)
        )
    }
    if (purgeVocabularyDetails) {
      conceptSetExpressionDataFrame <- conceptSetExpressionDataFrame %>%
        dplyr::mutate(
          conceptName = "",
          standardConcept = "",
          standardConceptCaption = "",
          invalidReason = "",
          invalidReasonCaption = "",
          conceptCode = "",
          domainId = "",
          vocabularyId = "",
          conceptClassId = ""
        )
    }
    # note: r dataframe objects are always expected to have variables in camel case.
    # so the case conversion below should always be valid, if convention is followed
    colnames(conceptSetExpressionDataFrame) <-
      toupper(SqlRender::camelCaseToSnakeCase(colnames(conceptSetExpressionDataFrame)))
    
    conceptSetExpression <- list()
    conceptSetExpression$items <- list()
    if (nrow(conceptSetExpressionDataFrame) > 0) {
      for (i in (1:nrow(conceptSetExpressionDataFrame))) {
        conceptSetExpression$items[[i]] <- list()
        conceptSetExpression$items[[i]]$concept <-
          conceptSetExpressionDataFrame[i, ] %>%
          dplyr::select(-.data$INCLUDE_DESCENDANTS,
                        -.data$INCLUDE_MAPPED,
                        -.data$IS_EXCLUDED) %>%
          as.list()
        conceptSetExpression$items[[i]]$isExcluded <- 
        conceptSetExpressionDataFrame$IS_EXCLUDED[i]
        conceptSetExpression$items[[i]]$includeDescendants <-
          conceptSetExpressionDataFrame$INCLUDE_DESCENDANTS[i]
        conceptSetExpression$items[[i]]$includeMapped <-
          conceptSetExpressionDataFrame$INCLUDE_MAPPED[i]
      }
    }
    return(conceptSetExpression)
  }
