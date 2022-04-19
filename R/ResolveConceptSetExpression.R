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

#' Given a concept set expression, get the resolved concepts
#'
#' @template Connection
#'
#' @template ConceptSetExpression
#'
#' @template VocabularyDatabaseSchema
#' 
#' @template ConceptPrevalenceTable
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
resolveConceptSetExpression <- function(conceptSetExpression,
                                        connection = NULL,
                                        connectionDetails = NULL,
                                        vocabularyDatabaseSchema = "vocabulary",
                                        conceptPrevalenceTable = NULL) {
  # convert concept set expression R object (list) to data frame
  conceptSetExpressionDataFrame <-
    getConceptSetExpressionDataFrameFromConceptSetExpression(
      connection = connection,
      connectionDetails = connectionDetails,
      conceptSetExpression = conceptSetExpression,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
  
  # get all descendant concept ids (as dataframe) for concepts that have
  # includeDescendants selected in conceptSetExpression
  conceptIdsWithIncludeDescendants <-
    conceptSetExpressionDataFrame %>%
    dplyr::filter(.data$includeDescendants == TRUE) %>%
    dplyr::pull(.data$conceptId)
  
  if (length(conceptIdsWithIncludeDescendants) == 0) {
    return(NULL)
  }
  descendantConcepts <-
    GetConceptDescendant(
      connection = connection,
      connectionDetails = connectionDetails,
      conceptIds = conceptIdsWithIncludeDescendants,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
  
  conceptIdDetailsForDescendantConcepts <-
    getConceptIdDetails(
      conceptIds = descendantConcepts$descendantConceptId %>% unique(),
      connection = connection,
      connectionDetails = connectionDetails,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
      conceptPrevalenceTable = conceptPrevalenceTable
    )
  browser()
  descendantConcepts <- descendantConcepts %>%
    dplyr::inner_join(conceptIdDetailsForDescendantConcepts, by = "conceptId")
  
  
  # get all conceptIds (as dataframe) that are excluded in concept set expression
  excludedConceptIds <- conceptSetExpressionDataFrame %>%
    dplyr::filter(.data$isExcluded == TRUE) %>%
    dplyr::select(.data$conceptId)
  
  # get all conceptIds (as dataframe) that are excluded in concept set expression with descendants
  excludedConceptIdsWithDescendants <- descendantConcepts %>%
    dplyr::filter(
      .data$ancestorConceptId %in% (
        conceptSetExpressionDataFrame %>%
          dplyr::filter(.data$isExcluded == TRUE) %>%
          dplyr::filter(.data$includeDescendants == TRUE) %>%
          dplyr::pull(.data$conceptId) %>%
          unique()
      )
    )
  
  # conceptIds in conceptSetExpression table
  conceptIdsInConceptSetExpressionTableToBeIncluded <-
    union(
      x = conceptSetExpressionDataFrame %>%
        dplyr::pull(.data$conceptId) %>%
        unique(),
      y = descendantConcepts %>%
        dplyr::pull(.data$conceptId) %>%
        unique()
    ) %>% unique()
  
  
  conceptIdsInConceptSetExpressionTableToBeExcluded <-
    union(
      x = excludedConceptIds %>%
        dplyr::pull(.data$conceptId) %>%
        unique(),
      y = excludedConceptIdsWithDescendants %>%
        dplyr::pull(.data$conceptId) %>%
        unique()
    ) %>%
    unique()
  
  # removed all excluded conceptIds including those with descendants == TRUE
  resolvedConceptIds <-
    setdiff(x = conceptIdsInConceptSetExpressionTableToBeIncluded,
            y = conceptIdsInConceptSetExpressionTableToBeExcluded)
  
  # get all resolved concept Ids as data frame
  resolvedConceptIds <- dplyr::union(
    conceptSetExpressionDataFrame %>%
      dplyr::filter(.data$isExcluded == FALSE) %>%
      dplyr::select(
        .data$conceptId,
        .data$conceptName,
        .data$conceptCode,
        .data$domainId,
        .data$vocabularyId,
        .data$conceptClassId,
        .data$standardConcept,
        .data$invalidReason
      ),
    descendantConcepts %>%
      dplyr::select(
        .data$conceptId,
        .data$conceptName,
        .data$conceptCode,
        .data$domainId,
        .data$vocabularyId,
        .data$conceptClassId,
        .data$standardConcept,
        .data$invalidReason
      )
  ) %>%
    dplyr::filter(.data$conceptId %in% resolvedConceptIds) %>%
    dplyr::select(
      .data$conceptId,
      .data$conceptName,
      .data$conceptCode,
      .data$domainId,
      .data$vocabularyId,
      .data$conceptClassId,
      .data$standardConcept,
      .data$invalidReason
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$conceptId)
  
  return(resolvedConceptIds)
}
