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

# Given a concept set expression, get the resolved concepts
#' @export
resolveConceptSetExpression <- function(conceptSetExpression,
                                        connection
) {
  # convert concept set expression R object (list) to data frame
  conceptSetExpressionDataFrame <-
    getConceptSetExpressionDataFrameFromConceptSetExpression(connection = connection,
                                                             conceptSetExpression =
                                                               conceptSetExpression)
  
  # get all descendant concept ids (as dataframe) for concepts that have
  # includeDescendants selected in conceptSetExpression
  descendantConcepts <-
    getDescendantConcepts(
      connection = connection,
      descendantConceptId = conceptSetExpressionDataFrame %>%
        dplyr::filter(.data$includeDescendants == TRUE) %>%
        dplyr::pull(.data$conceptId)
    )
  
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
          dplyr::pull(.data$conceptId)
      )
    )
  
  # conceptIds in conceptSetExpression table
  conceptIdsInConceptSetExpressionTableToBeIncluded <-
    union(
      x = conceptSetExpressionDataFrame %>%
        dplyr::pull(.data$conceptId),
      y = descendantConcepts %>% dplyr::pull(.data$conceptId)
    )
  conceptIdsInConceptSetExpressionTableToBeExcluded <-
    union(
      x = excludedConceptIds %>% dplyr::pull(.data$conceptId),
      y = excludedConceptIdsWithDescendants %>% dplyr::pull(.data$conceptId)
    )
  
  
  # removed all excluded conceptIds including those with descendants == TRUE
  resolvedConceptIds <-
    setdiff(x = conceptIdsInConceptSetExpressionTableToBeIncluded,
            y = conceptIdsInConceptSetExpressionTableToBeExcluded)
  
  #get all resolved concept Ids as data frame
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
  
  # get the mapped concepts for the resolved conceptIds
  mappedConcepts <-
    getMappedConcepts(
      connection = connection,
      mappedConceptId =  resolvedConceptIds %>%
        dplyr::pull(.data$conceptId)
    ) %>%
    dplyr::filter(.data$standardConceptId != .data$conceptId) %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      y = resolvedConceptIds %>%
        dplyr::select(.data$conceptId,
                      .data$conceptName) %>%
        dplyr::rename(standardConceptName = .data$conceptName),
      by = c("standardConceptId" = "conceptId")
    ) %>%
    dplyr::relocate(.data$standardConceptId, .data$standardConceptName) %>%
    dplyr::arrange(.data$standardConceptId, .data$standardConceptName)
  
  output <- list(resolvedConcepts = resolvedConceptIds,
                 mappedConcepts = mappedConcepts)
  return(output)
}