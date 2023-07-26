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

#' Given a concept set expression, get the resolved concepts
#'
#' @template Connection
#'
#' @template ConceptSetExpression
#'
#' @template VocabularyDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
resolveConceptSetExpression <- function(conceptSetExpression,
                                        connection = NULL,
                                        connectionDetails = NULL,
                                        tempEmulationSchema = NULL,
                                        vocabularyDatabaseSchema = "vocabulary") {
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  # convert concept set expression R object (list) to data frame
  conceptSetExpressionDataFrame <-
    convertConceptSetExpressionToDataFrame(
      updateVocabularyFields = FALSE,
      conceptSetExpression = conceptSetExpression,
      tempEmulationSchema = tempEmulationSchema
    )
  
  # get all descendant concept ids (as dataframe) for concepts that have
  # includeDescendants selected in conceptSetExpression
  conceptIdsWithIncludeDescendants <-
    conceptSetExpressionDataFrame |>
    dplyr::filter(.data$includeDescendants == TRUE) |>
    dplyr::pull(.data$conceptId)
  
  if (length(conceptIdsWithIncludeDescendants) == 0) {
    # get all resolved concept Ids
    resolvedConceptIds <-
      setdiff(
        conceptSetExpressionDataFrame$conceptId,
        conceptSetExpressionDataFrame |>
          dplyr::filter(.data$isExcluded == TRUE) |>
          dplyr::pull(.data$conceptId)
      )
  } else {
    descendantConcepts <-
      getConceptDescendant(
        connection = connection,
        connectionDetails = connectionDetails,
        conceptIds = conceptIdsWithIncludeDescendants,
        tempEmulationSchema = tempEmulationSchema,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema
      )
    
    # get all conceptIds (as dataframe) that are excluded in concept set expression
    excludedConceptIds <- conceptSetExpressionDataFrame |>
      dplyr::filter(.data$isExcluded == TRUE) |>
      dplyr::select(.data$conceptId)
    
    # get all conceptIds (as dataframe) that are excluded in concept set expression with descendants
    excludedConceptIdsWithDescendants <- descendantConcepts |>
      dplyr::filter(.data$ancestorConceptId %in% (
        c(
          conceptSetExpressionDataFrame |>
            dplyr::filter(.data$isExcluded == TRUE) |>
            dplyr::filter(.data$includeDescendants == TRUE) |>
            dplyr::pull(.data$conceptId) |>
            unique(),
          0
        ) |> unique()
      )) |>
      dplyr::select(.data$descendantConceptId) |>
      dplyr::distinct()
    
    # conceptIds in conceptSetExpression table
    conceptIdsInConceptSetExpressionTableToBeIncluded <-
      union(
        x = conceptSetExpressionDataFrame |>
          dplyr::pull(.data$conceptId) |>
          unique(),
        y = descendantConcepts |>
          dplyr::pull(.data$descendantConceptId) |>
          unique()
      ) |> unique()
    
    
    conceptIdsInConceptSetExpressionTableToBeExcluded <-
      union(
        x = excludedConceptIds |>
          dplyr::pull(.data$conceptId) |>
          unique(),
        y = excludedConceptIdsWithDescendants |>
          dplyr::pull(.data$descendantConceptId) |>
          unique()
      ) |>
      unique()
    
    # removed all excluded conceptIds including those with descendants == TRUE
    resolvedConceptIdArray <-
      setdiff(x = conceptIdsInConceptSetExpressionTableToBeIncluded,
              y = conceptIdsInConceptSetExpressionTableToBeExcluded)
    
    # get all resolved concept Ids
    resolvedConceptIds <- dplyr::union(
      conceptSetExpressionDataFrame |>
        dplyr::filter(.data$isExcluded == FALSE) |>
        dplyr::select(.data$conceptId),
      descendantConcepts |>
        dplyr::select(.data$descendantConceptId) |>
        dplyr::rename("conceptId" = .data$descendantConceptId)
    ) |>
      dplyr::filter(.data$conceptId %in% resolvedConceptIdArray) |>
      dplyr::pull(.data$conceptId) |>
      unique()
  }
  
  conceptIdDetails <-
    getConceptIdDetails(
      conceptIds = resolvedConceptIds,
      connection = connection,
      tempEmulationSchema = tempEmulationSchema,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
  
  return(conceptIdDetails)
}
