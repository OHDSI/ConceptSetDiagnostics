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

#' Convert concept set expression in a data frame format convert to R (list) expression
#'
#' @description
#' Convert concept set expression in a data frame format convert to R (list) expression
#'
#' @param conceptSetExpressionDataFrame   Concept set expression in data frame format with required fields
#'                                        conceptId. If includeMapped, isExcluded or includeDescendants
#'                                        are missing value or is not existent - it is assumed to be FALSE.
#'                                        All column names should be in camelCase format.
#'
#' @param selectAllDescendants            Do you want to over ride the concept set
#'                                        expression by add select descendants for concept ids
#'                                        in concept set expression.
#'
#' @template UpdateVocabularyFields
#'
#' @template Connection
#'
#' @template VocabularyDatabaseSchema
#'
#' @return
#' Returns a R list object
#'
#' @export
convertConceptSetDataFrameToExpression <-
  function(conceptSetExpressionDataFrame,
           selectAllDescendants = FALSE,
           updateVocabularyDetails = FALSE,
           connectionDetails = NULL,
           connection = NULL,
           vocabularyDatabaseSchema = NULL) {
    if (!"includeMapped" %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$includeMapped <- FALSE
    }
    if (!"isExcluded" %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$isExcluded <- FALSE
    }
    if (!"includeDescendants" %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$includeDescendants <- FALSE
    }
    if (!"conceptName" %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$conceptName <- as.character("")
    }
    if (!"standardConcept" %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$standardConcept <- as.character("")
    }
    if (!"standardConceptCaption" %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$standardConceptCaption <-
        as.character("")
    }
    if (!"invalidReason" %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$invalidReason <- as.character("")
    }
    if (!"invalidReasonCaption" %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$invalidReasonCaption <-
        as.character("")
    }
    if (!"conceptCode" %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$conceptCode <- as.character("")
    }
    if (!"domainId" %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$domainId <- as.character("")
    }
    if (!"vocabularyId" %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$vocabularyId <- as.character("")
    }
    if (!"conceptClassId" %in% colnames(conceptSetExpressionDataFrame)) {
      conceptSetExpressionDataFrame$conceptClassId <- as.character("")
    }

    if (selectAllDescendants) {
      conceptSetExpressionDataFrame <-
        dplyr::bind_rows(
          conceptSetExpressionDataFrame %>%
            dplyr::filter(.data$standardConcept == "S") %>%
            dplyr::mutate(includeDescendants = TRUE),
          conceptSetExpressionDataFrame %>%
            dplyr::filter(!.data$standardConcept == "S") %>%
            dplyr::mutate(includeDescendants = FALSE)
        )
    }

    if (updateVocabularyDetails) {
      if (is.null(vocabularyDatabaseSchema)) {
        stop(
          "VocabularyDatabaseSchema with OMOP vocabulary tables is needed to update Vocabulary details."
        )
      }
      if (is.null(connection)) {
        connection <- DatabaseConnector::connect(connectionDetails)
        on.exit(DatabaseConnector::disconnect(connection))
      }

      conceptIds <-
        conceptSetExpressionDataFrame$conceptId %>% unique()
      conceptIdDetails <- getConceptIdDetails(
        conceptIds = conceptIds,
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema
      )

      conceptSetExpressionDataFrame <-
        conceptSetExpressionDataFrame %>%
        dplyr::select(
          -.data$conceptName, -.data$standardConcept, -.data$standardConceptCaption, -.data$invalidReason, -.data$invalidReasonCaption, -.data$conceptCode, -.data$domainId, -.data$vocabularyId, -.data$conceptClassId
        ) %>%
        dplyr::left_join(conceptIdDetails,
          by = "conceptId"
        ) %>%
        dplyr::select(
          .data$conceptId,
          .data$conceptName,
          .data$standardConcept,
          .data$standardConceptCaption,
          .data$invalidReason,
          .data$invalidReasonCaption,
          .data$conceptCode,
          .data$domainId,
          .data$vocabularyId,
          .data$conceptClassId,
          .data$includeMapped,
          .data$isExcluded,
          .data$includeDescendants
        ) %>%
        tidyr::replace_na(
          replace = list(
            conceptName = as.character(""),
            standardConcept = as.character(""),
            standardConceptCaption = as.character(""),
            invalidReason = as.character(""),
            invalidReasonCaption = as.character(""),
            conceptCode = as.character(""),
            domainId = as.character(""),
            vocabularyId = as.character(""),
            conceptClassId = as.character("")
          )
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
          dplyr::select(-.data$INCLUDE_DESCENDANTS, -.data$INCLUDE_MAPPED, -.data$IS_EXCLUDED) %>%
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
