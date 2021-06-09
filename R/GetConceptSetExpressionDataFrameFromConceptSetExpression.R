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

#' convert a concept set expression object into a data frame object
#'
#' @template Connection
#'
#' @template ConnectionDetails
#'
#' @param updateVocabularyFields  Do you want to update the details of concepts from the vocabulary tables?
#'
#' @param recordCount             Do you want the function to return record count for the concept ids?
#'
#' @template VocabularyDatabaseSchema
#'
#' @template conceptSetExpression
#'
#' @export
getConceptSetExpressionDataFrameFromConceptSetExpression <-
  function(conceptSetExpression,
           updateVocabularyFields = FALSE,
           recordCount = FALSE,
           connection = NULL,
           connectionDetails = NULL,
           vocabularyDatabaseSchema = 'vocabulary',
           conceptPrevalenceSchema = 'concept_prevalence') {
    if (length(conceptSetExpression) == 0) {
      return(NULL)
    }
    
    if ("items" %in% names(conceptSetExpression)) {
      items <- conceptSetExpression$items
    } else {
      items <- conceptSetExpression
    }
    
    items2 <- list()
    for (i in (1:length(items))) {
      if (purrr::vec_depth(items[[i]]) <= 3) {
        items2[[i]] <- purrr::flatten_dfr(.x = purrr::map_depth(items[[i]],
                                                                .depth = 2,
                                                                ~ ifelse(is.null(.x), NA, .x)))
      } else {
        if ('CONCEPT_ID' %in% names(items[[i]][[1]])) {
          warning(
            paste0(
              "record in concept set expression with concept id ",
              (items[[i]][[1]]$CONCEPT_ID),
              " does not conform with the standard structure in concept set expression"
            )
          )
        }
      }
    }
    conceptSetExpressionDetails <- dplyr::bind_rows(items2)
    
    # ensure case is uniform
    if ('concept_id' %in% tolower(colnames(conceptSetExpressionDetails))) {
      if ('isExcluded' %in% colnames(conceptSetExpressionDetails)) {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
          dplyr::rename(is_excluded = .data$isExcluded)
      } else {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
          dplyr::mutate(is_excluded = FALSE)
      }
      if ('includeDescendants' %in% colnames(conceptSetExpressionDetails)) {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
          dplyr::rename(include_descendants = .data$includeDescendants)
      } else {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
          dplyr::mutate(include_descendants = FALSE)
      }
      if ('includeMapped' %in% colnames(conceptSetExpressionDetails)) {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
          dplyr::rename(include_mapped = .data$includeMapped)
      } else {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
          dplyr::mutate(include_mapped = FALSE)
      }
      colnames(conceptSetExpressionDetails) <-
        SqlRender::snakeCaseToCamelCase(colnames(conceptSetExpressionDetails))
    }
    
    if (!'isExcluded' %in% colnames(conceptSetExpressionDetails)) {
      conceptSetExpressionDetails$isExcluded <- FALSE
    }
    if (!'includeDescendants' %in% colnames(conceptSetExpressionDetails)) {
      conceptSetExpressionDetails$includeDescendants <- FALSE
    }
    if (!'includeMapped' %in% colnames(conceptSetExpressionDetails)) {
      conceptSetExpressionDetails$includeMapped <- FALSE
    }
    
    # if there are some missing values, NA - then make them FALSE (Default)
    conceptSetExpressionDetails <-
      tidyr::replace_na(
        data = conceptSetExpressionDetails,
        replace = list(
          isExcluded = FALSE,
          includeDescendants = FALSE,
          includeMapped = FALSE
        )
      )
    
    colnames <- colnames(conceptSetExpressionDetails)
    if (updateVocabularyFields) {
      if (!is.null(connection) ||
          !is.null(connectionDetails)) {
        details <- getConceptIdDetails(
          connection = connection,
          connectionDetails = connectionDetails,
          vocabularyDatabaseSchema = vocabularyDatabaseSchema,
          conceptPrevalenceSchema = conceptPrevalenceSchema,
          conceptIds = conceptSetExpressionDetails$conceptId %>% unique()
        )
        conceptSetExpressionDetails <-
          conceptSetExpressionDetails %>%
          dplyr::select(
            .data$conceptId,
            .data$includeDescendants,
            .data$includeMapped,
            .data$isExcluded
          ) %>%
          dplyr::left_join(y = details, by = 'conceptId')
        
        conceptSetExpressionDetails <-
          tidyr::replace_na(data = conceptSetExpressionDetails,
                            replace = list(invalidReason = 'V')) %>%
          dplyr::mutate(
            invalidReasonCaption = dplyr::case_when(
              invalidReason == 'V' ~ 'Valid',
              invalidReason == 'D' ~ 'Deleted',
              invalidReason == 'U' ~ 'Updated',
              TRUE ~ 'Valid'
            )
          ) %>%
          dplyr::mutate(
            standardConceptCaption = dplyr::case_when(
              standardConcept == 'S' ~ 'Standard',
              standardConcept == 'C' ~ 'Classification',
              TRUE ~ 'Non-standard'
            )
          )
      } else {
        warning("No connection provided. Vocabulary will not be updated. Continuing.")
      }
    }
    
    if ('standardConceptCaption' %in% colnames(conceptSetExpressionDetails) &&
        !'standardConcept' %in% colnames(conceptSetExpressionDetails)) {
      conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
        dplyr::mutate(
          standardConcept = dplyr::case_when(
            .data$standardConceptCaption == 'Standard' ~ 'S',
            .data$standardConceptCaption == 'Classification' ~ 'C'
          )
        )
    }
    if ('standardConcept' %in% tolower(colnames(conceptSetExpressionDetails)) &&
        !'standardConceptCaption' %in% tolower(colnames(conceptSetExpressionDetails))) {
      conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
        dplyr::mutate(
          dplyr::case_when(
            .data$standardConcept == 'S' ~ 'Standard',
            .data$standardConcept == 'C' ~ 'Classification',
            TRUE ~ 'Non-Standard'
          )
        )
    }
    
    conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
      dplyr::relocate(dplyr::all_of(c(
        'includeDescendants', 'includeMapped', 'isExcluded'
      )),
      .after = dplyr::last_col()) %>%
      dplyr::relocate('conceptId')
    
    if (!recordCount) {
      if ('rc' %in% colnames(conceptSetExpressionDetails)) {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
          dplyr::select(-.data$rc, -.data$dbc, -.data$drc, -.data$ddbc)
      }
    } else {
      if (!'rc' %in% colnames(conceptSetExpressionDetails)) {
        conceptIds <- conceptSetExpressionDetails$conceptId %>%
          unique() %>%
          getConceptIdDetails(
            connection = connection,
            connectionDetails = connectionDetails,
            vocabularyDatabaseSchema = vocabularyDatabaseSchema,
            conceptPrevalenceSchema = conceptPrevalenceSchema
          ) %>%
          dplyr::select(.data$conceptId,
                        .data$rc,
                        .data$dbc,
                        .data$drc,
                        .data$ddbc)
        conceptSetExpressionDetails <-
          conceptSetExpressionDetails %>%
          dplyr::left_join(y = conceptIds, by = "conceptId") %>%
          dplyr::arrange(dplyr::desc(.data$drc))
      } else {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
          dplyr::arrange(dplyr::desc(.data$drc))
      }
    }
    return(conceptSetExpressionDetails)
  }
