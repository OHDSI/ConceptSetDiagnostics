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


#' Extract concept sets from cohort definition set
#'
#' @description
#' given a cohort definition set (data frame with cohortId, json), this function
#' extracts the concept set json and sql for all cohorts,
#' compares concept sets across cohort definitions, assigns unique id.
#'
#' @template CohortDefinitionSet
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
extractConceptSetsInCohortDefinitionSet <-
  function(cohortDefinitionSet) {
    # cohorts should be a dataframe with at least cohortId and json

    conceptSets <- list()
    for (i in (1:nrow(cohortDefinitionSet))) {
      cohort <- cohortDefinitionSet[i, ]
      cohortJsonAsList <- RJSONIO::fromJSON(
        content = cohort$json,
        digits = 23
      )
      conceptSetsInCohortDefinition <- NULL
      conceptSetsInCohortDefinition <-
        try(expr = extractConceptSetsInCohortDefinition(cohortExpression = cohortJsonAsList),
            silent = TRUE
        )

      if (all(
        !is.null(conceptSetsInCohortDefinition),
        !class(conceptSetsInCohortDefinition) == 'try-error'
      )) {
        
        conceptSets[[i]] <- conceptSetsInCohortDefinition %>%
          dplyr::select(-.data$uniqueConceptSetId) %>%
          dplyr::mutate(cohortId = cohort$cohortId) %>%
          dplyr::relocate(.data$cohortId, .data$conceptSetId)
      }
    }
    if (length(conceptSets) == 0) {
      return(NULL)
    }
    conceptSets <- dplyr::bind_rows(conceptSets) %>%
      dplyr::arrange(.data$cohortId, .data$conceptSetId)

    conceptSetSig <- list()
    for (i in (1:nrow(conceptSets))) {
      conceptSetSig[[i]] <- conceptSets[i, ]
      conceptSetExpressionSignature <-
        convertConceptSetExpressionToDataFrame(conceptSetExpression = conceptSetSig[[i]]$conceptSetExpression %>%
          RJSONIO::fromJSON(digits = 23)) %>%
        dplyr::select(
          .data$conceptId,
          .data$includeDescendants,
          .data$includeMapped,
          .data$isExcluded
        ) %>%
        dplyr::distinct() %>%
        dplyr::arrange(.data$conceptId) %>%
        RJSONIO::toJSON(digits = 23, pretty = TRUE)
      conceptSetSig[[i]]$conceptSetExpressionSignature <-
        conceptSetExpressionSignature
      conceptSetSig[[i]] <- conceptSetSig[[i]] %>%
        dplyr::select(
          .data$cohortId,
          .data$conceptSetId,
          .data$conceptSetExpressionSignature
        ) %>%
        dplyr::distinct()
    }
    conceptSetSig <- dplyr::bind_rows(conceptSetSig)
    uniqueConceptSets <- conceptSetSig %>%
      dplyr::select(.data$conceptSetExpressionSignature) %>%
      dplyr::distinct() %>%
      dplyr::mutate(uniqueConceptSetId = dplyr::row_number())

    conceptSetSig <- conceptSetSig %>%
      dplyr::inner_join(uniqueConceptSets,
        by = "conceptSetExpressionSignature"
      ) %>%
      dplyr::select(-.data$conceptSetExpressionSignature)

    conceptSets <- conceptSets %>%
      dplyr::left_join(conceptSetSig, by = c(
        "cohortId",
        "conceptSetId"
      )) %>%
      dplyr::distinct() %>%
      dplyr::relocate(
        .data$uniqueConceptSetId,
        .data$cohortId,
        .data$conceptSetId
      ) %>%
      dplyr::arrange(
        .data$uniqueConceptSetId,
        .data$cohortId,
        .data$conceptSetId
      )
    return(conceptSets)
  }
