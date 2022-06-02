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


#' combine concept sets
#'
#' @description
#' given a cohort definition set, this function extracts the concept set json and sql for all cohorts,
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
    # cohorts should be a dataframe with at least cohortId, sql and json
    
    conceptSets <- list()
    
    for (i in (1:nrow(cohortDefinitionSet))) {
      cohort <- cohortDefinitionSet[i,]
      cohortId <- cohort$cohortId
      cohortJsonAsList <- RJSONIO::fromJSON(content = cohort$json,
                                            digits = 23)
      conceptSetExpression <-
        extractConceptSetExpressionsFromCohortExpression(cohortExpression = cohortJsonAsList)
      
      if (nrow(conceptSetExpression) > 0) {
        conceptSetExpression2 <- list()
        for (j in (1:nrow(conceptSetExpression))) {
          conceptSetExpression2[[j]] <- conceptSetExpression[j, ]
          conceptSetExpression2[[j]]$conceptSetExpressionSignature <-
            getConceptSetExpressionDataFrameFromConceptSetExpression(
              conceptSetExpression = conceptSetExpression2[[j]][1, ]$conceptSetExpression %>%
                RJSONIO::fromJSON(digits = 23)
            ) %>%
            dplyr::select(
              .data$conceptId,
              .data$includeDescendants,
              .data$includeMapped,
              .data$isExcluded
            ) %>%
            dplyr::distinct() %>%
            dplyr::arrange(.data$conceptId) %>%
            RJSONIO::toJSON(digits = 23, pretty = TRUE)
        }
        conceptSetExpression <-
          dplyr::bind_rows(conceptSetExpression2)
      }
      
      cohortSql <-
        getCohortSqlFromCohortDefinition(cohortExpression = cohortJsonAsList)
      conceptSetSql <-
        extractConceptSetsSqlFromCohortSql(cohortSql = cohortSql)
      
      if (nrow(conceptSetExpression) > 0 &
          nrow(conceptSetSql) > 0) {
        conceptSetsJsonAndSql <-
          dplyr::inner_join(x = conceptSetExpression,
                            y = conceptSetSql,
                            by = c("conceptSetId")) %>%
          dplyr::mutate(cohortId = cohort$cohortId)
        
        conceptSets[[i]] <- cohort %>%
          dplyr::left_join(conceptSetsJsonAndSql,
                           by = c("cohortId"))
      }
    }
    if (length(conceptSets) == 0) {
      return(NULL)
    }
    conceptSets <- dplyr::bind_rows(conceptSets) %>%
      dplyr::arrange(.data$cohortId, .data$conceptSetId)
    
    if (nrow(conceptSets) == 0) {
      return(NULL)
    }
    
    uniqueConceptSets <- conceptSets %>%
      dplyr::select(.data$conceptSetExpressionSignature) %>%
      dplyr::distinct() %>%
      dplyr::mutate(uniqueConceptSetId = dplyr::row_number())
    
    conceptSets <- conceptSets %>%
      dplyr::inner_join(uniqueConceptSets, by = "conceptSetExpressionSignature") %>%
      dplyr::select(-.data$conceptSetExpressionSignature) %>%
      dplyr::distinct() %>%
      dplyr::relocate(.data$uniqueConceptSetId,
                      .data$cohortId,
                      .data$conceptSetId) %>%
      dplyr::arrange(.data$uniqueConceptSetId,
                     .data$cohortId,
                     .data$conceptSetId)
    return(conceptSets)
  }
