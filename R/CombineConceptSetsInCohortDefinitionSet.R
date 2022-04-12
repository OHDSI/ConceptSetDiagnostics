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
#' @return
#' Returns a tibble data frame.
#'
#' @export
combineConceptSetsInCohortDefinitionSet <-
  function(cohortDefinitionSet) {
    #cohorts should be a dataframe with at least cohortId, sql and json
    checkmate::reportAssertions(checkIfCohortDefinitionSet(cohorts))
    
    conceptSets <- list()
    
    for (i in (1:nrow(cohortDefinitionSet))) {
      cohort <- cohortDefinitionSet[i, ]
      conceptSetExpression <-
        extractConceptSetExpressionsFromCohortExpression(cohortExpression = RJSONIO::fromJSON(content = cohortDefinitionSet$json,
                                                                                              digits = 23))
      conceptSetSql <-
        extractConceptSetsSqlFromCohortJson(cohortSql = getCohortSqlFromCohortExpressionUsingCirceR(cohortExpression = cohortDefinitionSet$json))
      
      conceptSets[[i]] <- cohort %>%
        dplyr::left_join(dplyr::inner_join(
          x = conceptSetJson,
          y = conceptSetSql,
          by = c("cohortId", "conceptSetId")
        ),
        by = c("cohortId"))
    }
    if (length(conceptSets) == 0) {
      return(NULL)
    }
    conceptSets <- dplyr::bind_rows(conceptSets) %>%
      dplyr::arrange(.data$conceptSetId)
    
    # TO DO!!! refactor how concept sets are uniquely identified
    uniqueConceptSets <- conceptSets %>%
      dplyr::select(.data$conceptSetExpression) %>%
      dplyr::distinct() %>%
      dplyr::mutate(uniqueConceptSetId = dplyr::row_number())
    
    conceptSets <- conceptSets %>%
      dplyr::inner_join(uniqueConceptSets, by = "conceptSetExpression") %>%
      dplyr::distinct() %>%
      dplyr::relocate(.data$uniqueConceptSetId,
                      .data$cohortId,
                      .data$conceptSetId) %>%
      dplyr::arrange(.data$uniqueConceptSetId,
                     .data$cohortId,
                     .data$conceptSetId)
    return(conceptSets)
  }
