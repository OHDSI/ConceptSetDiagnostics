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

#' Get concept set details from cohort definition
#'
#' @description
#' Get concept set details from cohort definition
#'
#' @template CohortExpression
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
extractConceptSetDetailsFromCohortExpression <-
  function(cohortExpression) {
    if ("expression" %in% names(cohortExpression)) {
      expression <- cohortExpression$expression
    } else {
      expression <- cohortExpression
    }
    
    if (is.null(expression$ConceptSets)) {
      return(NULL)
    }
    
    # use circe to render cohort sql and extract concept set sql
    circeRenderedSqlExpression <-
      getCohortSqlFromCohortExpressionUsingCirceR(cohortExpression = expression,
                                                  generateStats = TRUE)
    extractedConceptSetSql <-
      extractConceptSetsSqlFromCohortSql(cohortSql = circeRenderedSqlExpression)
    
    # extract concept set expression from cohort expression
    conceptSetExpression <-
      extractConceptSetExpressionsFromCohortExpression(cohortExpression = expression)
    
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
    conceptSetExpression <- dplyr::bind_rows(conceptSetExpression2)
    
    uniqueConceptSets <- conceptSetExpression %>%
      dplyr::select(.data$conceptSetExpressionSignature) %>%
      dplyr::distinct() %>%
      dplyr::mutate(uniqueConceptSetId = dplyr::row_number())
    
    conceptSetExpression <- conceptSetExpression %>%
      dplyr::left_join(uniqueConceptSets,
                       by = "conceptSetExpressionSignature") %>%
      dplyr::select(-.data$conceptSetExpressionSignature)
    
    data <- dplyr::inner_join(x = conceptSetExpression,
                              y = extractedConceptSetSql,
                              by = c("conceptSetId"))
    return(data)
  }
