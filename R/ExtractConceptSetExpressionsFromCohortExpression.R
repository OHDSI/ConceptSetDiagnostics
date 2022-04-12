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


#' get concept set expressions from cohort expression
#'
#' @description
#' given a cohort expression (R-list object, not JSON), this function parses the list and returns the concept set components
#'
#' @template CohortExpression
#'
#' @return
#' Returns a tibble data frame with one row per concept set
#'
#' @export
extractConceptSetExpressionsFromCohortExpression <-
  function(cohortExpression) {
    if ("expression" %in% names(cohortExpression)) {
      expression <- cohortExpression$expression
    } else {
      expression <- cohortExpression
    }
    conceptSetExpression <- list()
    if (length(expression$ConceptSets) > 0) {
      for (i in (1:length(expression$ConceptSets))) {
        conceptSetExpression[[i]] <-
          tidyr::tibble(
            conceptSetId = expression$ConceptSets[[i]]$id,
            conceptSetName = expression$ConceptSets[[i]]$name,
            conceptSetExpression = expression$ConceptSets[[i]]$expression$items %>% RJSONIO::toJSON(digits = 23)
          )
      }
    } else {
      conceptSetExpression <- dplyr::tibble()
    }
    return(dplyr::bind_rows(conceptSetExpression))
  }
