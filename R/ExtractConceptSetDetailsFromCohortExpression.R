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
    }
    else {
      expression <- cohortExpression
    }
    
    if (is.null(expression$ConceptSets)) {
      return(NULL)
    }
    
    # use circe to render cohort sql and extract concept set sql
    circeRenderedSqlExpression <-
      getCohortSqlFromCohortExpressionUsingCirceR(expression = expression,
                                                  generateStats = TRUE)
    extractedConceptSetSql <-
      extractConceptSetsSqlFromCohortSql(cohortSql = circeRenderedSqlExpression)
    
    # extract concept set expression from cohort expression
    extractedConceptSetExpression <-
      extractConceptSetExpressionsFromCohortExpression(cohortDefinitionExpression)
    
    
    
    # TO DO!!!!!!
    # getConceptSetExpressionDataFrameFromConceptSetExpression
    # convert to data frame and then to named list object
    # assign unique id inside cohort definition expression
    
    data <- dplyr::inner_join(x = extractedConceptSetExpression,
                              y = extractedConceptSetSql,
                              by = c("conceptSetId"))
    return(data)
  }
