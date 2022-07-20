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




#' get cohort sql from cohort expression
#'
#' @description
#' given a cohort expression R object, this function generates the cohort sql using circeR. The returned
#' SQL is a string that may be used with SqlRender.
#'
#' @template CohortExpression
#'
#' @param generateStats  Do you want to include cohort generation stats tables in the SQL?
#'
#' @return
#' Returns string (sql)
#'
#' @export
getCohortSqlFromCohortDefinition <-
  function(cohortExpression,
           generateStats = TRUE) {
    if ("expression" %in% names(cohortExpression)) {
      expression <- cohortExpression$expression
    } else {
      expression <- cohortExpression
    }
    
    # use circe to render cohort sql
    circeRCohortExpressionFromJson <-
      CirceR::cohortExpressionFromJson(expressionJson = RJSONIO::toJSON(x = expression,
                                                                        digits = 23))
    circeRenderedSqlExpression <-
      CirceR::buildCohortQuery(
        expression = circeRCohortExpressionFromJson,
        options = CirceR::createGenerateOptions(generateStats = generateStats)
      )
    return(circeRenderedSqlExpression)
  }
