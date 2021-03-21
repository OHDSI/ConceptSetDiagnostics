# Copyright 2020 Observational Health Data Sciences and Informatics
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

#' @export
getConceptSetDetailsFromCohortDefinition <-
  function(cohortDefinitionExpression) {
    if ("expression" %in% names(cohortDefinitionExpression)) {
      expression <- cohortDefinitionExpression$expression
    }
    else {
      expression <- cohortDefinitionExpression
    }
    
    if (is.null(expression$ConceptSets)) {
      return(dplyr::tibble())
    }
    
    conceptSetExpression <- list()
    conceptSetExpressionDetails <- list()
    for (i in (1:length(expression$ConceptSets))) {
      conceptSetExpression[[i]] <-
        list(
          id = expression$ConceptSets[[i]]$id,
          name = expression$ConceptSets[[i]]$name,
          expression = expression$ConceptSets[[i]]$expression,
          json = RJSONIO::toJSON(
            expression$ConceptSets[[i]]$expression$items,
            pretty = TRUE,
            digits = 23
          )
        )
      conceptSetExpressionDetails[[i]] <-
        dplyr::tibble(
          id = expression$ConceptSets[[i]]$id,
          getConceptSetExpressionDataFrameFromConceptSetExpression(conceptSetExpression =
                                                                     expression$ConceptSets[[i]]$expression)
        )
    }
    conceptSetExpressionDetails <-
      dplyr::bind_rows(conceptSetExpressionDetails)
    output <- list(conceptSetExpression = conceptSetExpression,
                   conceptSetExpressionDetails = conceptSetExpressionDetails)
    return(output)
  }