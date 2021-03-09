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
          getConceptSetDataFrameFromExpression(conceptSetExpression =
                                                 expression$ConceptSets[[i]]$expression)
        )
    }
    conceptSetExpressionDetails <-
      dplyr::bind_rows(conceptSetExpressionDetails)
    output <- list(conceptSetExpression = conceptSetExpression,
                   conceptSetExpressionDetails = conceptSetExpressionDetails)
    return(output)
  }