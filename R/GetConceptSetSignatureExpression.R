# given a concept set expression, return its signature
#' @export
getConceptSetSignatureExpression <-
  function(connection, conceptSetExpression) {
    expressionDataFrame <-
      getConceptSetDataFrameFromExpression(conceptSetExpression = conceptSetExpression)
    optimizedExpression <-
      optimizeConceptSetExpression(connection = connection,
                                   conceptSetExpression = conceptSetExpression)
    
    retained <- optimizedExpression %>%
      dplyr::mutate(isExcluded = as.logical(.data$excluded)) %>%
      dplyr::filter(.data$removed == 0) %>%
      dplyr::select(.data$conceptId, .data$isExcluded)
    
    if (nrow(retained) > 0) {
      expressionDataFrame <- expressionDataFrame %>%
        dplyr::inner_join(retained, by = c('conceptId', 'isExcluded'))
    }
    
    expressionDataFrame <- expressionDataFrame %>%
      dplyr::arrange(.data$conceptId)
    
    # strip all meta information
    conceptSetExpression <-
      getConceptSetExpressionFromConceptTable(conceptTable = expressionDataFrame,
                                              purgeVocabularyDetails = TRUE)
    return(conceptSetExpression)
  }
