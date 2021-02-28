#' @export
getConceptSetExpressionFromConceptTable <-
  function(conceptTable, selectAllDescendants = FALSE) {
    if (!'includeMapped' %in% colnames(conceptTable)) {
      conceptTable$includeMapped <- FALSE
    }
    if (!'isExcluded' %in% colnames(conceptTable)) {
      conceptTable$isExcluded <- FALSE
    }
    if (!'includeDescendants' %in% colnames(conceptTable)) {
      if (selectAllDescendants) {
        conceptTable$includeDescendants <- TRUE
      } else {
        conceptTable$includeDescendants <- FALSE
      }
    } else {
      if (selectAllDescendants) {
        conceptTable$includeDescendants <- TRUE
      }
    }
    
    conceptSetExpression <- list()
    conceptSetExpression$items <- list()
    for (i in (1:nrow(conceptTable))) {
      conceptSetExpression$items[[i]] <- list()
      conceptSetExpression$items[[i]]$concept$concept_id <-
        conceptTable[i,]$conceptId
      conceptSetExpression$items[[i]]$isExcluded <-
        conceptTable$isExcluded[i]
      conceptSetExpression$items[[i]]$includeDescendants <-
        conceptTable$includeDescendants[i]
      conceptSetExpression$items[[i]]$includeMapped <-
        conceptTable$includeMapped[i]
    }
    return(conceptSetExpression)
  }
