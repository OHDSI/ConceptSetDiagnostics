#' @export
getConceptSetExpressionFromConceptTable <-
  function(conceptTable, selectAllDescendants = FALSE, purgeVocabularyDetails = FALSE) {
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
    if (purgeVocabularyDetails) {
      conceptTable <- conceptTable %>% 
        dplyr::mutate(conceptName = "",
                      standardConcept = "",
                      standardConceptCaption = "",
                      invalidReason = "",
                      invalidReasonCaption = "",
                      conceptCode = "",
                      domainId = "",
                      vocabularyId = "",
                      conceptClassId = "")
    }
    # note: r dataframe objects are always expected to have variables in camel case.
    # so the case conversion below should always be valid, if convention is followed
    colnames(conceptTable) <- toupper(SqlRender::camelCaseToSnakeCase(colnames(conceptTable)))
    
    conceptSetExpression <- list()
    conceptSetExpression$items <- list()
    if (nrow(conceptTable) > 0) {
      for (i in (1:nrow(conceptTable))) {
        conceptSetExpression$items[[i]] <- list()
        conceptSetExpression$items[[i]]$concept <-
          conceptTable[i,] %>%
          dplyr::select(-.data$INCLUDE_DESCENDANTS,
                        -.data$INCLUDE_MAPPED,
                        -.data$IS_EXCLUDED) %>%
          as.list()
        conceptTable$IS_EXCLUDED[i]
        conceptSetExpression$items[[i]]$includeDescendants <-
          conceptTable$INCLUDE_DESCENDANTS[i]
        conceptSetExpression$items[[i]]$includeMapped <-
          conceptTable$INCLUDE_MAPPED[i]
      }
    }
    return(conceptSetExpression)
  }
