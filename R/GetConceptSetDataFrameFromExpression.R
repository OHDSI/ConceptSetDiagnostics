#' @export
getConceptSetDataFrameFromExpression <-
  function(conceptSetExpression) {
    if ("items" %in% names(conceptSetExpression)) {
      items <- conceptSetExpression$items
    } else {
      items <- conceptSetExpression
    }
    conceptSetExpressionDetails <- items %>%
      purrr::map_df(.f = purrr::flatten)
    
    # ensure case is uniform
    if ('concept_id' %in% tolower(colnames(conceptSetExpressionDetails))) {
      if ('isExcluded' %in% colnames(conceptSetExpressionDetails)) {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
          dplyr::rename(is_excluded = .data$isExcluded)
      }
      if ('includeDescendants' %in% colnames(conceptSetExpressionDetails)) {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
          dplyr::rename(include_descendants = .data$includeDescendants)
      }
      if ('includeMapped' %in% colnames(conceptSetExpressionDetails)) {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
          dplyr::rename(include_mapped = .data$includeMapped)
      }
      colnames(conceptSetExpressionDetails) <-
        SqlRender::snakeCaseToCamelCase(colnames(conceptSetExpressionDetails))
    }
    
    if (!'isExcluded' %in% colnames(conceptSetExpressionDetails)) {
      conceptSetExpressionDetails$isExcluded <- FALSE
    }
    if (!'includeDescendants' %in% colnames(conceptSetExpressionDetails)) {
      conceptSetExpressionDetails$includeDescendants <- FALSE
    }
    if (!'includeMapped' %in% colnames(conceptSetExpressionDetails)) {
      conceptSetExpressionDetails$includeMapped <- FALSE
    }
    
    # if there are some missing values, NA - then make them FALSE (Default)
    conceptSetExpressionDetails <-
      tidyr::replace_na(
        data = conceptSetExpressionDetails,
        replace = list(
          isExcluded = FALSE,
          includeDescendants = FALSE,
          includeMapped = FALSE
        )
      )
    return(conceptSetExpressionDetails)
  }