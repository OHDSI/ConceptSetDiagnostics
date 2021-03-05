#' @export
getConceptSetDataFrameFromExpression <-
  function(conceptSetExpression) {
    
    if (length(conceptSetExpression) == 0) {return(NULL)}
    
    if ("items" %in% names(conceptSetExpression)) {
      items <- conceptSetExpression$items
    } else {
      items <- conceptSetExpression
    }
    
    items2 <- list()
    for (i in (1:length(items))) {
      items2[[i]] <- purrr::flatten_dfr(.x = purrr::map_depth(items[[i]],
                                                              .depth = 2, 
                                                              ~ifelse(is.null(.x), NA, .x) ))
    }
    conceptSetExpressionDetails <- dplyr::bind_rows(items2)
    
    # ensure case is uniform
    if ('concept_id' %in% tolower(colnames(conceptSetExpressionDetails))) {
      if ('isExcluded' %in% colnames(conceptSetExpressionDetails)) {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
          dplyr::rename(is_excluded = .data$isExcluded)
      } else {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>% 
          dplyr::mutate(is_excluded = FALSE)
      }
      if ('includeDescendants' %in% colnames(conceptSetExpressionDetails)) {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
          dplyr::rename(include_descendants = .data$includeDescendants)
      } else {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>% 
          dplyr::mutate(include_descendants = FALSE)
      }
      if ('includeMapped' %in% colnames(conceptSetExpressionDetails)) {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>%
          dplyr::rename(include_mapped = .data$includeMapped)
      } else {
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>% 
          dplyr::mutate(include_mapped = FALSE)
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
    
    if ('standardConceptCaption' %in% colnames(conceptSetExpressionDetails) &&
        !'standardConcept' %in% colnames(conceptSetExpressionDetails)) {
      conceptSetExpressionDetails <- conceptSetExpressionDetails %>% 
        dplyr::mutate(standardConcept = dplyr::case_when(.data$standardConceptCaption == 'Standard' ~ 'S',
                                       .data$standardConceptCaption == 'Classification' ~ 'C'))
    }
    if ('standardConcept' %in% tolower(colnames(conceptSetExpressionDetails)) &&
        !'standardConceptCaption' %in% tolower(colnames(conceptSetExpressionDetails))) {
      conceptSetExpressionDetails <- conceptSetExpressionDetails %>% 
        dplyr::mutate(dplyr::case_when(.data$standardConcept == 'S' ~ 'Standard',
                                       .data$standardConcept == 'C' ~ 'Classification',
                                       TRUE ~ 'Non-Standard'))
    }
    return(conceptSetExpressionDetails)
  }