#' @export
getConceptSetDataFrameFromExpression <-
  function(conceptSetExpression,
           updateVocabularyFields = FALSE,
           connection = NULL,
           vocabularyDatabaseSchema = 'vocabulary') {
    
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
    
    colnames <- colnames(conceptSetExpressionDetails)
    if (updateVocabularyFields) {
      if (!is.null(connection)) {
        details <- getConceptIdDetails(connection = connection, 
                                       vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                       conceptIds = conceptSetExpressionDetails$conceptId %>% unique()) %>% 
          dplyr::select(.data$conceptId, 
                        .data$conceptName, 
                        .data$vocabularyId,
                        .data$standardConcept, 
                        .data$invalidReason,
                        .data$conceptCode,
                        .data$conceptClassId,
                        .data$domainId)
        conceptSetExpressionDetails <- conceptSetExpressionDetails %>% 
          dplyr::select(.data$conceptId, 
                        .data$includeDescendants,
                        .data$includeMapped,
                        .data$isExcluded) %>% 
          dplyr::left_join(y = details, by = 'conceptId')
        conceptSetExpressionDetails <- tidyr::replace_na(data = conceptSetExpressionDetails, 
                                                         replace = list(invalidReason = 'V')) %>% 
          dplyr::mutate(invalidReasonCaption = dplyr::case_when(invalidReason == 'V' ~ 'Valid',
                                                                invalidReason == 'D' ~ 'Deleted',
                                                                invalidReason == 'U' ~ 'Updated',
                                                                TRUE ~ 'Valid')) %>% 
          dplyr::mutate(standardConceptCaption = dplyr::case_when(standardConcept == 'S' ~ 'Standard',
                                                                  standardConcept == 'C' ~ 'Classification',
                                                                  TRUE ~ 'Non-standard'))
      } else {
        warning("No connection provided. Vocabulary will not be updated. Continuing.")
      }
    }
    
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
    
    conceptSetExpressionDetails <- conceptSetExpressionDetails %>% 
      dplyr::relocate(dplyr::all_of(c('includeDescendants','includeMapped','isExcluded')), 
                      .after = dplyr::last_col()) %>% 
      dplyr::relocate('conceptId')
    
    return(conceptSetExpressionDetails)
  }