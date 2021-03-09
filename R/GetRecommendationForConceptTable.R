#' @export
getRecommendationForConceptTable <-
  function(connection,
           conceptSetExpressionTable,
           vocabularyDatabaseSchema = 'vocabulary',
           vocabularyIdForRecommender = c('SNOMED', 'ICD')) {
    searchVocabularyId <-
      paste0(vocabularyIdForRecommender, collapse = "|")
    
    forRecommendation <- conceptSetExpressionTable %>%
      dplyr::filter(.data$domainId %in% c('Observation', 'Condition')) %>%
      dplyr::filter(stringr::str_detect(string = .data$vocabularyId,
                                        pattern = searchVocabularyId)) %>%
      dplyr::pull(.data$conceptId) %>%
      unique()
    recommendedStandard <-
      getRecommendationStandard(connection = connection,
                                conceptList = forRecommendation)
    recommendedSource <-
      getRecommendationSource(connection = connection,
                              conceptList = forRecommendation)
    
    data <- list()
    data$recommendedStandard <- recommendedStandard
    data$recommendedSource <- recommendedSource
    return(data)
  }