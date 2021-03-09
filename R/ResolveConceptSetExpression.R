# Given a concept set expression, get the resolved concepts
#' @export
resolveConceptSetExpression <- function(connection,
                                        dbms = 'postgres',
                                        conceptSetExpression) {
  # convert concept set expression R object (list) to data frame
  conceptSetExpressionTable <-
    getConceptSetDataFrameFromExpression(conceptSetExpression =
                                           conceptSetExpression)
  
  # get all descendant concept ids (as dataframe) for concepts that have
  # includeDescendants selected in conceptSetExpression
  descendantConcepts <-
    getDescendantConcepts(
      connection = connection,
      dbms = dbms,
      descendantConceptId = conceptSetExpressionTable %>%
        dplyr::filter(.data$includeDescendants == TRUE) %>%
        dplyr::pull(.data$conceptId)
    )
  
  # get all conceptIds (as dataframe) that are excluded in concept set expression
  excludedConceptIds <- conceptSetExpressionTable %>%
    dplyr::filter(.data$isExcluded == TRUE) %>%
    dplyr::select(.data$conceptId)
  
  # get all conceptIds (as dataframe) that are excluded in concept set expression with descendants
  excludedConceptIdsWithDescendants <- descendantConcepts %>%
    dplyr::filter(
      .data$ancestorConceptId %in% (
        conceptSetExpressionTable %>%
          dplyr::filter(.data$isExcluded == TRUE) %>%
          dplyr::filter(.data$includeDescendants == TRUE) %>%
          dplyr::pull(.data$conceptId)
      )
    )
  
  # conceptIds in conceptSetExpression table
  conceptIdsInConceptSetExpressionTableToBeIncluded <-
    union(
      x = conceptSetExpressionTable %>%
        dplyr::pull(.data$conceptId),
      y = descendantConcepts %>% dplyr::pull(.data$conceptId)
    )
  conceptIdsInConceptSetExpressionTableToBeExcluded <-
    union(
      x = excludedConceptIds %>% dplyr::pull(.data$conceptId),
      y = excludedConceptIdsWithDescendants %>% dplyr::pull(.data$conceptId)
    )
  
  
  # removed all excluded conceptIds including those with descendants == TRUE
  resolvedConceptIds <-
    setdiff(x = conceptIdsInConceptSetExpressionTableToBeIncluded,
            y = conceptIdsInConceptSetExpressionTableToBeExcluded)
  
  #get all resolved concept Ids as data frame
  resolvedConceptIds <- dplyr::union(
    conceptSetExpressionTable %>%
      dplyr::filter(.data$isExcluded == FALSE) %>%
      dplyr::select(
        .data$conceptId,
        .data$conceptName,
        .data$conceptCode,
        .data$domainId,
        .data$vocabularyId,
        .data$conceptClassId,
        .data$standardConcept,
        .data$invalidReason
      ),
    descendantConcepts %>%
      dplyr::select(
        .data$conceptId,
        .data$conceptName,
        .data$conceptCode,
        .data$domainId,
        .data$vocabularyId,
        .data$conceptClassId,
        .data$standardConcept,
        .data$invalidReason
      )
  ) %>%
    dplyr::filter(.data$conceptId %in% resolvedConceptIds) %>%
    dplyr::select(
      .data$conceptId,
      .data$conceptName,
      .data$conceptCode,
      .data$domainId,
      .data$vocabularyId,
      .data$conceptClassId,
      .data$standardConcept,
      .data$invalidReason
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$conceptId)
  
  # get the mapped concepts for the resolved conceptIds
  mappedConcepts <-
    getMappedConcepts(
      connection = connection,
      mappedConceptId =  resolvedConceptIds %>%
        dplyr::pull(.data$conceptId)
    ) %>%
    dplyr::filter(.data$standardConceptId != .data$conceptId) %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      y = resolvedConceptIds %>%
        dplyr::select(.data$conceptId,
                      .data$conceptName) %>%
        dplyr::rename(standardConceptName = .data$conceptName),
      by = c("standardConceptId" = "conceptId")
    ) %>%
    dplyr::relocate(.data$standardConceptId, .data$standardConceptName) %>%
    dplyr::arrange(.data$standardConceptId, .data$standardConceptName)
  
  output <- list(resolvedConcepts = resolvedConceptIds,
                 mappedConcepts = mappedConcepts)
  return(output)
}