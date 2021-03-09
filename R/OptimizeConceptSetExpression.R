# given a concept set expression, get optimized concept set expression
#' @export
optimizeConceptSetExpression <-
  function(connection,
           conceptSetExpression,
           vocabularyDatabaseSchema = 'vocabulary') {
    conceptSetExpressionTable <-
      getConceptSetDataFrameFromExpression(connection = connection,
                                           conceptSetExpression =
                                             conceptSetExpression)
    
    if (nrow(conceptSetExpressionTable) <= 1) {
      # no optimization necessary
      return(
        conceptSetExpressionTable %>%
          dplyr::mutate(
            excluded = as.integer(.data$isExcluded),
            removed = 0
          ) %>%
          dplyr::select(.data$conceptId, .data$excluded, .data$removed)
      )
    }
    
    conceptSetConceptIdsExcluded <- conceptSetExpressionTable %>%
      dplyr::filter(.data$isExcluded == TRUE) %>%
      dplyr::pull(.data$conceptId)
    conceptSetConceptIdsDescendantsExcluded <-
      conceptSetExpressionTable %>%
      dplyr::filter(.data$isExcluded == TRUE &&
                      .data$includeDescendants == TRUE) %>%
      dplyr::pull(.data$conceptId)
    conceptSetConceptIdsNotExcluded <-
      conceptSetExpressionTable %>%
      dplyr::filter(!.data$isExcluded == TRUE) %>%
      dplyr::pull(.data$conceptId)
    conceptSetConceptIdsDescendantsNotExcluded <-
      conceptSetExpressionTable %>%
      dplyr::filter(!.data$isExcluded == TRUE &&
                      .data$includeDescendants == TRUE) %>%
      dplyr::pull(.data$conceptId)
    
    if (any(
      is.null(conceptSetConceptIdsExcluded),
      is.na(conceptSetConceptIdsExcluded),
      length(conceptSetConceptIdsExcluded) == 0
    )) {
      conceptSetConceptIdsExcluded <- 0
    }
    if (any(
      is.null(conceptSetConceptIdsDescendantsExcluded),
      is.na(conceptSetConceptIdsDescendantsExcluded),
      length(conceptSetConceptIdsDescendantsExcluded) == 0
    )) {
      conceptSetConceptIdsDescendantsExcluded <- 0
    }
    if (any(
      is.null(conceptSetConceptIdsNotExcluded),
      is.na(conceptSetConceptIdsNotExcluded),
      length(conceptSetConceptIdsNotExcluded) == 0
    )) {
      conceptSetConceptIdsNotExcluded <- 0
    }
    if (any(
      is.null(conceptSetConceptIdsDescendantsNotExcluded),
      is.na(conceptSetConceptIdsDescendantsNotExcluded),
      length(conceptSetConceptIdsDescendantsNotExcluded) == 0
    )) {
      conceptSetConceptIdsDescendantsNotExcluded <- 0
    }
    
    #switch between sql with or without temp table based on
    #number of concept ids to optimize
    numberOfConceptIds <- length(unique(
      c(
        conceptSetConceptIdsExcluded,
        conceptSetConceptIdsDescendantsExcluded,
        conceptSetConceptIdsNotExcluded,
        conceptSetConceptIdsDescendantsNotExcluded
      )
    ))
    
    sqlWithTemporaryTable <-
      SqlRender::loadRenderTranslateSql(
        sqlFilename = "optimizeConceptSetWithTemporaryTable.sql",
        packageName = "ConceptSetDiagnostics",
        dbms = dbms,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        conceptSetConceptIdsExcluded = conceptSetConceptIdsExcluded,
        conceptSetConceptIdsDescendantsExcluded = conceptSetConceptIdsDescendantsExcluded,
        conceptSetConceptIdsNotExcluded = conceptSetConceptIdsNotExcluded,
        conceptSetConceptIdsDescendantsNotExcluded = conceptSetConceptIdsDescendantsNotExcluded
      )
    
    sqlWithoutTemporaryTable <-
      SqlRender::loadRenderTranslateSql(
        sqlFilename = "OptimizeConceptSetWithoutTempTable.sql",
        packageName = "ConceptSetDiagnostics",
        dbms = dbms,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        conceptSetConceptIdsExcluded = conceptSetConceptIdsExcluded,
        conceptSetConceptIdsDescendantsExcluded = conceptSetConceptIdsDescendantsExcluded,
        conceptSetConceptIdsNotExcluded = conceptSetConceptIdsNotExcluded,
        conceptSetConceptIdsDescendantsNotExcluded = conceptSetConceptIdsDescendantsNotExcluded
      )
    
    
    if (numberOfConceptIds > 100) {
      sql <- sqlWithTemporaryTable
      renderTranslateExecuteSql(connection = connection,
                                sql = sql)
      retrieveSql <-
        SqlRender::translate(sql = "SELECT * FROM #optimized_set;", targetDialect = "postgresql")
    } else {
      sql <- sqlWithoutTemporaryTable
      retrieveSql <- sql
    }
    
    data <-
      DatabaseConnector::querySql(
        connection = connection,
        sql = retrieveSql,
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::arrange(1) %>%
      dplyr::tibble() %>%
      dplyr::filter(.data$conceptId != 0)
    
    return(data)
  }