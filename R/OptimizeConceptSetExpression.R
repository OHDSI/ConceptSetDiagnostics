# given a concept set expression, get optimized concept set expression
#' @export
optimizeConceptSetExpression <-
  function(conceptSetExpression,
           connection,
           vocabularyDatabaseSchema = 'vocabulary') {
    pathToSql <- system.file("sql/sql_server",
                             "optimizeConceptSetWithTemporaryTable.sql",
                             package = 'ConceptSetDiagnostics')
    sqlWithTemporaryTable <-
      SqlRender::readSql(sourceFile = pathToSql)
    
    pathToSql <- system.file("sql/sql_server",
                             "optimizeConceptSetWithoutTempTable.sql",
                             package = 'ConceptSetDiagnostics')
    sqlWithoutTemporaryTable <-
      SqlRender::readSql(sourceFile = pathToSql)
    
    conceptSetExpressionTable <-
      getConceptSetDataFrameFromExpression(conceptSetExpression =
        conceptSetExpression)
    
    conceptSetExpressionTable <-
      tidyr::replace_na(
        data = conceptSetExpressionTable,
        replace = list(
          isExcluded = FALSE,
          includeDescendants = FALSE,
          includeMapped = FALSE
        )
      )
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
    if (length(unique(
      c(
        conceptSetConceptIdsExcluded,
        conceptSetConceptIdsDescendantsExcluded,
        conceptSetConceptIdsNotExcluded,
        conceptSetConceptIdsDescendantsNotExcluded
      )
    )) > 100) {
      sql <- sqlWithTemporaryTable
    } else {
      sql <- sqlWithoutTemporaryTable
    }
    
    sql <- SqlRender::render(
      sql = sql,
      vocabulary_database_schema = vocabularyDatabaseSchema,
      conceptSetConceptIdsExcluded = conceptSetConceptIdsExcluded,
      conceptSetConceptIdsDescendantsExcluded = conceptSetConceptIdsDescendantsExcluded,
      conceptSetConceptIdsNotExcluded = conceptSetConceptIdsNotExcluded,
      conceptSetConceptIdsDescendantsNotExcluded = conceptSetConceptIdsDescendantsNotExcluded
    )
    
    if (length(unique(
      c(
        conceptSetConceptIdsExcluded,
        conceptSetConceptIdsDescendantsExcluded,
        conceptSetConceptIdsNotExcluded,
        conceptSetConceptIdsDescendantsNotExcluded
      )
    )) > 100) {
      DatabaseConnector::renderTranslateExecuteSql(connection = connection,
                                                   sql = sql)
      retrieveSql <-
        SqlRender::render(sql = "SELECT * FROM #optimized_set;")
    } else {
      retrieveSql <- sql
    }
    
    data <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = retrieveSql,
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::arrange(1) %>%
      dplyr::tibble() %>% 
      dplyr::filter(.data$conceptId != 0)
    return(data)
    
  }