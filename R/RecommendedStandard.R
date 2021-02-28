# given a list of standard conceptIds, get recommended concepts.
#' @export
recommendedStandard <-
  function(connection,
           conceptList,
           vocabularyDatabaseSchema = 'vocabulary') {
    # Filtering strings to letters, numbers and spaces only to avoid SQL injection:
    conceptList <-  gsub("[^a-zA-Z0-9 ,]", " ", conceptList)
    
    pathToSql <- system.file("sql/sql_server",
                             "RecommendedStandard.sql",
                             package = 'ConceptSetDiagnostics')
    sql <- SqlRender::readSql(sourceFile = pathToSql)
    
    data <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        source_list = conceptList[[1]],
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::tibble() %>%
      dplyr::filter(!.data$conceptId %in% conceptList) %>%
      dplyr:::arrange(dplyr::desc(.data$descendantRecordCount)) %>%
      dplyr::rename(
        rc = recordCount,
        dc = databaseCount,
        drc = descendantRecordCount,
        dbc = descendantDatabaseCount
      )
    return(data)
  }