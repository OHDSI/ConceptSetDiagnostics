# given a list of non standard conceptIds, get recommended conceptIds
#' @export
recommenderSource <-
  function(connection,
           conceptList,
           vocabularyDatabaseSchema = 'vocabulary') {
      # Filtering strings to letters, numbers and spaces only to avoid SQL injection:
      conceptList <-  gsub("[^a-zA-Z0-9 ,]", " ", conceptList)
      
      pathToSql <- system.file("sql/sql_server",
                               "RecommendationSource.sql",
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