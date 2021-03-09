# given a list of non standard conceptIds, get recommended conceptIds
#' @export
getRecommendedSource <-
  function(connection,
           conceptList,
           dbms = 'postgresql',
           vocabularyDatabaseSchema = 'vocabulary') {
    # Filtering strings to letters, numbers and spaces only to avoid SQL injection:
    conceptList <-  gsub("[^a-zA-Z0-9 ,]", " ", conceptList)
    
    sql <-
      SqlRender::loadRenderTranslateSql(
        sqlFilename = "RecommendationSource.sql",
        packageName = "ConceptSetDiagnostics",
        dbms = dbms,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        source_list = conceptList[[1]]
      )
    
    data <-
      renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::tibble() %>%
      dplyr::filter(!.data$conceptId %in% conceptList) %>%
      dplyr:::arrange(dplyr::desc(.data$descendantRecordCount)) %>%
      dplyr::rename(
        rc = .data$recordCount,
        dc = .data$databaseCount,
        drc = .data$descendantRecordCount,
        dbc = .data$descendantDatabaseCount
      )
    return(data)
  }