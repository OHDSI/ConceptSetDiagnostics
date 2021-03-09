# given a list of standard conceptIds, get recommended concepts.
#' @export
getRecommendedStandard <-
  function(connection,
           conceptList,
           dbms = 'postgresql',
           vocabularyDatabaseSchema = 'vocabulary') {
    # Filtering strings to letters, numbers and spaces only to avoid SQL injection:
    conceptList <-  gsub("[^a-zA-Z0-9 ,]", " ", conceptList)
    
    sql <-
      SqlRender::loadRenderTranslateSql(
        sqlFilename = "RecommendedStandard.sql",
        packageName = "ConceptSetDiagnostics",
        dbms = dbms,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        source_list = conceptList[[1]]
      )
    
    data <-
      DatabaseConnector::querySql(
        connection = connection,
        sql = sql,
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