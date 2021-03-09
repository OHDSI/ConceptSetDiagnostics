# given a list of conceptIds, get their descendants
#' @export
getDescendantConcepts <-
  function(connection,
           descendantConceptId,
           vocabularyDatabaseSchema = 'vocabulary',
           dbms = 'postgresql') {
    if (any(
      is.null(descendantConceptId),
      is.na(descendantConceptId),
      length(descendantConceptId) == 0
    )) {
      descendantConceptId <- 0
    }
    sql <-
      SqlRender::loadRenderTranslateSql(
        sqlFilename = "GetDescendantConcepts.sql",
        packageName = "ConceptSetDiagnostics",
        dbms = dbms,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        conceptsIdsToGetDescendants = descendantConceptId
      )
    
    data <-
      DatabaseConnector::querySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::arrange(1)
    return(data)
  }