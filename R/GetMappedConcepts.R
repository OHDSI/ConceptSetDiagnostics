# given a list of conceptIds, get their mapped
#' @export
getMappedConcepts <-
  function(connection,
           mappedConceptId,
           vocabularyDatabaseSchema = 'vocabulary',
           dbms = 'postgresql') {
    if (any(is.null(mappedConceptId),
            is.na(mappedConceptId),
            length(mappedConceptId) == 0)) {
      mappedConceptId <- 0
    }
    sql <-
      SqlRender::loadRenderTranslateSql(
        sqlFilename = "GetMappedSourcecodes.sql",
        packageName = "ConceptSetDiagnostics",
        dbms = dbms,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        conceptsIdsToGetMapped = mappedConceptId
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