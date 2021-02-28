# given a list of conceptIds, get their mapped
#' @export
mappedConcepts <-
  function(dataSource = .GlobalEnv,
           sql = SqlRender::readSql("sql/GetMappedSourcecodes.sql"),
           mappedConceptId) {
    if (is(dataSource, "environment")) {
      dplyr::tibble("Functionality  not available in local mode. Please connect to database.")
    } else {
      if (any(is.null(mappedConceptId),
              is.na(mappedConceptId),
              length(mappedConceptId) == 0)) {
        mappedConceptId <- 0
      }
      data <-
        renderTranslateQuerySql(
          connection = dataSource$connection,
          vocabulary_database_schema = dataSource$vocabularyDatabaseSchema,
          conceptsIdsToGetMapped = mappedConceptId,
          sql = sql, 
          snakeCaseToCamelCase = TRUE
        ) %>% 
        dplyr::arrange(1)
      return(data)
    }
  }