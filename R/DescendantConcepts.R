# given a list of conceptIds, get their descendants
#' @export
descendantConcepts <-
  function(dataSource = .GlobalEnv,
           sql = SqlRender::readSql("sql/GetDescendantConcepts.sql"),
           descendantConceptId) {
    if (is(dataSource, "environment")) {
      dplyr::tibble("Functionality  not available in local mode. Please connect to database.")
    } else {
      if (any(is.null(descendantConceptId),
              is.na(descendantConceptId),
              length(descendantConceptId) == 0)) {
        descendantConceptId <- 0
      }
      sql <- SqlRender::render(sql = sql)
      data <-
        renderTranslateQuerySql(
          connection = dataSource$connection,
          vocabulary_database_schema = dataSource$vocabularyDatabaseSchema,
          conceptsIdsToGetDescendants = descendantConceptId,
          sql = sql, 
          snakeCaseToCamelCase = TRUE
        ) %>% 
        dplyr::arrange(1)
      return(data)
    }
  }