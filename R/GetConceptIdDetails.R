# Concept search using string
#' @export
getConceptIdDetails <-
  function(connection,
           vocabularyDatabaseSchema = 'vocabulary',
           conceptIds) {
    sql <- "SELECT c.CONCEPT_ID,
            	c.CONCEPT_NAME,
            	c.VOCABULARY_ID,
            	c.STANDARD_CONCEPT,
            	c.INVALID_REASON,
            	c.CONCEPT_CODE,
            	c.CONCEPT_CLASS_ID,
            	c.DOMAIN_ID,
            	ISNULL(universe.RC, 0) RC,
            	ISNULL(universe.DBC, 0) DBC,
            	ISNULL(universe.DRC, 0) DRC,
            	ISNULL(universe.DDBC, 0) DDBC
            FROM @vocabulary_database_schema.concept c
            LEFT JOIN concept_prevalence.universe ON c.concept_id = universe.concept_id
            WHERE c.CONCEPT_ID IN (@concept_id_list)
            ORDER BY ISNULL(universe.DRC, 0) DESC;"
    
    sql <- SqlRender::render(
      sql = sql,
      vocabulary_database_schema = vocabularyDatabaseSchema,
      concept_id_list = conceptIds
    )
    data <-
      renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::tibble()
    return(data)
  }