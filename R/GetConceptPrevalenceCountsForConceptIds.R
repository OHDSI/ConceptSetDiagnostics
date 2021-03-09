#' @export
getConceptPrevalenceCountsForConceptIds <- function(connection,
                                                    conceptIdsList,
                                                    conceptPrevalenceSchema = 'concept_prevalence',
                                                    dbms = 'postgresql') {
  sql <- "select *
          from @concept_prevalence.cp_master
          where concept_id in (@concept_list);"
  sql <- SqlRender::renderSql(
    sql = sql,
    concept_list = conceptIdsList,
    concept_prevalence = conceptPrevalenceSchema
  )
  sql <- SqlRender::translate(sql = sql,
                              targetDialect = dbms)
  if (length(conceptIdsList) > 0) {
    data <-
      DatabaseConnector::querySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::arrange(1)
    return(data)
  }
}