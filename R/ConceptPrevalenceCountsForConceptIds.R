#' @export
getConceptPrevalenceCountsForConceptIds <- function(dataSource = .GlobalEnv,
                                                    conceptIdsList) {
  
  sql <- "select *
          from concept_prevalence.cp_master
          where concept_id in (@concept_list);"
  if (length(conceptIdsList) > 0) {
    data <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = dataSource$connection,
        concept_list = conceptIdsList,
        sql = sql, 
        snakeCaseToCamelCase = TRUE
      ) %>%  
      dplyr::arrange(1)
    return(data) 
  } else {
    return(dplyr::tibble())
  }
}