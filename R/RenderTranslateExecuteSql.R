renderTranslateExecuteSql <-
  function(connection,
           sql,
           ...,
           snakeCaseToCamelCase = FALSE) {
    if (is(connection, "Pool")) {
      # Connection pool is used by Shiny app, which always uses PostgreSQL:
      sql <- SqlRender::render(sql, ...)
      sql <- SqlRender::translate(sql, targetDialect = "postgresql")
      
      tryCatch({
        DatabaseConnector::dbExecute(connection, sql)
      }, error = function(err) {
        writeLines(sql)
        stop(err)
      })
    } else {
      return(
        DatabaseConnector::renderTranslateExecuteSql(
          connection = connection,
          sql = sql,
          ...,
          snakeCaseToCamelCase = snakeCaseToCamelCase
        ) %>% dplyr::tibble()
      )
    }
  }