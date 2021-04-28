# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of ConceptSetDiagnostics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


renderTranslateExecuteRetrieveSql <-
  function(sql,
           connection,
           connectionDetails,
           ...) {
    # Set up connection to server ----------------------------------------------------
    if (is.null(connection)) {
      if (!is.null(connectionDetails)) {
        writeLines("Connecting to database using provided connection details.")
        connection <- DatabaseConnector::connect(connectionDetails)
        on.exit(DatabaseConnector::disconnect(connection))
      } else {
        stop("No connection or connectionDetails provided.")
      }
    } else if (methods::is(connection, "Pool")) {
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
      if (is.null(attr(connection, "dbms"))) {
        stop("dbms not provided. Unable to translate query.")
      }
      DatabaseConnector::renderTranslateExecuteSql(
        connection = connection,
        sql = sql,
        ...
      ) %>% dplyr::tibble()
    }
    data <-
      renderTranslateQuerySql(
        connection = connection,
        sql = "SELECT * FROM #optimized_set;",
        snakeCaseToCamelCase = TRUE
      )
    return(data)
  }
