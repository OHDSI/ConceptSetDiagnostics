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
#


#' get concept id count
#'
#' @description
#' Get the count for an array of concept id(s) from concept prevalence table.
#'
#' @template Connection
#'
#' @template ConceptIds
#'
#' @template ConceptPrevalenceTable
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
getConceptPrevalenceCounts <- function(conceptIds,
                                       connection = NULL,
                                       connectionDetails = NULL,
                                       conceptPrevalenceTable) {
  start <- Sys.time()
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  tempTableName <- loadTempConceptTable(conceptIds = conceptIds,
                                        connection = connection)
  
  sql <- "SELECT cp.*
          FROM @conceptPrevalenceTable cp
          INNER JOIN @concept_id_table ci
          ON cp.concept_id = ci.concept_id;"
  
  data <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      conceptPrevalenceTable = conceptPrevalenceTable,
      concept_id_table = tempTableName,
      sql = sql,
      snakeCaseToCamelCase = TRUE
    ) %>% dplyr::tibble()
  
  dropTempConceptTable(connection = connection,
                       tempTableName = tempTableName)
  
  return(data)
}
