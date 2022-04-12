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

#' get concept id details
#'
#' @description
#' given an array of conceptIds, get their details
#'
#' @template Connection
#'
#' @template ConceptIds
#'
#' @template VocabularyDatabaseSchema
#'
#' @template ConceptPrevalenceTable
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
getConceptIdDetails <-
  function(conceptIds,
           connection = NULL,
           connectionDetails = NULL,
           vocabularyDatabaseSchema = 'vocabulary',
           conceptPrevalenceTable = 'concept_prevalence') {
    if (length(conceptIds) == 0) {
      stop('No concept id provided')
    }
    
    start <- Sys.time()
    
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    
    sql <-
      SqlRender::loadRenderTranslateSql(
        sqlFilename = "GetConceptIdDetails.sql",
        packageName = utils::packageName(),
        dbms = connection@dbms,
        vocabulary_database_schema = vocabularyDatabaseSchema
      )
    
    data <- DatabaseConnector::querySql(
      connection = connection,
      sql = sql,
      snakeCaseToCamelCase = TRUE,
      concept_ids = conceptIds
    ) %>%
      tidyr::tibble()
    
    if (!is.null(conceptPrevalenceTable)) {
      conceptPrevalence <- tryCatch(
        expr = {
          DatabaseConnector::querySql(
            connection = connection,
            sql = "SELECT CONCEPT_ID, RC, DBC, DRC, DDBC
              FROM @concept_prevalence_table
              WHERE CONCEPT_ID IN (@concept_ids);",
            snakeCaseToCamelCase = TRUE,
            concept_ids = conceptIds
          ) %>%
            tidyr::tibble()
        },
        finally =
          ParallelLogger::logInfo(" - Failed to obtain concept prevalence.")
      )
    }
    
    if (exists("conceptPrevalence") &&
        dplyr::is.tbl(conceptPrevalence)) {
      data <- data %>%
        dplyr::left_join(conceptPrevalence,
                         by = "conceptId")
    }
    
    return(data)
  }
