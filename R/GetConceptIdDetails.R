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
#' @template TempEmulationSchema
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
getConceptIdDetails <-
  function(conceptIds,
           connection = NULL,
           connectionDetails = NULL,
           vocabularyDatabaseSchema = "vocabulary",
           tempEmulationSchema = NULL,
           conceptPrevalenceTable = NULL) {
    if (length(conceptIds) == 0) {
      stop("No concept id provided")
    }
    
    start <- Sys.time()
    
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    
    
    conceptIdTable <-
      dplyr::tibble(conceptId = conceptIds %>% unique())
    
    tempTableName <-
      paste0("#t", (as.numeric(as.POSIXlt(Sys.time(
      )))) * 100000)
    DatabaseConnector::insertTable(
      connection = connection,
      tableName = tempTableName,
      dropTableIfExists = TRUE,
      tempTable = TRUE,
      tempEmulationSchema = tempEmulationSchema,
      data = conceptIdTable,
      camelCaseToSnakeCase = TRUE,
      bulkLoad = TRUE,
      progressBar = FALSE,
      createTable = TRUE
    )
    
    sql <- "
    SELECT c.CONCEPT_ID,
      	c.CONCEPT_NAME,
      	c.VOCABULARY_ID,
      	c.STANDARD_CONCEPT,
      	c.INVALID_REASON,
      	c.CONCEPT_CODE,
      	c.CONCEPT_CLASS_ID,
      	c.DOMAIN_ID
      FROM @vocabulary_database_schema.concept c
      INNER JOIN @concept_id_table tt
      ON c.concept_id = tt.concept_id;
    "
    
    data <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = sql,
      snakeCaseToCamelCase = TRUE,
      concept_id_table = tempTableName,
      vocabulary_database_schema = vocabularyDatabaseSchema
    ) %>%
      tidyr::tibble()
    
    if (!is.null(conceptPrevalenceTable)) {
      conceptPrevalence <- tryCatch(expr = {
        DatabaseConnector::renderTranslateQuerySql(
          connection = connection,
          sql = "SELECT cp.CONCEPT_ID, cp.RC, cp.DBC, cp.DRC, cp.DDBC
                 FROM @concept_prevalence_table cp
                 INNER JOIN @concept_id_table tt
                 ON cp.concept_id = tt.concept_id;",
          snakeCaseToCamelCase = TRUE,
          concept_id_table = tempTableName,
          concept_prevalence_table = conceptPrevalenceTable
        ) %>%
          tidyr::tibble()
      })
    }
    
    if (exists("conceptPrevalence") &&
        dplyr::is.tbl(conceptPrevalence)) {
      data <- data %>%
        dplyr::left_join(conceptPrevalence,
                         by = "conceptId")
    }
    
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = "DROP TABLE IF EXISTS @concept_id_table;",
      profile = FALSE,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema,
      concept_id_table = tempTableName
    )
    
    return(data)
  }
