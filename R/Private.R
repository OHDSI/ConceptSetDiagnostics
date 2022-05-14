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


checkIfCohortDefinitionSet <- function(cohortDefinitionSet) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(x = cohorts,
                             min.cols = 1,
                             add = errorMessage)
  checkmate::assertNames(
    x = colnames(cohorts),
    must.include = c("cohortId"),
    add = errorMessage
  )
  errorMessage
}


# private function - not exported
hasData <- function(data) {
  if (is.null(data)) {
    return(FALSE)
  }
  if (is.data.frame(data)) {
    if (nrow(data) == 0) {
      return(FALSE)
    }
  }
  if (!is.data.frame(data)) {
    if (length(data) == 0) {
      return(FALSE)
    }
    if (length(data) == 1) {
      if (is.na(data)) {
        return(FALSE)
      }
      if (data == "") {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}


loadTempConceptTable <- function(conceptIds,
                                 connection,
                                 tempEmulationSchema = NULL) {
  conceptIdTable <-
    dplyr::tibble(conceptId = conceptIds %>% unique())
  
  tempTableName <-
    paste0("#t", (as.numeric(as.POSIXlt(Sys.time(
      
    )))) * 100000)
  
  
  DatabaseConnector::renderTranslateExecuteSql(connection = connection,
                                               sql = "DROP TABLE IF EXISTS @concept_id_table;
                                                      --HINT DISTRIBUTE_ON_KEY(concept_id)
                                                      CREATE TABLE @concept_id_table (concept_id BIGINT);", 
                                               profile = FALSE, 
                                               progressBar = FALSE, 
                                               reportOverallTime = FALSE, 
                                               tempEmulationSchema = tempEmulationSchema,
                                               concept_id_table = tempTableName)
  
  DatabaseConnector::insertTable(
    connection = connection,
    tableName = tempTableName,
    dropTableIfExists = FALSE,
    tempTable = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    data = conceptIdTable,
    camelCaseToSnakeCase = TRUE,
    bulkLoad = TRUE,
    progressBar = FALSE,
    createTable = FALSE
  )
  if (connection@dbms %in% c("redshift", "postgresql")) {
    # Some performance tuning:
    DatabaseConnector::renderTranslateExecuteSql(connection = connection,
                                                 sql = "ANALYZE @concept_id_table;", 
                                                 profile = FALSE, 
                                                 progressBar = FALSE, 
                                                 reportOverallTime = FALSE, 
                                                 tempEmulationSchema = tempEmulationSchema,
                                                 concept_id_table = tempTableName)
  }
  
  return(tempTableName)
}


dropTempConceptTable <- function(tempEmulationSchema = NULL,
                                 connection,
                                 tempTableName) {
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "DROP TABLE IF EXISTS @concept_id_table;",
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE,
    tempEmulationSchema = tempEmulationSchema,
    concept_id_table = tempTableName
  )
}