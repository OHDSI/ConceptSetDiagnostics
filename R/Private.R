# Copyright 2022 Observational Health Data Sciences and Informatics
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
  checkmate::assertDataFrame(
    x = cohortDefinitionSet,
    min.cols = 1,
    add = errorMessage
  )
  checkmate::assertNames(
    x = colnames(cohortDefinitionSet),
    must.include = c("cohortId"),
    add = errorMessage
  )
  if (errorMessage$isEmpty()) {
    return(NULL)
  } else {
    return(errorMessage)
  }
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
                                 tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  conceptIdTable <-
    dplyr::tibble(conceptId = conceptIds %>% unique() %>% as.integer())

  tempTableName <-
    paste0("#t", (as.numeric(as.POSIXlt(Sys.time()))) * 100000)

  invisible(utils::capture.output(
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
    ),
    file = nullfile()
  ))
  if (connection@dbms %in% c("redshift", "postgresql")) {
    # Some performance tuning:
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = "ANALYZE @concept_id_table;",
      profile = FALSE,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema,
      concept_id_table = tempTableName
    )
  }

  return(tempTableName)
}


dropTempConceptTable <-
  function(tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
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
