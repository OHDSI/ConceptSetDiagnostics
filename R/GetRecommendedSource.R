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

#' given a list of non standard conceptIds, get recommended conceptIds
#'
#' @template Connection
#'
#' @template ConceptIds
#'
#' @template VocabularyDatabaseSchema
#'
#' @template ConceptPrevalenceSchema
#'
#' @template TempEmulationSchema
#'
#' @export
getRecommendedSource <-
  function(conceptIds,
           vocabularyDatabaseSchema = "vocabulary",
           connection = NULL,
           connectionDetails = NULL,
           conceptPrevalenceSchema = "concept_prevalence",
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
    # Filtering strings to numbers only to avoid SQL injection:
    conceptIds <- gsub("[^0-9 ,]", " ", conceptIds)

    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }

    conceptPrevalenceTables <-
      DatabaseConnector::getTableNames(
        connection = connection,
        databaseSchema = conceptPrevalenceSchema
      ) |>
      tolower()

    conceptPrevalenceTablesExist <- FALSE

    if (all(
      "recommender_set" %in% conceptPrevalenceTables,
      "cp_master" %in% conceptPrevalenceTables,
      "recommended_blacklist" %in% conceptPrevalenceTables
    )) {
      conceptPrevalenceTablesExist <- TRUE
    }

    if (!conceptPrevalenceTablesExist) {
      stop(
        "Concept Prevalence schema does not have the required concept prevalence tables. recommender_set, cp_master, recommended_blacklist"
      )
    }

    tempTableName <- loadTempConceptTable(
      conceptIds = conceptIds,
      connection = connection,
      tempEmulationSchema = tempEmulationSchema
    )

    sql <- SqlRender::loadRenderTranslateSql(
      sqlFilename = "RecommendationSource.sql",
      packageName = "ConceptSetDiagnostics",
      dbms = connection@dbms,
      vocabulary_database_schema = vocabularyDatabaseSchema,
      concept_prevalence_schema = conceptPrevalenceSchema,
      concept_id_temp_table = tempTableName
    )

    writeLines(" - Finding recommended source concepts.")
    DatabaseConnector::executeSql(
      connection = connection,
      sql = sql,
      profile = FALSE,
      progressBar = FALSE,
      reportOverallTime = FALSE
    ) |> dplyr::tibble()

    data <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = "SELECT * FROM #recommended_src;",
      snakeCaseToCamelCase = TRUE
    )

    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = "
              DROP TABLE IF EXISTS #recommended_src;
              DROP TABLE IF EXISTS @concept_id_temp_table",
      concept_id_temp_table = tempTableName,
      profile = FALSE,
      progressBar = FALSE,
      reportOverallTime = FALSE
    )

    return(data)
  }
