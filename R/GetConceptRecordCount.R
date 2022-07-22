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

#' Given conceptId(s) get the record count.
#'
#' @description
#' Given conceptId(s) get the record count.
#'
#' @template Connection
#'
#' @template ConceptIds
#'
#' @template cdmDatabaseSchema
#'
#' @template VocabularyDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
# function: getConceptRecordCount ----
getConceptRecordCount <- function(conceptIds,
                                  connection = NULL,
                                  connectionDetails = NULL,
                                  cdmDatabaseSchema,
                                  vocabularyDatabaseSchema = cdmDatabaseSchema,
                                  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  tempTableName <- loadTempConceptTable(
    conceptIds = conceptIds,
    connection = connection,
    tempEmulationSchema = tempEmulationSchema
  )

  domains <-
    getDomainInformation(packageName = "ConceptSetDiagnostics")
  domains <- domains$wide %>%
    dplyr::filter(.data$isEraTable == FALSE)
  # filtering out ERA tables because they are supposed to be derived tables, and counting them is double counting

  sqlDdlDrop <-
    "DROP TABLE IF EXISTS @concept_count_temp;"
  sqlDdlCreate <- "
  CREATE TABLE @concept_count_temp (
                                    	concept_id INT,
                                    	event_year INT,
                                    	event_month INT,
                                    	concept_is_standard VARCHAR(1),
                                    	concept_count BIGINT,
                                    	subject_count BIGINT
                                    	);"
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sqlDdlDrop,
    tempEmulationSchema = tempEmulationSchema,
    concept_mapping_table = paste0(tempTableName, "cc"),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sqlDdlCreate,
    tempEmulationSchema = tempEmulationSchema,
    concept_mapping_table = paste0(tempTableName, "cc"),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  # REASON for many SQL --DISTINCT subject_count cannot be computed from aggregation query of calendar month level data
  sql1 <- "INSERT INTO @concept_count_temp
          	SELECT @domain_concept_id concept_id,
          		YEAR(@domain_start_date) event_year,
          		MONTH(@domain_start_date) event_month,
          		'Y' concept_is_standard,
          		COUNT_BIG(*) concept_count,
          		COUNT_BIG(DISTINCT person_id) subject_count
          	FROM @cdm_database_schema.@domain_table dt
          	WHERE @domain_concept_id IN (
          		SELECT DISTINCT concept_id
          		FROM @concept_id_universe
          		)
          		AND YEAR(@domain_start_date) > 0
          		AND @domain_concept_id > 0
          	GROUP BY @domain_concept_id,
          		YEAR(@domain_start_date),
          		MONTH(@domain_start_date);"
  sql2 <- " INSERT INTO @concept_count_temp
            SELECT @domain_concept_id concept_id,
            	YEAR(@domain_start_date) event_year,
            	0 AS event_month,
            	'Y' concept_is_standard,
            	COUNT_BIG(*) concept_count,
            	COUNT_BIG(DISTINCT person_id) subject_count
            FROM @cdm_database_schema.@domain_table
            WHERE @domain_concept_id IN (
          		SELECT DISTINCT concept_id
          		FROM @concept_id_universe
          		)
          		AND YEAR(@domain_start_date) > 0
            	AND @domain_concept_id > 0
            GROUP BY @domain_concept_id,
            	YEAR(@domain_start_date);"
  sql3 <- "INSERT INTO @concept_count_temp
            SELECT @domain_concept_id concept_id,
            	0 as event_year,
            	0 as event_month,
          		'Y' concept_is_standard,
            	COUNT_BIG(*) concept_count,
            	COUNT_BIG(DISTINCT person_id) subject_count
            FROM @cdm_database_schema.@domain_table dt
            WHERE @domain_concept_id IN (
          		SELECT DISTINCT concept_id
          		FROM @concept_id_universe
          		)
          		AND YEAR(@domain_start_date) > 0
            AND @domain_concept_id > 0
            GROUP BY @domain_concept_id;"


  sql4 <- "INSERT INTO @concept_count_temp
          	SELECT @domain_concept_id concept_id,
          		YEAR(@domain_start_date) event_year,
          		MONTH(@domain_start_date) event_month,
          		'N' concept_is_standard,
          		COUNT_BIG(*) concept_count,
          		COUNT_BIG(DISTINCT person_id) subject_count
          	FROM @cdm_database_schema.@domain_table dt
          	LEFT JOIN (
          	  SELECT concept_id
          	  FROM @vocabulary_database_schema.CONCEPT
          	  WHERE standard_concept = 'S'
          	) std
          	ON @domain_concept_id = std.concept_id
          	WHERE @domain_concept_id IN (
          		SELECT DISTINCT concept_id
          		FROM @concept_id_universe
          		)
          		AND YEAR(@domain_start_date) > 0
          		AND @domain_concept_id > 0
          		AND std.concept_id IS NULL
          	GROUP BY @domain_concept_id,
          		YEAR(@domain_start_date),
          		MONTH(@domain_start_date);"
  sql5 <- " INSERT INTO @concept_count_temp
            SELECT @domain_concept_id concept_id,
            	YEAR(@domain_start_date) event_year,
            	0 AS event_month,
            	'N' concept_is_standard,
            	COUNT_BIG(*) concept_count,
            	COUNT_BIG(DISTINCT person_id) subject_count
            FROM @cdm_database_schema.@domain_table dt
            LEFT JOIN (
          	  SELECT concept_id
          	  FROM @vocabulary_database_schema.CONCEPT
          	  WHERE standard_concept = 'S'
          	) std ON @domain_concept_id = std.concept_id
            WHERE @domain_concept_id IN (
          		SELECT DISTINCT concept_id
          		FROM @concept_id_universe
          		)
          		AND YEAR(@domain_start_date) > 0
            	AND @domain_concept_id > 0
            	AND std.concept_id IS NULL
            GROUP BY @domain_concept_id,
            	YEAR(@domain_start_date);"
  sql6 <- " INSERT INTO @concept_count_temp
            SELECT @domain_concept_id concept_id,
            	0 AS event_year,
            	0 AS event_month,
            	'N' concept_is_standard,
            	COUNT_BIG(*) concept_count,
            	COUNT_BIG(DISTINCT person_id) subject_count
            FROM @cdm_database_schema.@domain_table dt
            LEFT JOIN (
          	  SELECT concept_id
          	  FROM @vocabulary_database_schema.CONCEPT
          	  WHERE standard_concept = 'S'
          	) std ON @domain_concept_id = std.concept_id
            WHERE @domain_concept_id IN (
          		SELECT DISTINCT concept_id
          		FROM @concept_id_universe
          		)
          		AND YEAR(@domain_start_date) > 0
            	AND @domain_concept_id > 0
            	AND std.concept_id IS NULL
            GROUP BY @domain_concept_id;"

  for (i in (1:nrow(domains))) {
    rowData <- domains[i, ]
    ParallelLogger::logTrace(paste0(
      "   - Working on ",
      rowData$domainTable,
      ".",
      rowData$domainConceptId
    ))
    ParallelLogger::logTrace("    - Counting concepts by calendar month and year")
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql1,
      tempEmulationSchema = tempEmulationSchema,
      domain_table = rowData$domainTable,
      domain_concept_id = rowData$domainConceptId,
      cdm_database_schema = cdmDatabaseSchema,
      domain_start_date = rowData$domainStartDate,
      concept_count_temp = paste0(tempTableName, "cc"),
      concept_id_universe = tempTableName,
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
    ParallelLogger::logTrace("    - Counting concepts by calendar year")
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql2,
      tempEmulationSchema = tempEmulationSchema,
      domain_table = rowData$domainTable,
      domain_concept_id = rowData$domainConceptId,
      cdm_database_schema = cdmDatabaseSchema,
      domain_start_date = rowData$domainStartDate,
      concept_count_temp = paste0(tempTableName, "cc"),
      concept_id_universe = tempTableName,
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
    ParallelLogger::logTrace("    - Counting concepts without calendar period")
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql3,
      tempEmulationSchema = tempEmulationSchema,
      domain_table = rowData$domainTable,
      domain_concept_id = rowData$domainConceptId,
      cdm_database_schema = cdmDatabaseSchema,
      domain_start_date = rowData$domainStartDate,
      concept_count_temp = paste0(tempTableName, "cc"),
      concept_id_universe = tempTableName,
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
  }

  for (i in (1:nrow(domains))) {
    rowData <- domains[i, ]
    if (nchar(rowData$domainSourceConceptId) > 4) {
      ParallelLogger::logTrace(
        paste0(
          "   - Working on ",
          rowData$domainTable,
          ".",
          rowData$domainSourceConceptId
        )
      )
      ParallelLogger::logTrace("    - Counting concepts by calendar month and year")
      DatabaseConnector::renderTranslateExecuteSql(
        connection = connection,
        sql = sql4,
        tempEmulationSchema = tempEmulationSchema,
        domain_table = rowData$domainTable,
        domain_concept_id = rowData$domainSourceConceptId,
        cdm_database_schema = cdmDatabaseSchema,
        domain_start_date = rowData$domainStartDate,
        concept_count_temp = paste0(tempTableName, "cc"),
        concept_id_universe = tempTableName,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        progressBar = FALSE,
        reportOverallTime = FALSE
      )
      ParallelLogger::logTrace("    - Counting concepts by calendar year")
      DatabaseConnector::renderTranslateExecuteSql(
        connection = connection,
        sql = sql5,
        tempEmulationSchema = tempEmulationSchema,
        domain_table = rowData$domainTable,
        domain_concept_id = rowData$domainSourceConceptId,
        cdm_database_schema = cdmDatabaseSchema,
        domain_start_date = rowData$domainStartDate,
        concept_count_temp = paste0(tempTableName, "cc"),
        concept_id_universe = tempTableName,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        progressBar = FALSE,
        reportOverallTime = FALSE
      )
      ParallelLogger::logTrace("    - Counting concepts - no calendar stratification")
      DatabaseConnector::renderTranslateExecuteSql(
        connection = connection,
        sql = sql6,
        tempEmulationSchema = tempEmulationSchema,
        domain_table = rowData$domainTable,
        domain_concept_id = rowData$domainSourceConceptId,
        cdm_database_schema = cdmDatabaseSchema,
        domain_start_date = rowData$domainStartDate,
        concept_count_temp = paste0(tempTableName, "cc"),
        concept_id_universe = tempTableName,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        progressBar = FALSE,
        reportOverallTime = FALSE
      )
    }
  }
  retrieveSql <- "SELECT concept_id, event_year, event_month,
                    sum(concept_count) concept_count,
                    max(subject_count) subject_count
                  FROM @concept_count_temp
                  GROUP BY concept_id, event_year, event_month
                  ORDER By concept_id, event_year, event_month;"
  data <- renderTranslateQuerySql(
    connection = connection,
    sql = retrieveSql,
    snakeCaseToCamelCase = TRUE
  ) %>%
    dplyr::tibble()

  # i was thinking of keeping counts at the table level - but the file size became too big
  # so i decided to not include them
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sqlDdlDrop,
    tempEmulationSchema = tempEmulationSchema,
    concept_count_temp = paste0(tempTableName, "cc"),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  return(data)
}
