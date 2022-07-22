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

#' Given conceptId(s) get the counts of occurrence with mapping.
#'
#' @description
#' Given conceptId(s) get the counts of occurrence with mapping.
#'
#' @template Connection
#'
#' @template ConceptIds
#'
#' @template VocabularyDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @param minCellCount                The minimum cell count for fields containing person/subject count.
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
# function: getCountOfSourceCodesMappedToStandardConcept ----
getCountOfSourceCodesMappedToStandardConcept <- function(conceptIds,
                                                         connection = NULL,
                                                         connectionDetails = NULL,
                                                         cdmDatabaseSchema,
                                                         tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                                         minCellCount = 0) {
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
    dplyr::filter(nchar(.data$domainSourceConceptId) > 1)

  sqlConceptMapping <-
    " DROP TABLE IF EXISTS @concept_mapping_table;
      CREATE TABLE @concept_mapping_table  (concept_id INT,
                                            source_concept_id INT,
                                            domain_table VARCHAR(20),
                                            concept_count BIGINT,
                                            subject_count BIGINT);"
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sqlConceptMapping,
    tempEmulationSchema = tempEmulationSchema,
    concept_mapping_table = paste0(tempTableName, "cc"),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  sqlMapping <- "
                  INSERT INTO @concept_mapping_table
                  SELECT @domain_concept_id concept_id,
                  	@domain_source_concept_id source_concept_id,
                  	'@domainTableShort' domain_table,
                  	COUNT(*) AS concept_count,
                  	COUNT(DISTINCT person_id) AS subject_count
                  FROM @cdm_database_schema.@domain_table
                  WHERE
                  		@domain_source_concept_id IS NOT NULL
                  		AND @domain_source_concept_id > 0
                  		AND @domain_concept_id IN
                  		(SELECT concept_id FROM @concept_id_table)
                  GROUP BY @domain_concept_id,
                  	@domain_source_concept_id
                  ORDER BY @domain_concept_id,
                  	@domain_source_concept_id;"

  conceptMapping <- list()
  for (i in (1:nrow(domains))) {
    rowData <- domains[i, ]

    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sqlMapping,
      tempEmulationSchema = tempEmulationSchema,
      domain_table = rowData$domainTable,
      domain_concept_id = rowData$domainConceptId,
      domain_source_concept_id = rowData$domainSourceConceptId,
      cdm_database_schema = cdmDatabaseSchema,
      concept_id_table = tempTableName,
      concept_mapping_table = paste0(tempTableName, "cc"),
      domainTableShort = rowData$domainTableShort,
      reportOverallTime = FALSE,
      progressBar = FALSE
    )
  }
  sql <- "SELECT DISTINCT *
          FROM @concept_mapping_table
          ORDER BY domain_table,
                  concept_id,
                  source_concept_id,
                  concept_count,
                  subject_count;"
  conceptMapping <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = sql,
      concept_mapping_table = paste0(tempTableName, "cc"),
      snakeCaseToCamelCase = TRUE,
      tempEmulationSchema = tempEmulationSchema
    )
  conceptMapping <- conceptMapping %>%
    dplyr::arrange(
      .data$domainTable,
      .data$conceptId,
      .data$sourceConceptId,
      .data$conceptCount,
      .data$subjectCount
    ) %>%
    dplyr::tibble()

  if (nrow(conceptMapping) > 0) {
    conceptMapping <- dplyr::bind_rows(
      conceptMapping,
      conceptMapping %>%
        dplyr::group_by(
          .data$conceptId,
          .data$sourceConceptId
        ) %>%
        dplyr::summarise(
          conceptCount = sum(.data$conceptCount),
          subjectCount = max(.data$subjectCount),
          .groups = "keep"
        ) %>%
        dplyr::mutate(domainTable = "All")
    ) %>%
      dplyr::distinct()
  }

  conceptMapping <- conceptMapping %>%
    dplyr::filter(.data$subjectCount > minCellCount)

  sqlDdlDrop <-
    "DROP TABLE IF EXISTS @concept_mapping_table;"
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sqlDdlDrop,
    concept_mapping_table = paste0(tempTableName, "cc"),
    tempEmulationSchema = tempEmulationSchema,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  return(conceptMapping)
}
