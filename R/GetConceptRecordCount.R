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
#' @template CdmDatabaseSchema
#'
#' @template VocabularyDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @param minCellCount                The minimum cell count for fields containing person/subject count.
#'
#' @param incidence                   Do you want to limit to first dispensation by person?
#'
#' @param domain                      domains to look for concept id
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
# function: getConceptRecordCount ----
getConceptRecordCount <- function(conceptIds = NULL,
                                  connection = NULL,
                                  connectionDetails = NULL,
                                  cdmDatabaseSchema,
                                  vocabularyDatabaseSchema = cdmDatabaseSchema,
                                  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                  minCellCount = 0,
                                  domainTableName = c(
                                    "drug_exposure",
                                    "condition_occurrence",
                                    "procedure_occurrence",
                                    "mesaurement",
                                    "observation"
                                  ),
                                  incidence = FALSE) {
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  uploadedConceptTable <- ''
  if (!is.null(conceptIds)) {
    uploadedConceptTable <- loadTempConceptTable(conceptIds = conceptIds,
                                                 connection = connection)
  }
  
  domainInformation <-
    getDomainInformation(packageName = "ConceptSetDiagnostics")
  
  domainsWide <- domainInformation$wide |>
    dplyr::filter(domainTable %in% c(domainTableName)) |>
    dplyr::filter(.data$isEraTable == FALSE)
  
  domainsLong <- domainInformation$long |>
    dplyr::filter(domainTable %in% c(domainTableName)) |>
    dplyr::filter(eraTable == FALSE)
  # filtering out ERA tables because they are supposed to be derived tables, and counting them is double counting
  
  # REASON for many SQL --DISTINCT subject_count cannot be computed from aggregation query of calendar month level data
  sql1 <- "SELECT @domain_concept_id concept_id,
          		YEAR(@domain_start_date) event_year,
          		MONTH(@domain_start_date) event_month,
          		'Y' concept_is_standard,
          		COUNT_BIG(*) concept_count,
          		COUNT_BIG(DISTINCT dt.person_id) subject_count
          	FROM @cdm_database_schema.@domain_table dt
          	
            {@incidence} ? {
            INNER JOIN 
              (
                SELECT person_id, 
                        @domain_concept_id concept_id, 
                        min(@domain_start_date) start_date
                FROM @cdm_database_schema.@domain_table in1
                {@concept_id_universe != ''} ? {
                    INNER JOIN (
                            		SELECT DISTINCT concept_id
                            		FROM @concept_id_universe
                            		) ci
                    ON @domain_concept_id = ci.concept_id
                }
                GROUP By person_id, @domain_concept_id
              ) in0
              ON dt.person_id = in0.person_id
              AND dt.@domain_concept_id = in0.concept_id
              AND dt.@domain_start_date = in0.start_date
            }
          
            {@concept_id_universe != ''} ? {
            INNER JOIN (
                    		SELECT DISTINCT concept_id
                    		FROM @concept_id_universe
                    		) c
            ON @domain_concept_id = c.concept_id
            }
          	WHERE  YEAR(@domain_start_date) > 0
          		AND @domain_concept_id > 0
          	GROUP BY @domain_concept_id,
          		YEAR(@domain_start_date),
          		MONTH(@domain_start_date);"
  sql2 <- " SELECT @domain_concept_id concept_id,
            	YEAR(@domain_start_date) event_year,
            	0 AS event_month,
            	'Y' concept_is_standard,
            	COUNT_BIG(*) concept_count,
            	COUNT_BIG(DISTINCT dt.person_id) subject_count
            FROM @cdm_database_schema.@domain_table dt
            	
            {@incidence} ? {
            INNER JOIN 
              (
                SELECT person_id, 
                        @domain_concept_id concept_id, 
                        min(@domain_start_date) start_date
                FROM @cdm_database_schema.@domain_table in1
                {@concept_id_universe != ''} ? {
                    INNER JOIN (
                            		SELECT DISTINCT concept_id
                            		FROM @concept_id_universe
                            		) ci
                    ON @domain_concept_id = ci.concept_id
                }
                GROUP By person_id, @domain_concept_id
              ) in0
              ON dt.person_id = in0.person_id
              AND dt.@domain_concept_id = in0.concept_id
              AND dt.@domain_start_date = in0.start_date
            }
          
            {@concept_id_universe != ''} ? {
            INNER JOIN (
                    		SELECT DISTINCT concept_id
                    		FROM @concept_id_universe
                    		) c
            ON @domain_concept_id = c.concept_id
            }
            WHERE YEAR(@domain_start_date) > 0
            	AND @domain_concept_id > 0
            GROUP BY @domain_concept_id,
            	YEAR(@domain_start_date);"
  sql3 <- "SELECT @domain_concept_id concept_id,
            	0 as event_year,
            	0 as event_month,
          		'Y' concept_is_standard,
            	COUNT_BIG(*) concept_count,
            	COUNT_BIG(DISTINCT dt.person_id) subject_count
            FROM @cdm_database_schema.@domain_table dt
          	
            {@incidence} ? {
            INNER JOIN 
              (
                SELECT person_id, 
                        @domain_concept_id concept_id, 
                        min(@domain_start_date) start_date
                FROM @cdm_database_schema.@domain_table in1
                {@concept_id_universe != ''} ? {
                    INNER JOIN (
                            		SELECT DISTINCT concept_id
                            		FROM @concept_id_universe
                            		) ci
                    ON @domain_concept_id = ci.concept_id
                }
                GROUP By person_id, @domain_concept_id
              ) in0
              ON dt.person_id = in0.person_id
              AND dt.@domain_concept_id = in0.concept_id
              AND dt.@domain_start_date = in0.start_date
            }
          
            {@concept_id_universe != ''} ? {
            INNER JOIN (
                    		SELECT DISTINCT concept_id
                    		FROM @concept_id_universe
                    		) c
            ON @domain_concept_id = c.concept_id
            }
            WHERE YEAR(@domain_start_date) > 0
            AND @domain_concept_id > 0
            GROUP BY @domain_concept_id;"
  
  
  sql4 <- "SELECT @domain_concept_id concept_id,
          		YEAR(@domain_start_date) event_year,
          		MONTH(@domain_start_date) event_month,
          		'N' concept_is_standard,
          		COUNT_BIG(*) concept_count,
          		COUNT_BIG(DISTINCT dt.person_id) subject_count
          	FROM @cdm_database_schema.@domain_table dt
          	
            {@incidence} ? {
            INNER JOIN 
              (
                SELECT person_id, 
                        @domain_concept_id concept_id, 
                        min(@domain_start_date) start_date
                FROM @cdm_database_schema.@domain_table in1
                {@concept_id_universe != ''} ? {
                    INNER JOIN (
                            		SELECT DISTINCT concept_id
                            		FROM @concept_id_universe
                            		) ci
                    ON @domain_concept_id = ci.concept_id
                }
                GROUP By person_id, @domain_concept_id
              ) in0
              ON dt.person_id = in0.person_id
              AND dt.@domain_concept_id = in0.concept_id
              AND dt.@domain_start_date = in0.start_date
            }
          
          	INNER JOIN (
          	  SELECT concept_id
          	  FROM @vocabulary_database_schema.CONCEPT
          	  WHERE standard_concept != 'S' OR standard_concept IS NULL
          	) std
          	ON @domain_concept_id = std.concept_id
            {@concept_id_universe != ''} ? {
            INNER JOIN (
                    		SELECT DISTINCT concept_id
                    		FROM @concept_id_universe
                    		) c
            ON @domain_concept_id = c.concept_id
            }
          	WHERE YEAR(@domain_start_date) > 0
          		AND @domain_concept_id > 0
          		AND std.concept_id IS NULL
          	GROUP BY @domain_concept_id,
          		YEAR(@domain_start_date),
          		MONTH(@domain_start_date);"
  
  sql5 <- " SELECT @domain_concept_id concept_id,
            	YEAR(@domain_start_date) event_year,
            	0 AS event_month,
            	'N' concept_is_standard,
            	COUNT_BIG(*) concept_count,
            	COUNT_BIG(DISTINCT dt.person_id) subject_count
            FROM @cdm_database_schema.@domain_table dt
              
            {@incidence} ? {
            INNER JOIN 
              (
                SELECT person_id, 
                        @domain_concept_id concept_id, 
                        min(@domain_start_date) start_date
                FROM @cdm_database_schema.@domain_table in1
                {@concept_id_universe != ''} ? {
                    INNER JOIN (
                            		SELECT DISTINCT concept_id
                            		FROM @concept_id_universe
                            		) ci
                    ON @domain_concept_id = ci.concept_id
                }
                GROUP By person_id, @domain_concept_id
              ) in0
              ON dt.person_id = in0.person_id
              AND dt.@domain_concept_id = in0.concept_id
              AND dt.@domain_start_date = in0.start_date
            }
          
            INNER JOIN (
          	  SELECT concept_id
          	  FROM @vocabulary_database_schema.CONCEPT
          	  WHERE standard_concept != 'S' OR standard_concept IS NULL
          	) std ON @domain_concept_id = std.concept_id
            {@concept_id_universe != ''} ? {
            INNER JOIN (
                    		SELECT DISTINCT concept_id
                    		FROM @concept_id_universe
                    		) c
            ON @domain_concept_id = c.concept_id
            }
            WHERE YEAR(@domain_start_date) > 0
            	AND @domain_concept_id > 0
            	AND std.concept_id IS NULL
            GROUP BY @domain_concept_id,
            	YEAR(@domain_start_date);"
  
  sql6 <- " SELECT @domain_concept_id concept_id,
            	0 AS event_year,
            	0 AS event_month,
            	'N' concept_is_standard,
            	COUNT_BIG(*) concept_count,
            	COUNT_BIG(DISTINCT dt.person_id) subject_count
            FROM @cdm_database_schema.@domain_table dt
          	
            {@incidence} ? {
            INNER JOIN 
              (
                SELECT person_id, 
                        @domain_concept_id concept_id, 
                        min(@domain_start_date) start_date
                FROM @cdm_database_schema.@domain_table in1
                {@concept_id_universe != ''} ? {
                    INNER JOIN (
                            		SELECT DISTINCT concept_id
                            		FROM @concept_id_universe
                            		) ci
                    ON @domain_concept_id = ci.concept_id
                }
                GROUP By person_id, @domain_concept_id
              ) in0
              ON dt.person_id = in0.person_id
              AND dt.@domain_concept_id = in0.concept_id
              AND dt.@domain_start_date = in0.start_date
            }
          
            INNER JOIN (
          	  SELECT concept_id
          	  FROM @vocabulary_database_schema.CONCEPT
          	  WHERE standard_concept != 'S' OR standard_concept IS NULL
          	) std ON @domain_concept_id = std.concept_id
            {@concept_id_universe != ''} ? {
            INNER JOIN (
                    		SELECT DISTINCT concept_id
                    		FROM @concept_id_universe
                    		) c
            ON @domain_concept_id = c.concept_id
            }
            WHERE YEAR(@domain_start_date) > 0
            	AND @domain_concept_id > 0
            	AND std.concept_id IS NULL
            GROUP BY @domain_concept_id;"
  
  data <- c()
  
  for (i in (1:nrow(domainsWide))) {
    rowData <- NULL
    rowData <- domainsWide[i,]
    
    writeLines(paste0(
      " - Working on ",
      rowData$domainTable,
      ".",
      rowData$domainConceptId
    ))
    
    longData <- NULL
    longData <- domainsLong |>
      dplyr::filter(domainTable == rowData$domainTable) |>
      dplyr::filter(domainField == rowData$domainConceptId)
    
    data1 <- NULL
    data1 <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = sql1,
      snakeCaseToCamelCase = TRUE,
      domain_table = rowData$domainTable,
      domain_concept_id = rowData$domainConceptId,
      cdm_database_schema = cdmDatabaseSchema,
      domain_start_date = rowData$domainStartDate,
      concept_id_universe = uploadedConceptTable,
      incidence = incidence
    ) |>
      dplyr::mutate(
        domainTableShort = longData$domainTableShort,
        domainFieldShort = longData$domainFieldShort
      )
    
    data2 <- NULL
    data2 <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = sql2,
      snakeCaseToCamelCase = TRUE,
      domain_table = rowData$domainTable,
      domain_concept_id = rowData$domainConceptId,
      cdm_database_schema = cdmDatabaseSchema,
      domain_start_date = rowData$domainStartDate,
      concept_id_universe = uploadedConceptTable,
      incidence = incidence
    ) |>
      dplyr::mutate(
        domainTableShort = longData$domainTableShort,
        domainFieldShort = longData$domainFieldShort
      )
    
    data3 <- NULL
    data3 <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = sql3,
      snakeCaseToCamelCase = TRUE,
      domain_table = rowData$domainTable,
      domain_concept_id = rowData$domainConceptId,
      cdm_database_schema = cdmDatabaseSchema,
      domain_start_date = rowData$domainStartDate,
      concept_id_universe = uploadedConceptTable,
      incidence = incidence
    ) |>
      dplyr::mutate(
        domainTableShort = longData$domainTableShort,
        domainFieldShort = longData$domainFieldShort
      )
    
    data <- dplyr::bind_rows(data,
                             data1,
                             data2,
                             data3) |>
      dplyr::tibble()
  }
  
  for (i in (1:nrow(domainsWide))) {
    rowData <- NULL
    rowData <- domainsWide[i,]
    
    writeLines(paste0(
      " - Working on ",
      rowData$domainTable,
      ".",
      rowData$domainSourceConceptId
    ))
    longData <- NULL
    longData <- domainsLong |>
      dplyr::filter(domainTable == rowData$domainTable) |>
      dplyr::filter(domainField == rowData$domainSourceConceptId)
    
    if (nchar(rowData$domainSourceConceptId) > 4) {
      data4 <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql4,
        snakeCaseToCamelCase = TRUE,
        domain_table = rowData$domainTable,
        domain_concept_id = rowData$domainSourceConceptId,
        cdm_database_schema = cdmDatabaseSchema,
        domain_start_date = rowData$domainStartDate,
        concept_id_universe = uploadedConceptTable,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        incidence = incidence
      ) |>
        dplyr::mutate(
          domainTableShort = longData$domainTableShort,
          domainFieldShort = longData$domainFieldShort
        )
      
      data5 <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql5,
        snakeCaseToCamelCase = TRUE,
        domain_table = rowData$domainTable,
        domain_concept_id = rowData$domainSourceConceptId,
        cdm_database_schema = cdmDatabaseSchema,
        domain_start_date = rowData$domainStartDate,
        concept_id_universe = uploadedConceptTable,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        incidence = incidence
      ) |>
        dplyr::mutate(
          domainTableShort = longData$domainTableShort,
          domainFieldShort = longData$domainFieldShort
        )
      
      data6 <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql6,
        snakeCaseToCamelCase = TRUE,
        domain_table = rowData$domainTable,
        domain_concept_id = rowData$domainSourceConceptId,
        cdm_database_schema = cdmDatabaseSchema,
        domain_start_date = rowData$domainStartDate,
        concept_id_universe = uploadedConceptTable,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        incidence = incidence
      ) |>
        dplyr::mutate(
          domainTableShort = longData$domainTableShort,
          domainFieldShort = longData$domainFieldShort
        )
      
      data <- dplyr::bind_rows(data,
                               data4,
                               data5,
                               data6) |>
        dplyr::tibble()
    }
  }
  
  dataAggregate <- data |>
    dplyr::group_by(conceptId,
                    conceptIsStandard,
                    eventYear,
                    eventMonth) |>
    dplyr::filter(eventYear == 0) |> 
    dplyr::filter(eventMonth == 0) |> 
    dplyr::summarise(conceptCount = sum(conceptCount),
                     subjectCount = max(subjectCount)) |>
    dplyr::ungroup() |>
    dplyr::mutate(domainTableShort = 'AL',
                  domainFieldShort = 'ALL')
  
  data <- dplyr::bind_rows(dataAggregate,
                           data)
  
  if (!is.null(minCellCount)) {
    data <- data |> dplyr::filter(subjectCount > minCellCount)
  }
  
  return(data)
}
