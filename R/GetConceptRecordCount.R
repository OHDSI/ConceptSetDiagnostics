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
    uploadedConceptTable <-
      loadTempConceptTable(conceptIds = conceptIds,
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
  sql <- "

          DROP TABLE IF EXISTS #concept_count_table;

          {@concept_id_universe != ''} ? {
            DROP TABLE IF EXISTS #concept_id_unv_2;
            CREATE TABLE #concept_id_unv_2 as
              SELECT DISTINCT u.concept_id
              FROM @concept_id_universe u
              INNER JOIN (
                SELECT concept_id
                FROM @vocabulary_database_schema.CONCEPT
                WHERE concept_id > 0
                {@is_standard == 'Y'} ? {} : {
                  AND (standard_concept != 'S' OR standard_concept IS NULL)
                }
                ) c
                ON u.concept_id = c.concept_id
          ;
          }

          SELECT {@use_group_by} ? {@group_by_select}
          		{@is_standard == 'Y'} ? {'Y'} : {'N'} concept_is_standard,
          		COUNT_BIG(*) concept_count,
          		COUNT_BIG(DISTINCT dt.person_id) subject_count,
          		MIN(@domain_start_date) min_date,
          		MAX(@domain_start_date) max_date,
          		COUNT(DISTINCT @domain_start_date) unique_dates
          INTO #concept_count_table
          	FROM @cdm_database_schema.@domain_table dt

            {@incidence} ? {
            -- limit to first occurrence of concept id by person_id
            INNER JOIN
              (
                SELECT person_id,
                        @domain_concept_id concept_id,
                        min(@domain_start_date) start_date
                FROM @cdm_database_schema.@domain_table in1
                {@concept_id_universe != ''} ? {
                    INNER JOIN #concept_id_unv_2 ci
                    ON @domain_concept_id = ci.concept_id
                }
                GROUP By person_id, @domain_concept_id
              ) in0
              ON dt.person_id = in0.person_id
              AND dt.@domain_concept_id = in0.concept_id
              AND dt.@domain_start_date = in0.start_date
            }

            {@concept_id_universe != ''} ? {
            INNER JOIN #concept_id_unv_2 c
            ON @domain_concept_id = c.concept_id
            }
          	WHERE  DATEPART(yy, @domain_start_date) > 0
            {@use_group_by} ? {@group_by};

            DROP TABLE IF EXISTS #concept_id_unv_2;"
  
  
  
  
  #by conceptId
  #by calendar year
  #by calendar year, calendar quarter
  #by calendar year, calendar quarter, calendar month
  #no conceptId
  #by calendar year
  #by calendar year, calendar quarter
  #by calendar year, calendar quarter, calendar month
  
  iterations <- domainsWide |>
    tidyr::crossing(dplyr::tibble(
      calendarGroup = c(
        "{@keep_concept_id == 'Y'} ? {GROUP BY @domain_concept_id}",
        "GROUP BY {@keep_concept_id == 'Y'} ? {@domain_concept_id,} DATEPART(yy, @domain_start_date)",
        "GROUP BY {@keep_concept_id == 'Y'} ? {@domain_concept_id,} DATEPART(yy, @domain_start_date), DATEPART(qq, @domain_start_date)",
        "GROUP BY {@keep_concept_id == 'Y'} ? {@domain_concept_id,} DATEPART(yy, @domain_start_date), DATEPART(qq, @domain_start_date), DATEPART(mm, @domain_start_date)"
      ),
      calendarGroupSelect = c(
        "{@keep_concept_id == 'Y'} ? {@domain_concept_id concept_id, }",
        "{@keep_concept_id == 'Y'} ? {@domain_concept_id concept_id, } DATEPART(yy, @domain_start_date) calendar_year,",
        "{@keep_concept_id == 'Y'} ? {@domain_concept_id concept_id, } DATEPART(yy, @domain_start_date) calendar_year, DATEPART(qq, @domain_start_date) calendar_quarter,",
        "{@keep_concept_id == 'Y'} ? {@domain_concept_id concept_id,} DATEPART(yy, @domain_start_date) calendar_year, DATEPART(qq, @domain_start_date) calendar_quarter, DATEPART(mm, @domain_start_date) calendar_month,"
      ),
      calendarType = c("N",
                       "Y",
                       "Q",
                       "M")
    )) |>
    tidyr::crossing(dplyr::tibble(isStandard = c('Y', 'N')))
  
  if (length(conceptIds) > 1) {
    iterations <- iterations |>
      tidyr::crossing(dplyr::tibble(keepConceptId = c('Y', 'N')))
  } else {
    iterations <- iterations |>
      tidyr::crossing(dplyr::tibble(keepConceptId = c('Y')))
  }
  
  existingOutput <- c()
  
  for (i in (1:nrow(iterations))) {
    rowData <- iterations[i,]
    
    extraMessage <-
      paste0("Working on ",
             rowData$domainTable,
             ".",
             rowData$domainConceptId,
             ".")
    showProgress(
      currentIteration = i,
      totalIterations = nrow(iterations),
      extraMessage = extraMessage
    )
    
    if (any(
      rowData$keepConceptId == 'Y',
      stringr::str_detect(string = rowData$calendarGroup,
                          pattern = "domain_start_date")
    )) {
      sqlCalendarGroup <- rowData$calendarGroup
      sqlCalendarGroupSelect <- rowData$calendarGroupSelect
      
      
      if (rowData$keepConceptId == 'Y') {
        sqlCalendarGroup <-
          SqlRender::render(
            sql = sqlCalendarGroup,
            keep_concept_id = rowData$keepConceptId,
            domain_concept_id = rowData$domainConceptId
          )
        sqlCalendarGroupSelect <-
          SqlRender::render(
            sql = sqlCalendarGroupSelect,
            keep_concept_id = rowData$keepConceptId,
            domain_concept_id = rowData$domainConceptId
          )
      }
      
      if (stringr::str_detect(string = rowData$calendarGroup,
                              pattern = "domain_start_date")) {
        sqlCalendarGroup <-
          SqlRender::render(sql = sqlCalendarGroup,
                            domain_start_date = rowData$domainStartDate)
        sqlCalendarGroupSelect <-
          SqlRender::render(sql = sqlCalendarGroupSelect,
                            domain_start_date = rowData$domainStartDate)
      }
    } else {
      sqlCalendarGroup <- ""
      sqlCalendarGroupSelect <- ""
    }
    
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      domain_table = rowData$domainTable,
      domain_concept_id = rowData$domainConceptId,
      cdm_database_schema = cdmDatabaseSchema,
      vocabulary_database_schema = vocabularyDatabaseSchema,
      domain_start_date = rowData$domainStartDate,
      concept_id_universe = uploadedConceptTable,
      incidence = incidence,
      is_standard = rowData$isStandard,
      use_group_by = nchar(stringr::str_trim(sqlCalendarGroup)) > 2,
      group_by = sqlCalendarGroup,
      group_by_select = sqlCalendarGroupSelect,
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
    
    longData <- domainsLong |>
      dplyr::filter(domainTable == rowData$domainTable) |>
      dplyr::filter(domainField == rowData$domainConceptId)
    
    output <- DatabaseConnector::querySql(
      connection = connection,
      sql = "SELECT * FROM #concept_count_table;",
      snakeCaseToCamelCase = TRUE
    ) |>
      dplyr::mutate(
        domainTableShort = longData$domainTableShort,
        domainFieldShort = longData$domainFieldShort,
        calendarType = rowData$calendarType,
        isStandard = rowData$isStandard
      )
    
    existingOutput <- dplyr::bind_rows(existingOutput,
                                       output) |>
      dplyr::tibble()
  }
  
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE,
    sql = "DROP TABLE IF EXISTS #concept_count_table;
           DROP TABLE IF EXISTS #concept_id_unv_2;"
  )
  
  existingOutput <- tidyr::replace_na(
    data = existingOutput,
    replace = list(
      calendarYear = 0,
      calendarQuarter = 0,
      calendarMonth = 0,
      conceptId = 0
    )
  )
  
  dataAggregate <- existingOutput |>
    dplyr::group_by(conceptId,
                    isStandard,
                    calendarYear,
                    calendarQuarter,
                    calendarMonth) |>
    dplyr::filter(calendarYear == 0) |>
    dplyr::filter(calendarQuarter == 0) |>
    dplyr::summarise(conceptCount = sum(conceptCount),
                     subjectCount = max(subjectCount)) |>
    dplyr::ungroup() |>
    dplyr::mutate(domainTableShort = 'AL',
                  domainFieldShort = 'ALL')
  
  existingOutput <- dplyr::bind_rows(dataAggregate,
                                     existingOutput)
  
  if (!is.null(minCellCount)) {
    existingOutput <-
      existingOutput |> dplyr::filter(subjectCount > minCellCount)
  }
  
  return(existingOutput)
}
