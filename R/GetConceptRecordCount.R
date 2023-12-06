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
                                  )) {
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
                ) c
                ON u.concept_id = c.concept_id
          ;
          }
          --PERCENTILES are difficult and will need subqueries
          SELECT
              {@include_concept_id} ? {@domain_concept_id} : {0} concept_id,
          		{@is_source_field} ? {1} : {0} is_source_field,
              {@gender_concept_id} ? {p.gender_concept_id} : {0} gender_concept_id,
              {@use_date_year} ? {DATEPART(yy, @domain_start_date)} : {0} calendar_year,
              {@use_date_month} ? {DATEPART(mm, @domain_start_date)} : {0} calendar_month,
              {@use_date_quarter} ? {DATEPART(qq, @domain_start_date)} : {0} calendar_quarter,
              '@domain_table_short' domain_table_short,
              '@domain_field_short' domain_field_short,
              '@calendar_type' calendar_type,
          		COUNT_BIG(*) concept_count,
          		COUNT_BIG(DISTINCT dt.person_id) subject_count,
          		MIN(@domain_start_date) min_date,
          		MAX(@domain_start_date) max_date,
          		COUNT(DISTINCT @domain_start_date) unique_dates,
          		AVG(DATEDIFF(day, op.observation_period_start_date, @domain_start_date)) prior_obs_avg,
          		MIN(DATEDIFF(day, op.observation_period_start_date, @domain_start_date)) prior_obs_min,
          		MAX(DATEDIFF(day, op.observation_period_start_date, @domain_start_date)) prior_obs_max,
          		STDDEV(DATEDIFF(day, op.observation_period_start_date, @domain_start_date)) prior_obs_std,
          		SUM(DATEDIFF(day, op.observation_period_start_date, @domain_start_date)) prior_obs_sum,
          		AVG(DATEDIFF(day, @domain_start_date, op.observation_period_end_date)) post_obs_avg,
          		MIN(DATEDIFF(day, @domain_start_date, op.observation_period_end_date)) post_obs_min,
          		MAX(DATEDIFF(day, @domain_start_date, op.observation_period_end_date)) post_obs_max,
          		STDDEV(DATEDIFF(day, @domain_start_date, op.observation_period_end_date)) post_obs_std,
          		SUM(DATEDIFF(day, @domain_start_date, op.observation_period_end_date)) sum_obs_median,
          		AVG(DATEPART(yy, @domain_start_date) - year_of_birth) age_avg,
          		MIN(DATEPART(yy, @domain_start_date) - year_of_birth) age_min,
          		MAX(DATEPART(yy, @domain_start_date) - year_of_birth) age_max,
          		STDDEV(DATEPART(yy, @domain_start_date) - year_of_birth) age_std,
          		SUM(DATEPART(yy, @domain_start_date) - year_of_birth) age_sum
          INTO #concept_count_table
          FROM @cdm_database_schema.@domain_table dt
          INNER JOIN @cdm_database_schema.observation_period op
          ON dt.person_id = op.person_id
          	 AND @domain_start_date >= op.observation_period_start_date
          	 AND @domain_start_date <= op.observation_period_end_date
          INNER JOIN @cdm_database_schema.person p
          ON dt.person_id = p.person_id

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
          	    AND year_of_birth > 0
            {@use_group_by} ? {GROUP BY
                {@include_concept_id} ? {@domain_concept_id, }
                {@gender_concept_id} ? {p.gender_concept_id, }
                {@use_date_year} ? {DATEPART(yy, @domain_start_date),}
                {@use_date_month} ? {DATEPART(mm, @domain_start_date),}
                {@use_date_quarter} ? {DATEPART(qq, @domain_start_date),}};

            "
  
  iterations <- domainsLong |>
    tidyr::crossing(dplyr::tibble(includeConceptId = c("Y", "N", ""))) |>
    tidyr::crossing(dplyr::tibble(genderConceptId = c(0, 8507, 8532))) |>
    tidyr::crossing(
      dplyr::bind_rows(
        dplyr::tibble(
          useDateYear = c(""),
          useDateQuarter = c(""),
          useDateMonth = c(""),
          calendarType = c("N")
        ),
        dplyr::tibble(
          useDateYear = c("Y"),
          useDateQuarter = c(""),
          useDateMonth = c(""),
          calendarType = c("Y")
        ),
        dplyr::tibble(
          useDateYear = c("Y"),
          useDateQuarter = c("Y"),
          useDateMonth = c(""),
          calendarType = c("Q")
        ),
        dplyr::tibble(
          useDateYear = c("Y"),
          useDateQuarter = c("Y"),
          useDateMonth = c("Y"),
          calendarType = c("M")
        )
      )
    ) |>
    tidyr::crossing(dplyr::tibble(incidence = c("Y", "N")))
  
  existingOutput <- c()
  
  for (i in (1:nrow(iterations))) {
    rowData <- iterations[i, ]
    
    extraMessage <-
      paste0("Working on ",
             rowData$domainTable,
             ".",
             rowData$domainField,
             ".")
    progress <- (i / nrow(iterations)) * 100
    message <-
      sprintf("\rProgress: %d/%d (%0.2f%%)",
              i,
              nrow(iterations),
              progress)
    
    ParallelLogger::logInfo(message)
    
    showProgress(
      currentIteration = i,
      totalIterations = nrow(iterations),
      extraMessage = extraMessage
    )
    
    sqlRendered <- SqlRender::render(
      sql = sql,
      cdm_database_schema = cdmDatabaseSchema,
      vocabulary_database_schema = vocabularyDatabaseSchema,
      concept_id_universe = uploadedConceptTable,
      use_group_by = any(
        rowData$includeConceptId == 'Y',
        rowData$genderConceptId > 0,
        rowData$useDateYear == 'Y',
        rowData$useDateQuarter == 'Y',
        rowData$useDateMonth == 'Y'
      ),
      include_concept_id = (rowData$includeConceptId == 'Y'),
      domain_concept_id = rowData$domainField,
      domain_start_date = domainsWide |>
        dplyr::filter(domainTable == rowData$domainTable) |>
        dplyr::pull(domainStartDate),
      domain_table = rowData$domainTable,
      gender_concept_id = (rowData$genderConceptId > 0),
      incidence = (rowData$incidence == 'Y'),
      is_source_field = (rowData$isSourceField),
      use_date_year = (rowData$useDateYear == 'Y'),
      use_date_quarter = (rowData$useDateQuarter == 'Y'),
      use_date_month = (rowData$useDateMonth == 'Y'),
      domain_table_short = rowData$domainTableShort,
      domain_field_short = rowData$domainFieldShort,
      calendar_type = rowData$calendarType
    )
    
    # Regular expression to find a comma followed by any whitespace (including line breaks) and a semicolon
    regexPattern <- ",[\\s\\n\\r]*;"
    
    # Replace the pattern with just a semicolon
    sqlRendered <- gsub(regexPattern, ";", sqlRendered, perl = TRUE)
    
    sqlTranslated <-
      SqlRender::translate(
        sql = sqlRendered,
        targetDialect = connection@dbms,
        tempEmulationSchema = tempEmulationSchema
      )
    
    DatabaseConnector::executeSql(
      connection = connection,
      sql = sqlTranslated,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      profile = FALSE
    )
    
    output <- DatabaseConnector::querySql(
      connection = connection,
      sql = "SELECT * FROM #concept_count_table;",
      snakeCaseToCamelCase = TRUE
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
  
  existingOutput <- existingOutput |>
    dplyr::inner_join(
      domainInformation |>
        dplyr::select(
          domainTableShort,
          domainFieldShort,
          domainTable,
          domainField
        )
    ) |>
    dplyr::select(-domainFieldShort,-domainTableShort)
  
  if (!is.null(minCellCount)) {
    existingOutput <-
      existingOutput |> dplyr::filter(subjectCount > minCellCount)
  }
  
  return(existingOutput)
}
