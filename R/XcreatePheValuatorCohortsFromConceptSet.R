


#' @export
#'
createPheValuatorCohortsFromConceptSet <- function(connection,
                                                   cdmDatabaseSchema,
                                                   vocabularyDatabaseSchema = cdmDatabaseSchema,
                                                   conceptSetExpression,
                                                   tempEmulationSchema = NULL) {
  conceptIds <- ConceptSetDiagnostics::resolveConceptSetExpression(connection = connection,
                                                                   conceptSetExpression = conceptSetExpress)
  
  tempTableName <-
    paste0("#t", (as.numeric(as.POSIXlt(Sys.time(
    )))) * 100000)
  
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
      progressBar = TRUE,
      createTable = TRUE
    ),
    file = nullfile()
  ))
  
  ParallelLogger::logInfo("Creating event cohorts for concept set expression -1")
  ConceptSetDiagnostics::instantiateCohortFromConceptSetExpression(
    connection = connection,
    cdmDatabaseSchema = cdmDatabaseSchema,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    cohortDatabaseSchema = targetCohortDatabaseSchema,
    cohortId = -1,
    cohortTable = targetCohortTableName,
    restrictToObservationPeriod = TRUE,
    conceptSetExpression = RJSONIO::fromJSON(content = conceptSetExpression,
                                             digits = 23)
  )
  
  ParallelLogger::logInfo("Creating prevalence cohort 2")
  ### Prevalence cohort is defined as first occurrence of eventCohort
  ## prevalenceCohort (2) "First occurrence of conceptSetExpression till end of continuous observation"
  sql <- "DELETE FROM @cohort_database_schema.@cohort_table
          WHERE cohort_definition_id IN (2);

          INSERT INTO @cohort_database_schema.@cohort_table (cohort_definition_id, subject_id,
                                                             cohort_start_date, cohort_end_date)
          SELECT 2 cohort_definition_id,
                  c.subject_id,
                  min(c.cohort_start_date) cohort_start_date,
                  max(op.observation_period_end_date) cohort_end_date
          FROM @cohort_database_schema.@cohort_table c
          INNER JOIN @cdm_database_schema.observation_period op
          ON c.subject_id = op.person_id
          AND c.cohort_start_date >= op.observation_period_start_date
          AND c.cohort_end_date <= op.observation_period_end_date
          WHERE cohort_definition_id IN (-1)
          GROUP BY subject_id;"
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sql,
    cohort_database_schema = targetCohortDatabaseSchema,
    cdm_database_schema = cdmDatabaseSchema,
    cohort_table = targetCohortTableName,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  
  ParallelLogger::logInfo("Creating xSens 3")
  ### xSens cohort is an event cohort of visit dates that started all days after eventCohort
  sql <- "  DELETE FROM @cohort_database_schema.@cohort_table
            WHERE cohort_definition_id = -3;

            INSERT INTO @cohort_database_schema.@cohort_table (
            	cohort_definition_id,
            	subject_id,
            	cohort_start_date,
            	cohort_end_date
            	)

            SELECT -3 cohort_definition_id,
            	subject_id,
            	min(cohort_start_date) cohort_start_date,
            	cohort_end_date
            FROM (
            	SELECT v.person_id subject_id,
            		v.visit_start_date cohort_start_date,
            		max(CASE
            				WHEN DATEADD(d, 365, v.visit_start_date) > op.observation_period_end_date
            					THEN op.observation_period_end_date
            				ELSE DATEADD(d, 365, v.visit_start_date)
            				END) cohort_end_date
            	FROM @cdm_database_schema.visit_occurrence v
            	INNER JOIN (
            		SELECT subject_id,
            			min(cohort_start_date) cohort_start_date
            		FROM @cohort_database_schema.@cohort_table
            		WHERE cohort_definition_id IN (- 1)
            		GROUP BY subject_id
            		) p
            		ON v.person_id = p.subject_id
            			AND v.visit_start_date > p.cohort_start_date
            	INNER JOIN @cdm_database_schema.observation_period op
            		ON v.person_id = op.person_id
            			AND v.visit_start_date >= op.observation_period_start_date
            			AND v.visit_start_date <= op.observation_period_end_date
            	GROUP BY v.person_id,
            		        v.visit_start_date
            	) f
              GROUP BY subject_id,
                      cohort_end_date;"
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sql,
    cdm_database_schema = cdmDatabaseSchema,
    cohort_database_schema = targetCohortDatabaseSchema,
    cohort_table = targetCohortTableName,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  CohortAlgebra::unionCohorts(
    connection = connection,
    sourceCohortDatabaseSchema = targetCohortDatabaseSchema,
    sourceCohortTable = targetCohortTableName,
    targetCohortDatabaseSchema = targetCohortDatabaseSchema,
    targetCohortTable = targetCohortTableName,
    oldToNewCohortId = dplyr::tibble(oldCohortId = -3,
                                     newCohortId = 3),
    tempEmulationSchema = tempEmulationSchema,
    purgeConflicts = TRUE
  )
  
  ParallelLogger::logInfo("Creating base population 4")
  # xBasePopulation (4)
  ## LogicDescription
  ### evaluationPopulation or Base Population is an event cohort indexed on any visit in a subject who
  ### never belonged to the xSens cohort, OR indexed on visit that starts after
  ### the first occurrence of event cohort. All events will have atleast 365days of observation days after and
  ### cohort_end_date is cohort_start_date + 365.
  sql <- "  DELETE FROM @cohort_database_schema.@cohort_table
            WHERE cohort_definition_id = -4;

            INSERT INTO @cohort_database_schema.@cohort_table (cohort_definition_id,
                                                                subject_id,
                                                                cohort_start_date,
                                                                cohort_end_date)
            SELECT DISTINCT -4 cohort_definition_id,
                    subject_id,
                    min(cohort_start_date) cohort_start_date,
                    cohort_end_date
            FROM
            (
              SELECT v.person_id subject_id,
                      visit_start_date cohort_start_date,
                  		max(CASE
                  				WHEN DATEADD(d, 365, v.visit_start_date) > op.observation_period_end_date
                  					THEN op.observation_period_end_date
                  				ELSE DATEADD(d, 365, v.visit_start_date)
                  				END) cohort_end_date
              FROM
                @cdm_database_schema.visit_occurrence v
              LEFT JOIN
                (
                  SELECT DISTINCT subject_id
                  FROM @cohort_database_schema.@cohort_table
                  WHERE cohort_definition_id = -1
                ) rm
              ON v.person_id = rm.subject_id
              INNER JOIN @cdm_database_schema.observation_period op
              ON v.person_id = op.person_id
                AND v.visit_start_date >= op.observation_period_start_date
                AND v.visit_end_date <= op.observation_period_end_date
          			AND DATEADD(d, 365, v.visit_start_date) <= op.observation_period_end_date
              WHERE rm.subject_id IS NULL
              GROUP BY v.person_id,
                      v.visit_start_date

              UNION ALL

              SELECT v.person_id subject_id,
                    v.visit_start_date cohort_start_date,
                		max(CASE
                				WHEN DATEADD(d, 365, v.visit_start_date) > op.observation_period_end_date
                					THEN op.observation_period_end_date
                				ELSE DATEADD(d, 365, v.visit_start_date)
                				END) cohort_end_date
              FROM
                  @cdm_database_schema.visit_occurrence v
              INNER JOIN
                  (
                    SELECT
                            subject_id,
                            min(cohort_start_date) cohort_start_date
                    FROM @cohort_database_schema.@cohort_table
                    WHERE cohort_definition_id IN (-1)
                    GROUP BY subject_id
                  ) p
              ON v.person_id = p.subject_id
                AND v.visit_start_date = p.cohort_start_date
              INNER JOIN @cdm_database_schema.observation_period op
              ON v.person_id = op.person_id
              AND v.visit_start_date >= op.observation_period_start_date
              AND v.visit_end_date <= op.observation_period_end_date
              GROUP BY v.person_id,
                        v.visit_start_date
            ) f
            GROUP BY subject_id,
                      cohort_end_date;"
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sql,
    cdm_database_schema = cdmDatabaseSchema,
    cohort_database_schema = targetCohortDatabaseSchema,
    cohort_table = targetCohortTableName,
    # condition on the same day as visit with 365 days post observation days
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  CohortAlgebra::unionCohorts(
    connection = connection,
    sourceCohortDatabaseSchema = targetCohortDatabaseSchema,
    sourceCohortTable = targetCohortTableName,
    targetCohortDatabaseSchema = targetCohortDatabaseSchema,
    targetCohortTable = targetCohortTableName,
    oldToNewCohortId = dplyr::tibble(oldCohortId = -4,
                                     newCohortId = 4),
    tempEmulationSchema = tempEmulationSchema,
    purgeConflicts = TRUE
  )
  
  ParallelLogger::logInfo("Creating xSpec 5 - 10")
  # xSpec1 (> =5 <= 10 )
  # "First visit after the first event + satisfies additional criteria such as two (or more) events of eventCohort
  # in a window of time prior (generally 21 to 1)"
  
  sql <- "DELETE
          FROM @cohort_database_schema.@cohort_table
          WHERE cohort_definition_id = -@xSpec_id;

          INSERT INTO @cohort_database_schema.@cohort_table (
          	cohort_definition_id,
          	subject_id,
          	cohort_start_date,
          	cohort_end_date
          	)
          SELECT -@xSpec_id cohort_definition_id,
          	subject_id,
          	min(cohort_start_date) cohort_start_date,
          	cohort_end_date
          FROM (
          	SELECT v.person_id subject_id,
          		v.visit_start_date cohort_start_date,
          		max(CASE
          				WHEN DATEADD(d, 365, v.visit_end_date) > op.observation_period_end_date
          					THEN op.observation_period_end_date
          				ELSE DATEADD(d, 365, v.visit_end_date)
          				END) cohort_end_date
          	FROM @cdm_database_schema.visit_occurrence v
          	INNER JOIN @cdm_database_schema.observation_period op
          		ON v.person_id = op.person_id
          			AND v.visit_start_date >= op.observation_period_start_date
          			AND v.visit_start_date <= op.observation_period_end_date
          			AND DATEADD(d, 365, v.visit_start_date) <= op.observation_period_end_date
          	INNER JOIN (
          		SELECT subject_id,
          			min(cohort_start_date) cohort_start_date
          		FROM @cohort_database_schema.@cohort_table
          		WHERE cohort_definition_id IN (- 1)
          		GROUP BY subject_id
          		) p
          		ON v.person_id = p.subject_id
          			AND DATEADD(DAY, @offset, v.visit_start_date) <= p.cohort_start_date
          			AND v.visit_start_date > p.cohort_start_date
          	INNER JOIN @cohort_database_schema.@cohort_table e
          		ON v.person_id = e.subject_id
          			AND DATEADD(DAY, @offset, v.visit_start_date) <= e.cohort_start_date
          			AND v.visit_start_date > e.cohort_start_date
          	WHERE e.cohort_definition_id = - 1
          	GROUP BY v.person_id,
          	          v.visit_start_date
          	HAVING count(DISTINCT e.cohort_start_date) >= @min_days_overlap
          	) f
          GROUP BY subject_id,
          	cohort_end_date;"
  
  xSpecIds <- c(5:10) %>% sort()
  for (j in (1:length(xSpecIds))) {
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      cdm_database_schema = cdmDatabaseSchema,
      cohort_database_schema = targetCohortDatabaseSchema,
      cohort_table = targetCohortTableName,
      offset = offsetCohortStartDate,
      xSpec_id = xSpecIds[[j]],
      min_days_overlap = j + 1,
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
    CohortAlgebra::unionCohorts(
      connection = connection,
      sourceCohortDatabaseSchema = targetCohortDatabaseSchema,
      sourceCohortTable = targetCohortTableName,
      targetCohortDatabaseSchema = targetCohortDatabaseSchema,
      targetCohortTable = targetCohortTableName,
      oldToNewCohortId = dplyr::tibble(oldCohortId = xSpecIds[[j]] * -1,
                                       newCohortId = xSpecIds[[j]]),
      tempEmulationSchema = tempEmulationSchema,
      purgeConflicts = TRUE
    )
  }
  
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = " DELETE FROM @target_cohort_database_schema.@target_cohort_table
            WHERE cohort_definition_id < 0;
            UPDATE STATISTICS @target_cohort_database_schema.@target_cohort_table;",
    target_cohort_database_schema = targetCohortDatabaseSchema,
    target_cohort_table = targetCohortTableName,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  DatabaseConnector::disconnect(connection = connection)
}