DROP TABLE IF EXISTS #temp_cohort;
	CREATE TABLE #temp_cohort (
		subject_id BIGINT,
		cohort_start_date DATE
		);

INSERT INTO #temp_cohort (
	subject_id,
	cohort_start_date
	)
SELECT DISTINCT person_id subject_id,
	condition_start_date cohort_start_date
FROM @cdm_database_schema.condition_occurrence t
INNER JOIN @concept_id_table c
	ON condition_concept_id = c.concept_id;

INSERT INTO #temp_cohort (
	subject_id,
	cohort_start_date
	)
SELECT DISTINCT person_id subject_id,
	condition_start_date cohort_start_date
FROM @cdm_database_schema.condition_occurrence t
INNER JOIN @concept_id_table c
	ON condition_source_concept_id = c.concept_id;

INSERT INTO #temp_cohort (
	subject_id,
	cohort_start_date
	)
SELECT DISTINCT person_id subject_id,
	drug_exposure_start_date cohort_start_date
FROM @cdm_database_schema.drug_exposure t
INNER JOIN @concept_id_table c
	ON drug_concept_id = c.concept_id;

INSERT INTO #temp_cohort (
	subject_id,
	cohort_start_date
	)
SELECT DISTINCT person_id subject_id,
	drug_exposure_start_date cohort_start_date
FROM @cdm_database_schema.drug_exposure t
INNER JOIN @concept_id_table c
	ON drug_source_concept_id = c.concept_id;

INSERT INTO #temp_cohort (
	subject_id,
	cohort_start_date
	)
SELECT DISTINCT person_id subject_id,
	procedure_date cohort_start_date
FROM @cdm_database_schema.procedure_occurrence t
INNER JOIN @concept_id_table c
	ON procedure_concept_id = c.concept_id;

INSERT INTO #temp_cohort (
	subject_id,
	cohort_start_date
	)
SELECT DISTINCT person_id subject_id,
	procedure_date cohort_start_date
FROM @cdm_database_schema.procedure_occurrence t
INNER JOIN @concept_id_table c
	ON procedure_source_concept_id = c.concept_id;

INSERT INTO #temp_cohort (
	subject_id,
	cohort_start_date
	)
SELECT DISTINCT person_id subject_id,
	measurement_date cohort_start_date
FROM @cdm_database_schema.measurement t
INNER JOIN @concept_id_table c
	ON measurement_concept_id = c.concept_id;

INSERT INTO #temp_cohort (
	subject_id,
	cohort_start_date
	)
SELECT DISTINCT person_id subject_id,
	measurement_date cohort_start_date
FROM @cdm_database_schema.measurement t
INNER JOIN @concept_id_table c
	ON measurement_source_concept_id = c.concept_id;

INSERT INTO #temp_cohort (
	subject_id,
	cohort_start_date
	)
SELECT DISTINCT person_id subject_id,
	observation_date cohort_start_date
FROM @cdm_database_schema.observation t
INNER JOIN @concept_id_table c
	ON observation_concept_id = c.concept_id;

INSERT INTO #temp_cohort (
	subject_id,
	cohort_start_date
	)
SELECT DISTINCT person_id subject_id,
	observation_date cohort_start_date
FROM @cdm_database_schema.observation t
INNER JOIN @concept_id_table c
	ON observation_source_concept_id = c.concept_id;
	
DELETE FROM @cohort_database_schema.@cohort_table
WHERE cohort_definition_id = @cohort_id;

INSERT INTO @cohort_database_schema.@cohort_table (
	cohort_definition_id,
	subject_id,
	cohort_start_date,
	cohort_end_date
	)
SELECT DISTINCT @cohort_id cohort_definition_id,
	t.subject_id,
	t.cohort_start_date,
	t.cohort_start_date cohort_end_date
FROM #temp_cohort t
{@restrict_to_observation_period} ? {
  INNER JOIN @cdm_database_schema.observation_period op
  ON t.subject_id = op.person_id
  AND t.cohort_start_date >= op.observation_period_start_date
  AND t.cohort_start_date <= op.observation_period_end_date
} 
ORDER BY subject_id,
	cohort_start_date;
DROP TABLE IF EXISTS #temp_cohort;

UPDATE STATISTICS  @cohort_database_schema.@cohort_table;



