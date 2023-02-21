DROP TABLE IF EXISTS @temp_table_name;
DROP TABLE IF EXISTS @temp_table_name_1;
DROP TABLE IF EXISTS @temp_table_name_2;
DROP TABLE IF EXISTS @temp_table_name_3;
DROP TABLE IF EXISTS @temp_table_name_4;
DROP TABLE IF EXISTS @temp_table_name_5;
DROP TABLE IF EXISTS @temp_table_name_6;
DROP TABLE IF EXISTS @temp_table_name_7;
DROP TABLE IF EXISTS @temp_table_name_8;
DROP TABLE IF EXISTS @temp_table_name_9;
DROP TABLE IF EXISTS @temp_table_name_10;

SELECT person_id,
	concept_id,
	condition_start_date concept_date
INTO @temp_table_name_1
FROM @cdm_database_schema.condition_occurrence t
INNER JOIN @concept_id_table c
	ON condition_concept_id = c.concept_id;

SELECT person_id,
	concept_id,
	condition_start_date concept_date
INTO @temp_table_name_2
FROM @cdm_database_schema.condition_occurrence t
INNER JOIN @concept_id_table c
	ON condition_source_concept_id = c.concept_id;

SELECT person_id,
	concept_id,
	drug_exposure_start_date concept_date
INTO @temp_table_name_3
FROM @cdm_database_schema.drug_exposure t
INNER JOIN @concept_id_table c
	ON drug_concept_id = c.concept_id;

SELECT person_id,
	concept_id,
	drug_exposure_start_date concept_date
INTO @temp_table_name_4
FROM @cdm_database_schema.drug_exposure t
INNER JOIN @concept_id_table c
	ON drug_source_concept_id = c.concept_id;

SELECT person_id,
	concept_id,
	procedure_date concept_date
INTO @temp_table_name_5
FROM @cdm_database_schema.procedure_occurrence t
INNER JOIN @concept_id_table c
	ON procedure_concept_id = c.concept_id;

SELECT person_id,
	concept_id,
	procedure_date concept_date
INTO @temp_table_name_6
FROM @cdm_database_schema.procedure_occurrence t
INNER JOIN @concept_id_table c
	ON procedure_source_concept_id = c.concept_id;

SELECT person_id,
	concept_id,
	measurement_date concept_date
INTO @temp_table_name_7
FROM @cdm_database_schema.measurement t
INNER JOIN @concept_id_table c
	ON measurement_concept_id = c.concept_id;

SELECT person_id,
	concept_id,
	measurement_date concept_date
INTO @temp_table_name_8
FROM @cdm_database_schema.measurement t
INNER JOIN @concept_id_table c
	ON measurement_source_concept_id = c.concept_id;

SELECT person_id,
	concept_id,
	observation_date concept_date
INTO @temp_table_name_9
FROM @cdm_database_schema.observation t
INNER JOIN @concept_id_table c
	ON observation_concept_id = c.concept_id;

SELECT person_id,
	concept_id,
	observation_date concept_date
INTO @temp_table_name_10
FROM @cdm_database_schema.observation t
INNER JOIN @concept_id_table c
	ON observation_source_concept_id = c.concept_id;

SELECT DISTINCT f.person_id,
	{@limit_to_person_date} ? {f.concept_id,} 
	f.concept_date,
	COUNT(*) record_count
INTO @temp_table_name
FROM (
	SELECT *
	FROM @temp_table_name_1
	
	UNION ALL
	
	SELECT *
	FROM @temp_table_name_2
	
	UNION ALL
	
	SELECT *
	FROM @temp_table_name_3
	
	UNION ALL
	
	SELECT *
	FROM @temp_table_name_4
	
	UNION ALL
	
	SELECT *
	FROM @temp_table_name_5
	
	UNION ALL
	
	SELECT *
	FROM @temp_table_name_6
	
	UNION ALL
	
	SELECT *
	FROM @temp_table_name_7
	
	UNION ALL
	
	SELECT *
	FROM @temp_table_name_8
	
	UNION ALL
	
	SELECT *
	FROM @temp_table_name_9
	
	UNION ALL
	
	SELECT *
	FROM @temp_table_name_10
	) f 
{@restrict_to_observation_period} ? {
INNER JOIN @cdm_database_schema.observation_period op
	ON f.person_id = op.person_id
		AND f.concept_date >= op.observation_period_start_date
		AND f.concept_date <= op.observation_period_end_date 
}
GROUP BY {@limit_to_person_date} ? {f.concept_id,} 
  f.concept_date,
	f.person_id;
	         
DROP TABLE IF EXISTS @concept_id_table;
DROP TABLE IF EXISTS @temp_table_name_1;
DROP TABLE IF EXISTS @temp_table_name_2;
DROP TABLE IF EXISTS @temp_table_name_3;
DROP TABLE IF EXISTS @temp_table_name_4;
DROP TABLE IF EXISTS @temp_table_name_5;
DROP TABLE IF EXISTS @temp_table_name_6;
DROP TABLE IF EXISTS @temp_table_name_7;
DROP TABLE IF EXISTS @temp_table_name_8;
DROP TABLE IF EXISTS @temp_table_name_9;
DROP TABLE IF EXISTS @temp_table_name_10;