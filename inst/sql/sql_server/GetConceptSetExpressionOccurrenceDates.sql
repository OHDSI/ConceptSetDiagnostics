DROP TABLE IF EXISTS @temp_table_name;


SELECT DISTINCT person_id,
	concept_id,
	MIN(concept_date) first_concept_date,
	MAX(concept_date) last_concept_id,
	COUNT(DISTINCT person_id) person_count,
	COUNT(DISTINCT CONCAT(CAST(person_id AS VARCHAR), "_", CAST(concept_date AS VARCHAR))) record_count
INTO @temp_table_name	
FROM (
	FROM @cdm_database_schema.condition_occurrence t
	INNER JOIN @concept_id_table c
		ON condition_concept_id = c.concept_id
	
	UNION
	
	SELECT DISTINCT person_id,
		concept_id,
		condition_start_date concept_date
	FROM @cdm_database_schema.condition_occurrence t
	INNER JOIN @concept_id_table c
		ON condition_concept_id = c.concept_id
	
	UNION
	
	SELECT DISTINCT person_id,
		concept_id,
		drug_exposure_start_date concept_date
	FROM @cdm_database_schema.drug_exposure t
	INNER JOIN @concept_id_table c
		ON drug_concept_id = c.concept_id
	
	UNION
	
	SELECT DISTINCT person_id,
		concept_id,
		drug_exposure_start_date concept_date
	FROM @cdm_database_schema.drug_exposure t
	INNER JOIN @concept_id_table c
		ON drug_source_concept_id = c.concept_id
	
	UNION
	
	SELECT DISTINCT person_id,
		concept_id,
		procedure_date concept_date
	FROM @cdm_database_schema.drug_exposure t
	INNER JOIN @concept_id_table c
		ON procedure_concept_id = c.concept_id
	
	UNION
	
	SELECT DISTINCT person_id,
		concept_id,
		procedure_date concept_date
	FROM @cdm_database_schema.drug_exposure t
	INNER JOIN @concept_id_table c
		ON procedure_source_concept_id = c.concept_id
	
	UNION
	
	SELECT DISTINCT person_id,
		concept_id,
		measurement_date concept_date
	FROM @cdm_database_schema.measurement t
	INNER JOIN @concept_id_table c
		ON measurement_concept_id = c.concept_id
	
	UNION
	
	SELECT DISTINCT person_id,
		concept_id,
		measurement_date concept_date
	FROM @cdm_database_schema.measurement t
	INNER JOIN @concept_id_table c
		ON measurement_source_concept_id = c.concept_id
	
	UNION
	
	SELECT DISTINCT person_id,
		concept_id,
		observation_date concept_date
	FROM @cdm_database_schema.measurement t
	INNER JOIN @concept_id_table c
		ON observation_concept_id = c.concept_id
	
	UNION
	
	SELECT DISTINCT person_id,
		concept_id,
		observation_date concept_date
	FROM @cdm_database_schema.measurement t
	INNER JOIN @concept_id_table c
		ON observation_source_concept_id = c.concept_id
	) f
  {@restrict_to_observation_period} ? {
    INNER JOIN @cdm_database_schema.observation_period op
    ON f.person_id = op.person_id
    AND f.concept_date >= op.observation_period_start_date
    AND f.concept_date <= op.observation_period_end_date
  }
	GROUP BY person_id,
	          concept_id
	{@first_occurrence_only} : {
	HAVING MIN(first_start_date) = concept_date
	}
	{@last_occurrence_only} : {
	HAVING MIN(max_start_date) = concept_date
	}
	ORDER BY person_id,
	         concept_id;
	         
DROP TABLE IF EXISTS @concept_id_table;