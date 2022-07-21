WITH list
AS (
	SELECT concept_id,
		concept_name,
		domain_id,
		vocabulary_id,
		standard_concept
	FROM @vocabulary_database_schema.concept
	WHERE concept_id IN (
			SELECT DISTINCT concept_id
			FROM @concept_id_temp_table
			)
	)
SELECT DISTINCT c2.concept_id,
	c2.concept_name,
	c2.domain_id,
	c2.vocabulary_id,
	c2.standard_concept,
	cp2.rc AS record_count,
	cp2.dbc AS database_count,
	cp2.drc AS descendant_record_count,
	cp2.ddbc AS descendant_database_count,
	c.concept_id AS source_concept_id,
	c.concept_name AS source_concept_name,
	c.vocabulary_id AS source_vocabulary_id,
	c.concept_code AS source_concept_code,
	cp.rc AS source_record_count,
	cp.dbc AS source_database_count
FROM list l
JOIN @concept_prevalence_schema.recommender_set r ON l.concept_id = r.source_id
JOIN @vocabulary_database_schema.concept c ON c.concept_id = r.concept_id
	AND c.standard_concept IS NULL
JOIN @concept_prevalence_schema.cp_master cp ON cp.concept_id = c.concept_id
JOIN @vocabulary_database_schema.concept_relationship cr ON cr.concept_id_1 = c.concept_id
	AND cr.relationship_id IN (
		'Maps to',
		'Maps to value'
		)
JOIN @vocabulary_database_schema.concept c2 ON c2.concept_id = cr.concept_id_2
	AND c2.standard_concept = 'S'
JOIN @concept_prevalence_schema.cp_master cp2 ON cp2.concept_id = c2.concept_id
LEFT JOIN @concept_prevalence_schema.recommended_blacklist rb ON c2.concept_id = rb.concept_id
WHERE rb.concept_id IS NULL
	AND NOT EXISTS (
		SELECT 1
		FROM list l2
		JOIN @vocabulary_database_schema.concept_relationship cr1 ON l2.concept_id = cr1.concept_id_2
			AND cr1.relationship_id = 'Maps to'
		WHERE cr1.concept_id_1 = r.concept_id
		)
ORDER BY cp.rc DESC,
	cp.dbc DESC;
