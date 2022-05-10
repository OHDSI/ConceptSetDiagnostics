WITH matched_concepts
AS (
	SELECT DISTINCT concept_id
	FROM @vocabulary_database_schema.concept
	WHERE LOWER(CONCEPT_NAME) LIKE '%@search_string%'
	  OR LOWER(CONCEPT_CODE) LIKE '%@search_string%'
	
	UNION
	
	SELECT DISTINCT concept_id
	FROM @vocabulary_database_schema.concept_synonym
	WHERE LOWER(CONCEPT_SYNONYM_NAME) LIKE '%@search_string%'
	)
SELECT c.CONCEPT_ID,
	c.CONCEPT_NAME,
	c.VOCABULARY_ID,
	c.STANDARD_CONCEPT,
	c.INVALID_REASON,
	c.CONCEPT_CODE,
	c.CONCEPT_CLASS_ID,
	c.DOMAIN_ID
FROM @vocabulary_database_schema.concept c
INNER JOIN matched_concepts ON c.concept_id = matched_concepts.concept_id;
