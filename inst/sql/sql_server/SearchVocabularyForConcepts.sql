WITH matched_concepts
AS (
	SELECT DISTINCT concept_id
	FROM @vocabulary_database_schema.concept
	WHERE LOWER(CONCEPT_NAME) LIKE '%@search_string%'
	  OR LOWER(CONCEPT_CODE) LIKE '%@search_string%'
	  OR LOWER(CONCEPT_ID) LIKE '%@search_string%'
	
	UNION
	
	SELECT DISTINCT concept_id
	FROM @vocabulary_database_schema.concept_synonym
	WHERE LOWER(CONCEPT_SYNONYM_NAME) LIKE '%@search_string%'
	)
SELECT c.CONCEPT_ID,
	c.CONCEPT_NAME,
	c.VOCABULARY_ID,
	ISNULL(c.STANDARD_CONCEPT, 'N') STANDARD_CONCEPT,
	ISNULL(c.INVALID_REASON, 'V') INVALID_REASON,
	c.CONCEPT_CODE,
	c.CONCEPT_CLASS_ID,
	c.DOMAIN_ID,
	ISNULL(universe.RC, 0) RC,
	ISNULL(universe.DBC, 0) DBC,
	ISNULL(universe.DRC, 0) DRC,
	ISNULL(universe.DDBC, 0) DDBC
FROM @vocabulary_database_schema.concept c
INNER JOIN matched_concepts ON c.concept_id = matched_concepts.concept_id
LEFT JOIN concept_prevalence.universe ON c.concept_id = universe.concept_id
ORDER BY ISNULL(universe.DRC, 0) DESC;
