SELECT c.CONCEPT_ID,
	c.CONCEPT_NAME,
	c.VOCABULARY_ID,
	c.STANDARD_CONCEPT,
	c.INVALID_REASON,
	c.CONCEPT_CODE,
	c.CONCEPT_CLASS_ID,
	c.DOMAIN_ID,
	ISNULL(universe.RC, 0) RC,
	ISNULL(universe.DBC, 0) DBC,
	ISNULL(universe.DRC, 0) DRC,
	ISNULL(universe.DDBC, 0) DDBC
FROM @vocabulary_database_schema.concept c
LEFT JOIN @concept_prevalence_schema.universe ON c.concept_id = universe.concept_id
WHERE c.CONCEPT_ID IN (@concept_ids)
ORDER BY ISNULL(universe.DRC, 0) DESC;
