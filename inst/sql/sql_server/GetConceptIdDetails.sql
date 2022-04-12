SELECT c.CONCEPT_ID,
	c.CONCEPT_NAME,
	c.VOCABULARY_ID,
	c.STANDARD_CONCEPT,
	c.INVALID_REASON,
	c.CONCEPT_CODE,
	c.CONCEPT_CLASS_ID,
	c.DOMAIN_ID
FROM @vocabulary_database_schema.concept c;
