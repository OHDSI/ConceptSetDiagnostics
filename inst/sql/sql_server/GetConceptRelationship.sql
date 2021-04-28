SELECT CONCEPT_ID_1,
	CONCEPT_ID_2,
	RELATIONSHIP_ID,
	VALID_START_DATE,
	VALID_END_DATE,
	INVALID_REASON
FROM @vocabulary_database_schema.concept_relationship
WHERE CONCEPT_ID_1 IN (@concept_ids)
	OR CONCEPT_ID_2 IN (@concept_ids);
