SELECT CONCEPT_ID,
	CONCEPT_SYNONYM_NAME,
	LANGUAGE_CONCEPT_ID
FROM @vocabulary_database_schema.concept_synonym
WHERE CONCEPT_ID IN (@concept_id_list);