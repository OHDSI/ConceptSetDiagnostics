WITH matched_concepts
AS (
	SELECT DISTINCT concept_id
	FROM @vocabulary_database_schema.concept
	WHERE CONCEPT_NAME_TSV @@ to_tsquery('@search_string_tsv')
		OR CONCEPT_CODE_TSV @@ to_tsquery('@search_string_tsv')
		OR CONCEPT_ID_TSV @@ to_tsquery('@search_string_tsv')
		OR CONCEPT_NAME_REVERSE_TSV @@ to_tsquery('@search_string_reverse_tsv')
	
	UNION
	
	SELECT DISTINCT concept_id
	FROM @vocabulary_database_schema.concept_synonym
	WHERE CONCEPT_SYNONYM_NAME_TSV @@ to_tsquery('@search_string_tsv')
		OR CONCEPT_SYNONYM_NAME_REVERSE_TSV @@ to_tsquery('@search_string_reverse_tsv')
	)
SELECT c.CONCEPT_ID,
	c.CONCEPT_NAME,
	c.VOCABULARY_ID,
	ISNULL(c.STANDARD_CONCEPT, 'N') STANDARD_CONCEPT,
	ISNULL(c.INVALID_REASON, 'V') INVALID_REASON,
	c.CONCEPT_CODE,
	c.CONCEPT_CLASS_ID,
	c.DOMAIN_ID 
{@concept_prevalence_table != '' } ? {,
	ISNULL(universe.RC, 0) RC,
	ISNULL(universe.DBC, 0) DBC,
	ISNULL(universe.DRC, 0) DRC,
	ISNULL(universe.DDBC, 0) DDBC }
FROM @vocabulary_database_schema.concept c
INNER JOIN matched_concepts
	ON c.concept_id = matched_concepts.concept_id {@concept_prevalence_table != '' } ? {
LEFT JOIN @concept_prevalence_table universe
	ON c.concept_id = universe.concept_id}
{@concept_prevalence_table != '' } ? {ORDER BY ISNULL(universe.DRC, 0) DESC};