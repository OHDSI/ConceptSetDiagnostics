DROP TABLE IF EXISTS #starting_concepts;
DROP TABLE IF EXISTS #concept_synonyms;
DROP TABLE IF EXISTS #search_strings;
DROP TABLE IF EXISTS #search_str_top1000;
DROP TABLE IF EXISTS #search_string_subset;
DROP TABLE IF EXISTS @orphan_concept_table;


-- Find directly included concept and source concepts that map to those
SELECT concept_id,
	concept_name
INTO #starting_concepts
FROM (
	SELECT c1.concept_id,
		c1.concept_name
	FROM @vocabulary_database_schema.concept c1
	WHERE c1.concept_id IN (
			SELECT concept_id
			FROM @concept_id_table
			)
	
	UNION
	
	SELECT c1.concept_id,
		c1.concept_name
	FROM @vocabulary_database_schema.concept_ancestor ca1
	INNER JOIN @vocabulary_database_schema.concept_relationship cr1
		ON ca1.descendant_concept_id = cr1.concept_id_2
			AND cr1.relationship_id = 'Maps to'
			AND cr1.invalid_reasON IS NULL
	INNER JOIN @vocabulary_database_schema.concept c1
		ON cr1.concept_id_1 = c1.concept_id
	WHERE ca1.ancestor_concept_id IN (
			SELECT concept_id
			FROM @concept_id_table
			)
	) tmp;

-- Find synonyms
SELECT cs1.concept_id,
	cs1.concept_synonym_name AS concept_name
INTO #concept_synonyms
FROM #starting_concepts sc1
INNER JOIN @vocabulary_database_schema.concept_synonym cs1
	ON sc1.concept_id = cs1.concept_id
WHERE concept_synonym_name IS NOT NULL;

-- Create list of search strings from concept names and synonyms, discarding those short than 5 and longer than 50 characters
SELECT concept_name,
	concept_name_length,
	concept_name_terms
INTO #search_strings
FROM (
	SELECT LOWER(concept_name) AS concept_name,
		LEN(concept_name) AS concept_name_length,
		LEN(concept_name) - LEN(REPLACE(concept_name, ' ', '')) + 1 AS concept_name_terms
	FROM #starting_concepts
	WHERE len(concept_name) > 5
		AND len(concept_name) < 50
	
	UNION
	
	SELECT LOWER(concept_name) AS concept_name,
		LEN(concept_name) AS concept_name_length,
		LEN(concept_name) - LEN(REPLACE(concept_name, ' ', '')) + 1 AS concept_name_terms
	FROM #concept_synonyms
	WHERE len(concept_name) > 5
		AND len(concept_name) < 50
	) tmp;

-- Order search terms by length (words and characters), take top 1000
SELECT concept_name,
	concept_name_length,
	concept_name_terms
INTO #search_str_top1000
FROM (
	SELECT concept_name,
		concept_name_length,
		concept_name_terms,
		row_number() OVER (
			ORDER BY concept_name_terms ASC,
				concept_name_length ASC
			) AS rn1
	FROM #search_strings
	) t1
WHERE rn1 < 1000;

-- If search string is substring of another search string, discard longer string
SELECT ss1.*
INTO #search_string_subset
FROM #search_str_top1000 ss1
WHERE ss1.concept_name NOT IN (
		SELECT ss1.concept_name
		FROM #search_str_top1000 ss1
		INNER JOIN #search_str_top1000 ss2
			ON ss2.concept_name_length < ss1.concept_name_length
				AND ss1.concept_name LIKE CONCAT (
					'%',
					ss2.concept_name,
					'%'
					)
		);

-- Create recommended list: concepts containing search string but not mapping to start set
SELECT DISTINCT c1.concept_id,
                c1.concept_name,
                c1.domain_id,
                c1.vocabulary_id,
                c1.concept_class_id,
                c1.standard_concept,
                c1.concept_code,
                c1.valid_start_date,
                c1.valid_end_date,
                c1.invalid_reason
INTO @orphan_concept_table
FROM (
	SELECT c1.*
	FROM @vocabulary_database_schema.concept c1
	LEFT JOIN #starting_concepts sc1
		ON c1.concept_id = sc1.concept_id
	WHERE sc1.concept_id IS NULL
	) c1
INNER JOIN #search_string_subset ss1
	ON LOWER(c1.concept_name) LIKE CONCAT (
			'%',
			ss1.concept_name,
			'%'
			);

DROP TABLE IF EXISTS #starting_concepts;
DROP TABLE IF EXISTS #concept_synonyms;
DROP TABLE IF EXISTS #search_strings;
DROP TABLE IF EXISTS #search_str_top1000;
DROP TABLE IF EXISTS #search_string_subset;
