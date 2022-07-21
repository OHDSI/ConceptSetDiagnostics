DROP TABLE IF EXISTS #starting_concepts;
DROP TABLE IF EXISTS #concept_synonyms;
DROP TABLE IF EXISTS #search_strings;
DROP TABLE IF EXISTS #search_str_top1000;
DROP TABLE IF EXISTS #search_sub_str;
DROP TABLE IF EXISTS #search_string_subset;
DROP TABLE IF EXISTS #orphan_concept_table;

SELECT DISTINCT concept_name
INTO #starting_concepts
FROM (
	SELECT DISTINCT c1.concept_name
	FROM @vocabulary_database_schema.concept c1
	WHERE c1.concept_id IN (
			SELECT concept_id
			FROM @concept_id_table
			)
	
	UNION
	
	SELECT DISTINCT c1.concept_name
	FROM @vocabulary_database_schema.concept_ancestor ca1
	INNER JOIN @vocabulary_database_schema.concept c1
		ON ca1.descendant_concept_id = c1.concept_id
	WHERE ca1.ancestor_concept_id IN (
			SELECT concept_id
			FROM @concept_id_table
			)
	
	UNION
	
	SELECT DISTINCT c1.concept_name
	FROM @vocabulary_database_schema.concept_ancestor ca1
	INNER JOIN @vocabulary_database_schema.concept_relationship cr1
		ON ca1.descendant_concept_id = cr1.concept_id_2
			AND cr1.relationship_id = 'Maps to'
			AND cr1.invalid_reasON IS NULL
	INNER JOIN @vocabulary_database_schema.concept c1
		ON ca1.descendant_concept_id = c1.concept_id
	WHERE ca1.ancestor_concept_id IN (
			SELECT concept_id
			FROM @concept_id_table
			)
	
	UNION
	
	SELECT cs1.concept_synonym_name AS concept_name
	FROM @vocabulary_database_schema.concept_ancestor ca1
	INNER JOIN @vocabulary_database_schema.concept_synonym cs1
		ON ca1.descendant_concept_id = cs1.concept_id
	WHERE ca1.ancestor_concept_id IN (
			SELECT concept_id
			FROM @concept_id_table
			)
	) tmp
ORDER BY concept_name;

-- Create list of search strings from concept names and synonyms, discarding those short than 5 and longer than 50 characters
SELECT DISTINCT concept_name,
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
	) tmp
ORDER BY concept_name;

-- Order search terms by length (words and characters), take top 1000
--HINT DISTRIBUTE_ON_KEY(concept_name)
SELECT DISTINCT concept_name,
	concept_name_length,
	concept_name_terms
INTO #search_str_top1000
FROM (
	SELECT concept_name,
		concept_name_length,
		concept_name_terms,
		row_number() OVER (ORDER BY concept_name_terms ASC,
				concept_name_length ASC
			) AS rn1
	FROM #search_strings
	) t1
WHERE rn1 < 1000
ORDER BY concept_name;

-- If search string is substring of another search string, discard longer string
WITH longerString
AS (
	SELECT DISTINCT ss1.concept_name
	FROM #search_str_top1000 ss1
	INNER JOIN #search_str_top1000 ss2
		ON ss2.concept_name_length < ss1.concept_name_length
			AND ss1.concept_name LIKE CONCAT (
				'%',
				ss2.concept_name,
				'%'
				)
	)
SELECT ss.*
INTO #search_string_subset
FROM #search_str_top1000 ss
LEFT JOIN longerString ls
	ON ss.concept_name = ls.concept_name;

-- Create recommended list: concepts containing search string but not mapping to start set
SELECT DISTINCT c1.*
INTO #orphan_concept_table
FROM #search_string_subset ss
INNER JOIN @vocabulary_database_schema.concept c1
	ON LOWER(c1.concept_name) = LOWER(ss.concept_name)
LEFT JOIN #starting_concepts sc
  ON LOWER(c1.concept_name) = LOWER(sc.concept_name)
WHERE sc.concept_name IS NULL
ORDER BY c1.concept_id;

DROP TABLE IF EXISTS #starting_concepts;
DROP TABLE IF EXISTS #concept_synonyms;
DROP TABLE IF EXISTS #search_strings;
DROP TABLE IF EXISTS #search_str_top1000;
DROP TABLE IF EXISTS #search_sub_str;
DROP TABLE IF EXISTS #search_string_subset;