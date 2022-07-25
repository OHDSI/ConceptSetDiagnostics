DROP TABLE IF EXISTS #given;
DROP TABLE IF EXISTS #with_descendants;
DROP TABLE IF EXISTS #mapped_non_standard;
DROP TABLE IF EXISTS #given_to_standard;
DROP TABLE IF EXISTS #given_to_ancestor;
DROP TABLE IF EXISTS #optimized;

{DEFAULT @conceptIds = 0 } 
{DEFAULT @conceptIdsWithIncludeDescendants = 0 }

-- Concepts that are part of the concept set definition that are "EXCLUDED = N, DECENDANTS = Y or N"
--HINT DISTRIBUTE_ON_KEY(original_concept_id)
SELECT DISTINCT c.concept_id original_concept_id,
	c.*
INTO #given
FROM @vocabulary_database_schema.concept c
WHERE concept_id IN (@conceptIds);

-- Concepts that are part of the concept set definition that are "EXCLUDED = N, DECENDANTS = Y"
--HINT DISTRIBUTE_ON_KEY(original_concept_id)
SELECT DISTINCT ca.ancestor_concept_id original_concept_id,
	ca.*
INTO #with_descendants
FROM @vocabulary_database_schema.concept_ancestor ca
WHERE ancestor_concept_id IN (@conceptIdsWithIncludeDescendants);

-- Mapped non-standard concepts
--HINT DISTRIBUTE_ON_KEY(original_concept_id)
SELECT DISTINCT AC.concept_id original_concept_id,
	cr.*
INTO #mapped_non_standard
FROM vocabulary_20220409.concept_relationship cr
INNER JOIN (
	SELECT DISTINCT a1.original_concept_id,
		a1.original_concept_id concept_id
	FROM given a1
	
	UNION
	
	SELECT DISTINCT a2.ancestor_concept_id original_concept_id,
		a2.descendant_concept_id concept_id
	FROM with_descendants a2
	) AC
	ON cr.concept_id_1 = AC.concept_id
INNER JOIN vocabulary_20220409.concept c
	ON c.concept_id = cr.concept_id_2
WHERE COALESCE(cr.invalid_reason, '') = ''
	AND relationship_id = 'Maps to'
	AND COALESCE(c.standard_concept, '') = 'S'
	AND cr.concept_id_1 != cr.concept_id_2;

--HINT DISTRIBUTE_ON_KEY(original_concept_id)
SELECT DISTINCT g.original_concept_id,
	ISNULL(mns.concept_id_2, g.concept_id) standard_concept_id
INTO #given_to_standard
FROM #given g
LEFT JOIN #mapped_non_standard mns
	ON g.original_concept_id = mns.concept_id_1;

--HINT DISTRIBUTE_ON_KEY(original_concept_id)
SELECT original_concept_id,
	standard_concept_id,
	ancestor_concept_id,
	ISNULL(rn, 0) rn
INTO #given_to_ancestor
FROM (
	SELECT sc.original_concept_id,
		sc.standard_concept_id,
		ISNULL(ancestor.ancestor_concept_id, sc.standard_concept_id) ancestor_concept_id,
		ROW_NUMBER() OVER (
			PARTITION BY sc.ORIGINAL_CONCEPT_ID,
			sc.STANDARD_CONCEPT_ID ORDER BY MAX_LEVELS_OF_SEPARATION DESC,
				MIN_LEVELS_OF_SEPARATION DESC
			) rn
	FROM #given_to_standard sc
	LEFT JOIN #with_descendants ancestor
		ON sc.standard_concept_id = ancestor.descendant_concept_id
	) f;

-- All not excluded concept id
--HINT DISTRIBUTE_ON_KEY(original_concept_id)
SELECT m.original_concept_id,
	m.ancestor_concept_id replacement_concept_id,
	CASE 
		WHEN m.original_concept_id = m.ancestor_concept_id
			THEN NULL
		ELSE m.original_concept_id
		END AS removed_concept_id
INTO #optimized
FROM #given_to_ancestor m
INNER JOIN (
	SELECT original_concept_id,
		standard_concept_id,
		MIN(rn) rn
	FROM #given_to_ancestor
	GROUP BY original_concept_id,
		standard_concept_id
	) grp
	ON m.original_concept_id = grp.original_concept_id
		AND grp.standard_concept_id = grp.standard_concept_id
		AND m.rn = grp.rn;


DROP TABLE IF EXISTS #given;
DROP TABLE IF EXISTS #with_descendants;
DROP TABLE IF EXISTS #mapped_non_standard;
DROP TABLE IF EXISTS #given_to_standard;
DROP TABLE IF EXISTS #given_to_ancestor;
