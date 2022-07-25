DROP TABLE IF EXISTS #concept_list;
DROP TABLE IF EXISTS #mapped_concepts;
DROP TABLE IF EXISTS #rec_01;
DROP TABLE IF EXISTS #rec_02;
DROP TABLE IF EXISTS #rec_03;
DROP TABLE IF EXISTS #rec_04;
DROP TABLE IF EXISTS #rec_05;
DROP TABLE IF EXISTS #rec;
DROP TABLE IF EXISTS #rec_no_rb;
DROP TABLE IF EXISTS #rec_std_1;
DROP TABLE IF EXISTS #rec_std_2;
DROP TABLE IF EXISTS #rec_std;

--HINT DISTRIBUTE_ON_KEY(concept_id)
SELECT DISTINCT concept_id,
	concept_name,
	domain_id,
	vocabulary_id,
	standard_concept
INTO #concept_list
FROM @vocabulary_database_schema.concept
WHERE concept_id IN (
		SELECT DISTINCT concept_id
		FROM @concept_id_temp_table
		);

--HINT DISTRIBUTE_ON_KEY(concept_id_1)		
SELECT DISTINCT cr2.*
INTO #mapped_concepts
FROM #concept_list l
INNER JOIN @vocabulary_database_schema.concept_relationship cr2
	ON l.concept_id = cr2.concept_id_2
WHERE cr2.relationship_id = 'Maps to';

--list all included concepts
--HINT DISTRIBUTE_ON_KEY(concept_id)
SELECT DISTINCT i.concept_id,
	'Included' AS concept_in_set
INTO #rec_01
FROM #concept_list i;

--find not included concepts found by orphan check via standards
--HINT DISTRIBUTE_ON_KEY(concept_id)
SELECT DISTINCT rc1.concept_id,
	'Not included - recommended via standard' AS concept_in_set
INTO #rec_02
FROM #concept_list i
INNER JOIN @concept_prevalence_schema.recommender_set rc1
	ON i.concept_id = rc1.source_id
INNER JOIN @vocabulary_database_schema.concept c1
	ON rc1.concept_id = c1.concept_id
		AND c1.standard_concept = 'S';

--HINT DISTRIBUTE_ON_KEY(concept_id)
SELECT DISTINCT cr1.concept_id_2 concept_id,
	'Not included - recommended via source' AS concept_in_set
INTO #rec_03
FROM #concept_list i
INNER JOIN @concept_prevalence_schema.recommender_set rc1
	ON i.concept_id = rc1.source_id
INNER JOIN @vocabulary_database_schema.concept c1
	ON rc1.concept_id = c1.concept_id
INNER JOIN @vocabulary_database_schema.concept_relationship cr1
	ON c1.concept_id = cr1.concept_id_1
-- excluding those sources that already have one standard counterpart in our input list
LEFT JOIN #mapped_concepts a
	ON a.concept_id_1 = cr1.concept_id_1
WHERE a.concept_id_2 IS NULL
	AND cr1.relationship_id IN (
		'Maps to',
		'Maps to value'
		)
	AND c1.standard_concept IS NULL;

-- find all not included parents
--HINT DISTRIBUTE_ON_KEY(concept_id)
SELECT DISTINCT ca.ancestor_concept_id,
	'Not included - parent' AS concept_in_set
INTO #rec_04
FROM #concept_list i
INNER JOIN @vocabulary_database_schema.concept_ancestor ca
	ON i.concept_id = ca.descendant_concept_id
WHERE ca.min_levels_of_separation = 1;

-- find all not included children
--HINT DISTRIBUTE_ON_KEY(concept_id)
SELECT DISTINCT ca.descendant_concept_id,
	'Not included - descendant' AS concept_in_set
INTO #rec_05
FROM #concept_list i
INNER JOIN @vocabulary_database_schema.concept_ancestor ca
	ON i.concept_id = ca.ancestor_concept_id;

--HINT DISTRIBUTE_ON_KEY(concept_id)
SELECT DISTINCT concept_id,
	concept_in_set
INTO #rec
FROM (
	SELECT *
	FROM #rec_01
	
	UNION
	
	SELECT *
	FROM #rec_02
	
	UNION
	
	SELECT *
	FROM #rec_03
	
	UNION
	
	SELECT *
	FROM #rec_04
	
	UNION
	
	SELECT *
	FROM #rec_05
	) f;

--HINT DISTRIBUTE_ON_KEY(concept_id)
SELECT DISTINCT r.concept_id,
	r.concept_in_set
INTO #rec_no_rb
FROM #rec r
LEFT JOIN @concept_prevalence_schema.recommended_blacklist rb
	ON r.concept_id = rb.concept_id
WHERE rb.concept_id IS NULL;

--HINT DISTRIBUTE_ON_KEY(concept_id)
SELECT DISTINCT c.concept_id,
	c.concept_name,
	c.vocabulary_id,
	c.domain_id,
	c.standard_concept,
	r.concept_in_set,
	coalesce(cp.rc, 0) AS record_count,
	coalesce(cp.dbc, 0) AS database_count,
	coalesce(cp.drc, 0) AS descendant_record_count,
	coalesce(cp.ddbc, 0) AS descendant_database_count
INTO #rec_std_1
FROM #rec_no_rb r
INNER JOIN @vocabulary_database_schema.concept c
	ON c.concept_id = r.concept_id
LEFT JOIN @concept_prevalence_schema.cp_master cp
	ON r.concept_id = cp.concept_id
WHERE r.concept_in_set = 'Included';

-- remove recommended non standards that are mapped to recommended standards
--HINT DISTRIBUTE_ON_KEY(concept_id)
SELECT DISTINCT rns.concept_id
INTO #rec_ns_mapt_std
FROM #rec_no_rb rns
INNER JOIN @vocabulary_database_schema.concept_relationship cr1
	ON rns.concept_id = cr1.concept_id_2
INNER JOIN #rec_no_rb rs
	ON cr1.concept_id_1 = rs.concept_id
WHERE cr1.relationship_id = 'Maps to';

--HINT DISTRIBUTE_ON_KEY(concept_id)
SELECT DISTINCT c.concept_id,
	c.concept_name,
	c.vocabulary_id,
	c.domain_id,
	c.standard_concept,
	r.concept_in_set,
	coalesce(cp.rc, 0) AS record_count,
	coalesce(cp.dbc, 0) AS database_count,
	coalesce(cp.drc, 0) AS descendant_record_count,
	coalesce(cp.ddbc, 0) AS descendant_database_count
INTO #rec_std_2
FROM #rec_no_rb r
LEFT JOIN #rec_std_1 rs1
	ON r.concept_id = rs1.concept_id
INNER JOIN @vocabulary_database_schema.concept c
	ON c.concept_id = r.concept_id
LEFT JOIN @concept_prevalence_schema.cp_master cp
	ON r.concept_id = cp.concept_id
LEFT JOIN #rec_ns_mapt_std ne
	ON r.concept_id = ne.concept_id
WHERE rs1.concept_id IS NULL
	AND ne.concept_id IS NULL;

--HINT DISTRIBUTE_ON_KEY(concept_id)
SELECT DISTINCT concept_id,
	concept_name,
	vocabulary_id,
	domain_id,
	standard_concept,
	concept_in_set,
	record_count,
	database_count,
	descendant_record_count,
	descendant_database_count
INTO #rec_std
FROM (
	SELECT *
	FROM #rec_std_1 a
	
	UNION
	
	SELECT *
	FROM #rec_std_2 b
	) c;

DROP TABLE IF EXISTS #concept_list;
DROP TABLE IF EXISTS #mapped_concepts;
DROP TABLE IF EXISTS #rec_01;
DROP TABLE IF EXISTS #rec_02;
DROP TABLE IF EXISTS #rec_03;
DROP TABLE IF EXISTS #rec_04;
DROP TABLE IF EXISTS #rec_05;
DROP TABLE IF EXISTS #rec;
DROP TABLE IF EXISTS #rec_no_rb;
DROP TABLE IF EXISTS #rec_std_1;
DROP TABLE IF EXISTS #rec_std_2;