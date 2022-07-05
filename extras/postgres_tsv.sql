#https://pgtune.leopard.in.ua/ make sure the postgres instance is optimized using this tool

CREATE EXTENSION pg_trgm;
CREATE EXTENSION btree_gin;

SET search_path TO vocabulary;


ALTER TABLE concept 
	DROP COLUMN IF EXISTS full_text_search;
	
ALTER TABLE concept 
	ADD COLUMN full_text_search tsvector;

drop view if exists concept_with_full_text;
create view concept_with_full_text as
SELECT c.concept_id,
	to_tsvector(
					c.concept_code || ' ' ||
					c.concept_name || ' ' ||
					COALESCE(cs.concept_synonym_name, ' ')) 
 AS document
FROM concept c
LEFT JOIN 
(
SELECT concept_id, STRING_AGG( concept_synonym_name, ' ') AS concept_synonym_name
FROM concept_synonym
GROUP BY concept_id
) cs
ON c.concept_id = cs.concept_id;

select *
from concept_with_full_text
limit 100;


UPDATE concept
SET full_text_search = concept_with_full_text.document
FROM concept_with_full_text
WHERE concept_with_full_text.concept_id = concept.concept_id;

drop view if exists concept_with_full_text;

CREATE INDEX idx_concept_name_gin ON concept USING GIN (full_text_search);