SET search_path TO vocabulary2;

CREATE EXTENSION pg_trgm;
CREATE EXTENSION btree_gin;

ALTER TABLE concept 
	DROP COLUMN IF EXISTS CONCEPT_NAME_TSV;	
	
ALTER TABLE concept 
	ADD COLUMN CONCEPT_NAME_TSV tsvector
    	GENERATED ALWAYS AS (to_tsvector('english', LOWER(concept_name))) STORED;

ALTER TABLE concept 
	DROP COLUMN IF EXISTS CONCEPT_CODE_TSV;
ALTER TABLE concept 
	ADD COLUMN CONCEPT_CODE_TSV tsvector
    	GENERATED ALWAYS AS (to_tsvector('english', LOWER(concept_code))) STORED;

ALTER TABLE concept 
	DROP COLUMN IF EXISTS CONCEPT_ID_TSV;
ALTER TABLE concept 
	ADD COLUMN CONCEPT_ID_TSV tsvector
    	GENERATED ALWAYS AS (to_tsvector('english', "concept_id"::text)) STORED;

ALTER TABLE concept 
	ADD COLUMN CONCEPT_NAME_REVERSE_TSV tsvector
    	GENERATED ALWAYS AS (to_tsvector('english', reverse(LOWER(concept_name)))) STORED;


ALTER TABLE concept_synonym 
	DROP COLUMN IF EXISTS concept_synonym_name_TSV;
ALTER TABLE concept_synonym
	ADD COLUMN concept_synonym_name_TSV tsvector
    	GENERATED ALWAYS AS (to_tsvector('english', LOWER(concept_synonym_name))) STORED;
ALTER TABLE concept_synonym 
	DROP COLUMN IF EXISTS concept_synonym_name_REVERSE_TSV;
ALTER TABLE concept_synonym
	ADD COLUMN concept_synonym_name_REVERSE_TSV tsvector
    	GENERATED ALWAYS AS (to_tsvector('english', reverse(LOWER(concept_synonym_name)))) STORED;
		
		
		
CREATE INDEX concept_name_gin_idx ON concept USING GIN (LOWER(concept_name));
CREATE INDEX concept_code_gin_idx ON concept USING GIN (LOWER(concept_code));
CREATE INDEX concept_id_gin_idx ON concept USING GIN (concept_id);
CREATE INDEX concept_name_reverse_gin_idx ON concept USING GIN (reverse(LOWER(concept_name)));
CREATE INDEX concept_synonym_name_gin_idx ON concept_synonym USING GIN (LOWER(concept_synonym_name));
CREATE INDEX concept_synonym_name_REVERSE_gin_idx ON concept_synonym USING GIN (reverse(LOWER(concept_synonym_name)));