SET search_path TO vocabulary2;

CREATE EXTENSION pg_trgm;
CREATE EXTENSION btree_gin;

ALTER TABLE concept 
	ADD COLUMN CONCEPT_NAME_TSV tsvector
    	GENERATED ALWAYS AS (to_tsvector('english', concept_name)) STORED;
CREATE INDEX concept_name_gin_idx ON concept USING GIN (concept_name);

ALTER TABLE concept 
	ADD COLUMN CONCEPT_CODE_TSV tsvector
    	GENERATED ALWAYS AS (to_tsvector('english', concept_code)) STORED;
CREATE INDEX concept_code_gin_idx ON concept USING GIN (concept_code);

ALTER TABLE concept 
	ADD COLUMN CONCEPT_ID_TSV tsvector
    	GENERATED ALWAYS AS (to_tsvector('english', "concept_id"::text)) STORED;
CREATE INDEX concept_id_gin_idx ON concept USING GIN (concept_id);

ALTER TABLE concept 
	ADD COLUMN CONCEPT_NAME_REVERSE_TSV tsvector
    	GENERATED ALWAYS AS (to_tsvector('english', reverse(concept_name))) STORED;
CREATE INDEX concept_name_reverse_gin_idx ON concept USING GIN (reverse(concept_name));



ALTER TABLE concept_synonym
	ADD COLUMN concept_synonym_name_TSV tsvector
    	GENERATED ALWAYS AS (to_tsvector('english', concept_synonym_name)) STORED;
CREATE INDEX concept_synonym_name_gin_idx ON concept_synonym USING GIN (concept_synonym_name);

ALTER TABLE concept_synonym
	ADD COLUMN concept_synonym_name_REVERSE_TSV tsvector
    	GENERATED ALWAYS AS (to_tsvector('english', reverse(concept_synonym_name))) STORED;
CREATE INDEX concept_synonym_name_REVERSE_gin_idx ON concept_synonym USING GIN (reverse(concept_synonym_name));