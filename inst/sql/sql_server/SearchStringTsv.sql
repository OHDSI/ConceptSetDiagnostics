SELECT c.CONCEPT_ID,
        	c.CONCEPT_NAME,
        	c.VOCABULARY_ID,
        	c.STANDARD_CONCEPT,
        	c.INVALID_REASON,
        	c.CONCEPT_CODE,
        	c.CONCEPT_CLASS_ID,
        	c.DOMAIN_ID,
          Least(ts_rank(FULL_TEXT_SEARCH, phraseto_tsquery('@search_string')),
				        ts_rank(FULL_TEXT_SEARCH, websearch_to_tsquery('@search_string'))) AS rank,
        	Least(ts_rank_cd(FULL_TEXT_SEARCH, phraseto_tsquery('@search_string')),
				        ts_rank_cd(FULL_TEXT_SEARCH, websearch_to_tsquery('@search_string'))) AS rank_cd
FROM @vocabulary_database_schema.concept c
WHERE FULL_TEXT_SEARCH @@ phraseto_tsquery('@search_string') OR
      FULL_TEXT_SEARCH @@ websearch_to_tsquery('@search_string')
ORDER BY rank_cd, rank
;