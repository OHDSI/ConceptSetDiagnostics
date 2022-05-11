# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of ConceptSetDiagnostics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# Concept search using string

#' Get concepts that match a string search
#'
#' @template Connection
#'
#' @template VocabularyDatabaseSchema
#'
#' @param searchString A phrase (can be multiple words) to search for.
#'
#' @export
getStringSearchConceptsUsingFullText <-
  function(searchString,
           vocabularyDatabaseSchema = "vocabulary",
           connection = NULL,
           connectionDetails = NULL) {
    start <- Sys.time()
    
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    
    # Note this function is designed for postgres with TSV enabled.
    
    sql <- "
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
        ;"
    
    data <-
      DatabaseConnector::renderTranslateQuerySql(
        sql = sql,
        connection = connection,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        search_string = searchString,
        snakeCaseToCamelCase = TRUE
      ) %>%
      tidyr::tibble()
    return(data)
  }
