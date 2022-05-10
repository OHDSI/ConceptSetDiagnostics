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
getStringSearchConcepts <-
  function(searchString,
           vocabularyDatabaseSchema = "vocabulary",
           connection = NULL,
           connectionDetails = NULL) {
    start <- Sys.time()
    
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    
    # Filtering strings to letters, numbers and spaces only to avoid SQL injection
    # also making search string of lower case - to make search uniform.
    searchString <-
      stringr::str_squish(tolower(gsub("[^a-zA-Z0-9 ,]", " ", searchString)))
    
    sql <-  "
    WITH matched_concepts
    AS (
    	SELECT DISTINCT concept_id
    	FROM @vocabulary_database_schema.concept
    	WHERE LOWER(CONCEPT_NAME) LIKE '%@search_string%'
    	  OR LOWER(CONCEPT_CODE) LIKE '%@search_string%'

    	UNION

    	SELECT DISTINCT concept_id
    	FROM @vocabulary_database_schema.concept_synonym
    	WHERE LOWER(CONCEPT_SYNONYM_NAME) LIKE '%@search_string%'
    	)
    SELECT c.CONCEPT_ID,
    	c.CONCEPT_NAME,
    	c.VOCABULARY_ID,
    	c.STANDARD_CONCEPT,
    	c.INVALID_REASON,
    	c.CONCEPT_CODE,
    	c.CONCEPT_CLASS_ID,
    	c.DOMAIN_ID
    FROM @vocabulary_database_schema.concept c
    INNER JOIN matched_concepts ON c.concept_id = matched_concepts.concept_id;"
    
    data <-
      DatabaseConnector::renderTranslateQuerySql(
        sql = sql,
        connection = connection,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        search_string = searchString,
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::tibble()
    return(data)
  }
