# Copyright 2020 Observational Health Data Sciences and Informatics
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

#' Concept search using string
#'
#' @template Connection
#'
#' @template ConceptIds
#'
#' @template VocabularyDatabaseSchema
#'
#' @export
getConceptIdDetails <-
  function(conceptIds,
           connection,
           vocabularyDatabaseSchema = 'vocabulary') {
    sql <- "SELECT c.CONCEPT_ID,
            	c.CONCEPT_NAME,
            	c.VOCABULARY_ID,
            	c.STANDARD_CONCEPT,
            	c.INVALID_REASON,
            	c.CONCEPT_CODE,
            	c.CONCEPT_CLASS_ID,
            	c.DOMAIN_ID,
            	ISNULL(universe.RC, 0) RC,
            	ISNULL(universe.DBC, 0) DBC,
            	ISNULL(universe.DRC, 0) DRC,
            	ISNULL(universe.DDBC, 0) DDBC
            FROM @vocabulary_database_schema.concept c
            LEFT JOIN concept_prevalence.universe ON c.concept_id = universe.concept_id
            WHERE c.CONCEPT_ID IN (@concept_id_list)
            ORDER BY ISNULL(universe.DRC, 0) DESC;"
    
    sql <- SqlRender::render(
      sql = sql,
      vocabulary_database_schema = vocabularyDatabaseSchema,
      concept_id_list = conceptIds
    )
    data <-
      renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::tibble()
    return(data)
  }