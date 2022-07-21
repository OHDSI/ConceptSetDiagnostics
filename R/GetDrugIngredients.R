# Copyright 2022 Observational Health Data Sciences and Informatics
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

#' Get ingredient information
#' @description
#' Given an array of drug concept ids, returns their ingredients
#'
#' @template Connection
#'
#' @param conceptIds An array of concept ids to find ingredients for
#'
#' @template VocabularyDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
getDrugIngredients <-
  function(connection = NULL,
           connectionDetails = NULL,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           conceptIds,
           vocabularyDatabaseSchema = "vocabulary") {
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }

    tempTableName <- loadTempConceptTable(
      conceptIds = conceptIds,
      connection = connection,
      tempEmulationSchema = tempEmulationSchema
    )

    sql <- "SELECT DISTINCT ca.drug_concept_id,
            	d.concept_Name drug_name,
            	d.concept_Code drug_concept_code,
            	d.concept_Class_id drug_concept_class,
            	ingredient_concept_id,
            	ingredient_name,
            	ingredient_concept_code,
            	ingredient_concept_class,
            	ingredient_vocabulary_id
            FROM (
                    SELECT ca.ancestor_concept_id ingredient_concept_id,
                            c.concept_name ingredient_name,
                            c.concept_code ingredient_concept_code,
                            c.concept_class_id ingredient_concept_class,
                            c.vocabulary_id ingredient_vocabulary_id,
                            ca.descendant_concept_id drug_concept_id
                    FROM @vocabulary_database_schema.concept_ancestor ca
                    INNER JOIN @vocabulary_database_schema.concept c
                      ON ca.ancestor_concept_id = c.concept_id
                    WHERE LOWER(c.concept_class_id) = 'ingredient'
                          AND ca.descendant_concept_id IN
                                                          (SELECT DISTINCT CONCEPT_ID
                                                            FROM @concept_id_table cid)
            ) ca
            INNER JOIN @vocabulary_database_schema.concept d ON ca.drug_concept_id = d.concept_id
            ORDER BY ingredient_concept_id, ca.drug_concept_id;
    "

    data <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        concept_id_table = tempTableName,
        tempEmulationSchema = tempEmulationSchema,
        snakeCaseToCamelCase = TRUE
      ) %>%
      tidyr::tibble()

    dropTempConceptTable(
      connection = connection,
      tempTableName = tempTableName,
      tempEmulationSchema = tempEmulationSchema
    )

    return(data)
  }
