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

#' Given a concept set expression, get the resolved concepts
#'
#' @description
#' Given a concept set expression, get the resolved concepts
#'
#' @template Connection
#'
#' @template ConceptSetExpression
#'
#' @template VocabularyDatabaseSchema
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
getExcludedConceptsInConceptSetExpression <-
  function(conceptSetExpression,
           connection,
           connectionDetails = NULL,
           vocabularyDatabaseSchema = "vocabulary") {

    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }

    conceptSetDataFrame <-
      getConceptSetExpressionDataFrameFromConceptSetExpression(conceptSetExpression = conceptSetExpression)

    if (!"isExcluded" %in% colnames(conceptSetDataFrame)) {
      return(NULL)
    }

    excludeRows <- conceptSetDataFrame %>%
      dplyr::filter(.data$isExcluded == 1)
    excludeRowsDescendants <- excludeRows %>%
      dplyr::filter(.data$includeDescendants == TRUE)
    excludeRowsNoDescendants <- excludeRows %>%
      dplyr::filter(.data$includeDescendants == FALSE)


    sql <-
      "SELECT concept_id
      	FROM @vocabulary_database_schema.concept_ancestor anc
      	ancestor_concept_id IN (@excludeWithDescendants);"

    excludeConceptIdsWithDescendants <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        excludeWithDescendants = excludeRowsDescendants$conceptId %>% unique()
      )

    allExcludedConceptIds <-
      dplyr::bind_rows(
        excludeConceptIdsWithDescendants,
        excludeRowsNoDescendants %>% dplyr::select(conceptId)
      ) %>%
      dplyr::distinct()

    data <-
      getConceptIdDetails(
        conceptIds = allExcludedConceptIds$conceptId %>% unique(),
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema
      )

    return(data)
  }
