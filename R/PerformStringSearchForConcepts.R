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
performStringSearchForConcepts <-
  function(searchString,
           vocabularyDatabaseSchema = "vocabulary",
           connection = NULL,
           connectionDetails = NULL) {
    if (nchar(searchString) <= 3) {
      stop("search string is shorter than 3 characters.")
    }

    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }

    fieldsInConceptTable <-
      DatabaseConnector::dbListFields(
        conn = connection,
        name = "concept"
      )
    fieldsInConceptTable <-
      tolower(sort(unique(fieldsInConceptTable)))

    if (tolower("FULL_TEXT_SEARCH") %in% fieldsInConceptTable) {
      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "SearchStringTsv.sql",
        packageName = "ConceptSetDiagnostics",
        dbms = connection@dbms,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        search_string = searchString
      )
    } else {
      # Filtering strings to letters, numbers and spaces only to avoid SQL injection
      # also making search string of lower case - to make search uniform.
      searchString <-
        stringr::str_squish(tolower(gsub("[^a-zA-Z0-9 ,]", " ", searchString)))

      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "SearchString.sql",
        packageName = "ConceptSetDiagnostics",
        dbms = connection@dbms,
        vocabulary_database_schema = vocabularyDatabaseSchema,
        search_string = searchString
      )
    }

    data <-
      DatabaseConnector::querySql(
        sql = sql,
        connection = connection,
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::tibble()
    return(data)
  }
