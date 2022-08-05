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
#' @param searchPhrases An array of phrases (each phrase may be multiple words,
#'                      but each phrase should be quoted and comma separated) to search for.
#'                      Phrases that have a character length of <4 are not searched.
#'
#' @param vocabularyIdOfInterest A list of vocabulary ids to filter the results.
#'
#' @param domainIdOfInterest     A list of domain ids to filter the results.
#'
#' @param retrieveInvalidConcepts Do you want to retrieve invalid concepts. Default = FALSE
#'
#' @export
performStringSearchForConcepts <-
  function(searchPhrases,
           vocabularyDatabaseSchema = "vocabulary",
           connection = NULL,
           connectionDetails = NULL,
           vocabularyIdOfInterest = c("SNOMED", "HCPCS", "ICD10CM", "ICD10", "ICD9CM", "ICD9", "Read"),
           domainIdOfInterest = c("Condition", "Procedure", "Observation"),
           retrieveInvalidConcepts = FALSE) {
    if (!hasData(searchPhrases)) {
      writeLines(" - searchPhrases does not have data. No search performed.")
      return(NULL)
    }
    
    eligibleToBeSearched <- searchPhrases[nchar(searchPhrases) >= 4]
    if (length(dplyr::setdiff(x = searchPhrases, y = eligibleToBeSearched)) > 0) {
      writeLines(
        text = paste0(
          " - The following phrases are less than 4 characters and will not be searched: '",
          paste0(
            dplyr::setdiff(x = searchPhrases, y = eligibleToBeSearched),
            collapse = "', '"
          ),
          "'"
        )
      )
    }
    
    if (length(eligibleToBeSearched) == 0) {
      writeLines(" - No search phrases have more than 3 characters. No search performed.")
      return(NULL)
    }
    
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    
    fieldsInConceptTable <-
      DatabaseConnector::dbListFields(conn = connection,
                                      name = "concept")
    fieldsInConceptTable <-
      tolower(sort(unique(fieldsInConceptTable)))
    
    data <- c()
    
    for (i in (1:length(eligibleToBeSearched))) {
      if (tolower("FULL_TEXT_SEARCH") %in% fieldsInConceptTable) {
        searchString <-
          stringr::str_squish(tolower(gsub(
            "[^a-zA-Z0-9 ,]", " ", eligibleToBeSearched[[i]]
          )))
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
        
        sql <- SqlRender::loadRenderTranslateSql(
          sqlFilename = "SearchString.sql",
          packageName = "ConceptSetDiagnostics",
          dbms = connection@dbms,
          vocabulary_database_schema = vocabularyDatabaseSchema,
          search_string = searchString
        )
      }
      data[[i]] <-
        DatabaseConnector::querySql(
          sql = sql,
          connection = connection,
          snakeCaseToCamelCase = TRUE
        ) %>%
        dplyr::tibble()
    }
    
    if (!hasData(data)) {
      return(NULL)
    }
    
    data <- data %>%
      dplyr::bind_rows() %>%
      dplyr::distinct()
    
    if (all(nrow(data) > 0, "rank" %in% colnames(data))) {
      data <- data %>%
        dplyr::group_by(
          .data$conceptId,
          .data$conceptName,
          .data$vocabularyId,
          .data$standardConcept,
          .data$invalidReason,
          .data$conceptCode,
          .data$conceptClassId,
          .data$domainId
        ) %>%
        dplyr::summarise(rank = min(.data$rank),
                         rankCd = min(.data$rankCd)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(.data$rankCd, .data$rank) %>%
        dplyr::distinct()
    }
    data <- data %>%
      dplyr::mutate(
        standardConceptCaption = dplyr::case_when(
          .data$standardConcept == "S" ~ "Standard",
          .data$standardConcept == "C" ~ "Classification",
          TRUE ~ "Non-Standard"
        )
      ) %>%
      dplyr::mutate(
        invalidReasonCaption = dplyr::case_when(
          invalidReason == "V" ~ "Valid",
          invalidReason == "D" ~ "Deleted",
          invalidReason == "U" ~ "Updated",
          TRUE ~ "Valid"
        )
      )
    
    # filter to domain of interest
    if (length(domainIdOfInterest) > 0) {
      data <- data %>%
        dplyr::filter(.data$domainId %in% c(domainIdOfInterest))
    }
    
    # filter to vocabulary of interest
    if (length(vocabularyIdOfInterest) > 0) {
      data <- data %>%
        dplyr::filter(.data$vocabularyId %in% c(vocabularyIdOfInterest))
    }
    
    #filter invalid concepts
    
    if (!retrieveInvalidConcepts) {
      data <- data %>%
        dplyr::filter(.data$invalidReason %in% c("", "V"))
    }
    
    if (!hasData(data)) {
      return(NULL)
    }
    
    return(data)
  }
