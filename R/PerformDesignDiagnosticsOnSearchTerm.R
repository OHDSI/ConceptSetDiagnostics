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

# given a concept set table, perform design diagnostics
#' @export
performDesignDiagnosticsOnSearchTerm <-
  function(searchString,
           exportResults = FALSE,
           locationForResults = NULL,
           vocabularyDatabaseSchema = 'vocabulary',
           blackList = c(0),
           vocabularyIdOfInterest = c('SNOMED', 'HCPCS', 'ICD10CM', 'ICD10', 'ICD9CM', 'ICD9', 'Read'),
           domainIdOfInterest = c('Condition', 'Procedure', 'Observation'),
           connection) {
    
    # step perform string search
    searchResultConceptIds <- getStringSearchConcepts(connection = connection,
                                                      searchString = searchString) 
    if (length(vocabularyIdOfInterest) > 0) {
      searchResultConceptIds <- searchResultConceptIds %>% 
        dplyr::filter(.data$vocabularyId %in% vocabularyIdOfInterest)
    }
    if (length(domainIdOfInterest) > 0) {
      searchResultConceptIds <- searchResultConceptIds %>% 
        dplyr::filter(.data$domainId %in% domainIdOfInterest)
    }
    
    # develop a concept set expression based on string search
    conceptSetExpressionDataFrame <- 
      getConceptSetExpressionFromConceptSetExpressionDataFrame(
        conceptSetExpressionDataFrame = searchResultConceptIds,
        selectAllDescendants = TRUE) %>% 
      getConceptSetSignatureExpression(connection = connection) %>% 
      getConceptSetExpressionDataFrameFromConceptSetExpression(updateVocabularyFields = TRUE, 
                                                               connection = connection) 
    
    conceptSetExpression <- 
      getConceptSetExpressionFromConceptSetExpressionDataFrame(
        conceptSetExpressionDataFrame = conceptSetExpressionDataFrame)
    
    
    
    # resolve concept set expression to individual concept ids
    resolvedConceptIds <- 
      resolveConceptSetExpression(connection = connection, 
                                  conceptSetExpression = conceptSetExpression)
    
    recommendedConceptIds <- 
      getRecommendationForConceptSetExpression(
        conceptSetExpression = conceptSetExpression, 
        connection = connection, 
        vocabularyIdOfInterest = vocabularyIdOfInterest, 
        domainIdOfInterest = domainIdOfInterest)
    
    searchResult <- list(searchString = searchString,
                         searchResultConceptIds = searchResultConceptIds,
                         conceptSetExpressionDataFrame = conceptSetExpressionDataFrame,
                         resolvedConceptIds = resolvedConceptIds,
                         recommendedConceptIds = recommendedConceptIds)
    
    if (exportResults) {
      if (!is.null(locationForResults)) {
        dir.create(path = locationForResults, showWarnings = FALSE, recursive = TRUE)
        if (nrow(recommendedConceptIds$recommendedStandard) > 0) {
          readr::write_excel_csv(
            x = recommendedConceptIds$recommendedStandard,
            file = file.path(
              locationForResults,
              paste0("recommendedStandard.csv")
            ),
            append = FALSE,
            na = ""
          )
          writeLines(text = paste0(
            "Wrote recommendedStandard.csv to ",
            locationForResults
          ))
        } else {
          writeLines(
            text = paste0(
              "No recommendation. recommendedStandard.csv is not written to ",
              locationForResults
            )
          )
          unlink(
            x = file.path(
              locationForResults,
              paste0("recommendedStandard.csv")
            ),
            recursive = TRUE,
            force = TRUE
          )
        }
        if (nrow(recommendedConceptIds$recommendedSource) > 0) {
          readr::write_excel_csv(
            x = recommendedConceptIds$recommendedSource,
            file = file.path(
              locationForResults,
              paste0("recommendedSource.csv")
            ),
            append = FALSE,
            na = ""
          )
          writeLines(text = paste0(
            "Wrote recommendedSource.csv to ",
            locationForResults
          ))
        } else {
          writeLines(
            text = paste0(
              "No recommendation. recommendedSource.csv is not written to ",
              locationForResults
            )
          )
          unlink(
            x = file.path(
              locationForResults,
              paste0("recommendedSource.csv")
            ),
            recursive = TRUE,
            force = TRUE
          )
        }
      }
    }
    return(searchResult)
  }
