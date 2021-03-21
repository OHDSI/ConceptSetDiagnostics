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
performDesignDiagnosticsOnConceptTable <-
  function(conceptSetExpressionDataFrame,
           vocabularyDatabaseSchema = 'vocabulary',
           vocabularyIdForRecommender = c('SNOMED', 'ICD'),
           exportResults = TRUE,
           locationForResults,
           blackList = c(0),
           iteration = 1,
           connection) {
    result <- list()
    result$conceptSetExpressionDataFrame <- conceptSetExpressionDataFrame
    result$conceptSetExpression <-
      getConceptSetExpressionFromConceptSetExpressionDataFrame(conceptSetExpressionDataFrame = conceptSetExpressionDataFrame)
    
    ### optimized
    result$conceptSetExpressionOptimized <-
      optimizeConceptSetExpression(
        connection = connection,
        conceptSetExpression = result$conceptSetExpression,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema
      )
    
    result$conceptSetExpressionOptimized <-
      getConceptSetExpressionDataFrameFromConceptSetExpression(conceptSetExpression = conceptSetExpressionDataFrame)
    
    #################################
    recommendation <-
      getRecommendationForConceptSetExpressionDataFrame(
        connection = connection,
        conceptSetExpressionDataFrame = result$conceptSetExpressionDataFrame,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        vocabularyIdForRecommender = vocabularyIdForRecommender
      )
    result$recommendedStandard <-
      recommendation$recommendedStandard %>%
      dplyr::filter(!.data$conceptId %in% result$conceptSetExpressionDataFrame$conceptId) %>%
      dplyr::filter(!.data$conceptId %in% blackList)
    result$recommendedSource <- recommendation$recommendedSource %>%
      dplyr::filter(!.data$conceptId %in% result$conceptSetExpressionDataFrame$conceptId) %>%
      dplyr::filter(!.data$conceptId %in% blackList)
    
    
    if (exportResults) {
      if (nrow(result$recommendedStandard) > 0) {
        readr::write_excel_csv(
          x = result$recommendedStandard,
          file = file.path(
            locationForResults,
            paste0("recommendedStandard", iteration, ".csv")
          ),
          append = FALSE,
          na = ""
        )
        writeLines(text = paste0(
          "Wrote recommendedStandard",
          iteration,
          ".csv to ",
          locationForResults
        ))
      } else {
        writeLines(
          text = paste0(
            "No recommendation. recommendedStandard",
            iteration,
            ".csv is not written to ",
            locationForResults
          )
        )
        unlink(
          x = file.path(
            locationForResults,
            paste0("recommendedStandard", iteration, ".csv")
          ),
          recursive = TRUE,
          force = TRUE
        )
      }
      if (nrow(result$recommendedSource) > 0) {
        readr::write_excel_csv(
          x = result$recommendedSource,
          file = file.path(
            locationForResults,
            paste0("recommendedSource", iteration, ".csv")
          ),
          append = FALSE,
          na = ""
        )
        writeLines(text = paste0(
          "Wrote recommendedSource",
          iteration,
          ".csv to ",
          locationForResults
        ))
      } else {
        writeLines(
          text = paste0(
            "No recommendation. recommendedSource",
            iteration,
            ".csv is not written to ",
            locationForResults
          )
        )
        unlink(
          x = file.path(
            locationForResults,
            paste0("recommendedSource", iteration, ".csv")
          ),
          recursive = TRUE,
          force = TRUE
        )
      }
    }
    return(result)
  }
