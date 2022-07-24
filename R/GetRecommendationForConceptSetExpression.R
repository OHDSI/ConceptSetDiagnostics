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

#' Get recommended concepts for a concept set expression.
#'
#' @description
#' Get recommended concepts for a concept set expression.
#'
#' @template Connection
#'
#' @template ConceptIds
#'
#' @template VocabularyDatabaseSchema
#'
#' @template ConceptPrevalenceSchema
#'
#' @template TempEmulationSchema
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
getRecommendationForConceptSetExpression <-
  function(conceptSetExpression,
           vocabularyDatabaseSchema = "vocabulary",
           connection = NULL,
           connectionDetails = NULL,
           conceptPrevalenceSchema = "concept_prevalence",
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
    resolvedConceptIds <-
      resolveConceptSetExpression(
        conceptSetExpression,
        connection = connection,
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema
      )
    
    recommendedStandard <-
      getRecommendedStandard(
        conceptIds = resolvedConceptIds$conceptId %>% unique(),
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        connection = connection,
        connectionDetails = connectionDetails,
        conceptPrevalenceSchema = conceptPrevalenceSchema,
        tempEmulationSchema = tempEmulationSchema
      )
    
    recommendedSource <-
      getRecommendedSource(
        conceptIds = resolvedConceptIds$conceptId %>% unique(),
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        connection = connection,
        connectionDetails = connectionDetails,
        conceptPrevalenceSchema = conceptPrevalenceSchema,
        tempEmulationSchema = tempEmulationSchema
      )
    
    data <- list()
    data$recommendedStandard <- recommendedStandard
    data$recommendedSource <- recommendedSource
    return(data)
  }
