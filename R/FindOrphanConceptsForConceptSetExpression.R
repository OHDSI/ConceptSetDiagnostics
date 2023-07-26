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

#' Find orphan concepts for a concept set expression.
#'
#' @description
#' Find orphan concepts for a concept set expression.
#'
#' @template ConceptSetExpression
#'
#' @template Connection
#'
#' @template VocabularyDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
findOrphanConceptsForConceptSetExpression <-
  function(conceptSetExpression,
           vocabularyDatabaseSchema = "vocabulary",
           connection = NULL,
           connectionDetails = NULL,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
    
    resolvedConceptIds <-
      resolveConceptSetExpression(
        conceptSetExpression,
        connection = connection,
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema
      )

    mappedConceptIds <-
      dplyr::bind_rows(
        getMappedSourceConcepts(
          conceptIds = resolvedConceptIds$conceptId,
          connection = connection,
          connectionDetails = connectionDetails,
          tempEmulationSchema = tempEmulationSchema,
          vocabularyDatabaseSchema = vocabularyDatabaseSchema
        ),
        getMappedStandardConcepts(
          conceptIds = resolvedConceptIds$conceptId,
          connection = connection,
          connectionDetails = connectionDetails,
          tempEmulationSchema = tempEmulationSchema,
          vocabularyDatabaseSchema = vocabularyDatabaseSchema
        )
      )

    orphanConcepts <-
      findOrphanConcepts(
        connectionDetails = connectionDetails,
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        tempEmulationSchema = tempEmulationSchema,
        conceptIds = c(
          resolvedConceptIds$conceptId,
          mappedConceptIds$conceptId,
          mappedConceptIds$givenConceptId
        ) |> unique()
      )

    return(orphanConcepts)
  }
