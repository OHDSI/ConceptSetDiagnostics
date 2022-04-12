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

#' given a list of conceptIds, get their relationship
#'
#' @template Connection
#'
#' @template ConceptIds
#'
#' @template VocabularyDatabaseSchema
#' @export
getDeepConceptRelationship <-
  function(conceptIds,
           connection = NULL,
           connectionDetails = NULL,
           vocabularyDatabaseSchema = 'vocabulary',
           conceptPrevalenceSchema = 'concept_prevalence') {
    if (length(conceptIds) == 0) {
      stop('No concept id provided')
    }
    
    conceptAncestor <-
      ConceptSetDiagnostics::getConceptAncestor(conceptIds = conceptIds,
                                                connection = connection,
                                                connectionDetails = connectionDetails,
                                                vocabularyDatabaseSchema = vocabularyDatabaseSchema) %>%
      dplyr::mutate(
        relationshipDirection = dplyr::if_else(
          condition = .data$ancestorConceptId == conceptIds,
          true = 'd',
          false = 'u'
        )
      )
    conceptAncestor <- dplyr::bind_rows(
      conceptAncestor %>%
        dplyr::filter(.data$ancestorConceptId %in% conceptIds) %>%
        dplyr::rename(searchedConceptId = .data$ancestorConceptId) %>%
        dplyr::rename(conceptId = .data$descendantConceptId),
      conceptAncestor %>%
        dplyr::filter(.data$descendantConceptId %in% conceptIds) %>%
        dplyr::rename(searchedConceptId = .data$descendantConceptId) %>%
        dplyr::rename(conceptId = .data$ancestorConceptId)
    )
    
    conceptRelationship <-
      ConceptSetDiagnostics::getConceptRelationship(
        conceptIds = conceptAncestor$conceptId %>% unique(),
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema
      )
    
    conceptRelationship <- dplyr::bind_rows(
      conceptRelationship %>%
        dplyr::inner_join(conceptAncestor, by = c("conceptId1" = "conceptId")) %>%
        dplyr::rename(
          ancestorPathConceptId = .data$conceptId1,
          conceptId = .data$conceptId2
        ),
      conceptRelationship %>%
        dplyr::inner_join(conceptAncestor, by = c("conceptId2" = "conceptId")) %>%
        dplyr::rename(
          ancestorPathConceptId = .data$conceptId2,
          conceptId = .data$conceptId1
        )
    ) %>%
      dplyr::distinct() %>%
      dplyr::relocate(
        .data$searchedConceptId,
        .data$ancestorPathConceptId,
        .data$minLevelsOfSeparation,
        .data$maxLevelsOfSeparation,
        .data$relationshipDirection,
        .data$conceptId
      )
    
    if (nrow(conceptRelationship) == 0) {
      return(NULL)
    }
    
    conceptIdDetails <-
      ConceptSetDiagnostics::getConceptIdDetails(
        conceptIds = c(
          conceptRelationship$searchedConceptId,
          conceptRelationship$ancestorPathConceptId,
          conceptRelationship$conceptId
        ) %>% unique(),
        connection = connection,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        conceptPrevalenceSchema = conceptPrevalenceSchema
      )
    
    data <- list(conceptRelationship = conceptRelationship,
                 concepts = conceptIdDetails)
    
    return(data)
  }
