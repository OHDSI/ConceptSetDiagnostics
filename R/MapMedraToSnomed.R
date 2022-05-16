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

#' map MedDRA to SNOMED
#'
#' @description
#' given an array of conceptIds belonging to MedDRA vocabulary get its equivalent SNOMED ranked
#' using a combination of OMOP vocabulary mapping, lexical string matching and concept prevalence
#' counts
#'
#' @template Connection
#'
#' @template ConceptIds
#'
#' @template VocabularyDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @return
#' Returns a tibble data frame
#'
#' @export
mapMedraToSnomed <-
  function(conceptIds,
           connection = NULL,
           connectionDetails = NULL,
           tempEmulationSchema = NULL,
           vocabularyDatabaseSchema = "vocabulary") {
    if (length(conceptIds) == 0) {
      stop("No concept id provided")
    }
    
    start <- Sys.time()
    
    medDraRelationship <- getMedraRelationship(
      conceptIds = conceptIds,
      connection = connection,
      connectionDetails = connectionDetails,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    
    allMedDRAConcepts <- c(
      medDraRelationship$givenConceptId,
      medDraRelationship$socConceptId,
      medDraRelationship$hlgtConceptId,
      medDraRelationship$hltConceptId,
      medDraRelationship$ptConceptId,
      medDraRelationship$lltConceptId
    ) %>%
      unique() %>%
      sort()
    
    # since MedDRA is a classifier for SNOMED, snomed is a descendant to MedDRA
    medDRADescendants <- getConceptDescendant(
      conceptIds = allMedDRAConcepts,
      connection = connection,
      connectionDetails = connectionDetails,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    
    medDRAMapped <- getMappedStandardConcepts(
      conceptIds = allMedDRAConcepts,
      connection = connection,
      connectionDetails = connectionDetails,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    
    conceptIdDetails <- getConceptIdDetails(
      conceptIds = c(
        allMedDRAConcepts %>% unique(),
        medDRADescendants$ancestorConceptId,
        medDRADescendants$descendantConceptId,
        medDRAMapped$givenConceptId,
        medDRAMapped$conceptId
      ) %>% unique(),
      connection = connection,
      connectionDetails = connectionDetails,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    
    conceptSynonyms <- getConceptSynonym(
      conceptIds = c(conceptIdDetails$conceptId) %>% unique(),
      connection = connection,
      connectionDetails = connectionDetails,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    
    mappedUsingVocabaulary <- medDRADescendants %>%
      dplyr::filter(.data$ancestorConceptId != .data$descendantConceptId) %>%
      dplyr::rename(
        "medDraConceptId" = .data$ancestorConceptId,
        "snomedConceptId" = .data$descendantConceptId
      )  %>%
      dplyr::inner_join(
        conceptIdDetails %>%
          dplyr::filter(.data$vocabularyId == 'MedDRA') %>%
          dplyr::select(.data$conceptId,
                        .data$conceptClassId) %>%
          dplyr::rename("medDraConceptClassId" = .data$conceptClassId) %>%
          dplyr::distinct(),
        by = c("medDraConceptId" = "conceptId")
      ) %>%
      dplyr::inner_join(
        conceptIdDetails %>%
          dplyr::filter(.data$vocabularyId == 'SNOMED') %>%
          dplyr::select(.data$conceptId,
                        .data$conceptClassId) %>%
          dplyr::rename("snomedConceptClassId" = .data$conceptClassId) %>%
          dplyr::distinct(),
        by = c("snomedConceptId" = "conceptId")
      ) %>%
      dplyr::inner_join(
        dplyr::bind_rows(
          conceptIdDetails %>%
            dplyr::select(.data$conceptId,
                          .data$conceptName) %>%
            dplyr::rename(
              "medDraConceptId" = .data$conceptId,
              "medDraConceptName" = .data$conceptName
            ),
          conceptSynonyms %>%
            dplyr::select(.data$conceptId,
                          .data$conceptSynonymName) %>%
            dplyr::rename(
              "medDraConceptId" = .data$conceptId,
              "medDraConceptName" = .data$conceptSynonymName
            )
        ) %>%
          dplyr::distinct(),
        by = "medDraConceptId"
      ) %>%  dplyr::inner_join(
        dplyr::bind_rows(
          conceptIdDetails %>%
            dplyr::select(.data$conceptId,
                          .data$conceptName) %>%
            dplyr::rename(
              "snomedConceptId" = .data$conceptId,
              "snomedConceptName" = .data$conceptName
            ),
          conceptSynonyms %>%
            dplyr::select(.data$conceptId,
                          .data$conceptSynonymName) %>%
            dplyr::rename(
              "snomedConceptId" = .data$conceptId,
              "snomedConceptName" = .data$conceptSynonymName
            )
        ) %>%
          dplyr::distinct(),
        by = "snomedConceptId"
      ) %>%
      dplyr::mutate(
        stringDistance1 =
          stringdist::stringdist(
            a = .data$snomedConceptName,
            b = .data$medDraConceptName
          )
      ) %>%
      dplyr::mutate(
        stringDistance2 =
          stringdist::stringdist(
            a = .data$medDraConceptName,
            b = .data$snomedConceptName
          )
      ) %>%
      dplyr::mutate(
        stringDistanceScore = dplyr::if_else(
          condition = .data$stringDistance1 > .data$stringDistance2,
          true = .data$stringDistance2,
          false = .data$stringDistance1
        )
      ) %>%
      dplyr::group_by(.data$medDraConceptId) %>%
      dplyr::arrange(
        .data$minLevelsOfSeparation,
        .data$maxLevelsOfSeparation,
        .data$stringDistanceScore
      ) %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        .data$medDraConceptId,
        .data$medDraConceptName,
        .data$medDraConceptClassId,
        .data$snomedConceptId,
        .data$snomedConceptName,
        .data$snomedConceptClassId,
        .data$rank
      ) %>%
      dplyr::arrange(
        .data$medDraConceptId,
        .data$medDraConceptName,
        .data$medDraConceptClassId,
        .data$rank
      )
    return(mappedUsingVocabaulary)
  }
