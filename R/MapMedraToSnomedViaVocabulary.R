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
mapMedraToSnomedViaVocabulary <-
  function(conceptIds,
           connection = NULL,
           connectionDetails = NULL,
           tempEmulationSchema = NULL,
           vocabularyDatabaseSchema = "vocabulary") {
    if (length(conceptIds) == 0) {
      stop("No concept id provided")
    }
    
    start <- Sys.time()
    
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    givenConceptId <- dplyr::tibble(medDraConceptId = conceptIds %>% unique())
    
    medDraRelationship <- getMedraRelationship(
      conceptIds = givenConceptId$medDraConceptId,
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    
    writeLines(paste0("Found ", scales::comma(nrow(medDraRelationship$givenConceptId)), " MedDRA concepts."))
    if (nrow(medDraRelationship$givenConceptId) == 0) {
      return(NULL)
    }
    
    writeLines(paste0("Found ", scales::comma(nrow(medDraRelationship$givenConceptId %>% dplyr::filter(.data$givenConceptClassId == 'LLT'))), " MedDRA LLT concepts."))
    
    lltToPt <- 
      medDraRelationship$givenConceptId %>% 
      dplyr::filter(.data$givenConceptClassId == 'LLT') %>% 
      dplyr::select(.data$givenConceptId) %>% 
      dplyr::distinct() %>% 
      dplyr::inner_join(medDraRelationship$pt,
                        by = "givenConceptId") %>% 
      dplyr::select(.data$givenConceptId, .data$ptConceptId) %>% 
      dplyr::distinct() %>% 
      dplyr::rename("medDraConceptId" = .data$ptConceptId)
    
    givenConceptId <- dplyr::bind_rows(givenConceptId,
                                       lltToPt %>% 
                                         dplyr::select(.data$medDraConceptId)) %>% 
      dplyr::distinct()
    
    # since MedDRA is a classifier for SNOMED, snomed is a descendant to MedDRA
    medDRADescendants <- getConceptDescendant(
      conceptIds = givenConceptId$medDraConceptId,
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    
    # but sometimes we may have a relationship in concept relationship but not in concept ancestor - because the related concept is deprecated
    medDraRelated <- getConceptRelationship(
      conceptIds = c(givenConceptId$medDraConceptId,
                     medDRADescendants$descendantConceptId,
                     medDRADescendants$ancestorConceptId,
                     0) %>% unique(),
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    ) 
    medDraRelated <- medDraRelated %>% 
      dplyr::filter(.data$conceptId1 %in% c(givenConceptId$medDraConceptId)) %>% 
      dplyr::filter(.data$relationshipId %in% c("MedDRA - SNOMED eq"))
    
    conceptRelationship <- getConceptRelationship(
      conceptIds = c(medDraRelated$conceptId2,0) %>% unique(),
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    ) %>% 
      dplyr::filter(.data$conceptId1 %in% c(medDraRelated$conceptId2)) %>% 
      dplyr::filter(.data$relationshipId %in% c("Maps to"))
    
    conceptIdDetails <- getConceptIdDetails(
      conceptIds = c(conceptIds,
                     givenConceptId$medDraConceptId,
                     medDRADescendants$ancestorConceptId,
                     medDRADescendants$descendantConceptId,
                     medDraRelated$conceptId1,
                     medDraRelated$conceptId2,
                     conceptRelationship$conceptId1,
                     conceptRelationship$conceptId2) %>% unique() %>% sort(),
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    
    descendantsMappedToSnoMed <-
      medDRADescendants %>%
      dplyr::inner_join(conceptIdDetails,
                        by = c("descendantConceptId" = "conceptId")) %>%
      dplyr::filter(.data$vocabularyId == "SNOMED") %>% 
      dplyr::select(.data$ancestorConceptId,
                    .data$descendantConceptId,
                    .data$minLevelsOfSeparation,
                    .data$maxLevelsOfSeparation) %>% 
      dplyr::rename("medDraConceptId" = .data$ancestorConceptId,
                    "snomedConceptId" = .data$descendantConceptId)
    
    relatedToSnomed <-
      medDraRelated %>%
      dplyr::select(.data$conceptId1,
                    .data$conceptId2) %>% 
      dplyr::distinct() %>% 
      dplyr::inner_join(conceptIdDetails %>% 
                          dplyr::filter(.data$vocabularyId == 'SNOMED') %>% 
                          dplyr::select(.data$conceptId,
                                        .data$invalidReason),
                        by = c("conceptId2" = "conceptId")) %>%
      dplyr::rename("medDraConceptId" = .data$conceptId1,
                    "snomedConceptId" = .data$conceptId2)
    
    relatedToSnomedWithValid <- relatedToSnomed %>% 
      dplyr::filter(is.na(.data$invalidReason)) %>% 
      dplyr::select(-.data$invalidReason)
    
    relatedToSnomedWithInvalid <- relatedToSnomed %>% 
      dplyr::filter(!is.na(.data$invalidReason)) %>% 
      dplyr::select(.data$medDraConceptId,
                    .data$snomedConceptId) %>% 
      dplyr::rename("invalidSnomedConceptId" = .data$snomedConceptId) %>% 
      dplyr::inner_join(conceptRelationship %>% 
                          dplyr::select(.data$conceptId1,
                                        .data$conceptId2),
                        by = c("invalidSnomedConceptId" = "conceptId1")) %>% 
      dplyr::rename("snomedConceptId" = .data$conceptId2) %>% 
      dplyr::select(-.data$invalidSnomedConceptId)
    
  
    finalMappedConcepts <- dplyr::bind_rows(
      dplyr::bind_rows(relatedToSnomedWithInvalid,
                       relatedToSnomedWithValid,) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          minLevelsOfSeparation = 1,
          maxLevelsOfSeparation = 1
        ),
      descendantsMappedToSnoMed
    ) %>% 
      dplyr::distinct()

    conceptSynonyms <- getConceptSynonym(
      conceptIds = finalMappedConcepts$snomedConceptId %>% unique(),
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    browser()
    
    
    mappedUsingVocabaulary <- finalMappedConcepts %>%
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
      )
    
    mappedUsingVocabaulary <- givenConceptId %>% 
      dplyr::left_join(mappedUsingVocabaulary,
                       by = "medDraConceptId") %>%
      dplyr::arrange(
        .data$medDraConceptId,
        .data$medDraConceptName,
        .data$medDraConceptClassId,
        .data$rank
      )
    
    return(mappedUsingVocabaulary)
  }
