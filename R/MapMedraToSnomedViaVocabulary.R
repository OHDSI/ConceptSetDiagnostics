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
           vocabularyDatabaseSchema = "vocabulary") {
    if (length(conceptIds) == 0) {
      stop("No concept id provided")
    }
    
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    givenConceptId <-
      dplyr::tibble(medDraConceptId = conceptIds %>% unique())
    
    writeLines(paste0("Found ",
                      scales::comma(nrow(givenConceptId)),
                      " concept ids."))
    
    medDraRelationship <- getMedraRelationship(
      conceptIds = givenConceptId$medDraConceptId,
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    
    writeLines(
      paste0(
        "Found ",
        scales::comma(nrow(medDraRelationship$givenConceptId)),
        " MedDRA concepts in the given ",
        scales::comma(length(givenConceptId$medDraConceptId)),
        " concept ids."
      )
    )
    
    if (nrow(medDraRelationship$givenConceptId) == 0) {
      return(NULL)
    }
    
    writeLines(paste0("Found ", scales::comma(
      nrow(
        medDraRelationship$givenConceptId %>% dplyr::filter(.data$givenConceptClassId == 'LLT')
      )
    ), " MedDRA LLT concepts."))
    
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
    
    medDraSynonyms <- dplyr::bind_rows(
      givenConceptId %>%
        dplyr::inner_join(
          medDraRelationship$givenConceptId %>%
            dplyr::select(.data$givenConceptId,
                          .data$givenConceptName) %>%
            rename(
              "medDraConceptId" = .data$givenConceptId,
              "medDraConceptName" = .data$givenConceptName
            ) %>%
            dplyr::distinct(),
          by = "medDraConceptId"
        ),
      givenConceptId %>%
        dplyr::inner_join(
          medDraRelationship$pt %>%
            dplyr::select(.data$givenConceptId,
                          .data$ptConceptName) %>%
            rename(
              "medDraConceptId" = .data$givenConceptId,
              "medDraConceptName" = .data$ptConceptName
            ) %>%
            dplyr::distinct(),
          by = "medDraConceptId"
        ),
      givenConceptId %>%
        dplyr::inner_join(
          medDraRelationship$llt %>%
            dplyr::select(.data$givenConceptId,
                          .data$lltConceptName) %>%
            rename(
              "medDraConceptId" = .data$givenConceptId,
              "medDraConceptName" = .data$lltConceptName
            ) %>%
            dplyr::distinct(),
          by = "medDraConceptId"
        )
    ) %>%
      dplyr::distinct() %>%
      dplyr::arrange(.data$medDraConceptId)
    
    writeLines("Looking for snomed that are descendants to MedDRA in concept ancestor table.")
    # since MedDRA is a classifier for SNOMED, snomed is a descendant to MedDRA
    medDRADescendants <- getConceptDescendant(
      conceptIds = givenConceptId$medDraConceptId,
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    # filtering to snomed will be done later because we dont have conceptId details yet.
    
    writeLines("Looking for snomed that are related to MedDRA in concept relationship table.")
    
    # but sometimes we may have a relationship in concept relationship but not in concept ancestor - because the related concept is deprecated
    medDraRelated <- getConceptRelationship(
      conceptIds = c(
        givenConceptId$medDraConceptId,
        medDRADescendants$descendantConceptId,
        medDRADescendants$ancestorConceptId,
        0
      ) %>% unique(),
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    medDraRelated <- medDraRelated %>%
      dplyr::filter(.data$conceptId1 %in% c(givenConceptId$medDraConceptId)) %>%
      dplyr::filter(.data$relationshipId %in% c("MedDRA - SNOMED eq"))
    
    # do a second concept relationship call. This will be check if any of the snomed are related to another snomed
    # reason: if the MedDRA - SNOMED eq from prior step brings non-standard snomed, we need to map to standard
    conceptRelationship <- getConceptRelationship(
      conceptIds = c(medDraRelated$conceptId2, 0) %>% unique(),
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    ) %>%
      dplyr::filter(.data$conceptId1 %in% c(medDraRelated$conceptId2)) %>%
      dplyr::filter(.data$relationshipId %in% c("Maps to"))
    
    # getting details of concept ids
    conceptIdDetails <- getConceptIdDetails(
      conceptIds = c(
        conceptIds,
        givenConceptId$medDraConceptId,
        medDRADescendants$ancestorConceptId,
        medDRADescendants$descendantConceptId,
        medDraRelated$conceptId1,
        medDraRelated$conceptId2,
        conceptRelationship$conceptId1,
        conceptRelationship$conceptId2
      ) %>% unique() %>% sort(),
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    
    descendantsMappedToSnoMed <-
      medDRADescendants %>%
      dplyr::inner_join(
        conceptIdDetails %>%
          dplyr::filter(.data$vocabularyId == 'SNOMED') %>%
          dplyr::select(.data$conceptId) %>%
          dplyr::distinct(),
        by = c("descendantConceptId" = "conceptId")
      ) %>%
      dplyr::select(
        .data$ancestorConceptId,
        .data$descendantConceptId,
        .data$minLevelsOfSeparation,
        .data$maxLevelsOfSeparation
      ) %>%
      dplyr::rename(
        "medDraConceptId" = .data$ancestorConceptId,
        "snomedConceptId" = .data$descendantConceptId
      )
    
    relatedToSnomed <-
      medDraRelated %>%
      dplyr::select(.data$conceptId1,
                    .data$conceptId2) %>%
      dplyr::distinct() %>%
      dplyr::inner_join(
        conceptIdDetails %>%
          dplyr::filter(.data$vocabularyId == 'SNOMED') %>%
          dplyr::select(
            .data$conceptId,
            .data$standardConcept,
            .data$invalidReason
          ),
        by = c("conceptId2" = "conceptId")
      ) %>%
      dplyr::rename(
        "medDraConceptId" = .data$conceptId1,
        "snomedConceptId" = .data$conceptId2
      )
    
    relatedToSnomedWithValid <- relatedToSnomed %>%
      dplyr::filter(is.na(.data$invalidReason)) %>%
      dplyr::select(-.data$invalidReason)
    
    relatedToSnomedWithInvalid <- relatedToSnomed %>%
      dplyr::filter(!is.na(.data$invalidReason)) %>%
      dplyr::select(.data$medDraConceptId,
                    .data$snomedConceptId) %>%
      dplyr::rename("invalidSnomedConceptId" = .data$snomedConceptId) %>%
      dplyr::inner_join(
        conceptRelationship %>%
          dplyr::select(.data$conceptId1,
                        .data$conceptId2),
        by = c("invalidSnomedConceptId" = "conceptId1")
      ) %>%
      dplyr::rename("snomedConceptId" = .data$conceptId2) %>%
      dplyr::select(-.data$invalidSnomedConceptId)
    
    
    finalMappedConcepts <- dplyr::bind_rows(
      dplyr::bind_rows(relatedToSnomedWithInvalid,
                       relatedToSnomedWithValid) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          minLevelsOfSeparation = 0,
          maxLevelsOfSeparation = 0
        ),
      descendantsMappedToSnoMed
    ) %>%
      dplyr::distinct()
    
    writeLines("Get snomed concept synonyms")
    snomedSynonyms <- getConceptSynonym(
      conceptIds = c(finalMappedConcepts$snomedConceptId %>% unique()),
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    
    snomedSynonyms <-
      dplyr::bind_rows(
        finalMappedConcepts %>%
          dplyr::select(.data$snomedConceptId) %>%
          dplyr::rename("conceptId" = .data$snomedConceptId) %>%
          dplyr::distinct() %>%
          dplyr::inner_join(
            conceptIdDetails %>%
              dplyr::select(.data$conceptId,
                            .data$conceptName),
            by = "conceptId"
          ),
        finalMappedConcepts %>%
          dplyr::select(.data$snomedConceptId) %>%
          dplyr::rename("conceptId" = .data$snomedConceptId) %>%
          dplyr::distinct() %>%
          dplyr::inner_join(
            snomedSynonyms %>%
              dplyr::select(.data$conceptId,
                            .data$conceptSynonymName) %>%
              dplyr::distinct() %>%
              dplyr::rename("conceptName" = .data$conceptSynonymName),
            by = "conceptId"
          )
      ) %>%
      dplyr::distinct() %>%
      dplyr::rename(
        "snomedConceptId" = .data$conceptId,
        "snomedConceptName" = .data$conceptName
      ) %>%
      dplyr::arrange(.data$snomedConceptId)
    
    # walk back LLT from PT
    finalMappedConcepts <-
      dplyr::bind_rows(
        finalMappedConcepts %>%
          dplyr::anti_join(
            lltToPt %>%
              dplyr::select(.data$medDraConceptId) %>%
              dplyr::distinct(),
            by = "medDraConceptId"
          ),
        finalMappedConcepts %>%
          dplyr::inner_join(lltToPt,
                            by = "medDraConceptId") %>%
          dplyr::select(-.data$medDraConceptId) %>%
          dplyr::rename("medDraConceptId" = .data$givenConceptId)
      ) %>%
      dplyr::select(
        .data$medDraConceptId,
        .data$snomedConceptId,
        .data$minLevelsOfSeparation,
        .data$maxLevelsOfSeparation
      ) %>%
      dplyr::distinct() %>%
      dplyr::arrange(
        .data$medDraConceptId,
        .data$snomedConceptId,
        .data$minLevelsOfSeparation,
        .data$maxLevelsOfSeparation
      )
    
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
      )
    
    computingStringDistanceScores <- finalMappedConcepts %>%
      dplyr::select(.data$medDraConceptId,
                    .data$snomedConceptId) %>%
      dplyr::distinct() %>% #expand the medDra
      dplyr::inner_join(medDraSynonyms,
                        by = "medDraConceptId") %>% #expand the snomed
      dplyr::inner_join(snomedSynonyms,
                        by = "snomedConceptId")
    
    computingStringDistanceScores <-
      computingStringDistanceScores %>%
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
      dplyr::select(.data$medDraConceptId,
                    .data$snomedConceptId,
                    .data$stringDistanceScore) %>%
      dplyr::distinct()
    
    mappedUsingVocabaulary <- mappedUsingVocabaulary %>%
      dplyr::left_join(computingStringDistanceScores,
                       by = c("medDraConceptId",
                              "snomedConceptId")) %>%
      tidyr::replace_na(list(stringDistanceScore = 999)) %>%
      dplyr::group_by(.data$medDraConceptId) %>%
      dplyr::arrange(
        .data$minLevelsOfSeparation,
        .data$maxLevelsOfSeparation,
        .data$stringDistanceScore
      ) %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$medDraConceptId,
                    .data$snomedConceptId,
                    .data$rank) %>%
      dplyr::distinct()
    
    writeLines("remove any descendants of snomed when parent is mapped")
    conceptAncestorsForAllSnomed <- getConceptAncestor(
      conceptIds = mappedUsingVocabaulary$snomedConceptId %>% unique(),
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    
    listOfSnomeds <- mappedUsingVocabaulary %>%
      dplyr::select(.data$medDraConceptId,
                    .data$snomedConceptId) %>%
      dplyr::distinct() %>%
      dplyr::arrange()
    
    conceptAncestorsForAllSnomedRanked <- conceptAncestorsForAllSnomed %>%
      dplyr::inner_join(listOfSnomeds,
                        by = c("descendantConceptId" = "snomedConceptId")) %>%
      dplyr::inner_join(listOfSnomeds,
                        by = c("ancestorConceptId" = "snomedConceptId",
                               "medDraConceptId")) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$medDraConceptId,
                      .data$descendantConceptId) %>% 
      dplyr::arrange(dplyr::desc(.data$maxLevelsOfSeparation),
                     dplyr::desc(.data$minLevelsOfSeparation)) %>% 
      dplyr::mutate(ancestorRank = dplyr::row_number()) %>% 
      dplyr::arrange(.data$medDraConceptId,
                     .data$descendantConceptId,
                     .data$ancestorRank)
    
    canBeRolledUp <-
      mappedUsingVocabaulary %>%
      dplyr::inner_join(
        conceptAncestorsForAllSnomedRanked,
        by = c("snomedConceptId" = "descendantConceptId",
               "medDraConceptId")
      ) %>%
      dplyr::rename("descendantConceptId" = .data$snomedConceptId) %>%
      dplyr::select(.data$medDraConceptId,
                    .data$descendantConceptId,
                    .data$ancestorConceptId,
                    .data$ancestorRank) %>%
      dplyr::distinct()
    
    cannotBeRolledUp <-
      mappedUsingVocabaulary %>%
      dplyr::anti_join(
        canBeRolledUp %>%
          dplyr::select(.data$descendantConceptId,
                        .data$medDraConceptId) %>%
          dplyr::rename("snomedConceptId" = .data$descendantConceptId) %>%
          dplyr::distinct(),
        by = c("snomedConceptId", "medDraConceptId")
      ) %>%
      dplyr::select(.data$medDraConceptId,
                    .data$snomedConceptId) %>%
      dplyr::distinct()
    
    rolledUp <- canBeRolledUp %>%
      dplyr::inner_join(
        canBeRolledUp %>%
          dplyr::select(
            .data$medDraConceptId,
            .data$descendantConceptId,
            .data$ancestorRank
          ) %>%
          dplyr::group_by(.data$medDraConceptId,
                          .data$descendantConceptId) %>%
          dplyr::summarise(ancestorRank = min(.data$ancestorRank)),
        by = c("medDraConceptId",
               "descendantConceptId",
               "ancestorRank")
      ) %>% 
      dplyr::select(.data$medDraConceptId,
                    .data$ancestorConceptId) %>% 
      dplyr::rename("snomedConceptId" = .data$ancestorConceptId) %>%
      dplyr::arrange(.data$medDraConceptId) %>% 
      dplyr::distinct()

    reRank <-
      suppressWarnings(
        dplyr::bind_rows(
          rolledUp %>%
            dplyr::inner_join(
              mappedUsingVocabaulary,
              by = c("medDraConceptId", "snomedConceptId")
            ) %>%
            dplyr::distinct() %>%
            dplyr::group_by(.data$medDraConceptId,
                            .data$snomedConceptId) %>%
            dplyr::summarise(rank = min(.data$rank)) %>%
            dplyr::ungroup(),
          
          cannotBeRolledUp %>%
            dplyr::inner_join(
              mappedUsingVocabaulary,
              by = c("medDraConceptId", "snomedConceptId")
            ) %>%
            dplyr::distinct() %>%
            dplyr::group_by(.data$medDraConceptId,
                            .data$snomedConceptId) %>%
            dplyr::summarise(rank = min(.data$rank)) %>%
            dplyr::ungroup()
        )
      ) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$medDraConceptId) %>%
      dplyr::arrange(.data$rank) %>%
      dplyr::mutate(rn = dplyr::row_number()) %>%
      dplyr::select(-.data$rank) %>%
      dplyr::rename(rank = .data$rn) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data$medDraConceptId)
    
    mappedUsingVocabaulary <- dplyr::bind_rows(rolledUp,
                                               cannotBeRolledUp) %>%
      dplyr::distinct() %>%
      dplyr::left_join(reRank,
                       by = c("medDraConceptId", "snomedConceptId")) %>%
      dplyr::inner_join(
        conceptIdDetails %>%
          dplyr::select(
            .data$conceptId,
            .data$conceptName,
            .data$domainId,
            .data$conceptClassId,
            .data$standardConcept,
            .data$invalidReason
          ) %>%
          dplyr::distinct() %>%
          dplyr::rename(
            "snomedConceptId" = .data$conceptId,
            "snomedConceptName" = .data$conceptName,
            "snomedDomainId" = .data$domainId,
            "snomedConceptClassId" = .data$conceptClassId,
            "snomedStandardConcept" = .data$standardConcept,
            "snomedInvalidReason" = .data$invalidReason
          ),
        by = "snomedConceptId"
      )
    
    mappedUsingVocabaulary <- medDraRelationship$givenConceptId %>%
      dplyr::distinct() %>%
      dplyr::rename(
        "medDraConceptId" = .data$givenConceptId,
        "medDraConceptName" = .data$givenConceptName,
        "medDraVocabularyId" = .data$givenVocabularyId,
        "medDraDomainId" = .data$givenDomainId,
        "medDraConceptClassId" = .data$givenConceptClassId,
        "medDraInvalidReason" = .data$givenConceptIdInvalidReason
      ) %>%
      dplyr::left_join(mappedUsingVocabaulary,
                       by = "medDraConceptId") %>%
      dplyr::arrange(
        .data$medDraConceptId,
        .data$medDraConceptName,
        .data$medDraConceptClassId,
        .data$rank
      ) %>%
      dplyr::select(dplyr::starts_with(c("medDra", "snomed")),
                    .data$rank)
    
    return(mappedUsingVocabaulary)
  }
