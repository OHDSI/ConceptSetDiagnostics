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

#' get MedDRA relationship
#'
#' @description
#' given an array of conceptIds belonging to MedDRA vocabulary get its full MedDRA relationship
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
#' Returns a a list of tibble data frames
#' conceptId,
#' socConceptId, socConceptName,
#' HLTConceptId, HltConceptName,
#' HlgtConceptId, hlgtConceptName,
#' ptConceptId, ptConceptName,
#' lltConceptId, lltConceptName
#'
#' @export
getMedraRelationship <-
  function(conceptIds,
           connection = NULL,
           connectionDetails = NULL,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           vocabularyDatabaseSchema = "vocabulary") {
    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    conceptAncestor <- getConceptAncestor(
      conceptIds = conceptIds,
      connection = connection,
      tempEmulationSchema = tempEmulationSchema,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )

    conceptDescendants <- getConceptDescendant(
      conceptIds = conceptIds,
      connection = connection,
      tempEmulationSchema = tempEmulationSchema,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )

    conceptRelationship <- getConceptRelationship(
      conceptIds = conceptIds,
      connection = connection,
      tempEmulationSchema = tempEmulationSchema,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )

    conceptIdDetails <- getConceptIdDetails(
      conceptIds = c(
        conceptIds,
        conceptAncestor$ancestorConceptId,
        conceptAncestor$descendantConceptId,
        conceptDescendants$ancestorConceptId,
        conceptDescendants$descendantConceptId,
        conceptRelationship$conceptId1,
        conceptRelationship$conceptId2
      ) |> unique(),
      connection = connection,
      tempEmulationSchema = tempEmulationSchema,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    givenConceptId <- dplyr::tibble(givenConceptId = conceptIds |> unique()) |>
      dplyr::filter(.data$givenConceptId > 0) |>
      dplyr::inner_join(
        conceptIdDetails |>
          dplyr::rename(
            "givenConceptName" = .data$conceptName,
            "givenConceptId" = .data$conceptId,
            "givenVocabularyId" = .data$vocabularyId,
            "givenDomainId" = .data$domainId,
            "givenConceptClassId" = .data$conceptClassId,
            "givenConceptClassId" = .data$conceptClassId,
            "givenConceptIdInvalidReason" = .data$invalidReason
          ) |>
          dplyr::select(
            .data$givenConceptId,
            .data$givenConceptName,
            .data$givenVocabularyId,
            .data$givenDomainId,
            .data$givenConceptClassId,
            .data$givenConceptIdInvalidReason
          ),
        by = "givenConceptId"
      )

    mapMedDraAncestor <- function(conceptClass) {
      output <- givenConceptId |>
        dplyr::select(.data$givenConceptId) |>
        dplyr::distinct() |>
        dplyr::inner_join(conceptAncestor,
          by = c("givenConceptId" = "descendantConceptId")
        ) |>
        dplyr::inner_join(
          conceptIdDetails |>
            dplyr::filter(.data$vocabularyId == "MedDRA") |>
            dplyr::filter(is.na(.data$invalidReason)) |>
            dplyr::filter(.data$conceptClassId == !!conceptClass) |>
            dplyr::rename(
              !!paste0(tolower(conceptClass), "ConceptName") := .data$conceptName, !!paste0(tolower(conceptClass), "DomainId") := .data$domainId
            ) |>
            dplyr::select(
              .data$conceptId,
              paste0(tolower(conceptClass), "ConceptName"),
              paste0(tolower(conceptClass), "DomainId")
            ),
          by = c("ancestorConceptId" = "conceptId")
        ) |>
        dplyr::rename(!!paste0(tolower(conceptClass), "ConceptId") := .data$ancestorConceptId) |>
        dplyr::select(-.data$minLevelsOfSeparation, -.data$maxLevelsOfSeparation)
      return(output)
    }
    ancestorSocForGivenConceptId <-
      mapMedDraAncestor(conceptClass = "SOC")
    ancestorHlgtForGivenConceptId <-
      mapMedDraAncestor(conceptClass = "HLGT")
    ancestorHltForGivenConceptId <-
      mapMedDraAncestor(conceptClass = "HLT")
    ancestorPtForGivenConceptId <-
      mapMedDraAncestor(conceptClass = "PT")
    ancestorLltForGivenConceptId <-
      mapMedDraAncestor(conceptClass = "LLT")

    mapMedDraDescendant <- function(conceptClass) {
      output <- givenConceptId |>
        dplyr::select(.data$givenConceptId) |>
        dplyr::distinct() |>
        dplyr::inner_join(conceptDescendants,
          by = c("givenConceptId" = "ancestorConceptId")
        ) |>
        dplyr::inner_join(
          conceptIdDetails |>
            dplyr::filter(.data$vocabularyId == "MedDRA") |>
            dplyr::filter(is.na(.data$invalidReason)) |>
            dplyr::filter(.data$conceptClassId == !!conceptClass) |>
            dplyr::rename(
              !!paste0(tolower(conceptClass), "ConceptName") := .data$conceptName, !!paste0(tolower(conceptClass), "DomainId") := .data$domainId
            ) |>
            dplyr::select(
              .data$conceptId,
              paste0(
                tolower(conceptClass), "ConceptName"
              ),
              paste0(tolower(conceptClass), "DomainId")
            ),
          by = c("descendantConceptId" = "conceptId")
        ) |>
        dplyr::rename(!!paste0(tolower(conceptClass), "ConceptId") := .data$descendantConceptId) |>
        dplyr::select(-.data$minLevelsOfSeparation, -.data$maxLevelsOfSeparation)
      return(output)
    }
    descendantSocForGivenConceptId <-
      mapMedDraDescendant(conceptClass = "SOC")
    descendantHlgtForGivenConceptId <-
      mapMedDraDescendant(conceptClass = "HLGT")
    descendantHltForGivenConceptId <-
      mapMedDraDescendant(conceptClass = "HLT")
    descendantPtForGivenConceptId <-
      mapMedDraDescendant(conceptClass = "PT")
    descendantLltForGivenConceptId <-
      mapMedDraDescendant(conceptClass = "LLT")

    socForGivenConceptId <-
      dplyr::bind_rows(
        ancestorSocForGivenConceptId,
        descendantSocForGivenConceptId
      ) |>
      dplyr::distinct()

    hlgtForGivenConceptId <-
      dplyr::bind_rows(
        ancestorHlgtForGivenConceptId,
        descendantHlgtForGivenConceptId
      ) |>
      dplyr::distinct()

    hltForGivenConceptId <-
      dplyr::bind_rows(
        ancestorHltForGivenConceptId,
        descendantHltForGivenConceptId
      ) |>
      dplyr::distinct()

    ptForGivenConceptId <-
      dplyr::bind_rows(
        ancestorPtForGivenConceptId,
        descendantPtForGivenConceptId
      ) |>
      dplyr::distinct()

    lltForGivenConceptId <-
      dplyr::bind_rows(
        ancestorLltForGivenConceptId,
        descendantLltForGivenConceptId
      ) |>
      dplyr::distinct()

    output <- list(
      givenConceptId = givenConceptId,
      soc = socForGivenConceptId,
      hlgt = hlgtForGivenConceptId,
      hlt = hltForGivenConceptId,
      pt = ptForGivenConceptId,
      llt = lltForGivenConceptId
    )

    return(output)
  }
