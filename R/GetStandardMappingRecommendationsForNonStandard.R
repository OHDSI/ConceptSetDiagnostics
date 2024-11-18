# Copyright 2024 Observational Health Data Sciences and Informatics
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


#' Get standard mapping recommendations for non standard.
#'
#' @description
#' Get standard mapping recommendations for non standard.
#'
#' @template Connection
#'
#' @template VocabularyDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @return
#' Returns a list of objects (to be described.)
#'
#' @export
getStandardMappingRecommendationsForNonStandard <- function(connectionDetails = NULL,
                                                            connection = NULL,
                                                            vocabularyDatabaseSchema = cdmDatabaseSchema,
                                                            cdmDatabaseSchema = NULL,
                                                            tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                                            sourceVocabularyId = c("ICD10CM"),
                                                            sourceCodes,
                                                            removeSpecialCharacters = TRUE,
                                                            includeDescendants = FALSE) {
  if (is.null(vocabularyDatabaseSchema)) {
    stop("vocabularyDatabaseSchema cannot be NULL.")
  }
  
  output <- c()
  
  vocabularyIdsToFilter <- quoteAndJoinArray(sourceVocabularyId |> unique())
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(
      DatabaseConnector::dropEmulatedTempTables(connection = connection, tempEmulationSchema = tempEmulationSchema)
    )
    on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  }
  
  # Retrieve vocabulary IDs from OMOP to ensure consistency.
  omopVocabularyId <- ConceptSetDiagnostics::getVocabulary(
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema
  )
  
  #check if the request is for a vocabulary Id that does not map to OMOP.
  sourceVocabularyIdNotInOmop <- setdiff(sourceVocabularyId, omopVocabularyId$vocabularyId)
  
  if (length(sourceVocabularyIdNotInOmop) > 1) {
    stop(paste0(
      "The following sourceVocabularyId is not in OMOP. ",
      paste0(sourceVocabularyIdNotInOmop, collapse = ", ")
    ))
  }
  
  
  #download from remote vocabulary table a subset of omop concepts to map.
  omopVocabularyToMatch <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT concept_id, concept_code, vocabulary_id
            FROM @vocabulary_database_schema.concept
            WHERE vocabulary_id IN (@vocabulary_ids);",
    snakeCaseToCamelCase = TRUE,
    vocabulary_database_schema = vocabularyDatabaseSchema,
    vocabulary_ids = vocabularyIdsToFilter,
    tempEmulationSchema = tempEmulationSchema
  ) |>
    dplyr::tibble()
  
  
  #do the fuzzy match - but we expect to 100%match for this work.
  #message will be shown if there are approximate match
  fuzzyMatch <- getFuzzyMatchOfCodesToOmopConceptCode(
    sourceCodes = sourceCodes,
    omopConcepts = omopVocabularyToMatch,
    removeSpecialCharacters = removeSpecialCharacters
  )
  
  #only use perfect match moving forward, return all matches to user at codesWithConceptId. return all approximate
  # matches as FYI. they are ignored moving forward.
  
  output$codesWithConceptId <- fuzzyMatch$perfectMatch
  output$codesWithConceptIdApproximate <- fuzzyMatch$approximateMatch
  
  #only use perfect match moving forward
  
  # find standard mapping for non standard
  mappedStandard <- ConceptSetDiagnostics::getMappedStandardConcepts(
    conceptIds = output$codesWithConceptId$conceptId |> unique(),
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema
  )
  
  #are there any source codes without mapping? there should not be
  output$unmappedCodes <- output$codesWithConceptId |>
    dplyr::anti_join(
      mappedStandard |>
        dplyr::select(givenConceptId) |>
        dplyr::rename(conceptId = givenConceptId) |>
        dplyr::distinct()
    )
  
  if (nrow(output$unmappedCodes) > 0) {
    message("There are concept id without mapped standard. Please look at unmappedCodes.")
  }
  
  # a source concept may be mapped to more than one standard. count if that is occurring
  numberOfMappedStandardConceptsMappedToGivenSourceDf <-
    mappedStandard |>
    dplyr::select(givenConceptId, conceptId) |>
    dplyr::distinct() |>
    dplyr::group_by(givenConceptId) |>
    dplyr::summarise(numberOfMappedStandardConceptsMappedToGivenSource = n())
  
  mappedStandard <- mappedStandard |>
    dplyr::left_join(numberOfMappedStandardConceptsMappedToGivenSourceDf, by = "givenConceptId")
  
  
  if (includeDescendants) {
    # get descendants of all standard concepts
    descendantsOfStandardConcept <- ConceptSetDiagnostics::getConceptDescendant(
      conceptIds = mappedStandard$conceptId,
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    ) |>
      dplyr::filter(minLevelsOfSeparation > 0) |>
      dplyr::select(ancestorConceptId, descendantConceptId) |>
      dplyr::distinct()
    
    #get mapped concept for the standard esp descendants
    mappedSource <- ConceptSetDiagnostics::getMappedSourceConcepts(
      conceptIds = c(
        mappedStandard$conceptId,
        descendantsOfStandardConcept$descendantConceptId
      ) |> unique(),
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema
    )
    
    #filter to desired vocabulary
    mappedSourceFiltered <- mappedSource |>
      dplyr::filter(vocabularyId %in% c(sourceVocabularyId)) |>
      dplyr::left_join(
        output$codesWithConceptId |>
          dplyr::select(conceptId) |>
          dplyr::distinct() |>
          dplyr::mutate(isInputConceptId = 1)
      ) |>
      tidyr::replace_na(list(isInputConceptId = 0))
    
  } else {
    #get mapped concept for the standard esp descendants
    mappedSource <- ConceptSetDiagnostics::getMappedSourceConcepts(
      conceptIds = c(mappedStandard$conceptId) |> unique(),
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema
    )
    
    #filter to desired vocabulary
    mappedSourceFiltered <- mappedSource |>
      dplyr::filter(vocabularyId %in% c(sourceVocabularyId)) |>
      dplyr::left_join(
        output$codesWithConceptId |>
          dplyr::select(conceptId) |>
          dplyr::distinct() |>
          dplyr::mutate(isInputConceptId = 1)
      ) |>
      tidyr::replace_na(list(isInputConceptId = 0))
  }
  
  #find all concept id and get their detail
  conceptIds <- c(
    output$codesWithConceptId$conceptId,
    mappedStandard$conceptId,
    mappedSource$conceptId
  ) |>
    unique()
  
  output$conceptIdDetails <- ConceptSetDiagnostics::getConceptIdDetails(
    conceptIds = conceptIds,
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
  ) |>
    dplyr::arrange(conceptId)
  
  #this is the main output. it has the source and mapped standard
  output$sourceMappedToStandard <- mappedStandard |>
    dplyr::rename(sourceConceptId = givenConceptId, standardConceptId = conceptId) |>
    dplyr::select(
      sourceConceptId,
      standardConceptId,
      numberOfMappedStandardConceptsMappedToGivenSource
    ) |>
    dplyr::left_join(
      output$conceptIdDetails |>
        dplyr::rename(
          sourceConceptId = conceptId,
          sourceConceptName = conceptName,
          sourceConceptCode = conceptCode,
          sourceVocabularyId = vocabularyId
        ) |>
        dplyr::select(dplyr::starts_with("source")),
      by = "sourceConceptId"
    ) |>
    dplyr::left_join(
      output$conceptIdDetails |>
        dplyr::rename(
          standardConceptId = conceptId,
          standardConceptName = conceptName,
          standardConceptCode = conceptCode,
          standardVocabularyId = vocabularyId
        ) |>
        dplyr::select(dplyr::starts_with("standard")),
      by = "standardConceptId"
    ) |>
    dplyr::relocate(dplyr::starts_with("source"),
                    dplyr::starts_with("standard")) |>
    dplyr::arrange(sourceConceptId) |>
    dplyr::mutate(
      stringDistanceSourceToStandard = stringdist::stringdist(sourceConceptName, standardConceptName, method = "lcs")
    ) |>
    dplyr::left_join(
      mappedSourceFiltered |>
        dplyr::rename(standardConceptId = givenConceptId) |>
        dplyr::group_by(standardConceptId) |>
        dplyr::summarise(mappedNonStandardDirect = n()),
      by = "standardConceptId"
    ) |>
    dplyr::left_join(
      mappedSourceFiltered |>
        dplyr::rename(standardConceptId = givenConceptId) |>
        dplyr::group_by(standardConceptId) |>
        dplyr::summarise(mappedNonStandardDirectCodes = paste0(conceptCode, collapse = "\n")),
      by = "standardConceptId"
    ) |>
    dplyr::left_join(
      mappedSourceFiltered |>
        dplyr::rename(standardConceptId = givenConceptId) |>
        dplyr::group_by(standardConceptId) |>
        dplyr::summarise(mappedNonStandardDirectNames = paste0(conceptName, collapse = "\n")),
      by = "standardConceptId"
    ) |>
    dplyr::left_join(
      mappedSourceFiltered |>
        dplyr::rename(standardConceptId = givenConceptId) |>
        dplyr::filter(isInputConceptId == 0) |>
        dplyr::group_by(standardConceptId) |>
        dplyr::summarise(mappedNonStandardDirectNotInInput = n()),
      by = "standardConceptId"
    ) |>
    dplyr::left_join(
      mappedSourceFiltered |>
        dplyr::rename(standardConceptId = givenConceptId) |>
        dplyr::filter(isInputConceptId == 0) |>
        dplyr::group_by(standardConceptId) |>
        dplyr::summarise(
          mappedNonStandardDirectCodesNotInInput = paste0(conceptCode, collapse = "\n")
        ),
      by = "standardConceptId"
    ) |>
    dplyr::left_join(
      mappedSourceFiltered |>
        dplyr::rename(standardConceptId = givenConceptId) |>
        dplyr::filter(isInputConceptId == 0) |>
        dplyr::group_by(standardConceptId) |>
        dplyr::summarise(
          mappedNonStandardDirectNamesNotInInput = paste0(conceptName, collapse = "\n")
        ),
      by = "standardConceptId"
    )
  
  return(output)
  
}
