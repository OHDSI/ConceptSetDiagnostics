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

#' given a search string (s) perform concept set diagnostics
#'
#' @description
#' given an array of comma separated quoted search string (s), this function will perform a series
#' of operations that provides a recommended concept set expression, along with
#' potentially more recommended and orphan concepts.
#'
#' @param searchPhrases        An array of text-phrases within quotations that are separated by commas.
#'
#' @template Connection
#'
#' @template VocabularyDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @template ConceptPrevalenceSchema
#'
#' @param locationForResults  If you want to export results, please provide disk drive location
#'
#' @param vocabularyIdOfInterest A list of vocabulary ids to filter the results.
#'
#' @param domainIdOfInterest     A list of domain ids to filter the results.
#'
#' @param runPhoebeRecommendation      Do you want to run Concept Recommendation using PHOEBE?
#'
#' @param runOrphan                    Do you want to run Orphan Concept text search? This may take time.
#'
#' @export
performConceptSetDiagnostics <-
  function(searchPhrases,
           connection = NULL,
           connectionDetails = NULL,
           conceptPrevalenceSchema = NULL,
           runPhoebeRecommendation = TRUE,
           runOrphan = FALSE,
           locationForResults = NULL,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           vocabularyDatabaseSchema = "vocabulary",
           vocabularyIdOfInterest = c("SNOMED", "HCPCS", "ICD10CM", "ICD10", "ICD9CM", "ICD9", "Read"),
           domainIdOfInterest = c("Condition", "Procedure", "Observation")) {
    writeLines("Beginning Concept Set Diagnostics.")

    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }

    stringSearchResults <- performStringSearchForConcepts(
      searchPhrases = searchPhrases,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
      connection = connection
    )

    conceptSetExpression <-
      dplyr::tibble(
        dplyr::bind_rows(stringSearchResults) %>%
          dplyr::select(
            .data$conceptId,
            .data$conceptName,
            .data$domainId,
            .data$vocabularyId,
            .data$standardConcept,
            .data$standardConceptCaption,
            .data$invalidReason,
            .data$invalidReasonCaption,
            .data$conceptCode,
            .data$conceptClassId
          ) %>%
          dplyr::distinct()
      ) %>%
      convertConceptSetDataFrameToExpression(
        selectAllDescendants = TRUE,
        updateVocabularyFields = FALSE
      )

    writeLines(" - Creating concept sets from returned concepts and Optimizing.")
    # optimize
    optimized <- optimizeConceptSetExpression(
      conceptSetExpression = conceptSetExpression,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      connection = connection
    )

    # remove invalid
    optimizedConceptSetExpression <-
      optimized$optimizedConceptSetExpression %>%
      convertConceptSetExpressionToDataFrame() %>%
      dplyr::filter(.data$invalidReason %in% c("", "V")) %>%
      convertConceptSetDataFrameToExpression()
    rm("optimized")

    # filter to domain of interest
    if (length(domainIdOfInterest) > 0) {
      optimizedConceptSetExpression <- optimizedConceptSetExpression %>%
        convertConceptSetExpressionToDataFrame() %>%
        dplyr::filter(.data$domainId %in% c(domainIdOfInterest)) %>%
        convertConceptSetDataFrameToExpression()
    }

    # filter to vocabulary of interest
    if (length(vocabularyIdOfInterest) > 0) {
      optimizedConceptSetExpression <- optimizedConceptSetExpression %>%
        convertConceptSetExpressionToDataFrame() %>%
        dplyr::filter(.data$vocabularyId %in% c(vocabularyIdOfInterest)) %>%
        convertConceptSetDataFrameToExpression()
    }

    recommended <- NULL
    if (runPhoebeRecommendation) {
      writeLines(" - Finding recommended based on concept prevalence study.")
      recommended <- getRecommendationForConceptSetExpression(
        conceptSetExpression = optimizedConceptSetExpression,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        connection = connection,
        conceptPrevalenceSchema = conceptPrevalenceSchema,
        tempEmulationSchema = tempEmulationSchema
      )
    }

    orphan <- NULL
    if (runOrphan) {
      writeLines(" - Searching for potential orphans using string similarity.")
      orphan <- findOrphanConceptsForConceptSetExpression(
        conceptSetExpression = optimizedConceptSetExpression,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        connection = connection,
        tempEmulationSchema = tempEmulationSchema
      )
    }

    writeLines(" - Resolved concept ids.")
    resolvedConceptIds <-
      resolveConceptSetExpression(
        conceptSetExpression = optimizedConceptSetExpression,
        connection = connection,
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema
      )

    # get counts - from conceptPrevalence and from database

    searchResult <- list(
      searchPhrases = searchPhrases,
      stringSearchResults = stringSearchResults,
      optimizedConceptSetExpression = optimizedConceptSetExpression,
      optimizedConceptSetExpressionDataFrame = optimizedConceptSetExpression %>%
        convertConceptSetExpressionToDataFrame(),
      resolvedConceptIds = resolvedConceptIds,
      recommendedConceptIds = recommended,
      orphanConcepts = orphan
    )

    if (!is.null(locationForResults)) {
      writeLines(" - Writing files to disc.")
      dir.create(
        path = locationForResults,
        showWarnings = FALSE,
        recursive = TRUE
      )
      unlink(
        file.path(
          locationForResults,
          "conceptExpression.json"
        ),
        recursive = TRUE,
        force = TRUE
      )
      unlink(
        file.path(
          locationForResults,
          "conceptExpression.csv"
        ),
        recursive = TRUE,
        force = TRUE
      )

      if (!is.null(optimizedConceptSetExpression)) {
        SqlRender::writeSql(
          sql = RJSONIO::toJSON(
            x = optimizedConceptSetExpression,
            digits = 23,
            pretty = TRUE
          ),
          targetFile = file.path(
            locationForResults,
            "conceptExpression.json"
          )
        )
        readr::write_excel_csv(
          x = convertConceptSetExpressionToDataFrame(conceptSetExpression = optimizedConceptSetExpression),
          file = file.path(
            locationForResults,
            "conceptExpression.csv"
          ),
          na = "",
          append = FALSE
        )
      }


      unlink(
        x = file.path(
          locationForResults,
          paste0("recommendedSource.csv")
        ),
        recursive = TRUE,
        force = TRUE
      )
      unlink(
        x = file.path(
          locationForResults,
          paste0("recommendedStandard.csv")
        ),
        recursive = TRUE,
        force = TRUE
      )
      if (!is.null(recommended)) {
        readr::write_excel_csv(
          x = recommended$recommendedStandard,
          file = file.path(
            locationForResults,
            paste0("recommendedStandard.csv")
          ),
          append = FALSE,
          na = ""
        )
        readr::write_excel_csv(
          x = recommended$recommendedSource,
          file = file.path(
            locationForResults,
            paste0("recommendedSource.csv")
          ),
          append = FALSE,
          na = ""
        )
      }

      unlink(
        x = file.path(
          locationForResults,
          paste0("orphan.csv")
        ),
        recursive = TRUE,
        force = TRUE
      )
      if (!is.null(orphan)) {
        readr::write_excel_csv(
          x = orphan,
          file = file.path(
            locationForResults,
            paste0("orphan.csv")
          ),
          append = FALSE,
          na = ""
        )
      }
    }

    writeLines(" - Complete.")

    return(searchResult)
  }
