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

#' convert a concept set expression R list object into a data frame object
#'
#' @description
#' convert a concept set expression R list object into a data frame object
#'
#' @template Connection
#'
#'
#' @template UpdateVocabularyFields
#'
#' @template VocabularyDatabaseSchema
#'
#' @template conceptSetExpression
#'
#' @template TempEmulationSchema
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
convertConceptSetExpressionToDataFrame <-
  function(conceptSetExpression,
           updateVocabularyFields = FALSE,
           connection = NULL,
           connectionDetails = NULL,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           vocabularyDatabaseSchema = NULL) {
    if (length(conceptSetExpression) == 0) {
      stop(
        "Concept set expression was found to have a length of 0. No concept set expression found."
      )
    }

    if ("items" %in% names(conceptSetExpression)) {
      items <- conceptSetExpression$items
    } else {
      items <- conceptSetExpression
    }

    items2 <- list()

    errorMessage <-
      "Given concept set expression R list object does not conform to expected structure. \n
                      It is a vector that is more than 3 levels deep."
    
    for (i in (1:length(items))) {
      df <- as.data.frame(items[[i]]) |>
        dplyr::tibble()
      names(df) <- stringr::str_replace(string = tolower(names(df)),
                                        pattern = "concept.",
                                        replacement = "")
      
      if ('isExcluded' %in% names(df)) {
        df <- df |>
          dplyr::rename("is_excluded" = "isExcluded")
      } else if ('isexcluded' %in% names(df)) {
        df <- df |>
          dplyr::rename("is_excluded" = "isexcluded")
      } else {
        df <- df |>
          dplyr::mutate(is_excluded = FALSE)
      }
      
      if ('includeMapped' %in% names(df)) {
        df <- df |>
          dplyr::rename("include_mapped" = "includeMapped")
      } else if ('includemapped' %in% names(df)) {
        df <- df |>
          dplyr::rename("include_mapped" = "includemapped")
      } else {
        df <- df |>
          dplyr::mutate(include_mapped = FALSE)
      }
      
      if ('includeDescendants' %in% names(df)) {
        df <- df |>
          dplyr::rename("include_descendants" = "includeDescendants")
      } else if ('includedescendants' %in% names(df)) {
        df <- df |>
          dplyr::rename("include_descendants" = "includedescendants")
      } else {
        df <- df |>
          dplyr::mutate(include_descendants = FALSE)
      }
      items2[[i]] <- df
    }
    
    conceptSetExpressionDetails <- dplyr::bind_rows(items2) |> 
      SqlRender::snakeCaseToCamelCaseNames() |> 
      tidyr::replace_na(
        replace = list(
          isExcluded = FALSE,
          includeDescendants = FALSE,
          includeMapped = FALSE
        )
      )

    if (updateVocabularyFields) {
      if (is.null(vocabularyDatabaseSchema)) {
        stop(
          "VocabularyDatabaseSchema with OMOP vocabulary tables is needed to update Vocabulary details."
        )
      }
      if (is.null(connection)) {
        connection <- DatabaseConnector::connect(connectionDetails)
        on.exit(DatabaseConnector::disconnect(connection))
      }

      details <- getConceptIdDetails(
        connection = connection,
        connectionDetails = connectionDetails,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
        conceptIds = conceptSetExpressionDetails$conceptId |> unique(),
        tempEmulationSchema = tempEmulationSchema
      )
      conceptSetExpressionDetails <-
        conceptSetExpressionDetails |>
        dplyr::select(
          .data$conceptId,
          .data$includeDescendants,
          .data$includeMapped,
          .data$isExcluded
        ) |>
        dplyr::left_join(y = details, by = "conceptId")

      conceptSetExpressionDetails <-
        tidyr::replace_na(
          data = conceptSetExpressionDetails,
          replace = list(invalidReason = "V")
        ) |>
        dplyr::mutate(
          invalidReasonCaption = dplyr::case_when(
            invalidReason == "V" ~ "Valid",
            invalidReason == "D" ~ "Deleted",
            invalidReason == "U" ~ "Updated",
            TRUE ~ "Valid"
          )
        ) |>
        dplyr::mutate(
          standardConceptCaption = dplyr::case_when(
            standardConcept == "S" ~ "Standard",
            standardConcept == "C" ~ "Classification",
            TRUE ~ "Non-standard"
          )
        )
    }

    if ("standardConceptCaption" %in% colnames(conceptSetExpressionDetails) &&
      !"standardConcept" %in% colnames(conceptSetExpressionDetails)) {
      conceptSetExpressionDetails <- conceptSetExpressionDetails |>
        dplyr::mutate(
          standardConcept = dplyr::case_when(
            .data$standardConceptCaption == "Standard" ~ "S",
            .data$standardConceptCaption == "Classification" ~ "C"
          )
        )
    }
    if ("standardConcept" %in% colnames(conceptSetExpressionDetails) &&
      !"standardConceptCaption" %in% colnames(conceptSetExpressionDetails)) {
      conceptSetExpressionDetails <- conceptSetExpressionDetails |>
        dplyr::mutate(
          standardConceptCaption = dplyr::case_when(
            .data$standardConcept == "S" ~ "Standard",
            .data$standardConcept == "C" ~ "Classification",
            TRUE ~ "Non-Standard"
          )
        )
    }

    conceptSetExpressionDetails <- conceptSetExpressionDetails |>
      dplyr::relocate(
        dplyr::all_of(c(
          "includeDescendants", "includeMapped", "isExcluded"
        )),
        .after = dplyr::last_col()
      ) |>
      dplyr::relocate("conceptId")

    return(conceptSetExpressionDetails)
  }
