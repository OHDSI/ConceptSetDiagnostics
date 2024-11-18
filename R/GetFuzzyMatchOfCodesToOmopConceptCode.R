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


#' Peform fuzzy match of codes.
#'
#' @description
#' Given two data frames, one with codes from external source (e.g. read from spreadsheet) vs
#' another data frame with OMOP vocabulary, perform fuzzy string match.
#'
#' @param sourceCodes An array of codes to match to omop.
#'
#' @param omopConcepts A data frame with atleast conceptId, conceptCode
#'
#' @param removeSpecialCharacters During matching do you want to remove special characters.e.g. when
#'        source codes have period removed, but they exist in omop vocabulary concept code.
#'
#' @return
#' Returns a list of objects (to be described.)
#'
#' @export
getFuzzyMatchOfCodesToOmopConceptCode <- function(sourceCodes,
                                                  omopConcepts,
                                                  removeSpecialCharacters = TRUE) {
  sourceDf <- dplyr::tibble(conceptCodeSourceOriginal = sourceCodes) |>
    dplyr::distinct() |>
    dplyr::mutate(conceptCodeSource = conceptCodeSourceOriginal)
  
  targetDf <- omopConcepts |>
    dplyr::rename(conceptCodeOmopOriginal = conceptCode) |>
    dplyr::distinct() |>
    dplyr::mutate(conceptCodeOmop = conceptCodeOmopOriginal)
  
  if (removeSpecialCharacters) {
    #note: in current implementation removeSpecialCharacters only removes periods. This is mostly useful in ICD codes.
    sourceDf <- sourceDf |>
      dplyr::mutate(
        conceptCodeSource = stringr::str_remove_all(string = conceptCodeSourceOriginal, pattern = stringr::fixed("."))
      )
    
    targetDf <- targetDf |>
      dplyr::mutate(
        conceptCodeOmop = stringr::str_remove_all(string = conceptCodeOmopOriginal, pattern = stringr::fixed("."))
      )
  }
  
  codesWithConceptId <- fuzzyStringJoinDataFrame(
    df1 = sourceDf,
    df2 = targetDf,
    field1 = "conceptCodeSource",
    field2 = "conceptCodeOmop"
  ) |>
    dplyr::distinct()
  
  output <- c()
  
  # find imperfect matches
  output$approximateMatch <- codesWithConceptId |>
    dplyr::filter(conceptCodeSource != conceptCodeOmop)
  output$perfectMatch <- codesWithConceptId |>
    dplyr::filter(conceptCodeSource == conceptCodeOmop)
  
  output$approximateMatch <- output$approximateMatch |>
    dplyr::anti_join(output$perfectMatch |>
                       dplyr::select(conceptCodeSource) |>
                       dplyr::distinct())
  
  if (nrow(output$approximateMatch) > 0) {
    message("There are codes without perfect match. Please look at approximateMatch in output.")
  }
  
  return(output)
  
}