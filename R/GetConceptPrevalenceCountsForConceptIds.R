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


#' Get the count for an array of concept id(s) from concept prevalence table.
#'
#' @template Connection
#'
#' @template ConnectionDetails
#'
#' @template ConceptIds
#'
#' @param conceptPrevalenceSchema   The schema containing the concept prevalence data.
#'
#' @export
getConceptPrevalenceCountsForConceptIds <- function(conceptIds,
                                                    connection = NULL,
                                                    connectionDetails = NULL,
                                                    conceptPrevalenceSchema = 'concept_prevalence') {
  if (length(conceptIds) == 0) {
    stop('No concept id provided')
  }
  
  sql <- "select *
          from @concept_prevalence.cp_master
          where concept_id in (@concept_ids);"
  
  data <-
    renderTranslateQuerySql(
      connection = connection,
      connectionDetails = connectionDetails,
      concept_ids = conceptIds,
      concept_prevalence = conceptPrevalenceSchema,
      sql = sql,
      snakeCaseToCamelCase = TRUE
    ) %>%
    dplyr::arrange(1)
  return(data)
}
