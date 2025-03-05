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


#' Get Concept Set Recommender Data
#'
#' This function reads the concept recommender CSV file and returns the data as a data frame.
#' The CSV file is expected to be located in the "csv" folder with the file name
#' "concept_recommended_20240527.csv". After reading the data, the function converts the
#' column names from snake_case to camelCase using \code{SqlRender::snakeCaseToCamelCaseNames}.
#'
#' @return A data frame containing the concept set recommender data.
#'
#' @export
getConceptSetRecommenderData <- function() {
  data <- readr::read_csv(
    file = system.file(
      "csv",
      "concept_recommended_20240527.csv",
      package = utils::packageName()
    ),
    col_types = readr::cols()
  ) |>
    SqlRender::snakeCaseToCamelCaseNames()
  
  return(data)
}
