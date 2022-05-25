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

#' Get concept set details from cohort definition
#'
#' @description
#' Get concept set details from cohort definition
#'
#' @template CohortExpression
#'
#' @return
#' Returns a tibble data frame.
#'
#' @export
extractConceptSetsInCohortDefinition <-
  function(cohortExpression) {
    if ("expression" %in% names(cohortExpression)) {
      expression <- cohortExpression$expression
    } else {
      expression <- cohortExpression
    }
    
    if (is.null(expression$ConceptSets)) {
      return(NULL)
    }
    
    # use circe to render cohort sql and extract concept set sql
    circeRenderedSqlExpression <-
      getCohortSqlFromCohortDefinition(cohortExpression = expression,
                                                  generateStats = TRUE)
    extractedConceptSetSql <-
      extractConceptSetsSqlFromCohortSql(cohortSql = circeRenderedSqlExpression)
    
    # extract concept set expression from cohort expression
    conceptSetExpression <-
      extractConceptSetExpressionsFromCohortExpression(cohortExpression = expression)
    
    codeSetsInPrimaryCriteria <-
      expression$PrimaryCriteria$CriteriaList %>%
      unlist() %>%
      as.list()
    
    codeSetsInPrimaryCriteria <-
      codeSetsInPrimaryCriteria[[names(codeSetsInPrimaryCriteria)[stringr::str_detect(string = names(codeSetsInPrimaryCriteria),
                                                                                      pattern = "CodesetId")]]] %>% unique()
    
    conceptSetExpression2 <- list()
    for (j in (1:nrow(conceptSetExpression))) {
      conceptSetExpression2[[j]] <- conceptSetExpression[j, ]
      conceptSetExpression2[[j]]$conceptSetExpressionSignature <-
        getConceptSetExpressionDataFrameFromConceptSetExpression(
          conceptSetExpression = conceptSetExpression2[[j]][1, ]$conceptSetExpression %>%
            RJSONIO::fromJSON(digits = 23)
        ) %>%
        dplyr::select(
          .data$conceptId,
          .data$includeDescendants,
          .data$includeMapped,
          .data$isExcluded
        ) %>%
        dplyr::distinct() %>%
        dplyr::arrange(.data$conceptId) %>%
        RJSONIO::toJSON(digits = 23, pretty = TRUE)
    }
    conceptSetExpression <- dplyr::bind_rows(conceptSetExpression2) %>% 
      dplyr::left_join(dplyr::tibble(conceptSetId = codeSetsInPrimaryCriteria) %>% 
                         dplyr::mutate(conceptSetUsedInEntryEvent = 1),
                       by = "conceptSetId") %>% 
      tidyr::replace_na(replace = list(conceptSetUsedInEntryEvent = 0))
    
    uniqueConceptSets <- conceptSetExpression %>%
      dplyr::select(.data$conceptSetExpressionSignature) %>%
      dplyr::distinct() %>%
      dplyr::mutate(uniqueConceptSetId = dplyr::row_number())
    
    conceptSetExpression <- conceptSetExpression %>%
      dplyr::left_join(uniqueConceptSets,
                       by = "conceptSetExpressionSignature") %>%
      dplyr::select(-.data$conceptSetExpressionSignature)
    
    data <- dplyr::inner_join(x = conceptSetExpression,
                              y = extractedConceptSetSql,
                              by = c("conceptSetId"))
    return(data)
  }

#' get concept set expressions from cohort expression
#'
#' @description
#' given a cohort expression (R-list object, not JSON), this function 
#' parses the list and returns the concept set components
#'
#' @template CohortExpression
#'
#' @return
#' Returns a tibble data frame with one row per concept set
#'
extractConceptSetExpressionsFromCohortExpression <-
  function(cohortExpression) {
    if ("expression" %in% names(cohortExpression)) {
      expression <- cohortExpression$expression
    } else {
      expression <- cohortExpression
    }
    conceptSetExpression <- list()
    if (length(expression$ConceptSets) > 0) {
      for (i in (1:length(expression$ConceptSets))) {
        conceptSetExpression[[i]] <-
          tidyr::tibble(
            conceptSetId = expression$ConceptSets[[i]]$id,
            conceptSetName = expression$ConceptSets[[i]]$name,
            conceptSetExpression = expression$ConceptSets[[i]]$expression$items %>% RJSONIO::toJSON(digits = 23)
          )
      }
    } else {
      conceptSetExpression <- dplyr::tibble()
    }
    return(dplyr::bind_rows(conceptSetExpression))
  }



#' get concept set sql from cohort sql
#'
#' @description
#' given a cohort sql, this function parses the sql and return the concept set components
#'
#' @param cohortSql sql of the cohort definition.
#'
#' @return
#' Returns a tibble data frame with one row per concept set
#'
extractConceptSetsSqlFromCohortSql <- function(cohortSql) {
  if (length(cohortSql) > 1) {
    stop("Please check if more than one cohort SQL was provided.")
  }
  sql <- gsub("with primary_events.*", "", cohortSql)
  
  # Find opening and closing parentheses:
  starts <- stringr::str_locate_all(sql, "\\(")[[1]][, 1]
  ends <- stringr::str_locate_all(sql, "\\)")[[1]][, 1]
  
  x <- rep(0, nchar(sql))
  x[starts] <- 1
  x[ends] <- -1
  level <- cumsum(x)
  level0 <- which(level == 0)
  
  subQueryLocations <-
    stringr::str_locate_all(sql, "SELECT [0-9]+ as codeset_id")[[1]]
  subQueryCount <- nrow(subQueryLocations)
  conceptsetSqls <- vector("character", subQueryCount)
  conceptSetIds <- vector("integer", subQueryCount)
  
  temp <- list()
  if (subQueryCount > 0) {
    for (i in 1:subQueryCount) {
      startForSubQuery <- min(starts[starts > subQueryLocations[i, 2]])
      endForSubQuery <- min(level0[level0 > startForSubQuery])
      subQuery <-
        paste(
          stringr::str_sub(sql, subQueryLocations[i, 1], endForSubQuery),
          "C"
        )
      conceptsetSqls[i] <- subQuery
      conceptSetIds[i] <- stringr::str_replace(
        subQuery,
        pattern = stringr::regex(
          pattern = "SELECT ([0-9]+) as codeset_id.*",
          ignore_case = TRUE,
          multiline = TRUE,
          dotall = TRUE
        ),
        replacement = "\\1"
      ) %>%
        utils::type.convert(as.is = TRUE)
      temp[[i]] <- tidyr::tibble(
        conceptSetId = conceptSetIds[i],
        conceptSetSql = conceptsetSqls[i]
      )
    }
  } else {
    temp <- dplyr::tibble()
  }
  return(dplyr::bind_rows(temp))
}

